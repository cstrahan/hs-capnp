{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE BangPatterns          #-}

module Data.CapnProto.Layout where

import           Control.Monad
import           Control.Monad.ST         (ST, runST)
import           Data.Array.ST            (MArray, STUArray, newArray,
                                           readArray)
import           Data.Array.Unsafe        (castSTUArray)
import           Data.Bits
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.ByteString.Internal as BS (fromForeignPtr, memset)
import           Data.Int
import           Data.Monoid
import           Data.Word
import           Foreign.C.Types          (CChar, CSize)
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array    hiding (newArray)
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO
import           Data.IORef
import           Text.Printf

import           Data.CapnProto.Arena
import           Data.CapnProto.Units

--------------------------------------------------------------------------------

debug = hPutStrLn stderr

debugStructReader reader = do
    debug $ "    SR-data/ptrs-diff: "<>show (cast (structReaderPointers reader) `minusPtr` cast (structReaderData reader))
    debug $ "    SR-data-size     : "<>show (structReaderDataSize reader)
    debug $ "    SR-ptr-count     : "<>show (structReaderPtrCount reader)
  where
    cast p = castPtr p :: Ptr Word8

debugSegment :: Segment Reader -> IO ()
debugSegment reader = withSegment reader $ \ptr -> do
    bytes <- peekArray (fromIntegral (segmentSize reader) * bytesPerWord) (castPtr ptr) :: IO [Word8]
    let hex = unwords (map (printf "%02x") bytes)
    debug $ "SEGMENT: " <> hex

--------------------------------------------------------------------------------

data MessageSize = MessageSize
  { wordCount :: Word64
  , capCount :: Word64
  }

data ElementSize =
    SzVoid
  | SzBit
  | SzByte
  | SzTwoBytes
  | SzFourBytes
  | SzEightBytes
  | SzPointer
  | SzInlineComposite
  deriving (Show, Enum, Eq)

newtype TextReader = TextReader
  { textReaderData :: Maybe BS.ByteString
  }

newtype DataReader = DataReader
  { dataReaderData :: Maybe BS.ByteString
  }

data StructReader = StructReader
  { structReaderSegment  :: Segment Reader
  , structReaderData     :: Ptr Word8
  , structReaderPointers :: Ptr WirePointer
  , structReaderDataSize :: BitCount32
  , structReaderPtrCount :: WirePointerCount16
  }

data StructBuilder = StructBuilder
  { structBuilderSegment  :: Segment Builder
  , structBuilderData     :: Ptr Word8
  , structBuilderPointers :: Ptr WirePointer
  , structBuilderDataSize :: BitCount32
  , structBuilderPtrCount :: WirePointerCount16
  }

data UntypedListReader = UntypedListReader
  { untypedListReaderSegment        :: Segment Reader
  , untypedListReaderData           :: Ptr Word8
  , untypedListReaderElementCount   :: ElementCount32
  , untypedListReaderStep           :: BitCount32
  , untypedListReaderStructDataSize :: BitCount32
  , untypedListReaderStructPtrCount :: WirePointerCount16
  }

data StructRef = StructRef
    { structRefDataSize :: WordCount16
    , structRefPtrCount :: WirePointerCount16
    } deriving (Show)

wordSize :: StructRef -> WordCount32
wordSize ref = fromIntegral (structRefDataSize ref) + fromIntegral (structRefPtrCount ref)

data ListRef = ListRef
    { listRefElementSize              :: ElementSize
    , listRefElementCount             :: ElementCount32
    , listRefInlineCompositeWordCount :: WordCount32
    } deriving (Show)

toListRef :: WirePointer -> ListRef
toListRef ptr = ListRef (toEnum (fromIntegral ((upper32Bits ptr) .&. 7)))
                        ((upper32Bits ptr) `shiftR` 3)
                        ((upper32Bits ptr) `shiftR` 3)

toStructRef :: WirePointer -> StructRef
toStructRef ptr = StructRef (fromIntegral (upper32Bits ptr))
                            (fromIntegral ((upper32Bits ptr) `shiftR` 16))

--------------------------------------------------------------------------------

class Union a where
    type UnionTy a :: *
    which :: a -> IO (UnionTy a)

--------------------------------------------------------------------------------

data WirePointerKind =
    Struct
  | List
  | Far
  | Other
  deriving (Show, Eq, Enum)

-- | Little-endian encoded value.
-- TODO: handle endianness correctly
type WireValue a = a

data PointerReader = PointerReader
  { pointerReaderSegment :: Segment Reader
  , pointerReaderData    :: Ptr WirePointer
  }

data WirePointer = WirePointer
  { offsetAndKind :: WireValue Word32
  , upper32Bits   :: Word32
  } deriving (Show)

instance Nullable WirePointer where
    isNull ptr = offsetAndKind ptr == 0 && upper32Bits ptr == 0

nullWirePointer :: WirePointer
nullWirePointer = WirePointer { offsetAndKind = 0, upper32Bits = 0 }

instance Storable WirePointer where
    sizeOf _ = 8
    alignment _ = 8
    poke ptr (WirePointer a b) =
        let p0 = castPtr ptr :: Ptr (WireValue Word32)
            p1 = castPtr $ p0 `advancePtr` 1
        in poke p0 a >> poke p1 b
    peek ptr =
        let p0 = castPtr ptr :: Ptr (WireValue Word32)
            p1 = castPtr $ p0 `advancePtr` 1
        in WirePointer <$> peek p0 <*> peek p1

wirePointerKind :: WirePointer -> WirePointerKind
wirePointerKind ptr =
    case (offsetAndKind ptr) .&. 3 of
        0 -> Struct
        1 -> List
        2 -> Far
        3 -> Other

nonFarOffset :: WirePointer -> Int
nonFarOffset = (`shiftR` 2) . fromIntegral . offsetAndKind

wirePointerTarget :: Ptr WirePointer -> IO (Ptr CPWord)
wirePointerTarget ref = do
    wptr <- peek ref
    return $ (castPtr ref :: Ptr CPWord) `advancePtr` (nonFarOffset wptr + 1)

farPositionInSegment :: WirePointer -> WordCount32
farPositionInSegment = (`shiftR` 3) . offsetAndKind

isDoubleFar :: WirePointer -> Bool
isDoubleFar ptr = (offsetAndKind ptr .&. 4) /= 0

wptrSegmentId :: WirePointer -> SegmentId
wptrSegmentId ptr = fromIntegral (upper32Bits ptr)

inlineCompositeListElementCount :: WirePointer -> ElementCount32
inlineCompositeListElementCount ptr = offsetAndKind ptr `shiftR` 2

isCapability :: WirePointer -> Bool
isCapability wptr = wirePointerKind wptr == Other

containsInterval :: Segment Reader -> Ptr CPWord -> Ptr CPWord -> Bool
containsInterval segment from to =
    let thisBegin = unsafeSegmentPtr segment
        thisEnd = thisBegin `advancePtr` fromIntegral (segmentSize segment)
    in from >= thisBegin && to <= thisEnd && from <= to

setKindAndTargetForEmptyStruct :: WirePointer -> WirePointer
setKindAndTargetForEmptyStruct wptr = wptr
    { offsetAndKind = 0xfffffffc
    }

setOffset :: WirePointer -> Int -> WirePointer
setOffset wptr offset = wptr
    { offsetAndKind = (fromIntegral offset `shiftL` 2)  .|. (offsetAndKind wptr .&. 3)
    }

setOffsetAndKind :: WirePointer -> Int -> WirePointerKind -> WirePointer
setOffsetAndKind wptr offset kind = wptr
    { offsetAndKind = (fromIntegral offset `shiftL` 2)  .|. (fromIntegral $ fromEnum kind)
    }

calculateTargetOffset :: Ptr WirePointer -> Ptr CPWord -> Int
calculateTargetOffset ref target = ((target `minusPtr` ref) `div` bytesPerWord) - 1

setFar :: WirePointer -> Bool -> WordCount32 -> SegmentId -> WirePointer
setFar wptr doubleFar pos id = wptr
    { upper32Bits = pos
    , offsetAndKind =  (pos `shiftL` 3)
                   .|. (if doubleFar then 1 `shiftL` 2 else 0)
                   .|. (fromIntegral $ fromEnum (wirePointerKind wptr)) }

--------------------------------------------------------------------------------

getRoot :: Segment Reader -> Ptr CPWord -> Either String PointerReader
getRoot segment location =
    case boundsCheck segment location (location `plusPtr` pointerSizeInWords) Struct of
        Left msg -> Left msg
        Right _ -> Right $ PointerReader segment (castPtr location)

--------------------------------------------------------------------------------
-- wire helpers

roundBytesUpToWords :: ByteCount32 -> WordCount32
roundBytesUpToWords bytes = fromIntegral $ (bytes + 7) `div` fromIntegral bytesPerWord

roundBitsUpToWords :: BitCount64 -> WordCount32
roundBitsUpToWords bits = fromIntegral $ (bits + 63) `div` fromIntegral bitsPerWord

roundBitsUpToBytes :: BitCount64 -> ByteCount32
roundBitsUpToBytes bytes = fromIntegral $ (bytes + 7) `div` fromIntegral bitsPerByte

boundsCheck :: Segment Reader -> Ptr CPWord -> Ptr CPWord -> WirePointerKind -> Either String ()
boundsCheck segment start end kind =
    if isNull segment || containsInterval segment start end
      then Right ()
      else Left $ case kind of
                   List -> "Message contained out-of-bounds list pointer."
                   Struct -> "Message contained out-of-bounds struct pointer."
                   Far -> "Message contained out-of-bounds far pointer."
                   Other -> "Message contained out-of-bounds other pointer."

--------------------------------------------------------------------------------

allocate :: Ptr WirePointer -> Segment Builder -> WordCount32 -> WirePointerKind
         -> IO ( Ptr WirePointer -- The wire-ptr ref
               , Ptr CPWord      -- The content
               , Segment Builder -- The segment builder
               )
allocate ref segment amount kind = withSegment segment $ \_ -> do
    null <- wirePtrRefIsNull ref
    unless null $
        zeroObject segment ref

    if amount == 0 && kind == Struct
      then do
          wptr <- peek ref
          poke ref (setKindAndTargetForEmptyStruct wptr)
          return (ref, castPtr ref, segment)
      else segmentAllocate segment amount >>= \case
          Nothing -> do
              -- Need to allocate in a new segment. We'll need to
              -- allocate an extra pointer worth of space to act as
              -- the landing pad for a far pointer.
              let amountPlusRef = amount + fromIntegral pointerSizeInWords
              (segment, ptr)  <- arenaAllocate (segmentArena segment) amountPlusRef

              withSegment segment $ \_ -> do
                  -- Set up the original pointer to be a far pointer to
                  -- the new segment.
                  wptr <- peek ref
                  poke ref (setFar wptr False (getWordOffsetTo segment ptr) (segmentId segment))

                  -- Initialize the landing pad to indicate that the
                  -- data immediately follows the pad.
                  let ref = castPtr ptr
                      ptr = ptr `plusPtr` pointerSizeInWords
                      offset = calculateTargetOffset ref ptr
                  wptr <- peek ref
                  poke ref (setOffsetAndKind wptr offset kind)

                  return (ref, ptr, segment)
          Just ptr -> do
              wptr <- peek ref
              let offset = calculateTargetOffset ref ptr
              poke ref (setOffsetAndKind wptr offset kind)
              return (ref, ptr, segment)

followBuilderFars :: Ptr WirePointer -> Segment Builder
                  -> IO ( Ptr WirePointer
                        , Ptr CPWord
                        , Segment Builder
                        )
followBuilderFars ref segment = do
    wirePtr <- peek ref
    if wirePointerKind wirePtr /= Far
      then do
          contentPtr <- wirePointerTarget ref
          return (ref, contentPtr, segment)
      else do
          segment <- tryGetSegment (segmentArena segment) (wptrSegmentId wirePtr)
          withSegment segment $ \_ -> do
              let landingPad = getPtrUnchecked segment (farPositionInSegment wirePtr)

              if not (isDoubleFar wirePtr)
                then do
                    let ref = castPtr landingPad
                    content <- wirePointerTarget ref
                    return (ref, content, segment)
                else do
                    -- Landing pad is another far pointer. It is followed by a
                    -- tag describing the pointed-to object.
                    let ref = castPtr landingPad
                    wirePtr <- peek ref
                    let ref = landingPad `plusPtr` sizeOf (undefined :: WirePointer)
                    segment <- tryGetSegment (segmentArena segment) (wptrSegmentId wirePtr)
                    let content = getPtrUnchecked segment (farPositionInSegment wirePtr)
                    return (ref, content, segment)

followFars :: Ptr WirePointer -> Segment Reader
           -> IO ( WirePointer    -- The resolved non-far-pointer
                 , Ptr CPWord       -- The pointer the the actual content
                 , Segment Reader -- The target segment, in the case of far-pointers
                 )
followFars ref segment = do
    wirePtr <- peek ref
    -- If the segment is null, this is an unchecked message, so there are no FAR pointers.
    if (isNull segment) || wirePointerKind wirePtr /= Far
      then do
          let contentPtr = ((castPtr ref :: Ptr CPWord) `advancePtr` (nonFarOffset wirePtr + 1))
          return (wirePtr, contentPtr, segment)
      else do
          segment' <- tryGetSegment (segmentArena segment) (wptrSegmentId wirePtr)
          let padWords = if isDoubleFar wirePtr then 2 else 1
          withSegment segment' $ \segmentPtr -> do
              let landingPad = (castPtr segmentPtr :: Ptr WirePointer) `advancePtr` (fromIntegral (farPositionInSegment wirePtr))
              -- XXX bounds check
              if isDoubleFar wirePtr
                then do
                    -- Landing pad is another far pointer. It is
                    -- followed by a tag describing the pointed-to
                    -- object.
                    wirePtr' <- peek landingPad
                    segment'' <- tryGetSegment (segmentArena segment') (wptrSegmentId wirePtr')
                    contentPtr <- withSegment segment'' (return . (`advancePtr` fromIntegral (farPositionInSegment wirePtr')))
                    finalWirePtr <- peek $ landingPad `advancePtr` 1
                    return (finalWirePtr, contentPtr, segment'')
                else do
                    wirePtr' <- peek landingPad
                    let contentPtr = (castPtr landingPad :: Ptr CPWord) `advancePtr` (nonFarOffset wirePtr' + 1)
                    finalWirePtr <- peek $ landingPad
                    return (finalWirePtr, contentPtr, segment')

zeroObject :: Segment Builder -> Ptr WirePointer -> IO ()
zeroObject segment ref = do
    wptr <- peek ref
    let common = zeroObjectHelper segment ref =<< wirePointerTarget ref
    case wirePointerKind wptr of
        Struct -> common
        List -> common
        Other -> common
        Far -> do
            segment <- tryGetSegment (segmentArena segment) (wptrSegmentId wptr)
            let pad = castPtr $ getPtrUnchecked segment (farPositionInSegment wptr) :: Ptr WirePointer
            if isDoubleFar wptr
              then do
                  wptr <- peek pad
                  segment <- tryGetSegment (segmentArena segment) (wptrSegmentId wptr)

                  let content = getPtrUnchecked segment (farPositionInSegment wptr)

                  zeroObjectHelper segment (pad `advancePtr` 1) content

                  poke (castPtr pad :: Ptr CPWord) 0
                  poke ((castPtr pad :: Ptr CPWord) `advancePtr` 1) 0
              else do
                  zeroObject segment pad
                  poke (castPtr pad :: Ptr CPWord) 0

zeroObjectHelper :: Segment Builder -> Ptr WirePointer -> Ptr CPWord -> IO ()
zeroObjectHelper segment tag ptr = do
    wptr <- peek tag
    case wirePointerKind wptr of
        Other -> fail "Don't know how to handle OTHER"
        Struct -> do
            let dataSize = structRefDataSize (toStructRef wptr)
                pointerSection = castPtr $ ptr `advancePtr` fromIntegral dataSize :: Ptr WirePointer
                pointerCount = structRefPtrCount (toStructRef wptr)
                wordSize = dataSize + pointerCount
            loop 0 pointerCount $ \i ->
                zeroObject segment (pointerSection `advancePtr` fromIntegral i)
            zeroArray ptr (fromIntegral wordSize) 
        List -> do
            let elemSize = listRefElementSize (toListRef wptr)
                elemCount = listRefElementCount (toListRef wptr)
                listWords = elemCount * (roundBitsUpToWords $ fromIntegral $ dataBitsPerElement elemSize)
            case listRefElementSize (toListRef wptr) of
                SzVoid -> return ()
                SzBit -> zeroArray ptr (fromIntegral listWords)
                SzByte -> zeroArray ptr (fromIntegral listWords)
                SzTwoBytes -> zeroArray ptr (fromIntegral listWords)
                SzFourBytes -> zeroArray ptr (fromIntegral listWords)
                SzEightBytes -> zeroArray ptr (fromIntegral listWords)
                SzPointer -> do
                    loop 0 elemCount $ \i ->
                        zeroObject segment (castPtr ptr `advancePtr` fromIntegral i)
                    zeroArray ptr (fromIntegral elemCount)
                SzInlineComposite -> do
                    let elementTag = castPtr ptr :: Ptr WirePointer
                    wptr <- peek elementTag
                    let dataSize = (structRefDataSize $ toStructRef wptr)
                        pointerCount = (structRefPtrCount $ toStructRef wptr)
                        count = inlineCompositeListElementCount wptr
                        wordSize = dataSize + pointerCount
                        elemKind = wirePointerKind wptr
                    when (elemKind /= Struct) $
                        fail "Don't know how to handle non-STRUCT inline composite"
                    loopFold 0 count (ptr `advancePtr` 1) $ \pos _ -> do
                        let pos = pos `advancePtr` fromIntegral dataSize :: Ptr CPWord
                        loopFold 0 pointerCount pos $ \pos _ -> do
                            zeroObject segment (castPtr pos)
                            return $ pos `advancePtr` 1
                    zeroArray ptr (fromIntegral wordSize * fromIntegral count + 1)
        Far -> fail "Unexpected FAR pointer"

zeroPointerAndFars :: Segment Builder -> Ptr WirePointer -> IO ()
zeroPointerAndFars segment ref = do
    wptr <- peek ref
    when (wirePointerKind wptr == Far) $ do
        pad <- tryGetSegment (segmentArena segment) (wptrSegmentId wptr) >>= \segment ->
            return $ getPtrUnchecked segment (farPositionInSegment wptr)
        let numElements = if isDoubleFar wptr then 2 else 1
        zeroArray pad numElements
    poke (castPtr ref :: Ptr CPWord) 0

plusMessageSize :: MessageSize -> MessageSize -> MessageSize
plusMessageSize (MessageSize a1 b1) (MessageSize a2 b2) = MessageSize (a1 + a2) (b1 + b2)

totalSize :: Segment Reader -> Ptr WirePointer -> IO MessageSize
totalSize segment ref = do
    null <- wirePtrRefIsNull ref
    if null
      then return MessageSize { wordCount = 0, capCount = 0 }
      else do
          (wptr, ptr, segment) <- followFars ref segment
          case wirePointerKind wptr of
              Struct -> do
                  -- XXX bounds check
                  let dataSize = structRefDataSize (toStructRef wptr)
                      init = MessageSize { wordCount = fromIntegral dataSize, capCount = 0 }
                      pointerSection = castPtr $ ptr `advancePtr` fromIntegral dataSize :: Ptr WirePointer
                      count = structRefPtrCount (toStructRef wptr)

                  loopFold 0 count init $ \msize i ->
                    (msize `plusMessageSize`) <$> totalSize segment (pointerSection `advancePtr` fromIntegral i)
              List -> do
                  let elemCount = listRefElementCount (toListRef wptr)
                      elemSize = listRefElementSize (toListRef wptr)
                      totalWords = roundBitsUpToWords (fromIntegral elemCount * fromIntegral (dataBitsPerElement elemSize))
                      commonCase =
                          --XXX bounds check
                          return MessageSize { wordCount = fromIntegral totalWords, capCount = 0 }

                  case listRefElementSize (toListRef wptr) of
                      SzVoid -> return MessageSize { wordCount = 0, capCount = 0 }
                      SzBit -> commonCase
                      SzByte -> commonCase
                      SzTwoBytes -> commonCase
                      SzFourBytes -> commonCase
                      SzEightBytes -> commonCase
                      SzPointer -> do
                          --XXX bounds check
                          let init = MessageSize { wordCount = fromIntegral elemCount, capCount = 0 }
                          loopFold 0 elemCount init $ \msize i ->
                            (msize `plusMessageSize`) <$> totalSize segment (castPtr ptr `advancePtr` fromIntegral i)
                      SzInlineComposite -> do
                          let wordCount = listRefInlineCompositeWordCount (toListRef wptr)
                              init = MessageSize { wordCount = fromIntegral wordCount, capCount = 0 }
                          if wordCount == 0
                            then return init
                            else do
                                let wordCount = listRefInlineCompositeWordCount $ toListRef wptr
                                    elementTag = castPtr ptr :: Ptr WirePointer
                                wptr <- peek elementTag
                                let count = inlineCompositeListElementCount wptr
                                    dataSize = structRefDataSize $ toStructRef wptr
                                    pointerCount = structRefPtrCount $ toStructRef wptr
                                    wordSize = dataSize + pointerCount

                                when (wirePointerKind wptr /= Struct) $
                                  fail "Don't know how to handle non-STRUCT inline composite."

                                when (fromIntegral wordSize * count > wordCount) $
                                  fail "InlineComposite list's elements overrun its word count."

                                let pos = ptr `advancePtr` pointerSizeInWords
                                (msize, _) <- loopFold 0 elemCount (init, pos) $ \(msize, pos) _ -> do
                                    let pos = pos `advancePtr` fromIntegral dataSize
                                    loopFold 0 pointerCount (init, pos) $ \(msize, pos) _ -> do
                                        msize' <- (msize `plusMessageSize`) <$> totalSize segment (castPtr pos)
                                        return (msize', pos `advancePtr` pointerSizeInWords)
                                return msize
              Far -> fail "Unexpected FAR pointer."
              Other ->
                  if isCapability wptr
                    then return MessageSize { wordCount = 0, capCount = 1 }
                    else fail "Unknown pointer type."



-- This
--   ::std::ptr::copy_nonoverlapping(src, dst, count);
-- Is the same as
--   copyArray dst src count

transferPointer :: Segment Builder -> Ptr WirePointer -> Segment Builder -> Ptr WirePointer -> IO ()
transferPointer dstSegment dst srcSegment src = do
    wptr <- peek src
    if isNull wptr
      then poke (castPtr dst :: Ptr CPWord) 0
      else if wirePointerKind wptr == Far
        then copyArray dst src 1
        else do
            target <- wirePointerTarget src
            transferPointerSplit dstSegment dst srcSegment src target

transferPointerSplit :: Segment Builder -> Ptr WirePointer -> Segment Builder -> Ptr WirePointer -> Ptr CPWord -> IO ()
transferPointerSplit dstSegment dst srcSegment srcTag srcPtr =
    if dstSegment == srcSegment
      then do
          srcWptr <- peek srcTag
          dstWptr <- peek dst
          let offset = calculateTargetOffset dst srcPtr
          poke dst (setOffsetAndKind dstWptr offset (wirePointerKind srcWptr)) {
                upper32Bits = upper32Bits srcWptr
              }
      else
          segmentAllocate srcSegment 1 >>= \case
            Nothing -> fail "unimplemented" -- XXX need a double-far
            Just landingPadWord -> do
                srcWptr <- peek srcTag
                dstWptr <- peek dst
                let landingPad = castPtr landingPadWord :: Ptr WirePointer
                    offset = calculateTargetOffset landingPad srcPtr
                poke landingPad (setOffsetAndKind nullWirePointer offset (wirePointerKind srcWptr)) {
                        upper32Bits = upper32Bits srcWptr
                    }
                poke dst (setFar dstWptr False (getWordOffsetTo srcSegment landingPadWord) (segmentId srcSegment))

data StructSize = StructSize
  { structSizeData :: WordCount16
  , structSizePointers :: WirePointerCount16
  }

structSizeTotal :: StructSize -> WordCount32
structSizeTotal (StructSize x y) = fromIntegral x + fromIntegral y

initStructPointer :: Ptr WirePointer -> Segment Builder -> StructSize -> IO StructBuilder
initStructPointer ref segment size = do
    (ref, ptr, segment) <- allocate ref segment (structSizeTotal size) Struct
    return $
      StructBuilder
        segment
        (castPtr ptr)
        (castPtr $ ptr `advancePtr` (fromIntegral $ structSizeData size))
        ((fromIntegral $ structSizeData size) * fromIntegral bitsPerWord)
        (structSizePointers size)

-- TODO: assert ptr > segPtr
getWordOffsetTo :: Segment Builder -> Ptr a -> WordCount32
getWordOffsetTo segment ptr = fromIntegral wordCount
  where
    byteOffset = ptr `minusPtr` unsafeSegmentPtr segment
    wordCount = byteOffset `div` bytesPerWord

--------------------------------------------------------------------------------

getData :: PointerReader -> Ptr CPWord -> ByteCount32 -> IO DataReader
getData reader =
    readDataPointer (pointerReaderSegment reader) (pointerReaderData reader)

readDataPointer :: Segment Reader -> Ptr WirePointer -> Ptr CPWord -> ByteCount32 -> IO DataReader
readDataPointer segment ref defaultValue defaultSize = withSegment segment $ \_ -> do
    pred <- wirePtrRefIsNull ref
    if pred
      then do
          pred <- wirePtrRefIsNull (castPtr defaultValue)
          if pred
            then return $ DataReader Nothing
            else do
                p <- newForeignPtr_ defaultValue
                let bs = BS.fromForeignPtr (castForeignPtr p) 0 (fromIntegral defaultSize)
                return $ DataReader $ Just $ bs
      else do
          (wirePtr, content, segment) <- followFars ref segment
          let listRef = toListRef wirePtr
              size = listRefElementCount listRef
          when (wirePointerKind wirePtr /= List) $
            fail "Message contains non-list pointer where data was expected."
          when (listRefElementSize listRef /= SzByte) $
            fail "Message contains list pointer of non-bytes where text was expected."

          -- XXX bounds check

          let bs = BS.fromForeignPtr (segmentForeignPtr segment) (content `minusPtr` unsafeSegmentPtr segment) (fromIntegral size)
          return $ DataReader $ Just bs

getText :: PointerReader -> Ptr CPWord -> ByteCount32 -> IO TextReader
getText reader =
    readTextPointer (pointerReaderSegment reader) (pointerReaderData reader)

readTextPointer :: Segment Reader -> Ptr WirePointer -> Ptr CPWord -> ByteCount32 -> IO TextReader
readTextPointer segment ref defaultValue defaultSize = withSegment segment $ \sptr -> do
    pred <- wirePtrRefIsNull ref
    if pred
      then do
          pred <- wirePtrRefIsNull (castPtr defaultValue)
          if pred
            then return $ TextReader Nothing
            else do
                p <- newForeignPtr_ defaultValue
                let bs = BS.fromForeignPtr (castForeignPtr p) 0 (fromIntegral defaultSize)
                return $ TextReader $ Just $ bs
      else do
          (wirePtr, content, segment) <- followFars ref segment
          let listRef = toListRef wirePtr
              size = listRefElementCount listRef

          when (wirePointerKind wirePtr /= List) $
            fail "Message contains non-list pointer where text was expected."
          when (listRefElementSize listRef /= SzByte) $
            fail "Message contains list pointer of non-bytes where text was expected."

          -- XXX bounds check

          when (size <= 0) $
            fail "Message contains text that is not NUL-terminated."

          let strPtr = castPtr content :: Ptr CChar
          lastByte <- peek (strPtr `plusPtr` (fromIntegral size - 1)) :: IO CChar
          when (lastByte /= 0) $
            fail "Message contains text that is not NUL-terminated"

          let bs = BS.fromForeignPtr (segmentForeignPtr segment) (content `minusPtr` unsafeSegmentPtr segment) (fromIntegral size - 1)

          return $ TextReader $ Just bs

getList :: PointerReader -> ElementSize -> Ptr CPWord -> IO UntypedListReader
getList reader expectedSize =
    readListPointer (pointerReaderSegment reader) expectedSize (pointerReaderData reader)

readListPointer :: Segment Reader -> ElementSize -> Ptr WirePointer -> Ptr CPWord -> IO UntypedListReader
readListPointer segment expectedSize ref defaultValue = withSegment segment $ \_ -> do
    pred <- wirePtrRefIsNull ref
    if pred
      then do
          pred <- wirePtrRefIsNull $ castPtr defaultValue
          if pred
            then return defaultUntypedListReader
            else fail "Data.CapnProto.Layout.readListPointer: not implemented"
      else do
          (wirePtr, content, segment) <- followFars ref segment
          when (wirePointerKind wirePtr /= List) $
            {- fail "Message contains non-list pointer where list pointer was expected" -}
            fail $ "Message contains non-list pointer where list pointer was expected: "<>show (wirePointerKind wirePtr)
          let listRef = toListRef wirePtr
              elemSize = listRefElementSize listRef
          case elemSize of
              SzInlineComposite -> do
                  tag <- peek (castPtr content :: Ptr WirePointer)
                  content <- return $ content `plusPtr` sizeOf (undefined :: WirePointer)
                  -- XXX bounds check
                  when (wirePointerKind tag /= Struct) $
                    fail "InlineComposite lists of non-STRUCT type are not supported."

                  let size = inlineCompositeListElementCount tag
                      structRef = toStructRef tag
                      wordsPerElement = wordSize structRef
                      wordCount = listRefInlineCompositeWordCount listRef

                  when (size * wordsPerElement > wordCount) $
                    fail "InlineComposite list's elements overrun its word count."

                  let assertSizeGreaterThanZero =
                          when (structRefDataSize structRef <= 0) $
                            fail "Expected a primitive list, but got a list of pointer-only structs"
                      assertPtrCountGreaterThanZero =
                          when (structRefPtrCount structRef <= 0) $
                            fail "Expected a pointer list, but got a list of data-only structs"
                  case expectedSize of
                      SzVoid -> return ()
                      SzBit -> fail "Found struct list where bit list was expected."
                      SzByte -> assertSizeGreaterThanZero
                      SzTwoBytes -> assertSizeGreaterThanZero
                      SzFourBytes -> assertSizeGreaterThanZero
                      SzEightBytes -> assertSizeGreaterThanZero
                      SzPointer -> assertPtrCountGreaterThanZero
                      SzInlineComposite -> return ()

                  -- We expected a list of pointers but got a list of structs. Assuming the
                  -- first field in the struct is the pointer we were looking for, we want to
                  -- munge the pointer to point at the first element's pointer section.
                  content <- if expectedSize == SzPointer
                               then return $ content `advancePtr` (fromIntegral $ structRefDataSize structRef)
                               else return content

                  return $ UntypedListReader
                    segment
                    content
                    size
                    (wordsPerElement * fromIntegral bitsPerWord)
                    (fromIntegral (structRefDataSize structRef) * fromIntegral bitsPerWord)
                    (structRefPtrCount structRef)
              _ -> do
                  let dataSize = dataBitsPerElement elemSize
                      pointerCount = pointersPerElement elemSize
                      elemCount = listRefElementCount listRef
                      step = dataSize + pointerCount * fromIntegral bitsPerPointer
                      wordCount = roundBitsUpToWords (fromIntegral elemCount * fromIntegral step)

                  -- XXX bounds check

                  let expectedDataBitsPerElement = dataBitsPerElement expectedSize
                      expectedPointersPerElement = pointersPerElement expectedSize

                  when (expectedDataBitsPerElement > dataSize || expectedPointersPerElement > pointerCount) $
                    fail "Message contains list with incompatible element type."

                  return $ UntypedListReader
                    segment
                    (castPtr content)
                    elemCount
                    step
                    dataSize
                    (fromIntegral pointerCount)

getStruct :: PointerReader -> Ptr CPWord -> IO StructReader
getStruct reader = readStructPointer (pointerReaderSegment reader) (pointerReaderData reader)

readStructPointer :: Segment Reader -> Ptr WirePointer -> Ptr CPWord -> IO StructReader
readStructPointer segment ref defaultValue = withSegment segment $ \_ ->
    if isNull ref
      then do
          pred <- wirePtrRefIsNull $ castPtr defaultValue
          if pred
            then return defaultStructReader
            else fail "Data.CapnProto.Layout.readStructPointer: not implemented"
      else do
          (wirePtr, content, segment) <- followFars ref segment
          let dataWords = structRefDataSize . toStructRef $ wirePtr
              numPtrs = structRefPtrCount . toStructRef $ wirePtr
              ptrs = castPtr $ content `advancePtr` fromIntegral dataWords :: Ptr WirePointer
          -- XXX bounds check
          return $ StructReader segment (castPtr content) ptrs (fromIntegral dataWords * fromIntegral bitsPerWord) numPtrs

wirePtrRefIsNull :: Ptr WirePointer -> IO Bool
wirePtrRefIsNull ref =
    if ref == nullPtr
      then return True
      else do
          wirePtr <- peek ref
          return $ isNull wirePtr

pointersPerElement :: ElementSize -> WirePointerCount32
pointersPerElement size =
    case size of
        SzPointer -> 1
        _ -> 0

dataBitsPerElement :: ElementSize -> BitCount32
dataBitsPerElement size =
    case size of
        SzVoid -> 0
        SzBit -> 1
        SzByte -> 8
        SzTwoBytes -> 16
        SzFourBytes -> 32
        SzEightBytes -> 64
        SzPointer -> 0
        SzInlineComposite -> 0

instance Nullable StructReader where
    isNull reader = structReaderData reader == nullPtr

class FromPointerReader a where
    fromPointerReader :: PointerReader -> IO a

instance FromPointerReader PointerReader where
    fromPointerReader = return

class FromStructReader a where
    fromStructReader :: StructReader -> IO a

instance FromStructReader StructReader where
    fromStructReader = return


class StructField a where
    type DefaultTy a :: *
    type DefaultTy a = a

    getField :: StructReader -> ElementCount -> IO a
    getField' :: StructReader -> ElementCount -> DefaultTy a -> IO a

    default getField :: (Storable a, Zero a) => StructReader -> ElementCount -> IO a
    getField = getDataField

    default getField' :: (Storable a, Zero a, Bits a, DefaultTy a ~ a) => StructReader -> ElementCount -> DefaultTy a -> IO a
    getField' struct index def = fmap (`xor` def) (getField struct index)

instance StructField () where
    getField _ _ = return ()
    getField' _ _ _ = return ()

instance StructField Bool where
    type DefaultTy Bool = Bool
    getField = getBoolField
    getField' struct index def = do
        val <- getField struct index
        return $ (val || def) && (not (val && def))

instance StructField Word8
instance StructField Word16
instance StructField Word32
instance StructField Word64
instance StructField Int8
instance StructField Int16
instance StructField Int32
instance StructField Int64

instance StructField Float where
    type DefaultTy Float = Word32
    getField' struct index def = do
        val <- getField struct index
        return $ wordToFloat (floatToWord val `xor` def)

instance StructField Double where
    type DefaultTy Double = Word64
    getField' struct index def = do
        val <- getField struct index
        return $ wordToDouble (doubleToWord val `xor` def)

instance StructField TextReader where
    type DefaultTy TextReader = Ptr WirePointer
    getField reader index = getField' reader index nullPtr
    getField' reader index def = getText ptrReader (castPtr def) 0
      where
        ptrReader = getPointerField reader (fromIntegral index)

instance StructField DataReader where
    type DefaultTy DataReader = Ptr WirePointer
    getField reader index = getField' reader index nullPtr
    getField' reader index def = getData ptrReader (castPtr def) 0
      where
        ptrReader = getPointerField reader (fromIntegral index)

{- instance StructField PointerReader where -}
    {- getField reader index = return $ getPointerField reader (fromIntegral index) -}

instance StructField StructReader where
    type DefaultTy StructReader = Ptr WirePointer
    getField reader index = getField' reader index nullPtr
    getField' reader index def = getStruct ptrReader (castPtr def)
      where
        ptrReader = getPointerField reader (fromIntegral index)

instance (ListElement a) => StructField (ListReader a) where
    type DefaultTy (ListReader a) = Ptr WirePointer
    getField reader index = getField' reader index nullPtr
    getField' = getFieldHack undefined
      where
        getFieldHack :: (ListElement a) => a -> StructReader -> ElementCount -> DefaultTy (ListReader a) -> IO (ListReader a)
        getFieldHack dummy reader index def = ListReader <$> getList ptrReader (elementSize dummy) (castPtr def)
          where
            ptrReader = getPointerField reader (fromIntegral index)

---------------------

getDataField :: (Storable a, Zero a) => StructReader -> ElementCount -> IO a
getDataField = getDataFieldHack undefined
  where
    getDataFieldHack :: (Storable a, Zero a) => a -> StructReader -> ElementCount -> IO a
    getDataFieldHack dummy reader offset = withSegment (structReaderSegment reader) $ \_ ->
        if bitOffset <= dataBits
          then peekElemOff (castPtr (structReaderData reader)) (fromIntegral offset)
          else return zero
      where
        bitOffset = (fromIntegral offset + 1) * (sizeOf dummy * bitsPerByte)
        dataBits = fromIntegral (structReaderDataSize reader)

getBoolField :: StructReader -> ElementCount -> IO Bool
getBoolField reader offset = withSegment (structReaderSegment reader) $ \_ ->
    if bitOffset < structReaderDataSize reader
      then do
          byte <- peekElemOff (structReaderData reader) (fromIntegral q) :: IO Word8
          return $ byte .&. (1 `shiftL` (fromIntegral r)) /= 0
      else return False
  where
    bitOffset = fromIntegral offset
    (q,r) = bitOffset `quotRem` fromIntegral bitsPerByte

getPointerField :: StructReader -> WirePointerCount -> PointerReader
getPointerField reader ptrIndex =
    if (fromIntegral ptrIndex) < structReaderPtrCount reader
        then PointerReader
               (structReaderSegment reader)
               (structReaderPointers reader `advancePtr` fromIntegral ptrIndex)
        else defaultPointerReader

--------------------------------------------------------------------------------

defaultStructReader :: StructReader
defaultStructReader = StructReader nullSegment nullPtr nullPtr 0 0

defaultPointerReader :: PointerReader
defaultPointerReader = PointerReader nullSegment nullPtr

defaultUntypedListReader :: UntypedListReader
defaultUntypedListReader = UntypedListReader nullSegment nullPtr 0 0 0 0

--------------------------------------------------------------------------------

getStructElement :: UntypedListReader -> ElementCount32 -> StructReader
getStructElement reader index =
    StructReader (untypedListReaderSegment reader)
                 structData
                 structPointers
                 (untypedListReaderStructDataSize reader)
                 (untypedListReaderStructPtrCount reader)
  where
    indexBit = fromIntegral index * fromIntegral (untypedListReaderStep reader) :: BitCount64
    structData = untypedListReaderData reader `plusPtr` (fromIntegral (indexBit `div` fromIntegral bitsPerByte)) :: Ptr Word8
    structPointers = structData `plusPtr` (fromIntegral (untypedListReaderStructDataSize reader `div` fromIntegral bitsPerByte))

getPointerElement :: UntypedListReader -> ElementCount32 -> PointerReader
getPointerElement reader index =
    PointerReader (untypedListReaderSegment reader) ptr
  where
    ptr = untypedListReaderData reader `plusPtr` offset
    offset = fromIntegral index * (fromIntegral (untypedListReaderStep reader `div` fromIntegral bitsPerByte))

--------------------------------------------------------------------------------

-- TODO: rewrite-rules for:
--          map f (toList reader)
--          map f (reverse (toList reader))
--          foldM f (reverse (toList reader))
newtype ListReader a = ListReader UntypedListReader

listLength :: ListReader a -> ElementCount
listLength (ListReader reader) = fromIntegral $ untypedListReaderElementCount reader

toList :: (ListElement a) => ListReader a -> IO [a]
toList list = mapM (getElement list) [0..listLength list-1]

eachElement_ :: (ListElement a) => ListReader a -> (a -> IO b) -> IO ()
eachElement_ list fn =
    unless (listLength list == 0) $
      mapM_ (fn <=< getElement list) [0..listLength list-1]

eachElement :: (ListElement a) => ListReader a -> (a -> IO b) -> IO [b]
eachElement list fn =
    if listLength list == 0
      then return []
      else mapM (fn <=< getElement list) [0..listLength list-1]

mapElements :: (ListElement a) => (a -> IO b) -> ListReader a -> IO [b]
mapElements fn list =
    if listLength list == 0
      then return []
      else mapM (fn <=< getElement list) [0..listLength list-1]

foldrElements :: (ListElement a) => (a -> b -> IO b) -> b -> ListReader a -> IO b
foldrElements f z0 list = go len z0
  where
    len = listLength list
    go n z =
        if n == 0
          then return z
          else do
              elem <- getElement list (n-1)
              f elem z >>= go (n-1)

foldlElements :: (ListElement a) => (b -> a -> IO b) -> b -> ListReader a -> IO b
foldlElements f z0 list = go 0 z0
  where
    len = listLength list
    go n z =
        if n == len
          then return z
          else do
              elem <- getElement list n
              f z elem >>= go (n+1)

-- TODO:
-- * assert index < len
-- * checked/unchecked variants
class ListElement a where
    elementSize :: a -> ElementSize

    getUntypedElement :: UntypedListReader -> ElementCount -> IO a
    default getUntypedElement :: (Storable a) => UntypedListReader -> ElementCount -> IO a
    getUntypedElement reader index =
        peek ptr
      where
        offset = fromIntegral (index * fromIntegral (untypedListReaderStep reader) `div` fromIntegral bitsPerByte)
        ptr = untypedListReaderData reader `plusPtr` offset

    getElement :: ListReader a -> ElementCount -> IO a
    getElement (ListReader reader) = getUntypedElement reader

instance ListElement Bool where
    elementSize _ = SzBit
    getUntypedElement reader index = do
        val <- peek ptr
        return $ val .&. (1 `shiftL` fromIntegral bitNum) /= 0
      where
        bindex = fromIntegral index * fromIntegral (untypedListReaderStep reader) :: Word64
        (offset, bitNum) = bindex `quotRem` fromIntegral bitsPerByte
        ptr = untypedListReaderData reader `plusPtr` fromIntegral offset :: Ptr Word8

instance ListElement Word8 where
    elementSize _ = SzByte

instance ListElement Word16 where
    elementSize _ = SzTwoBytes

instance ListElement Word32 where
    elementSize _ = SzFourBytes

instance ListElement Word64 where
    elementSize _ = SzEightBytes

instance ListElement Int8 where
    elementSize _ = SzByte

instance ListElement Int16 where
    elementSize _ = SzTwoBytes

instance ListElement Int32 where
    elementSize _ = SzFourBytes

instance ListElement Int64 where
    elementSize _ = SzEightBytes

instance ListElement Float where
    elementSize _ = SzFourBytes

instance ListElement Double where
    elementSize _ = SzEightBytes

instance ListElement StructReader where
    elementSize _ = SzInlineComposite
    getUntypedElement reader index = return $ getStructElement reader (fromIntegral index)

instance ListElement TextReader where
    elementSize _ = SzPointer
    getUntypedElement reader index = getText ptrReader nullPtr 0
      where
        ptrReader = getPointerElement reader (fromIntegral index)

instance ListElement DataReader where
    elementSize _ = SzPointer
    getUntypedElement reader index = getData ptrReader nullPtr 0
      where
        ptrReader = getPointerElement reader (fromIntegral index)

instance (ListElement a) => ListElement (ListReader a) where
    elementSize _ = SzPointer
    getUntypedElement = getUntypedElementHack undefined
      where
        getUntypedElementHack :: (ListElement a) => a -> UntypedListReader -> ElementCount -> IO (ListReader a)
        getUntypedElementHack dummy reader index = ListReader <$> getList ptrReader (elementSize dummy) nullPtr
          where
            ptrReader = getPointerElement reader (fromIntegral index)

--------------------------------------------------------------------------------
loop :: (Monad m, Num a, Eq a) => a -> a -> (a -> m b) -> m ()
loop start end f = loopFold start end () (\_ x -> void $ f x )

loopFold :: (Monad m, Num a, Eq a) => a -> a -> acc -> (acc -> a -> m acc) -> m acc
loopFold start end = loopFold' start (/= end) (+1)

loopFold' :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> acc -> (acc -> a -> m acc) -> m acc
loopFold' start cond inc acc0 f = go acc0 start
  where
    go acc !x | cond x    = let macc = f acc x
                             in macc >>= \acc' -> acc' `seq` go acc' (inc x)
              | otherwise = return acc

zeroArray :: (Storable a) => Ptr a -> CSize -> IO ()
zeroArray = go undefined
  where
    go :: (Storable a) => a -> Ptr a -> CSize -> IO ()
    go dummy ptr count = void $ BS.memset (castPtr ptr) 0 (count * (fromIntegral $ sizeOf dummy))

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MagicHash             #-}

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
import qualified Data.ByteString.Unsafe   as BS
import           Data.Int
import           Data.IORef
import           Data.Monoid
import           Data.Word
import           Foreign.C.Types          (CChar, CSize)
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array    hiding (newArray)
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO
import           System.IO.Unsafe         (unsafePerformIO)
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

debugSegment :: SegmentReader -> IO ()
debugSegment reader = withSegmentReader reader $ \ptr -> do
    bytes <- peekArray (fromIntegral (segmentReaderSize reader) * bytesPerWord) (castPtr ptr) :: IO [Word8]
    let hex = unwords (map (printf "%02x") bytes)
    debug $ "SEGMENT: " <> hex

--------------------------------------------------------------------------------
class Union a where
    type UnionTy a :: *
    which :: a -> IO (UnionTy a)

data StructSize = StructSize
  { structSizeData     :: WordCount16
  , structSizePointers :: WirePointerCount16
  }

data MessageSize = MessageSize
  { wordCount :: Word64
  , capCount  :: Word64
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

data PointerReader = PointerReader
  { pointerReaderSegment :: SegmentReader
  , pointerReaderData    :: Ptr WirePointer
  }

data PointerBuilder = PointerBuilder
  { pointerBuilderSegment :: SegmentBuilder
  , pointerBuilderData    :: Ptr WirePointer
  }

newtype TextReader = TextReader
  { textReaderData :: BS.ByteString
  }

newtype TextBuilder = TextBuilder
  { textBuilderData :: BS.ByteString
  }

instance AsReader TextBuilder where
    type ReaderTy TextBuilder = TextReader
    asReader (TextBuilder bs) = TextReader bs

newtype DataReader = DataReader
  { dataReaderData :: BS.ByteString
  }

newtype DataBuilder = DataBuilder
  { dataBuilderData :: BS.ByteString
  }

instance AsReader DataBuilder where
    type ReaderTy DataBuilder = DataReader
    asReader (DataBuilder bs) = DataReader bs

data StructReader = StructReader
  { structReaderSegment  :: SegmentReader
  , structReaderData     :: Ptr Word8
  , structReaderPointers :: Ptr WirePointer
  , structReaderDataSize :: BitCount32
  , structReaderPtrCount :: WirePointerCount16
  }

data StructBuilder = StructBuilder
  { structBuilderSegment  :: SegmentBuilder
  , structBuilderData     :: Ptr Word8
  , structBuilderPointers :: Ptr WirePointer
  , structBuilderDataSize :: BitCount32
  , structBuilderPtrCount :: WirePointerCount16
  }

instance AsReader StructBuilder where
    type ReaderTy StructBuilder = StructReader
    asReader builder =
        StructReader
            (asReader $ structBuilderSegment builder)
            (structBuilderData builder)
            (structBuilderPointers builder)
            (structBuilderDataSize builder)
            (structBuilderPtrCount builder)

data ListReader a = ListReader
  { listReaderSegment        :: SegmentReader
  , listReaderData           :: Ptr Word8
  , listReaderElementCount   :: ElementCount32
  , listReaderStep           :: BitCount32
  , listReaderStructDataSize :: BitCount32
  , listReaderStructPtrCount :: WirePointerCount16
  }

data ListBuilder a = ListBuilder
  { listBuilderSegment        :: SegmentBuilder
  , listBuilderData           :: Ptr Word8
  , listBuilderElementCount   :: ElementCount32
  , listBuilderStep           :: BitCount32
  , listBuilderStructDataSize :: BitCount32
  , listBuilderStructPtrCount :: WirePointerCount16
  }

instance AsReader (ListBuilder a) where
    type ReaderTy (ListBuilder a) = (ListReader a)
    asReader builder =
        ListReader
            (asReader $ listBuilderSegment builder)
            (listBuilderData builder)
            (listBuilderElementCount builder)
            (listBuilderStep builder)
            (listBuilderStructDataSize builder)
            (listBuilderStructPtrCount builder)

data StructRef = StructRef
    { structRefDataSize :: WordCount16
    , structRefPtrCount :: WirePointerCount16
    } deriving (Show)

data ListRef = ListRef
    { listRefElementSize              :: ElementSize
    , listRefElementCount             :: ElementCount32
    , listRefInlineCompositeWordCount :: WordCount32
    } deriving (Show)

data WirePointerKind
  = Struct
  | List
  | Far
  | Other
  deriving (Show, Eq, Enum)

data WirePointer = WirePointer
  { offsetAndKind :: Word32
  , upper32Bits   :: Word32
  } deriving (Show)

instance Nullable WirePointer where
    isNull ptr = offsetAndKind ptr == 0 && upper32Bits ptr == 0

-- TODO: handle endianness correctly
instance Storable WirePointer where
    sizeOf _ = 8
    alignment _ = 8
    poke ptr (WirePointer a b) =
        let p0 = castPtr ptr :: Ptr Word32
            p1 = castPtr $ p0 `advancePtr` 1
        in poke p0 a >> poke p1 b
    peek ptr =
        let p0 = castPtr ptr :: Ptr Word32
            p1 = castPtr $ p0 `advancePtr` 1
        in WirePointer <$> peek p0 <*> peek p1


--------------------------------------------------------------------------------
-- Wire Helpers

roundBytesUpToWords :: ByteCount32 -> WordCount32
roundBytesUpToWords bytes = fromIntegral $ (bytes + 7) `div` fromIntegral bytesPerWord

roundBitsUpToWords :: BitCount64 -> WordCount32
roundBitsUpToWords bits = fromIntegral $ (bits + 63) `div` fromIntegral bitsPerWord

roundBitsUpToBytes :: BitCount64 -> ByteCount32
roundBitsUpToBytes bytes = fromIntegral $ (bytes + 7) `div` fromIntegral bitsPerByte

boundsCheck :: SegmentReader -> Ptr CPWord -> Ptr CPWord -> WirePointerKind -> IO ()
boundsCheck segment start end kind =
    unless (isNull segment || containsInterval segment start end) $
      fail $ case kind of
                    List -> "Message contained out-of-bounds list pointer."
                    Struct -> "Message contained out-of-bounds struct pointer."
                    Far -> "Message contained out-of-bounds far pointer."
                    Other -> "Message contained out-of-bounds other pointer."

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

pointersPerElement :: ElementSize -> WirePointerCount32
pointersPerElement size =
    case size of
        SzPointer -> 1
        _ -> 0

structSizeTotal :: StructSize -> WordCount32
structSizeTotal (StructSize x y) = fromIntegral x + fromIntegral y

toStructRef :: WirePointer -> StructRef
toStructRef ptr = StructRef (fromIntegral (upper32Bits ptr))
                            (fromIntegral ((upper32Bits ptr) `shiftR` 16))

setStructRef :: WirePointer -> WordCount16 -> WordCount16 -> WirePointer
setStructRef wptr dataSize pointerCount = wptr
    { upper32Bits = fromIntegral dataSize .|. (fromIntegral pointerCount) `shiftL` 16
    }

structRefWordSize :: StructRef -> WordCount32
structRefWordSize ref = fromIntegral (structRefDataSize ref) + fromIntegral (structRefPtrCount ref)

toListRef :: WirePointer -> ListRef
toListRef ptr = ListRef (toEnum (fromIntegral ((upper32Bits ptr) .&. 7)))
                        ((upper32Bits ptr) `shiftR` 3)
                        ((upper32Bits ptr) `shiftR` 3)

-- XXX assert!(ec < (1 << 29), "Lists are limited to 2**29 elements");
setListRef :: WirePointer -> ElementSize -> ElementCount32 -> WirePointer
setListRef wptr elemSize elemCount = wptr
    { upper32Bits = ((fromIntegral (fromEnum elemSize)) .&. 7) .|. ((upper32Bits wptr) `shiftL` 3)
    }

-- XXX assert!(wc < (1 << 29), "Inline composite lists are limited to 2**29 words");
setInlineComposite :: WirePointer -> WordCount32 -> WirePointer
setInlineComposite wptr wordCount = wptr
    { upper32Bits = ((fromIntegral (fromEnum SzInlineComposite)) .&. 7) .|. ((upper32Bits wptr) `shiftL` 3)
    }

defaultStructReader :: StructReader
defaultStructReader = StructReader nullSegmentReader nullPtr nullPtr 0 0

defaultPointerReader :: PointerReader
defaultPointerReader = PointerReader nullSegmentReader nullPtr

defaultListReader :: ListReader a
defaultListReader = ListReader nullSegmentReader nullPtr 0 0 0 0

defaultListBuilder :: ListBuilder a
defaultListBuilder = ListBuilder nullSegmentBuilder nullPtr 0 0 0 0

defaultWirePointer :: WirePointer
defaultWirePointer = WirePointer { offsetAndKind = 0, upper32Bits = 0 }

wirePointerKind :: WirePointer -> WirePointerKind
wirePointerKind ptr =
    case (offsetAndKind ptr) .&. 3 of
        0 -> Struct
        1 -> List
        2 -> Far
        3 -> Other

wirePtrRefIsNull :: Ptr WirePointer -> IO Bool
wirePtrRefIsNull ref =
    if ref == nullPtr
      then return True
      else do
          wirePtr <- peek ref
          return $ isNull wirePtr

nonFarOffset :: WirePointer -> Int
nonFarOffset = (`shiftR` 2) . fromIntegral . offsetAndKind

wirePointerTarget :: Ptr WirePointer -> IO (Ptr CPWord)
wirePointerTarget ref = do
    ref' <- peek ref
    return $ (castPtr ref :: Ptr CPWord) `advancePtr` (nonFarOffset ref' + 1)

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

setKindAndInlineCompositeListElementCount :: WirePointer -> WirePointerKind -> ElementCount32 -> WirePointer
setKindAndInlineCompositeListElementCount wptr kind elemCount = wptr
    { offsetAndKind = (fromIntegral elemCount `shiftL` 2)  .|. (fromIntegral $ fromEnum kind)
    }

calculateTargetOffset :: Ptr WirePointer -> Ptr CPWord -> Int
calculateTargetOffset ref target = ((target `minusPtr` ref) `div` bytesPerWord) - 1

setFar :: WirePointer -> Bool -> WordCount32 -> SegmentId -> WirePointer
setFar wptr doubleFar pos id = wptr
    { upper32Bits = pos
    , offsetAndKind =  (pos `shiftL` 3)
                   .|. (if doubleFar then 1 `shiftL` 2 else 0)
                   .|. (fromIntegral $ fromEnum (wirePointerKind wptr)) }

-----------
-- TODO: move this out into a different file, or inline in Serialize.hs
getRoot :: SegmentReader -> Ptr CPWord -> IO PointerReader
getRoot segment location = do
    boundsCheck segment location (location `advancePtr` pointerSizeInWords) Struct
    return $ PointerReader segment (castPtr location)
-----------

allocate :: Ptr WirePointer -> SegmentBuilder -> WordCount32 -> WirePointerKind
         -> IO ( Ptr WirePointer -- The wire-ptr ref
               , Ptr CPWord      -- The content
               , SegmentBuilder -- The segment builder
               )
allocate ref segment amount kind = withSegmentBuilder segment $ \_ -> do
    null <- wirePtrRefIsNull ref
    unless null $
        zeroObject segment ref

    if amount == 0 && kind == Struct
      then do
          ref' <- peek ref
          ref' <- return $ setKindAndTargetForEmptyStruct ref'
          poke ref ref'
          return (ref, castPtr ref, segment)
      else segmentAllocate segment amount >>= \case
          Nothing -> do
              -- Need to allocate in a new segment. We'll need to
              -- allocate an extra pointer worth of space to act as
              -- the landing pad for a far pointer.
              let amountPlusRef = amount + fromIntegral pointerSizeInWords
              (segment, ptr)  <- arenaAllocate (segmentBuilderGetArena segment) amountPlusRef

              withSegmentBuilder segment $ \_ -> do
                  -- Set up the original pointer to be a far pointer to
                  -- the new segment.
                  ref' <- peek ref
                  ref' <- return $ setFar ref' False (getWordOffsetTo segment ptr) (segmentBuilderId segment)
                  poke ref ref'

                  -- Initialize the landing pad to indicate that the
                  -- data immediately follows the pad.
                  let ref = castPtr ptr
                      ptr = ptr `plusPtr` pointerSizeInWords
                      offset = calculateTargetOffset ref ptr
                  ref' <- peek ref
                  ref' <- return $ setOffsetAndKind ref' offset kind
                  poke ref ref'

                  return (ref, ptr, segment)
          Just ptr -> do
              ref' <- peek ref
              let offset = calculateTargetOffset ref ptr
              ref' <- return $ setOffsetAndKind ref' offset kind
              poke ref ref'
              return (ref, ptr, segment)

followBuilderFars :: Ptr WirePointer -> SegmentBuilder
                  -> IO ( Ptr WirePointer
                        , Ptr CPWord
                        , SegmentBuilder
                        )
followBuilderFars ref segment = do
    wirePtr <- peek ref
    if wirePointerKind wirePtr /= Far
      then do
          contentPtr <- wirePointerTarget ref
          return (ref, contentPtr, segment)
      else do
          segment <- builderArenaGetSegment (segmentBuilderGetArena segment) (wptrSegmentId wirePtr)
          withSegmentBuilder segment $ \_ -> do
              let landingPad = segmentBuilderGetPtrUnchecked segment (farPositionInSegment wirePtr)

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
                    segment <- builderArenaGetSegment (segmentBuilderGetArena segment) (wptrSegmentId wirePtr)
                    let content = segmentBuilderGetPtrUnchecked segment (farPositionInSegment wirePtr)
                    return (ref, content, segment)

followFars :: Ptr WirePointer -> SegmentReader
           -> IO ( WirePointer    -- The resolved non-far-pointer
                 , Ptr CPWord       -- The pointer the the actual content
                 , SegmentReader -- The target segment, in the case of far-pointers
                 )
followFars ref segment = do
    ref' <- peek ref
    -- If the segment is null, this is an unchecked message, so there are no FAR pointers.
    if (isNull segment) || wirePointerKind ref' /= Far
      then do
          let contentPtr = ((castPtr ref :: Ptr CPWord) `advancePtr` (nonFarOffset ref' + 1))
          return (ref', contentPtr, segment)
      else do
          segment <- arenaGetSegment (segmentReaderArena segment) (wptrSegmentId ref')
          let padWords = if isDoubleFar ref' then 2 else 1
          withSegmentReader segment $ \segmentPtr -> do
              let pad = (castPtr segmentPtr :: Ptr WirePointer) `advancePtr` (fromIntegral (farPositionInSegment ref'))
              -- XXX bounds check
              if (not $ isDoubleFar ref')
                then do
                    let ref = pad
                    ref' <- peek ref
                    let contentPtr = castPtr $ pad `advancePtr` (nonFarOffset ref' + 1) :: Ptr CPWord
                    return (ref', contentPtr, segment)
                else do
                    -- Landing pad is another far pointer. It is
                    -- followed by a tag describing the pointed-to
                    -- object.
                    let ref = pad `advancePtr` 1
                    ref' <- peek ref

                    pad' <- peek pad
                    segment <- arenaGetSegment (segmentReaderArena segment) (wptrSegmentId pad')
                    withSegmentReader segment $ \segmentPtr -> do
                        let contentPtr = segmentPtr `advancePtr` fromIntegral (farPositionInSegment pad')
                        return (ref', contentPtr, segment)

zeroObject :: SegmentBuilder -> Ptr WirePointer -> IO ()
zeroObject segment ref = do
    ref' <- peek ref
    let common = zeroObjectHelper segment ref =<< wirePointerTarget ref
    case wirePointerKind ref' of
        Struct -> common
        List -> common
        Other -> common
        Far -> do
            segment <- builderArenaGetSegment (segmentBuilderGetArena segment) (wptrSegmentId ref')
            let pad = castPtr $ segmentBuilderGetPtrUnchecked segment (farPositionInSegment ref') :: Ptr WirePointer
            if isDoubleFar ref'
              then do
                  pad' <- peek pad
                  segment <- builderArenaGetSegment (segmentBuilderGetArena segment) (wptrSegmentId pad')

                  let content = segmentBuilderGetPtrUnchecked segment (farPositionInSegment pad')

                  zeroObjectHelper segment (pad `advancePtr` 1) content

                  poke (castPtr pad :: Ptr CPWord) 0
                  poke ((castPtr pad :: Ptr CPWord) `advancePtr` 1) 0
              else do
                  zeroObject segment pad
                  poke (castPtr pad :: Ptr CPWord) 0

zeroObjectHelper :: SegmentBuilder -> Ptr WirePointer -> Ptr CPWord -> IO ()
zeroObjectHelper segment tag ptr = do
    tag' <- peek tag
    case wirePointerKind tag' of
        Other -> fail "Don't know how to handle OTHER"
        Struct -> do
            let dataSize = structRefDataSize (toStructRef tag')
                pointerSection = castPtr $ ptr `advancePtr` fromIntegral dataSize :: Ptr WirePointer
                pointerCount = structRefPtrCount (toStructRef tag')
                wordSize = dataSize + pointerCount
            loop 0 pointerCount $ \i ->
                zeroObject segment (pointerSection `advancePtr` fromIntegral i)
            zeroArray ptr (fromIntegral wordSize)
        List -> do
            let elemSize = listRefElementSize (toListRef tag')
                elemCount = listRefElementCount (toListRef tag')
                listWords = elemCount * (roundBitsUpToWords $ fromIntegral $ dataBitsPerElement elemSize)
            case listRefElementSize (toListRef tag') of
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
                    elementTag' <- peek elementTag
                    let dataSize = (structRefDataSize $ toStructRef elementTag')
                        pointerCount = (structRefPtrCount $ toStructRef elementTag')
                        count = inlineCompositeListElementCount elementTag'
                        wordSize = dataSize + pointerCount
                        elemKind = wirePointerKind elementTag'
                    when (elemKind /= Struct) $
                        fail "Don't know how to handle non-STRUCT inline composite"
                    loopFold_ 0 count (ptr `advancePtr` 1) $ \pos _ -> do
                        let pos = pos `advancePtr` fromIntegral dataSize :: Ptr CPWord
                        loopFold 0 pointerCount pos $ \pos _ -> do
                            zeroObject segment (castPtr pos)
                            return $ pos `advancePtr` 1
                    zeroArray ptr (fromIntegral wordSize * fromIntegral count + 1)
        Far -> fail "Unexpected FAR pointer"

zeroPointerAndFars :: SegmentBuilder -> Ptr WirePointer -> IO ()
zeroPointerAndFars segment ref = do
    ref' <- peek ref
    when (wirePointerKind ref' == Far) $ do
        pad <- builderArenaGetSegment (segmentBuilderGetArena segment) (wptrSegmentId ref') >>= \segment ->
            return $ segmentBuilderGetPtrUnchecked segment (farPositionInSegment ref')
        let numElements = if isDoubleFar ref' then 2 else 1
        zeroArray pad numElements
    poke (castPtr ref :: Ptr CPWord) 0

plusMessageSize :: MessageSize -> MessageSize -> MessageSize
plusMessageSize (MessageSize a1 b1) (MessageSize a2 b2) = MessageSize (a1 + a2) (b1 + b2)

totalSize :: SegmentReader -> Ptr WirePointer -> IO MessageSize
totalSize segment ref = do
    null <- wirePtrRefIsNull ref
    if null
      then return MessageSize { wordCount = 0, capCount = 0 }
      else do
          (ref', ptr, segment) <- followFars ref segment
          case wirePointerKind ref' of
              Struct -> do
                  -- XXX bounds check
                  let dataSize = structRefDataSize (toStructRef ref')
                      init = MessageSize { wordCount = fromIntegral dataSize, capCount = 0 }
                      pointerSection = castPtr $ ptr `advancePtr` fromIntegral dataSize :: Ptr WirePointer
                      count = structRefPtrCount (toStructRef ref')

                  loopFold 0 count init $ \msize i ->
                    (msize `plusMessageSize`) <$> totalSize segment (pointerSection `advancePtr` fromIntegral i)
              List -> do
                  let elemCount = listRefElementCount (toListRef ref')
                      elemSize = listRefElementSize (toListRef ref')
                      totalWords = roundBitsUpToWords (fromIntegral elemCount * fromIntegral (dataBitsPerElement elemSize))
                      commonCase =
                          --XXX bounds check
                          return MessageSize { wordCount = fromIntegral totalWords, capCount = 0 }

                  case listRefElementSize (toListRef ref') of
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
                          let wordCount = listRefInlineCompositeWordCount (toListRef ref')
                              init = MessageSize { wordCount = fromIntegral wordCount, capCount = 0 }
                          if wordCount == 0
                            then return init
                            else do
                                let wordCount = listRefInlineCompositeWordCount $ toListRef ref'
                                    elementTag = castPtr ptr :: Ptr WirePointer
                                elementTag' <- peek elementTag
                                let count = inlineCompositeListElementCount elementTag'
                                    dataSize = structRefDataSize $ toStructRef elementTag'
                                    pointerCount = structRefPtrCount $ toStructRef elementTag'
                                    wordSize = dataSize + pointerCount

                                when (wirePointerKind elementTag' /= Struct) $
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
                  if isCapability ref'
                    then return MessageSize { wordCount = 0, capCount = 1 }
                    else fail "Unknown pointer type."

transferPointer :: SegmentBuilder -> Ptr WirePointer -> SegmentBuilder -> Ptr WirePointer -> IO ()
transferPointer dstSegment dst srcSegment src = do
    src' <- peek src
    if isNull src'
      then poke (castPtr dst :: Ptr CPWord) 0
      else if wirePointerKind src' == Far
        then copyArray dst src 1
        else do
            target <- wirePointerTarget src
            transferPointerSplit dstSegment dst srcSegment src target

transferPointerSplit :: SegmentBuilder -> Ptr WirePointer -> SegmentBuilder -> Ptr WirePointer -> Ptr CPWord -> IO ()
transferPointerSplit dstSegment dst srcSegment srcTag srcPtr =
    if unsafeSegmentBuilderPtr dstSegment == unsafeSegmentBuilderPtr srcSegment
      then do
          srcTag' <- peek srcTag
          dst' <- peek dst
          let offset = calculateTargetOffset dst srcPtr
          dst' <- return $ setOffsetAndKind dst' offset (wirePointerKind srcTag')
          dst' <- return $ dst' { upper32Bits = upper32Bits srcTag' }
          poke dst dst'
      else
          segmentAllocate srcSegment 1 >>= \case
            Nothing -> fail "unimplemented" -- XXX need a double-far
            Just landingPadWord -> do
                srcTag' <- peek srcTag
                dst' <- peek dst
                let landingPad = castPtr landingPadWord :: Ptr WirePointer
                    offset = calculateTargetOffset landingPad srcPtr
                landingPad' <- return $ setOffsetAndKind defaultWirePointer offset (wirePointerKind srcTag')
                landingPad' <- return $ landingPad' { upper32Bits = upper32Bits srcTag' }
                poke landingPad landingPad'

                dst' <- return $ setFar dst' False (getWordOffsetTo srcSegment landingPadWord) (segmentBuilderId srcSegment)
                poke dst dst'

initStructPointer :: Ptr WirePointer -> SegmentBuilder -> StructSize -> IO StructBuilder
initStructPointer ref segment size = do
    (ref, ptr, segment) <- allocate ref segment (structSizeTotal size) Struct
    return $
      StructBuilder
        segment
        (castPtr ptr)
        (castPtr $ ptr `advancePtr` (fromIntegral $ structSizeData size))
        ((fromIntegral $ structSizeData size) * fromIntegral bitsPerWord)
        (structSizePointers size)

getWritableStructPointer :: Ptr WirePointer -> SegmentBuilder -> StructSize -> Ptr CPWord -> IO StructBuilder
getWritableStructPointer ref segment size defaultValue = do
    ref' <- peek ref
    refTarget <- wirePointerTarget ref
    if isNull ref'
      then do
          null <- wirePtrRefIsNull (castPtr defaultValue)
          if null
             then initStructPointer ref segment size
             else fail "unimplemented"
      else do
          (oldRef, oldPtr, oldSegment) <- followBuilderFars ref segment
          oldRef' <- peek oldRef
          when  (wirePointerKind oldRef' /= Struct) $
            fail "Message contains non-struct pointer where struct pointer was expected."
          let oldDataSize = structRefDataSize (toStructRef oldRef')
              oldPointerCount = structRefPtrCount (toStructRef oldRef')
              oldPointerSection = castPtr $ oldPtr `advancePtr` fromIntegral oldDataSize :: Ptr WirePointer
          if oldDataSize < structSizeData size || oldPointerCount < structSizePointers size
            then do
                let newDataSize = max oldDataSize (structSizeData size)
                    newPointerCount = max oldPointerCount (structSizePointers size)
                    totalSize = fromIntegral newDataSize + fromIntegral newPointerCount :: WordCount32

                zeroPointerAndFars segment ref
                (ref, ptr, segment) <- allocate ref segment totalSize Struct
                ref' <- peek ref

                ref' <- return $ setStructRef ref' newDataSize newPointerCount
                poke ref ref'

                copyArray ptr oldPtr (fromIntegral oldDataSize)

                let newPointerSection = castPtr $ ptr `advancePtr` fromIntegral newDataSize :: Ptr WirePointer
                loop 0 oldPointerCount $ \i ->
                  transferPointer segment (newPointerSection `advancePtr` fromIntegral i) oldSegment (oldPointerSection `advancePtr` fromIntegral i)

                zeroArray oldPtr (fromIntegral oldDataSize + fromIntegral oldPointerCount)

                return $
                  StructBuilder
                    segment
                    (castPtr ptr)
                    newPointerSection
                    (fromIntegral newDataSize * fromIntegral bitsPerWord)
                    newPointerCount
            else
                return $
                  StructBuilder
                    oldSegment
                    (castPtr oldPtr)
                    oldPointerSection
                    (fromIntegral oldDataSize * fromIntegral bitsPerWord)
                    oldPointerCount

initListPointer :: Ptr WirePointer -> SegmentBuilder -> ElementCount32 -> ElementSize -> IO (ListBuilder a)
initListPointer ref segment elemCount elemSize = do
    when (elemSize /= SzInlineComposite) $
      fail "Should have called initStructListPointer instead"
    let dataSize = dataBitsPerElement elemSize
        pointerCount = pointersPerElement elemSize
        step = dataSize + (pointerCount * fromIntegral bitsPerPointer)
        wordCount = roundBitsUpToWords (fromIntegral elemCount * fromIntegral step)
    (ref, ptr, segment) <- allocate ref segment wordCount List
    ref' <- peek ref

    ref' <- return $ setListRef ref' elemSize elemCount
    poke ref ref'

    return $
      ListBuilder
        segment
        (castPtr ptr)
        step
        elemCount
        dataSize
        (fromIntegral pointerCount)

initStructListPointer :: Ptr WirePointer -> SegmentBuilder -> ElementCount32 -> StructSize -> IO (ListBuilder a)
initStructListPointer ref segment elemCount elemSize = do
    let wordsPerElement = structSizeTotal elemSize
        wordCount = elemCount * wordsPerElement

    (ref, ptr_, segment) <- allocate ref segment (fromIntegral pointerSizeInWords + fromIntegral wordCount) List
    let ptr = castPtr ptr_ :: Ptr WirePointer
    ref' <- peek ref
    ptr' <- peek ptr

    -- Initialize the pointer.
    ref' <- return $ setInlineComposite ref' wordCount
    poke ref ref'
    ptr' <- return $ setKindAndInlineCompositeListElementCount ptr' Struct elemCount
    ptr' <- return $ setStructRef ptr' (structSizeData elemSize) (structSizePointers elemSize)
    poke ptr ptr'

    let ptr1 = ptr `advancePtr` 1

    return $
      ListBuilder
        segment
        (castPtr ptr1)
        (wordsPerElement * fromIntegral bitsPerWord)
        elemCount
        (fromIntegral (structSizeData elemSize) * fromIntegral bitsPerWord)
        (structSizePointers elemSize)

getWritableListPointer :: Ptr WirePointer -> SegmentBuilder -> ElementSize -> Ptr CPWord -> IO (ListBuilder a)
getWritableListPointer origRef origSegment elemSize defaultValue = do
    origRef' <- peek origRef
    origRefTarget <- wirePointerTarget origRef
    if isNull origRef'
      then do
          null <- wirePtrRefIsNull (castPtr defaultValue)
          if null
            then return defaultListBuilder
            else fail "unimplemented"
      else do
          let ref = origRef
              segment = origSegment

          (ref, ptr, segment) <- followBuilderFars ref segment

          ref' <- peek ref
          when (wirePointerKind ref' /= List) $
            fail "Called get_list_{{field,element}}() but existing pointer is not a list."

          let oldSize = listRefElementSize (toListRef ref')

          if oldSize == SzInlineComposite
            then do
                let tag = castPtr ptr :: Ptr WirePointer
                tag' <- peek tag

                when (wirePointerKind tag' /= Struct) $
                  fail "InlineComposite list with non-STRUCT elements not supported."

                ptr <- return $ ptr `advancePtr` pointerSizeInWords

                let dataSize = structRefDataSize $ toStructRef tag'
                    pointerCount = structRefPtrCount $ toStructRef tag'
                    listFromPtr ptr =
                        ListBuilder
                            segment
                            ptr
                            (inlineCompositeListElementCount tag')
                            (structRefWordSize (toStructRef tag') * fromIntegral bitsPerWord)
                            (fromIntegral dataSize * fromIntegral bitsPerWord)
                            pointerCount
                    commonCase = do
                        when (dataSize < 1) $ fail "Existing list value is incompatible with expected type."
                        return $ listFromPtr $ castPtr ptr

                case elemSize of
                    SzVoid -> return $ listFromPtr $ castPtr ptr
                    SzBit -> fail "Found struct list where bit list was expected."
                    SzByte -> commonCase
                    SzTwoBytes -> commonCase
                    SzFourBytes -> commonCase
                    SzEightBytes -> commonCase
                    SzPointer -> do
                        when (pointerCount < 1) $ fail "Existing list value is incompatible with expected type."
                        return $ listFromPtr $ (castPtr ptr) `advancePtr` 1
                    SzInlineComposite -> fail "The impossible happened."
            else do
                let dataSize = dataBitsPerElement oldSize
                    pointerCount = pointersPerElement oldSize
                    step = dataSize * pointerCount * fromIntegral bitsPerPointer
                when (dataSize < dataBitsPerElement elemSize || pointerCount < pointersPerElement elemSize) $
                  fail "Existing list value is incompatible with expected type."

                return $
                  ListBuilder
                    segment
                    (castPtr ptr)
                    step
                    (listRefElementCount (toListRef ref'))
                    dataSize
                    (fromIntegral pointerCount)

getWritableStructListPointer :: Ptr WirePointer -> SegmentBuilder -> StructSize -> Ptr CPWord -> IO (ListBuilder a)
getWritableStructListPointer origRef origSegment elemSize defaultValue = do
    origRef' <- peek origRef
    origRefTarget <- wirePointerTarget origRef

    if isNull origRef'
      then
          if defaultValue == nullPtr
            then return defaultListBuilder
            else fail "unimplemented"
      else do
          -- We must verify that the pointer has the right size and potentially upgrade it if not.
          (oldRef, oldPtr, oldSegment) <- followBuilderFars origRef origSegment
          oldRef' <- peek oldRef

          when (wirePointerKind oldRef' /= List) $
            fail "Called getReaderList{{Field,Element}} but existing pointer is not a list."

          let oldSize = listRefElementSize $ toListRef oldRef'

          if oldSize == SzInlineComposite
            then do
                -- Existing list is InlineComposite, but we need to verify that the sizes match.
                let oldTag = castPtr oldPtr :: Ptr WirePointer
                oldPtr <- return $ oldPtr `advancePtr` pointerSizeInWords
                oldTag' <- peek oldTag
                when (wirePointerKind oldTag' /= Struct) $
                  fail "InlineComposite list with non-STRUCT elements not supported."

                let oldDataSize = structRefDataSize $ toStructRef oldTag'
                    oldPointerCount = structRefPtrCount $ toStructRef oldTag'
                    oldStep = oldDataSize + oldPointerCount
                    elemCount = inlineCompositeListElementCount oldTag'

                if oldDataSize >= structSizeData elemSize && oldPointerCount >= structSizePointers elemSize
                  then
                    -- Old size is at least as large as we need. Ship it.
                    return $ ListBuilder
                        oldSegment
                        (castPtr oldPtr)
                        elemCount
                        (fromIntegral oldStep * fromIntegral bitsPerWord)
                        (fromIntegral oldDataSize * fromIntegral bitsPerWord)
                        oldPointerCount
                  else
                    -- The structs in this list are smaller than expected, probably written using an older
                    -- version of the protocol. We need to make a copy and expand them.
                    fail "unimplemented"
            else do
                -- We're upgrading from a non-struct list.
                let oldDataSize = dataBitsPerElement oldSize
                    oldPointerCount = pointersPerElement oldSize
                    oldStep = oldDataSize + oldPointerCount * fromIntegral bitsPerPointer
                    elemCount = listRefElementCount $ toListRef oldRef'

                if oldSize == SzVoid
                  then initStructListPointer oldRef origSegment elemCount elemSize
                  else do
                      -- Upgrade to an inline composite list.
                      when (oldSize == SzBit) $
                        fail "Found bit list where struct list was expected; upgrading boolean lists to struct lists is no longer supported."

                      let (newDataSize, newPointerCount) =
                            if (oldSize == SzPointer)
                              then (structSizeData elemSize, max (structSizePointers elemSize) 1)
                              else (max (structSizeData elemSize) 1, structSizePointers elemSize)

                          newStep = newDataSize + newPointerCount
                          totalWords = elemCount * fromIntegral newStep

                      -- Don't let allocate() zero out the object just yet.
                      zeroPointerAndFars origSegment origRef

                      (newRef, newPtr, newSegment) <- allocate origRef origSegment (totalWords + fromIntegral pointerSizeInWords) List
                      newRef' <- peek newRef
                      newRef' <- return $ setInlineComposite newRef' totalWords
                      poke newRef newRef'

                      let tag = castPtr newPtr :: Ptr WirePointer
                      tag' <- peek tag
                      tag' <- return $ setKindAndInlineCompositeListElementCount tag' Struct elemCount
                      tag' <- return $ setStructRef tag' newDataSize newPointerCount
                      poke tag tag'
                      newPtr <- return $ newPtr `advancePtr` pointerSizeInWords

                      if oldSize == SzPointer
                        then do
                            let dst = castPtr $ newPtr `advancePtr` fromIntegral newDataSize :: Ptr CPWord
                                src = castPtr oldPtr :: Ptr WirePointer
                            loopFold_ 0 elemCount (src, dst) $ \(src, dst) _ -> do
                                transferPointer newSegment (castPtr dst) oldSegment src
                                return (src `advancePtr` 1, dst `advancePtr` fromIntegral newStep)
                        else do
                            let dst = castPtr newPtr :: Ptr CPWord
                                src = castPtr oldPtr :: Ptr Word8
                                oldByteStep = oldDataSize `div` fromIntegral bitsPerByte
                            loopFold_ 0 elemCount (src, dst) $ \(src, dst) _ -> do
                                copyArray (castPtr dst) src (fromIntegral oldByteStep)
                                return (src `advancePtr` fromIntegral oldByteStep, dst `advancePtr` fromIntegral newStep)

                      -- Zero out old location.
                      zeroArray (castPtr oldPtr :: Ptr Word8) (fromIntegral $ roundBitsUpToBytes (fromIntegral oldStep * fromIntegral elemCount))

                      return $
                        ListBuilder
                            newSegment
                            (castPtr newPtr)
                            elemCount
                            (fromIntegral newStep * fromIntegral bitsPerWord)
                            (fromIntegral newDataSize * fromIntegral bitsPerWord)
                            newPointerCount

initTextPointer :: Ptr WirePointer -> SegmentBuilder -> ByteCount32 -> IO TextBuilder
initTextPointer ref segment size = do
    -- The byte list must include a NUL terminator.
    let byteSize = size + 1

    -- Allocate the space.
    (ref, ptr, segment) <- allocate ref segment (roundBytesUpToWords byteSize) List

    -- Initialize the pointer.
    ref' <- peek ref
    ref' <- return $ setListRef ref' SzByte byteSize
    poke ref ref'

    let bs = BS.fromForeignPtr (segmentBuilderForeignPtr segment) (ptr `minusPtr` unsafeSegmentBuilderPtr segment) (fromIntegral byteSize)
    return $ TextBuilder bs

setTextPointer :: Ptr WirePointer -> SegmentBuilder -> BS.ByteString -> IO TextBuilder
setTextPointer ref segment value =
    BS.useAsCStringLen value $ \(srcPtr, len) -> do
        allocation@(TextBuilder bs) <- initTextPointer ref segment (fromIntegral len)
        BS.useAsCStringLen bs $ \(dstPtr, _) -> do
            moveArray dstPtr srcPtr len
            return allocation

getWritableTextPointer :: Ptr WirePointer -> SegmentBuilder -> BS.ByteString -> IO TextBuilder
getWritableTextPointer ref segment defaultValue = do
    ref' <- peek ref
    if isNull ref'
      then
        if BS.length defaultValue == 0
          then return $ TextBuilder defaultValue
          else fail "unimplemented"
      else do
        refTarget <- wirePointerTarget ref
        (ref, ptr, segment) <- followBuilderFars ref segment
        let cptr = castPtr ptr :: Ptr Word8

        when (wirePointerKind ref' /= List) $
          fail "Called getReaderText{{Field,Element}}() but existing pointer is not a list."

        when (listRefElementSize (toListRef ref') /= SzByte) $
          fail "Called getReaderText{{Field,Element}}() but existing list pointer is not byte-sized."

        let count = listRefElementCount (toListRef ref')
        nullMissing <- if count <= 0 then return True else do
            lastByte <- peek $ cptr `advancePtr` (fromIntegral count - 1)
            return $ lastByte == 0

        when nullMissing $
          fail "Text blob missing NUL terminator."

        let bs = BS.fromForeignPtr (segmentBuilderForeignPtr segment) (cptr `minusPtr` unsafeSegmentBuilderPtr segment) (fromIntegral (count - 1))
        return $ TextBuilder bs

initDataPointer :: Ptr WirePointer -> SegmentBuilder -> ByteCount32 -> IO DataBuilder
initDataPointer ref segment size = do
    -- Allocate the space.
    (ref, ptr, segment) <- allocate ref segment (roundBytesUpToWords size) List

    -- Initialize the pointer.
    ref' <- peek ref
    ref' <- return $ setListRef ref' SzByte size
    poke ref ref'

    let bs = BS.fromForeignPtr (segmentBuilderForeignPtr segment) (ptr `minusPtr` unsafeSegmentBuilderPtr segment) (fromIntegral size)
    return $ DataBuilder bs

setDataPointer :: Ptr WirePointer -> SegmentBuilder -> BS.ByteString -> IO DataBuilder
setDataPointer ref segment value =
    BS.useAsCStringLen value $ \(srcPtr, len) -> do
        allocation@(DataBuilder bs) <- initDataPointer ref segment (fromIntegral len)
        BS.useAsCStringLen bs $ \(dstPtr, _) -> do
            moveArray dstPtr srcPtr len
            return allocation

getWritableDataPointer :: Ptr WirePointer -> SegmentBuilder -> BS.ByteString -> IO DataBuilder
getWritableDataPointer ref segment defaultValue = do
    ref' <- peek ref
    if isNull ref'
      then
        if BS.length defaultValue == 0
          then return $ DataBuilder defaultValue
          else do
              setDataPointer ref segment defaultValue
              undefined
      else do
          refTarget <- wirePointerTarget ref
          (ref, ptr, segment) <- followBuilderFars ref segment
          ref' <- peek ref

          when (wirePointerKind ref' /= List) $
            fail "Called getReaderData{{Field,Element}}() but existing pointer is not a list."

          when (listRefElementSize (toListRef ref') /= SzByte) $
            fail "Called getReaderData{{Field,Element}}() but existing list pointer is not byte-sized."

          let len = listRefElementCount $ toListRef ref'
              bs = BS.fromForeignPtr (segmentBuilderForeignPtr segment) (ptr `minusPtr` unsafeSegmentBuilderPtr segment) (fromIntegral len)
          return $ DataBuilder bs

setStructPointer :: SegmentBuilder -> Ptr WirePointer -> StructReader -> IO (Ptr CPWord)
setStructPointer segment ref value = do
    let dataSize = roundBitsUpToWords $ fromIntegral $ structReaderDataSize value
        pointerCount = fromIntegral $ structReaderPtrCount value
        totalSize = dataSize + pointerCount

    (ref, ptr, segment) <- allocate ref segment totalSize Struct
    ref' <- peek ref
    ref' <- return $ setStructRef ref' (fromIntegral dataSize) (fromIntegral pointerCount)
    poke ref ref'

    if structReaderDataSize value == 1
      then do
          bit <- getReaderBoolField value 0
          poke (castPtr ptr :: Ptr Word8) (if bit then 1 else 0 :: Word8)
      else
          copyArray ptr (castPtr $ structReaderData value) (fromIntegral ((structReaderDataSize value) `div` fromIntegral bitsPerWord))

    let pointerSection = ptr `advancePtr` fromIntegral dataSize
    loop 0 pointerCount $ \i ->
      copyPointer segment (castPtr $ pointerSection `advancePtr` fromIntegral i) (structReaderSegment value) (structReaderPointers value `advancePtr` fromIntegral i)

    return ptr

setListPointer :: SegmentBuilder -> Ptr WirePointer -> ListReader a -> IO (Ptr CPWord)
setListPointer segment ref value = do
    let totalSize = roundBitsUpToWords (fromIntegral (listReaderElementCount value) * fromIntegral (listReaderStep value))

    if listReaderStep value <= fromIntegral bitsPerWord
      then do
        -- List of non-structs.
        (ref, ptr, segment) <- allocate ref segment totalSize List
        ref' <- peek ref

        if listReaderStructPtrCount value == 1
          then do
              -- List of pointers.
              ref' <- return $ setListRef ref' SzPointer (listReaderElementCount value)
              poke ref ref'
              loop 0 (listReaderElementCount value) $ \i ->
                copyPointer
                    segment
                    (castPtr $ ptr `advancePtr` fromIntegral i)
                    (listReaderSegment value)
                    (castPtr $ listReaderData value `advancePtr` fromIntegral i)
          else do
              -- List of data.
              elemSize <-
                    case listReaderStep value of
                        0 -> return SzVoid
                        1 -> return SzBit
                        8 -> return SzByte
                        16 -> return SzTwoBytes
                        32 -> return SzFourBytes
                        64 -> return SzEightBytes
                        _ -> fail $ "invalid list step size: "<>show (listReaderStep value)

              ref' <- return $ setListRef ref' elemSize (listReaderElementCount value)
              poke ref ref'
              copyArray (listReaderData value) (castPtr ptr) (fromIntegral totalSize)
        return ptr
      else do
        -- List of structs
        (ref, ptr, segment) <- allocate ref segment (totalSize + fromIntegral pointerSizeInWords) List
        ref' <- peek ref
        ref' <- return $ setInlineComposite ref' totalSize
        poke ref ref'

        let dataSize = roundBitsUpToWords (fromIntegral $ listReaderStructDataSize value)
            pointerCount = listReaderStructPtrCount value

        let tag = castPtr ptr :: Ptr WirePointer
        tag' <- peek tag
        tag' <- return $ setKindAndInlineCompositeListElementCount tag' Struct  (listReaderElementCount value)
        tag' <- return $ setStructRef tag' (fromIntegral dataSize) (fromIntegral pointerCount)
        poke tag tag'

        let dst = ptr `advancePtr` pointerSizeInWords :: Ptr CPWord
        let src = castPtr $ listReaderData value :: Ptr WirePointer
        loopFold_ 0 (listReaderElementCount value) (src, dst) $ \(src, dst) _ -> do
            copyArray (castPtr dst) src (fromIntegral (listReaderStructDataSize value `div` fromIntegral bitsPerWord))

            let dst = dst `advancePtr` fromIntegral dataSize
                src = src `advancePtr` fromIntegral dataSize

            (src, dst) <- loopFold 0 pointerCount (src, dst) $ \(src, dst) _ -> do
                copyPointer segment (castPtr dst) (listReaderSegment value) (castPtr src)

                let dst = dst `advancePtr` fromIntegral pointerSizeInWords
                    src = src `advancePtr` fromIntegral pointerSizeInWords

                return (src, dst)

            return (src, dst)

        return ptr

copyPointer :: SegmentBuilder -> Ptr WirePointer -> SegmentReader -> Ptr WirePointer -> IO (Ptr CPWord)
copyPointer dstSegment dst srcSegment src = do
    src' <- peek src
    if isNull src
      then do
          poke (castPtr dst :: Ptr CPWord) 0
          return nullPtr
      else do
          (src, ptr, srcSegment) <- followFars src srcSegment

          case wirePointerKind src' of
              Struct -> do
                  let dataSize = structRefDataSize (toStructRef src')
                      pointerCount = structRefPtrCount (toStructRef src')
                      pointerSection = ptr `advancePtr` fromIntegral dataSize
                  setStructPointer dstSegment dst $
                    StructReader
                        srcSegment
                        (castPtr ptr)
                        (castPtr pointerSection)
                        (fromIntegral dataSize * fromIntegral bitsPerWord)
                        (fromIntegral pointerCount)
              List -> do
                  let elemSize = listRefElementSize $ toListRef src'

                  if elemSize == SzInlineComposite
                    then do
                        let wordCount = listRefInlineCompositeWordCount $ toListRef src'
                            tag = castPtr ptr :: Ptr WirePointer
                        tag' <- peek tag
                        ptr <- return $ ptr `advancePtr` pointerSizeInWords

                        when (wirePointerKind tag' /= Struct) $
                            fail "InlineComposite lists of non-STRUCT type are not supported."

                        let elemCount = inlineCompositeListElementCount tag'
                            wordsPerElement = structRefWordSize $ toStructRef tag'

                        when (wordsPerElement * elemCount > wordCount) $
                            fail "InlineComposite list's elements overrun its word count."

                        let dataSize = structRefDataSize $ toStructRef tag'
                            pointerCount = structRefPtrCount $ toStructRef tag'
                        setListPointer dstSegment dst $
                          ListReader
                            srcSegment
                            (castPtr ptr)
                            elemCount
                            (wordsPerElement * fromIntegral bitsPerWord)
                            (fromIntegral dataSize * fromIntegral bitsPerWord)
                            pointerCount
                    else do
                        let dataSize = dataBitsPerElement elemSize
                            pointerCount = pointersPerElement elemSize
                            step = dataSize + pointerCount * fromIntegral bitsPerPointer
                            elemCount = listRefElementCount $ toListRef src'
                            wordCount = roundBitsUpToWords $ fromIntegral elemCount * fromIntegral step

                        setListPointer dstSegment dst $
                          ListReader
                            srcSegment
                            (castPtr ptr)
                            elemCount
                            step
                            dataSize
                            (fromIntegral pointerCount)
              Far -> fail "Far pointer should have been handled above"
              Other -> do
                  unless (isCapability src') $
                      fail "Unknown pointer type."
                  fail "not implemented"

readStructPointer :: SegmentReader -> Ptr WirePointer -> Ptr CPWord -> IO StructReader
readStructPointer segment ref defaultValue = withSegmentReader segment $ \_ ->
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

readListPointer :: SegmentReader -> ElementSize -> Ptr WirePointer -> Ptr CPWord -> IO (ListReader a)
readListPointer segment expectedSize ref defaultValue = withSegmentReader segment $ \_ -> do
    pred <- wirePtrRefIsNull ref
    if pred
      then do
          pred <- wirePtrRefIsNull $ castPtr defaultValue
          if pred
            then return defaultListReader
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
                      wordsPerElement = structRefWordSize structRef
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

                  return $ ListReader
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

                  return $ ListReader
                    segment
                    (castPtr content)
                    elemCount
                    step
                    dataSize
                    (fromIntegral pointerCount)

readTextPointer :: SegmentReader -> Ptr WirePointer -> BS.ByteString -> IO TextReader
readTextPointer segment ref defaultValue = withSegmentReader segment $ \sptr -> do
    pred <- wirePtrRefIsNull ref
    if pred
      then return $ TextReader defaultValue
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

          let bs = BS.fromForeignPtr (segmentReaderForeignPtr segment) (content `minusPtr` unsafeSegmentReaderPtr segment) (fromIntegral size - 1)

          return $ TextReader bs

readDataPointer :: SegmentReader -> Ptr WirePointer -> BS.ByteString -> IO DataReader
readDataPointer segment ref defaultValue = withSegmentReader segment $ \_ -> do
    pred <- wirePtrRefIsNull ref
    if pred
      then return $ DataReader defaultValue
      else do
          (wirePtr, content, segment) <- followFars ref segment
          let listRef = toListRef wirePtr
              size = listRefElementCount listRef
          when (wirePointerKind wirePtr /= List) $
            fail "Message contains non-list pointer where data was expected."
          when (listRefElementSize listRef /= SzByte) $
            fail "Message contains list pointer of non-bytes where text was expected."

          -- XXX bounds check

          let bs = BS.fromForeignPtr (segmentReaderForeignPtr segment) (content `minusPtr` unsafeSegmentReaderPtr segment) (fromIntegral size)
          return $ DataReader bs


--------------------------------------------------------------------------------
-- Structs

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

-----------------
-- Reader Getters
-----------------
getReaderBoolField :: StructReader -> ElementCount -> IO Bool
getReaderBoolField reader offset = withSegmentReader (structReaderSegment reader) $ \_ ->
    if bitOffset < structReaderDataSize reader
      then do
          byte <- peekElemOff (structReaderData reader) (fromIntegral q) :: IO Word8
          return $ byte .&. (1 `shiftL` (fromIntegral r)) /= 0
      else return False
  where
    bitOffset = fromIntegral offset
    (q,r) = bitOffset `quotRem` fromIntegral bitsPerByte

getReaderBoolFieldMasked :: StructReader -> ElementCount -> Bool -> IO Bool
getReaderBoolFieldMasked struct index def =
    getReaderBoolField struct index >>= \val -> return $ val `xorBool` def

getReaderNumericField :: (Numeric a) => StructReader -> ElementCount -> IO a
getReaderNumericField = go undefined
  where
    go :: (Storable a, Num a) => a -> StructReader -> ElementCount -> IO a
    go dummy reader offset = withSegmentReader (structReaderSegment reader) $ \_ ->
        if bitOffset <= dataBits
          then peekElemOff (castPtr (structReaderData reader)) (fromIntegral offset)
          else return 0
      where
        bitOffset = (fromIntegral offset + 1) * (sizeOf dummy * bitsPerByte)
        dataBits = fromIntegral (structReaderDataSize reader)

getReaderNumericFieldMasked :: (Numeric a) => StructReader -> ElementCount -> a -> IO a
getReaderNumericFieldMasked reader index def = fmap (`mask` def) (getReaderNumericField reader index)

getReaderPointerField :: StructReader -> WirePointerCount -> PointerReader
getReaderPointerField reader ptrIndex =
    if (fromIntegral ptrIndex) < structReaderPtrCount reader
        then PointerReader
               (structReaderSegment reader)
               (structReaderPointers reader `advancePtr` fromIntegral ptrIndex)
        else defaultPointerReader

------------------
-- Builder Getters
------------------
getBuilderBoolField :: StructBuilder -> ElementCount -> IO Bool
getBuilderBoolField reader offset = withSegmentBuilder (structBuilderSegment reader) $ \_ -> do
    byte <- peekElemOff (structBuilderData reader) (fromIntegral q) :: IO Word8
    return $ byte .&. (1 `shiftL` (fromIntegral r)) /= 0
  where
    bitOffset = fromIntegral offset
    (q,r) = bitOffset `quotRem` fromIntegral bitsPerByte

getBuilderBoolFieldMasked :: StructReader -> ElementCount -> Bool -> IO Bool
getBuilderBoolFieldMasked struct index def =
    getReaderBoolField struct index >>= \val -> return $ (val || def) && (not (val && def))

getBuilderNumericField :: (Numeric a) => StructBuilder -> ElementCount -> IO a
getBuilderNumericField = go undefined
  where
    go :: (Storable a, Num a) => a -> StructBuilder -> ElementCount -> IO a
    go dummy reader offset = withSegmentBuilder (structBuilderSegment reader) $ \_ ->
        peekElemOff (castPtr (structBuilderData reader)) (fromIntegral offset)
      where
        bitOffset = (fromIntegral offset + 1) * (sizeOf dummy * bitsPerByte)
        dataBits = fromIntegral (structBuilderDataSize reader)

getBuilderNumericFieldMasked :: (Numeric a) => StructBuilder -> ElementCount -> a -> IO a
getBuilderNumericFieldMasked reader index def = fmap (`mask` def) (getBuilderNumericField reader index)

getBuilderPointerField :: StructBuilder -> WirePointerCount -> PointerBuilder
getBuilderPointerField reader ptrIndex =
    PointerBuilder
           (structBuilderSegment reader)
           (structBuilderPointers reader `advancePtr` fromIntegral ptrIndex)

----------
-- Setters
----------
setBoolField :: StructBuilder -> ElementCount -> Bool -> IO ()
setBoolField builder offset value = withSegmentBuilder (structBuilderSegment builder) $ \_ -> do
    byte <- peek ptr
    poke ptr (byte .|. (1 `shiftL` fromIntegral r))
  where
    bitOffset = fromIntegral offset
    (q,r) = bitOffset `quotRem` fromIntegral bitsPerByte
    ptr = structBuilderData builder `advancePtr` fromIntegral q

setBoolFieldMasked :: StructBuilder -> ElementCount -> Bool -> Bool -> IO ()
setBoolFieldMasked struct index def val = setBoolField struct index (val `xorBool` def)

setNumericField :: (Numeric a) => StructBuilder -> ElementCount -> a -> IO ()
setNumericField builder offset = pokeElemOff (castPtr $ structBuilderData builder) (fromIntegral offset)

setNumericFieldMasked :: (Numeric a) => StructBuilder -> ElementCount -> a -> a -> IO ()
setNumericFieldMasked struct index def val = setNumericField struct index (val `mask` def)


--------------------------------------------------------------------------------
-- Pointers

-----------------
-- Reader Getters
-----------------
getReaderStruct :: PointerReader -> Ptr CPWord -> IO StructReader
getReaderStruct reader = readStructPointer (pointerReaderSegment reader) (pointerReaderData reader)

getReaderList :: ListElement a => PointerReader -> Ptr CPWord -> IO (ListReader a)
getReaderList = go undefined
  where
    go :: ListElement a => a -> PointerReader -> Ptr CPWord -> IO (ListReader a)
    go dummy reader def = readListPointer (pointerReaderSegment reader) (elementSize dummy) (pointerReaderData reader) (castPtr def)

getReaderText :: PointerReader -> BS.ByteString -> IO TextReader
getReaderText reader =
    readTextPointer (pointerReaderSegment reader) (pointerReaderData reader)

getReaderData :: PointerReader -> BS.ByteString -> IO DataReader
getReaderData reader =
    readDataPointer (pointerReaderSegment reader) (pointerReaderData reader)

------------------
-- Builder Getters
------------------
getBuilderStruct :: PointerBuilder -> StructSize -> Ptr CPWord -> IO StructBuilder
getBuilderStruct builder =
    getWritableStructPointer (pointerBuilderData builder) (pointerBuilderSegment builder)

getBuilderList :: ListElement a => PointerBuilder -> Ptr CPWord -> IO (ListBuilder a)
getBuilderList = go undefined
  where
    go :: ListElement a => a -> PointerBuilder -> Ptr CPWord -> IO (ListBuilder a)
    go dummy builder = getWritableListPointer (pointerBuilderData builder) (pointerBuilderSegment builder) (elementSize dummy)

getBuilderText :: PointerBuilder -> BS.ByteString -> IO TextBuilder
getBuilderText builder =
    getWritableTextPointer (pointerBuilderData builder) (pointerBuilderSegment builder)

getBuilderData :: PointerBuilder -> BS.ByteString -> IO DataBuilder
getBuilderData builder =
    getWritableDataPointer (pointerBuilderData builder) (pointerBuilderSegment builder)

-----------
-- Initters
-----------
initStruct :: PointerBuilder -> StructSize -> IO StructBuilder
initStruct builder =
    initStructPointer (pointerBuilderData builder) (pointerBuilderSegment builder)

initList :: ListElement a => PointerBuilder -> ElementCount32 -> IO (ListBuilder a)
initList = go undefined
  where
    go :: ListElement a => a -> PointerBuilder -> ElementCount32 -> IO (ListBuilder a)
    go dummy builder count = initListPointer (pointerBuilderData builder) (pointerBuilderSegment builder) count (elementSize dummy)

initText :: PointerBuilder -> ByteCount32 -> IO TextBuilder
initText builder =
    initTextPointer (pointerBuilderData builder) (pointerBuilderSegment builder)

initData :: PointerBuilder -> ByteCount32 -> IO DataBuilder
initData builder =
    initDataPointer (pointerBuilderData builder) (pointerBuilderSegment builder)

----------
-- Setters
----------
setStruct :: PointerBuilder -> StructReader -> IO ()
setStruct builder = void . setStructPointer (pointerBuilderSegment builder) (pointerBuilderData builder)

setList :: PointerBuilder -> ListReader a -> IO ()
setList builder = void . setListPointer (pointerBuilderSegment builder) (pointerBuilderData builder)

setText :: PointerBuilder -> BS.ByteString -> IO ()
setText builder = void . setTextPointer (pointerBuilderData builder) (pointerBuilderSegment builder)

setData :: PointerBuilder -> BS.ByteString -> IO ()
setData builder = void . setDataPointer (pointerBuilderData builder) (pointerBuilderSegment builder)

--------------------------------------------------------------------------------
-- Lists

getReaderStructElement :: ListReader a -> ElementCount32 -> StructReader
getReaderStructElement reader index =
    StructReader (listReaderSegment reader)
                 structData
                 structPointers
                 (listReaderStructDataSize reader)
                 (listReaderStructPtrCount reader)
  where
    indexBit = fromIntegral index * fromIntegral (listReaderStep reader) :: BitCount64
    structData = listReaderData reader `plusPtr` (fromIntegral (indexBit `div` fromIntegral bitsPerByte)) :: Ptr Word8
    structPointers = structData `plusPtr` (fromIntegral (listReaderStructDataSize reader `div` fromIntegral bitsPerByte))

getReaderPointerElement :: ListReader a -> ElementCount32 -> PointerReader
getReaderPointerElement reader index =
    PointerReader (listReaderSegment reader) ptr
  where
    ptr = listReaderData reader `plusPtr` offset
    offset = fromIntegral index * (fromIntegral (listReaderStep reader `div` fromIntegral bitsPerByte))

getBuilderPointerElement :: ListBuilder a -> ElementCount32 -> PointerBuilder
getBuilderPointerElement builder index =
    PointerBuilder (listBuilderSegment builder) ptr
  where
    ptr = listBuilderData builder `plusPtr` offset
    offset = fromIntegral index * (fromIntegral (listBuilderStep builder `div` fromIntegral bitsPerByte))

-- TODO:
-- * assert index < len
-- * checked/unchecked variants
class ListElement a where
    elementSize :: a -> ElementSize

    getReaderElement :: ListReader a -> ElementCount -> IO a
    default getReaderElement :: (Storable a) => ListReader a -> ElementCount -> IO a
    getReaderElement reader index =
        peek ptr
      where
        offset = index * fromIntegral (listReaderStep reader) `div` fromIntegral bitsPerByte
        ptr = listReaderData reader `plusPtr` fromIntegral offset

    getBuilderElement :: ListBuilder a -> ElementCount -> IO a
    getBuilderElement = undefined

    setBuilderElement :: ListBuilder a -> ElementCount -> a -> IO ()
    default setBuilderElement :: (Storable a) => ListBuilder a -> ElementCount -> a -> IO ()
    setBuilderElement builder index =
        poke ptr
      where
        offset = index * fromIntegral (listBuilderStep builder) `div` fromIntegral bitsPerByte
        ptr = listBuilderData builder `plusPtr` fromIntegral offset

instance ListElement Bool where
    elementSize _ = SzBit
    getReaderElement reader index = do
        val <- peek ptr
        return $ val .&. (1 `shiftL` fromIntegral bitNum) /= 0
      where
        bindex = fromIntegral index * fromIntegral (listReaderStep reader) :: Word64
        (offset, bitNum) = bindex `quotRem` fromIntegral bitsPerByte
        ptr = listReaderData reader `plusPtr` fromIntegral offset :: Ptr Word8

    setBuilderElement builder index value = do
        byte <- peek ptr
        poke ptr $ byte .|. (1 `shiftL` fromIntegral bitNum)
      where
        bindex = fromIntegral index * fromIntegral (listBuilderStep builder) :: Word64
        (offset, bitNum) = bindex `quotRem` fromIntegral bitsPerByte
        ptr = listBuilderData builder `plusPtr` fromIntegral offset :: Ptr Word8

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
    getReaderElement reader index = return $ getReaderStructElement reader (fromIntegral index)

    setBuilderElement builder index value =
        void $ setStructPointer (pointerBuilderSegment ptrBuilder) (pointerBuilderData ptrBuilder) value
      where
        ptrBuilder = getBuilderPointerElement builder (fromIntegral index)

instance ListElement TextReader where
    elementSize _ = SzPointer
    getReaderElement reader index = getReaderText ptrReader "\NULL"
      where
        ptrReader = getReaderPointerElement reader (fromIntegral index)

    setBuilderElement builder index (TextReader value) =
        void $ setTextPointer (pointerBuilderData ptrBuilder) (pointerBuilderSegment ptrBuilder) value
      where
        ptrBuilder = getBuilderPointerElement builder (fromIntegral index)

instance ListElement DataReader where
    elementSize _ = SzPointer
    getReaderElement reader index = getReaderData ptrReader ""
      where
        ptrReader = getReaderPointerElement reader (fromIntegral index)

    setBuilderElement builder index (DataReader value) =
        void $ setDataPointer (pointerBuilderData ptrBuilder) (pointerBuilderSegment ptrBuilder) value
      where
        ptrBuilder = getBuilderPointerElement builder (fromIntegral index)

instance (ListElement a) => ListElement (ListReader a) where
    elementSize _ = SzPointer
    getReaderElement reader index = getReaderList ptrReader nullPtr
      where
        ptrReader = getReaderPointerElement reader (fromIntegral index)

    setBuilderElement builder index value =
        void $ setListPointer (pointerBuilderSegment ptrBuilder) (pointerBuilderData ptrBuilder) value
      where
        ptrBuilder = getBuilderPointerElement builder (fromIntegral index)


--------------------------------------------------------------------------------
-- List Utils (TODO: rewrite-rules (if needed))

listLength :: ListReader a -> ElementCount
listLength reader = fromIntegral $ listReaderElementCount reader

toList :: (ListElement a) => ListReader a -> IO [a]
toList list = mapM (getReaderElement list) [0..listLength list-1]

eachElement_ :: (ListElement a) => ListReader a -> (a -> IO b) -> IO ()
eachElement_ list fn =
    unless (listLength list == 0) $
      mapM_ (fn <=< getReaderElement list) [0..listLength list-1]

eachElement :: (ListElement a) => ListReader a -> (a -> IO b) -> IO [b]
eachElement list fn =
    if listLength list == 0
      then return []
      else mapM (fn <=< getReaderElement list) [0..listLength list-1]

mapElements :: (ListElement a) => (a -> IO b) -> ListReader a -> IO [b]
mapElements fn list =
    if listLength list == 0
      then return []
      else mapM (fn <=< getReaderElement list) [0..listLength list-1]

foldrElements :: (ListElement a) => (a -> b -> IO b) -> b -> ListReader a -> IO b
foldrElements f z0 list = go len z0
  where
    len = listLength list
    go n z =
        if n == 0
          then return z
          else do
              elem <- getReaderElement list (n-1)
              f elem z >>= go (n-1)

foldlElements :: (ListElement a) => (b -> a -> IO b) -> b -> ListReader a -> IO b
foldlElements f z0 list = go 0 z0
  where
    len = listLength list
    go n z =
        if n == len
          then return z
          else do
              elem <- getReaderElement list n
              f z elem >>= go (n+1)


--------------------------------------------------------------------------------
-- Numeric

class (Num a, Storable a) => Numeric a where
    default mask :: Bits a => a -> a -> a
    mask :: a -> a -> a
    mask = xor

instance Numeric Word8
instance Numeric Word16
instance Numeric Word32
instance Numeric Word64
instance Numeric Int8
instance Numeric Int16
instance Numeric Int32
instance Numeric Int64
instance Numeric Float where
    mask = xorFloat
instance Numeric Double where
    mask = xorDouble


--------------------------------------------------------------------------------
-- Misc. Utils

-- loop from `start` (inclusive) to `end` (exclusive)
loop :: (Monad m, Num a, Eq a) => a -> a -> (a -> m b) -> m ()
loop start end f = loopFold_ start end () (\_ x -> void $ f x )

loopFold_ :: (Monad m, Num a, Eq a) => a -> a -> acc -> (acc -> a -> m acc) -> m ()
loopFold_ start end acc = void . loopFold start end acc

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

xorFloat :: Float -> Float -> Float
xorFloat a b = wordToFloat (floatToWord a `xor` floatToWord b)

xorDouble :: Double -> Double -> Double
xorDouble a b = wordToDouble (doubleToWord a `xor` doubleToWord b)

xorBool :: Bool -> Bool -> Bool
xorBool a b = (a || b) && (not (a && b))

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

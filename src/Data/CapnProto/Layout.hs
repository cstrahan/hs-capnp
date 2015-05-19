{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.CapnProto.Layout where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
{- import           Data.ByteString          (ByteString (..)) -}
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS (fromForeignPtr)
import           Data.Int
import           Data.List
import           Data.Monoid
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types          (CChar)
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO
import           Text.Printf

import           Data.CapnProto.Arena
import           Data.CapnProto.Mask
import           Data.CapnProto.Units

--------------------------------------------------------------------------------

debug = hPutStrLn stderr

debugSegment :: SegmentReader -> IO ()
debugSegment reader = withSegment reader $ \ptr -> do
    bytes <- peekArray (fromIntegral (segmentReaderSize reader) * bytesPerWord) (castPtr ptr) :: IO [Word8]
    let hex = unwords (map (printf "%02x") bytes)
    debug $ "SEGMENT: " <> hex

--------------------------------------------------------------------------------

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
  { textReaderData :: BS.ByteString
  }

newtype DataReader = DataReader
  { dataReaderData :: BS.ByteString
  }

data StructReader = StructReader
  { structReaderSegment  :: SegmentReader
  , structReaderData     :: Ptr Word8
  , structReaderPointers :: Ptr WirePointer
  , structReaderDataSize :: BitCount32
  , structReaderPtrCount :: WirePointerCount16
  }

data UntypedListReader = UntypedListReader
  { untypedListReaderSegment        :: SegmentReader
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

data NotInSchema = NotInSchema Word16 deriving (Show)

--------------------------------------------------------------------------------

data WirePointerKind =
    Struct
  | List
  | Far
  | Other
  deriving (Show, Eq)

-- | Little-endian encoded value.
-- TODO: handle endianness correctly
type WireValue a = a

data PointerReader = PointerReader
  { pointerReaderSegment :: SegmentReader
  , pointerReaderData    :: Ptr WirePointer
  }

data WirePointer = WirePointer
  { offsetAndKind :: WireValue Word32
  , upper32Bits   :: Word32
  } deriving (Show)

instance Nullable WirePointer where
    isNull ptr = offsetAndKind ptr == 0 && upper32Bits ptr == 0

instance Storable WirePointer where
    sizeOf _ = 8
    alignment _ = 8
    poke ptr (WirePointer a b) =
        let p0 = castPtr ptr :: Ptr (WireValue Word32)
            p1 = castPtr $ p0 `offsetPtr` 1
        in poke p0 a >> poke p1 b
    peek ptr =
        let p0 = castPtr ptr :: Ptr (WireValue Word32)
            p1 = castPtr $ p0 `offsetPtr` 1
        in WirePointer <$> peek p0 <*> peek p1

containsInterval :: SegmentReader -> Ptr Word -> Ptr Word -> Bool
containsInterval segment from to =
    let thisBegin = unsafeSegmentReaderPtr segment
        thisEnd = thisBegin `plusPtr` (fromIntegral (segmentReaderSize segment) * bytesPerWord)
    in from >= thisBegin && to <= thisEnd && from <= to

doBoundsCheck segment start end kind =
    case boundsCheck segment start end kind of
        Right _ -> return ()
        Left msg -> fail msg

boundsCheck :: SegmentReader -> Ptr Word -> Ptr Word -> WirePointerKind -> Either String ()
boundsCheck segment start end kind =
    if isNull segment || containsInterval segment start end
      then Right ()
      else Left $ case kind of
                   List -> "Message contained out-of-bounds list pointer."
                   Struct -> "Message contained out-of-bounds struct pointer."
                   Far -> "Message contained out-of-bounds far pointer."
                   Other -> "Message contained out-of-bounds other pointer."

getRoot :: SegmentReader -> Ptr Word -> Either String PointerReader
getRoot segment location =
    case boundsCheck segment location (location `plusPtr` pointerSizeInWords) Struct of
        Left msg -> Left msg
        Right _ -> Right $ PointerReader segment (castPtr location)

wirePointerKind :: WirePointer -> WirePointerKind
wirePointerKind ptr =
    case (offsetAndKind ptr) .&. 3 of
        0 -> Struct
        1 -> List
        2 -> Far
        3 -> Other

nonFarOffset :: WirePointer -> Int
nonFarOffset = (`shiftR` 2) . fromIntegral . offsetAndKind

farPositionInSegment :: WirePointer -> WordCount32
farPositionInSegment = (`shiftR` 3) . offsetAndKind

isDoubleFar :: WirePointer -> Bool
isDoubleFar ptr = (offsetAndKind ptr .&. 4) /= 0

segmentId :: WirePointer -> SegmentId
segmentId ptr = fromIntegral (upper32Bits ptr)

inlineCompositeListElementCount :: WirePointer -> ElementCount32
inlineCompositeListElementCount ptr = offsetAndKind ptr `shiftR` 2

getData :: PointerReader -> Ptr Word -> ByteCount32 -> IO DataReader
getData reader =
    readDataPointer (pointerReaderSegment reader) (pointerReaderData reader)

readDataPointer :: SegmentReader -> Ptr WirePointer -> Ptr Word -> ByteCount32 -> IO DataReader
readDataPointer segment ref defaultValue defaultSize = withSegment segment $ \_ -> do
    pred <- wirePtrRefIsNull ref
    if pred
      then fail "Data.CapnProto.Layout.readDataPointer: not implemented"
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

getText :: PointerReader -> Ptr Word -> ByteCount32 -> IO TextReader
getText reader =
    readTextPointer (pointerReaderSegment reader) (pointerReaderData reader)

readTextPointer :: SegmentReader -> Ptr WirePointer -> Ptr Word -> ByteCount32 -> IO TextReader
readTextPointer segment ref defaultValue defaultSize = withSegment segment $ \sptr -> do
    pred <- wirePtrRefIsNull ref
    if pred
      then fail "Data.CapnProto.Layout.readTextPointer: not implemented"
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

getList :: PointerReader -> ElementSize -> Ptr Word -> IO UntypedListReader
getList reader expectedSize =
    readListPointer (pointerReaderSegment reader) expectedSize (pointerReaderData reader)

readListPointer :: SegmentReader -> ElementSize -> Ptr WirePointer -> Ptr Word -> IO UntypedListReader
readListPointer segment expectedSize ref defaultValue = withSegment segment $ \_ ->
    if isNull ref
      then do
          pred <- wirePtrRefIsNull $ castPtr defaultValue
          if pred
            then return defaultUntypedListReader
            else fail "Data.CapnProto.Layout.readListPointer: not implemented"
      else do
          (wirePtr, content, segment) <- followFars ref segment
          when (wirePointerKind wirePtr /= List) $
            fail "Message contains non-list pointer where list pointer was expected"
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
                               then return $ content `plusPtr` ((fromIntegral $ structRefDataSize structRef) * bytesPerWord)
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

getStruct :: PointerReader -> Ptr Word -> IO StructReader
getStruct reader = readStructPointer (pointerReaderSegment reader) (pointerReaderData reader)

readStructPointer :: SegmentReader -> Ptr WirePointer -> Ptr Word -> IO StructReader
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
              ptrs = content `plusPtr` (bytesPerWord * fromIntegral dataWords)
          -- XXX bounds check
          return $ StructReader segment (castPtr content) ptrs (fromIntegral dataWords * fromIntegral bitsPerWord) numPtrs

wirePtrRefIsNull :: Ptr WirePointer -> IO Bool
wirePtrRefIsNull ref =
    if isNull ref
      then return True
      else do
          wirePtr <- peek ref
          if isNull wirePtr
            then return True
            else return False

followFars :: Ptr WirePointer -> SegmentReader
           -> IO ( WirePointer   -- The resolved non-far-pointer
                 , Ptr Word      -- The pointer the the actual content
                 , SegmentReader -- The target segment, in the case of far-pointers
                 )
followFars ref segment = do
    wirePtr <- peek ref
    if (isNull segment) || wirePointerKind wirePtr /= Far
      then do
          let contentPtr = (ref `plusPtr` (nonFarOffset wirePtr * bytesPerWord)) `plusPtr` sizeOf (undefined :: WirePointer)
          return (wirePtr, contentPtr, segment)
      else do
          let segment' = tryGetSegment (segmentReaderArena segment) (segmentId wirePtr)
              padWords = if isDoubleFar wirePtr then 2 else 1
          withSegment segment' $ \segmentPtr -> do
              let landingPad = segmentPtr `plusPtr` (bytesPerWord * fromIntegral (farPositionInSegment wirePtr))
              -- XXX bounds check
              if isDoubleFar wirePtr
                then do
                    wirePtr' <- peek landingPad
                    let segment'' = tryGetSegment (segmentReaderArena segment') (segmentId wirePtr')
                    contentPtr <- withSegment segment'' (return . (`plusPtr` fromIntegral (farPositionInSegment wirePtr' * fromIntegral bytesPerWord)))
                    finalWirePtr <- peek $ landingPad `plusPtr` bytesPerWord
                    return (finalWirePtr, contentPtr, segment'')
                else do
                    wirePtr' <- peek landingPad
                    let contentPtr = landingPad `plusPtr` (bytesPerWord * (nonFarOffset wirePtr' + 1))
                    finalWirePtr <- peek $ landingPad
                    return (finalWirePtr, contentPtr, segment')

roundBitsUpToWords :: BitCount64 -> WordCount32
roundBitsUpToWords bits = fromIntegral $ (bits + 63) `div` (fromIntegral bitsPerWord)

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
    getField :: StructReader -> ElementCount -> IO a

    default getField :: (Storable a, Zero a) => StructReader -> ElementCount -> IO a
    getField = getDataField

instance StructField Bool where
    getField = getBoolField

instance StructField Word8
instance StructField Word16
instance StructField Word32
instance StructField Word64
instance StructField Int8
instance StructField Int16
instance StructField Int32
instance StructField Int64
instance StructField Float
instance StructField Double

instance StructField TextReader where
    getField reader index = getText ptrReader nullPtr 0
      where
        ptrReader = getPointerField reader (fromIntegral index)

instance StructField DataReader where
    getField reader index = getData ptrReader nullPtr 0
      where
        ptrReader = getPointerField reader (fromIntegral index)

instance StructField PointerReader where
    getField reader index = return $ getPointerField reader (fromIntegral index)

instance StructField StructReader where
    getField reader index = getStruct ptrReader nullPtr
      where
        ptrReader = getPointerField reader (fromIntegral index)

instance (ListElement a) => StructField (ListReader a) where
    getField = getFieldHack undefined
      where
        getFieldHack :: (ListElement a) => a -> StructReader -> ElementCount -> IO (ListReader a)
        getFieldHack dummy reader index = ListReader <$> getList ptrReader (elementSize dummy) nullPtr
          where
            ptrReader = getPointerField reader (fromIntegral index)

getFieldMasked :: (StructField a, Zero a, Mask a) => StructReader -> ElementCount -> MaskTy a -> IO a
getFieldMasked reader offset m = fmap (`mask` m) $ getField reader offset

---------------------

getDataFieldMask :: (Storable a, Zero a, Mask a)
                 => StructReader -> ElementCount -> MaskTy a -> IO a
getDataFieldMask reader offset m = fmap (`mask` m) $ getDataField reader offset

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
               (structReaderPointers reader `plusPtr` (bytesPerWord * fromIntegral ptrIndex))
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
    offset = (fromIntegral (untypedListReaderStep reader `div` fromIntegral bitsPerByte))

--------------------------------------------------------------------------------

newtype ListReader a = ListReader UntypedListReader
listLength :: ListReader a -> Int
listLength (ListReader reader) = fromIntegral $ untypedListReaderElementCount reader

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

instance ListElement PointerReader where
    elementSize _ = SzPointer
    getUntypedElement reader index = return $ getPointerElement reader (fromIntegral index)

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

{-# INLINE offsetPtr #-}
offsetPtr :: Storable a => Ptr a -> Int -> Ptr a
offsetPtr = offsetPtrHack undefined
  where
    offsetPtrHack :: Storable a => a -> Ptr a -> Int -> Ptr a
    offsetPtrHack dummy ptr offset = ptr `plusPtr` (sizeOf dummy * offset)

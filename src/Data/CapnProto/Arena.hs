{-# LANGUAGE LambdaCase #-}

module Data.CapnProto.Arena where

import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.ByteString.Internal  (toForeignPtr)
import           Data.IORef
import           Data.Monoid
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Marshal.Array     hiding (newArray)
import           Foreign.Ptr
import           System.IO.Unsafe          (unsafeDupablePerformIO,
                                            unsafePerformIO)

import           Data.CapnProto.Units

--------------------------------------------------------------------------------
-- Segments

type SegmentId = Word32

data SegmentReader = SegmentReader
  { segmentReaderArena      :: Arena
  , segmentReaderForeignPtr :: ForeignPtr Word8
  , unsafeSegmentReaderPtr  :: Ptr CPWord
  , segmentReaderSize       :: WordCount32
  }

data SegmentBuilder = SegmentBuilder
  { segmentBuilderSegmentReader :: SegmentReader
  , segmentBuilderId            :: SegmentId
  , segmentBuilderPos           :: IORef (Ptr CPWord)
  }

segmentBuilderGetArena :: SegmentBuilder -> BuilderArena
segmentBuilderGetArena segment =
    case segmentReaderArena (segmentBuilderSegmentReader segment) of
        ABuilder arena -> arena
        _ -> error "the impossible happened"

segmentBuilderForeignPtr :: SegmentBuilder -> ForeignPtr Word8
segmentBuilderForeignPtr = segmentReaderForeignPtr . segmentBuilderSegmentReader

unsafeSegmentBuilderPtr  :: SegmentBuilder -> Ptr CPWord
unsafeSegmentBuilderPtr = unsafeSegmentReaderPtr . segmentBuilderSegmentReader

segmentBuilderSize :: SegmentBuilder -> WordCount32
segmentBuilderSize = segmentReaderSize . segmentBuilderSegmentReader

instance Nullable (SegmentReader) where
    isNull segment = unsafeSegmentReaderPtr segment == nullPtr

instance Nullable (SegmentBuilder) where
    isNull = isNull . segmentBuilderSegmentReader

{-# NOINLINE nullSegmentReader #-}
nullSegmentReader :: SegmentReader
nullSegmentReader =
    SegmentReader (AReader nullArenaReader)
            (unsafePerformIO . newForeignPtr_ $ nullPtr)
            nullPtr
            0
  where
    nullPos = unsafePerformIO $ newIORef nullPtr

{-# NOINLINE nullSegmentBuilder #-}
nullSegmentBuilder :: SegmentBuilder
nullSegmentBuilder =
    SegmentBuilder nullSegmentReader 0 nullPos
  where
    nullPos = unsafePerformIO $ newIORef nullPtr

withSegmentReader :: SegmentReader -> (Ptr CPWord -> IO b) -> IO b
withSegmentReader reader f = withForeignPtr (segmentReaderForeignPtr reader) $ \_ ->
    f (unsafeSegmentReaderPtr reader)

withSegmentBuilder :: SegmentBuilder -> (Ptr CPWord -> IO b) -> IO b
withSegmentBuilder = withSegmentReader . segmentBuilderSegmentReader

-- TODO: assert ptr > segPtr
getWordOffsetTo :: SegmentBuilder -> Ptr a -> WordCount32
getWordOffsetTo segment ptr = fromIntegral wordCount
  where
    byteOffset = ptr `minusPtr` unsafeSegmentBuilderPtr segment
    wordCount = byteOffset `div` bytesPerWord

containsInterval :: SegmentReader -> Ptr CPWord -> Ptr CPWord -> Bool
containsInterval segment from to =
    let thisBegin = unsafeSegmentReaderPtr segment
        thisEnd = thisBegin `advancePtr` fromIntegral (segmentReaderSize segment)
    in from >= thisBegin && to <= thisEnd && from <= to


segmentAllocate :: SegmentBuilder -> WordCount32 -> IO (Maybe (Ptr CPWord))
segmentAllocate segment amount = do
    currSize <- segmentCurrentSize segment
    if amount > segmentBuilderSize segment - currSize
      then return Nothing
      else do
          pos <- readIORef $ segmentBuilderPos segment
          writeIORef (segmentBuilderPos segment) (pos `plusPtr` (fromIntegral amount * bytesPerWord))
          return . Just $ pos

--------------------------------------------------------------------------------
-- Arenas

data Arena
    = AReader ReaderArena
    | ABuilder BuilderArena

data ReaderArena = ReaderArena
  { readerArenaMoreSegments :: IORef [SegmentReader] -- segments are lazily allocated, so this is mutable.
  }

data BuilderArena = BuilderArena
  { builderArenaMoreSegments       :: IORef [SegmentBuilder]
  , builderArenaAllocationStrategy :: AllocationStrategy
  , builderArenaNextSize           :: IORef Word32
  }

data AllocationStrategy
  = FixedSize
  | GrowHeuristically

nullArenaReader :: ReaderArena
nullArenaReader =
    ReaderArena (unsafePerformIO $ newIORef [nullSegmentReader])

nullArenaBuilder :: BuilderArena
nullArenaBuilder =
    BuilderArena (unsafePerformIO $ newIORef [nullSegmentBuilder])
          FixedSize
          (unsafePerformIO $ newIORef 0)

allocateSegmentBuilder :: BuilderArena -> Int -> IO SegmentBuilder
allocateSegmentBuilder arena numWords = do
    fptr <- mallocForeignPtrBytes (fromIntegral $ numWords * bytesPerWord)
    let ptr = castPtr $ unsafeForeignPtrToPtr fptr
        segmentId = 0 -- XXX
    pos <- newIORef ptr
    return $ SegmentBuilder
        (SegmentReader (ABuilder arena) fptr ptr (fromIntegral numWords))
        segmentId
        pos

segmentReaderGetPtrUnchecked :: SegmentReader -> WordCount32 -> Ptr CPWord
segmentReaderGetPtrUnchecked segment offset = unsafeSegmentReaderPtr segment `plusPtr` (fromIntegral offset * bytesPerWord)

segmentBuilderGetPtrUnchecked :: SegmentBuilder -> WordCount32 -> Ptr CPWord
segmentBuilderGetPtrUnchecked segment offset = unsafeSegmentBuilderPtr segment `plusPtr` (fromIntegral offset * bytesPerWord)

segmentCurrentSize :: SegmentBuilder -> IO WordCount32
segmentCurrentSize segment = do
    pos <- readIORef . segmentBuilderPos $ segment
    return $ fromIntegral $ (pos `minusPtr` unsafeSegmentBuilderPtr segment) `div` bytesPerWord

appendSegment :: BuilderArena -> SegmentBuilder -> IO ()
appendSegment arena segment = do
    segments <- readIORef . builderArenaMoreSegments $ arena
    writeIORef (builderArenaMoreSegments arena) (segments ++ [ segment ])

numSegments :: BuilderArena -> IO Int
numSegments arena = length <$> readIORef (builderArenaMoreSegments arena)

segmentReaderGetFirstSegment :: ReaderArena -> IO SegmentReader
segmentReaderGetFirstSegment arena = head <$> readIORef (readerArenaMoreSegments arena)

segmentBuilderGetFirstSegment :: BuilderArena -> IO SegmentBuilder
segmentBuilderGetFirstSegment arena = head <$> readIORef (builderArenaMoreSegments arena)

getLastSegment :: BuilderArena -> IO SegmentBuilder
getLastSegment arena = last <$> readIORef (builderArenaMoreSegments arena)

arenaAllocate :: BuilderArena -> WordCount32 -> IO (SegmentBuilder, Ptr CPWord)
arenaAllocate arena amount = do
    segment <- getLastSegment arena
    segmentAllocate segment amount >>= \case
        Just result -> return (segment, result)
        Nothing -> do
            id <- fromIntegral <$> numSegments arena
            (fptr, size) <- allocateOwnedMemory arena amount
            let ptr = castPtr $ unsafeForeignPtrToPtr fptr
            pos <- newIORef ptr
            let segment = SegmentBuilder (SegmentReader (ABuilder arena) fptr ptr size) id pos
            appendSegment arena segment
            return (segment, ptr)
  where
    allocateOwnedMemory :: BuilderArena -> WordCount32 -> IO (ForeignPtr Word8, WordCount32)
    allocateOwnedMemory arena minSize = do
        nextSize <- readIORef (builderArenaNextSize arena)
        let size = max minSize nextSize
        let sizeInBytes = fromIntegral size * bytesPerWord
        fptr <- mallocForeignPtrBytes $ sizeInBytes
        case builderArenaAllocationStrategy arena of
            GrowHeuristically ->
              void $ writeIORef (builderArenaNextSize arena) (nextSize + size)
            _ ->
              return ()
        return (fptr, size)

-- XXX
arenaFromByteStrings :: [BS.ByteString] -> ReaderArena
arenaFromByteStrings strs = arena
  where
    arena = ReaderArena (unsafePerformIO $ newIORef segments)
    segments = map segmentFromByteString strs
    segmentFromByteString str =
        let (fptr, offset, len) = toForeignPtr str
            ptr = castPtr $ plusPtr (unsafeForeignPtrToPtr fptr) offset
            segmentReader =
                SegmentReader
                    (AReader arena)
                    (castForeignPtr fptr)
                    ptr
                    (fromIntegral len `div` fromIntegral bytesPerWord)
        in segmentReader

readerArenaGetSegment :: ReaderArena -> SegmentId -> IO SegmentReader
readerArenaGetSegment arena id = do
    segments <- readIORef $ readerArenaMoreSegments arena
    case segments `at` fromIntegral id of
        Left err -> fail $ "Invalid segment id: "<>err
        Right res -> return res

builderArenaGetSegment :: BuilderArena -> SegmentId -> IO SegmentBuilder
builderArenaGetSegment arena id = do
    segments <- readIORef $ builderArenaMoreSegments arena
    case segments `at` fromIntegral id of
        Left err -> fail $ "Invalid segment id: "<>err
        Right res -> return res

arenaGetSegment :: Arena -> SegmentId -> IO SegmentReader
arenaGetSegment arena id =
    case arena of
        AReader reader -> readerArenaGetSegment reader id
        ABuilder builder -> segmentBuilderSegmentReader <$> builderArenaGetSegment builder id

--------------------------------------------------------------------------------
-- Misc. Utils

at :: [a] -> Int -> Either String a
at xs o | o < 0 = Left $ "index must not be negative, index=" ++ show o
         | otherwise = f o xs
    where f 0 (x:xs) = Right x
          f i (x:xs) = f (i-1) xs
          f i [] = Left $ "index too large, index=" ++ show o ++ ", length=" ++ show (o-i)

atMay :: [a] -> Int -> Maybe a
atMay xs o = eitherToMaybe $ at xs o

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

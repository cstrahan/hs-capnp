{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

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
import qualified Data.Vector.Mutable.Dynamic as VMD
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector                 as V

import           Data.CapnProto.Units

--------------------------------------------------------------------------------

data MessageReader = MessageReader
  { messageReaderArena :: ReaderArena
  }

data MessageBuilder = MessageBuilder
  { messageBuilderArena     :: BuilderArena
  , messageBuilderAllocator :: SomeAllocator
  }

class Allocator a where
    allocateSegment :: a -> Word32 -> IO (ForeignPtr CPWord, Word32)

data SomeAllocator = forall a. Allocator a => SomeAllocator a

data HeapAllocator = HeapAllocator
  { heapAllocatorNextSize :: IORef Word32
  , heapAllocatorMemory   :: VMD.IOVector (ForeignPtr CPWord)
  , heapAllocatorStrategy :: AllocationStrategy
  }

instance Allocator HeapAllocator where
    allocateSegment allocator minSize = do
        prevSize <- readIORef (heapAllocatorNextSize allocator)
        let size = max minSize prevSize
        newWords <- mallocForeignPtrBytes (fromIntegral $ size * fromIntegral bytesPerWord)
        VMD.pushBack (heapAllocatorMemory allocator) newWords
        when (heapAllocatorStrategy allocator == GrowHeuristically) $
            writeIORef (heapAllocatorNextSize allocator) (prevSize + size)
        return (newWords, size)

newHeapAllocator :: Word32 -> AllocationStrategy -> IO HeapAllocator
newHeapAllocator firstSegmentSize strategy =
    HeapAllocator <$> newIORef firstSegmentSize <*> VMD.new 1 <*> return strategy


--------------------------------------------------------------------------------

class AsReader builder where
    type ReaderTy builder :: *
    asReader :: builder -> ReaderTy builder

--------------------------------------------------------------------------------
-- Segments

type SegmentId = Word32

data SegmentReader = SegmentReader
  { segmentReaderArena      :: Arena
  , segmentReaderForeignPtr :: ForeignPtr Word8
  , unsafeSegmentReaderPtr  :: Ptr CPWord
  , segmentReaderSize       :: WordCount32
  }

instance Nullable (SegmentReader) where
    isNull segment = unsafeSegmentReaderPtr segment == nullPtr

instance AsReader SegmentReader where
    type ReaderTy SegmentReader = SegmentReader
    asReader = id

data SegmentBuilder = SegmentBuilder
  { segmentBuilderSegmentReader :: SegmentReader
  , segmentBuilderId            :: SegmentId
  , segmentBuilderPos           :: IORef (Ptr CPWord)
  }

instance Nullable (SegmentBuilder) where
    isNull = isNull . segmentBuilderSegmentReader

instance AsReader SegmentBuilder where
    type ReaderTy SegmentBuilder = SegmentReader
    asReader = segmentBuilderSegmentReader

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

--  raw_segments: &'static ReaderSegments,
--  pub segment0: SegmentReader,
--  pub more_segments: HashMap<SegmentId, Box<SegmentReader>>,
data ReaderArena = ReaderArena
  { readerArenaSegment0     :: SegmentReader
  , readerArenaMoreSegments :: VMD.IOVector SegmentReader -- segments are lazily allocated, so this is mutable.
  }

data BuilderArena = BuilderArena
  { builderArenaAllocator          :: SomeAllocator
  , builderArenaSegment0           :: SegmentBuilder
  , builderArenaMoreSegments       :: VMD.IOVector SegmentBuilder
  }

data AllocationStrategy
  = FixedSize
  | GrowHeuristically
  deriving (Eq)

nullArenaReader :: ReaderArena
nullArenaReader =
    ReaderArena nullSegmentReader (unsafePerformIO $ VMD.new 0)

-- TODO: do I need these?
--nullArenaBuilder :: BuilderArena
--nullArenaBuilder =
--    --BuilderArena (unsafePerformIO $ newIORef [nullSegmentBuilder])
--    BuilderArena (unsafePerformIO $ VMD.new 0)
--          FixedSize
--          (unsafePerformIO $ VMD.new 0)

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

segmentBuilderGetFirstSegment :: BuilderArena -> IO SegmentBuilder
segmentBuilderGetFirstSegment arena = VMD.readFront (builderArenaMoreSegments arena)

arenaAllocate :: BuilderArena -> WordCount32 -> IO (SegmentBuilder, Ptr CPWord)
arenaAllocate arena amount =
    segmentAllocate (builderArenaSegment0 arena) amount >>= \case
        Just result -> return (builderArenaSegment0 arena, result)
        Nothing -> do
            len <- VMD.length (builderArenaMoreSegments arena)
            if len == 0
              then allocateViaAllocator 1 (builderArenaAllocator arena)
              else do
                  segBuilder <- VMD.unsafeRead (builderArenaMoreSegments arena) (len - 1)
                  segmentAllocate segBuilder amount >>= \case
                      Just words -> return (segBuilder, words)
                      Nothing    -> allocateViaAllocator (fromIntegral len+1) (builderArenaAllocator arena)
  where
    allocateViaAllocator :: SegmentId -> SomeAllocator -> IO (SegmentBuilder, Ptr CPWord)
    allocateViaAllocator id (SomeAllocator allocator) = do
        (words, size) <- allocateSegment allocator amount
        wordsRef <- newIORef (unsafeForeignPtrToPtr words)
        let builder = SegmentBuilder (SegmentReader (ABuilder arena) (castForeignPtr words) (unsafeForeignPtrToPtr words) size) (fromIntegral id) wordsRef
        VMD.pushBack (builderArenaMoreSegments arena) builder
        segmentAllocate builder amount >>= \case
            Just words -> return (builder, words)
            Nothing ->
                -- Shouldn't happen; the allocator should provide enough space.
                fail "the impossible happened"

-- XXX
arenaFromByteStrings :: [BS.ByteString] -> ReaderArena
arenaFromByteStrings strs = arena
  where
    arena = ReaderArena (head segments) (unsafePerformIO $ VMD.unsafeThaw $ V.fromList $ tail segments)
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
readerArenaGetSegment arena id =
    if id == 0
      then return (readerArenaSegment0 arena)
      else do
          len <- (VMD.length (readerArenaMoreSegments arena))
          if fromIntegral len >= id - 1
                 then VMD.unsafeRead (readerArenaMoreSegments arena) (fromIntegral (id - 1))
                 else fail $ "Invalid segment id: " <> show id

builderArenaGetSegment :: BuilderArena -> SegmentId -> IO SegmentBuilder
builderArenaGetSegment arena id =
    if id == 0
      then return (builderArenaSegment0 arena)
      else do
          len <- (VMD.length (builderArenaMoreSegments arena))
          if fromIntegral len >= id - 1
                 then VMD.unsafeRead (builderArenaMoreSegments arena) (fromIntegral (id - 1))
                 else fail $ "Invalid segment id: " <> show id

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

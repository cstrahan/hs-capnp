{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}

module Data.CapnProto.Arena where

import qualified Data.ByteString           as BS
import           Data.ByteString.Internal  (toForeignPtr)
import           Data.Word
import           Data.IORef
import           Data.Monoid
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr
import           System.IO.Unsafe          (unsafePerformIO, unsafeDupablePerformIO)
import           Control.Monad

import           Data.CapnProto.Units

--------------------------------------------------------------------------------

-- for use as phantom types
data Reader
data Builder

type SegmentId = Word32

data Segment a = Segment
  { segmentArena      :: Arena a
  , segmentForeignPtr :: ForeignPtr Word8
  , unsafeSegmentPtr  :: Ptr Word
  , segmentSize       :: WordCount32
  , _segmentId        :: SegmentId
  , _segmentPos       :: IORef (Ptr Word)
  }

segmentId :: Segment Builder -> SegmentId
segmentId = _segmentId

segmentPos :: Segment Builder -> IORef (Ptr Word)
segmentPos = _segmentPos

instance Nullable (Segment a) where
    isNull segment = unsafeSegmentPtr segment == nullPtr

nullArena :: Arena Reader
nullArena =
    Arena (unsafePerformIO $ newIORef [nullSegment])
          FixedSize
          (unsafePerformIO $ newIORef 0)

{-# NOINLINE nullSegment #-}
nullSegment :: Segment Reader
nullSegment =
    Segment nullArena
            (unsafePerformIO . newForeignPtr_ $ nullPtr)
            nullPtr
            0
            0
            nullPos
  where
    nullPos = unsafePerformIO $ newIORef nullPtr

withSegment :: Segment a -> (Ptr Word -> IO b) -> IO b
withSegment reader f = withForeignPtr (segmentForeignPtr reader) $ \_ ->
    f (unsafeSegmentPtr reader)

data Arena a = Arena
  { arenaSegments :: IORef [Segment a]
  , _arenaAllocationStrategy :: AllocationStrategy
  , _arenaNextSize :: IORef Word32
  }

arenaAllocationStrategy :: Arena Builder -> AllocationStrategy
arenaAllocationStrategy = _arenaAllocationStrategy

arenaNextSize :: Arena Builder -> IORef Word32
arenaNextSize = _arenaNextSize

data AllocationStrategy
  = FixedSize
  | GrowHeuristically

allocateSegmentBuilder :: Arena Builder -> Int -> IO (Segment Builder)
allocateSegmentBuilder arena numWords = do
    fptr <- mallocForeignPtrBytes (fromIntegral $ numWords * bytesPerWord)
    let ptr = castPtr $ unsafeForeignPtrToPtr fptr
        segmentId = 0 -- XXX
    pos <- newIORef ptr
    return $ Segment
        arena
        fptr
        ptr
        (fromIntegral numWords)
        segmentId
        pos

---------------------------

appendSegment :: Arena Builder -> Segment Builder -> IO ()
appendSegment arena segment = do
    segments <- readIORef . arenaSegments $ arena
    writeIORef (arenaSegments arena) (segments ++ [ segment ])

numSegments :: Arena a -> IO Int
numSegments arena = length <$> readIORef (arenaSegments arena)

getFirstSegment :: Arena a -> IO (Segment a)
getFirstSegment arena = head <$> readIORef (arenaSegments arena)

getLastSegment :: Arena a -> IO (Segment a)
getLastSegment arena = last <$> readIORef (arenaSegments arena)

allocateOwnedMemory :: Arena Builder -> WordCount32 -> IO (ForeignPtr Word8, WordCount32)
allocateOwnedMemory arena minSize = do
    nextSize <- readIORef (arenaNextSize arena)
    let size = max minSize nextSize
    let sizeInBytes = fromIntegral size * bytesPerWord
    fptr <- mallocForeignPtrBytes $ sizeInBytes
    case arenaAllocationStrategy arena of
        GrowHeuristically ->
          void $ writeIORef (arenaNextSize arena) (nextSize + size)
        _ ->
          return ()
    return (fptr, size)

arenaAllocate :: Arena Builder -> WordCount32 -> IO (Segment Builder, Ptr Word)
arenaAllocate arena amount = do
    segment <- getLastSegment arena
    segmentAllocate segment amount >>= \case
        Just result -> return (segment, result)
        Nothing -> do
            id <- fromIntegral <$> numSegments arena
            (fptr, size) <- allocateOwnedMemory arena amount
            let ptr = castPtr $ unsafeForeignPtrToPtr fptr
            pos <- newIORef ptr
            let segment = Segment arena fptr ptr size id pos
            appendSegment arena segment
            return (segment, ptr)

---------------------------

segmentAllocate :: Segment Builder -> WordCount32 -> IO (Maybe (Ptr Word))
segmentAllocate segment amount = do
    currSize <- segmentCurrentSize segment
    if amount > segmentSize segment - currSize
      then return Nothing
      else do
          pos <- readIORef $ segmentPos segment
          writeIORef (segmentPos segment) (pos `plusPtr` (fromIntegral amount * bytesPerWord))
          return . Just $ pos

getPtrUnchecked :: Segment Builder -> WordCount32 -> Ptr Word
getPtrUnchecked segment offset = unsafeSegmentPtr segment `plusPtr` (fromIntegral offset * bytesPerWord)

segmentCurrentSize :: Segment Builder -> IO WordCount32
segmentCurrentSize segment = do
    pos <- readIORef . segmentPos $ segment
    return $ fromIntegral $ (pos `minusPtr` unsafeSegmentPtr segment) `div` bytesPerWord

-- XXX
arenaFromByteStrings :: [BS.ByteString] -> Arena Reader
arenaFromByteStrings strs = arena
  where
    arena = Arena (unsafePerformIO $ newIORef segments) FixedSize (unsafePerformIO $ newIORef 0)
    segments = map segmentFromByteString strs
    segmentFromByteString str =
        let (fptr, offset, len) = toForeignPtr str
            ptr = castPtr $ plusPtr (unsafeForeignPtrToPtr fptr) offset
            segmentReader =
                Segment
                    arena
                    (castForeignPtr fptr)
                    ptr
                    (fromIntegral len `div` fromIntegral bytesPerWord)
                    undefined
                    undefined
        in segmentReader

tryGetSegment :: Arena a -> SegmentId -> IO (Segment a)
tryGetSegment arena id = do
    segments <- readIORef $ arenaSegments arena
    case segments `at` fromIntegral id of
        Left err -> fail $ "Invalid segment id: "<>err
        Right res -> return res

--------------------------------------------------------------------------------

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

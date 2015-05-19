module Data.CapnProto.Arena where

import qualified Data.ByteString           as BS
import           Data.ByteString.Internal  (toForeignPtr)
import           Data.Word
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr
import           System.IO.Unsafe          (unsafePerformIO)

import           Data.CapnProto.Units

--------------------------------------------------------------------------------

type SegmentId = Word32

-- should probably be unpacked into the containing structures
data SegmentReader = SegmentReader
  { segmentReaderArena      :: Arena
  , segmentReaderForeignPtr :: ForeignPtr Word8
  , unsafeSegmentReaderPtr  :: Ptr Word
  , segmentReaderSize       :: WordCount32
  }

instance Nullable SegmentReader where
    isNull segment = unsafeSegmentReaderPtr segment == nullPtr

nullArena :: Arena
nullArena = ReaderArena nullSegment []

nullSegment :: SegmentReader
nullSegment = SegmentReader nullArena (unsafePerformIO . newForeignPtr_ $ nullPtr) nullPtr 0

withSegment :: SegmentReader -> (Ptr Word -> IO b) -> IO b
withSegment reader f = withForeignPtr (segmentReaderForeignPtr reader) $ \_ ->
    f (unsafeSegmentReaderPtr reader)

data Arena = ReaderArena
  { arenaSegment0 :: SegmentReader
  , arenaSegments :: [SegmentReader]
  }

-- XXX
arenaFromByteStrings :: [BS.ByteString] -> Arena
arenaFromByteStrings strs =
    let arena = ReaderArena (head segments) (tail segments)
        segments = map (segmentFromByteString arena) strs
    in arena

segmentFromByteString :: Arena -> BS.ByteString -> SegmentReader
segmentFromByteString arena str =
    let (fptr, offset, len) = toForeignPtr str
        ptr = castPtr $ plusPtr (unsafeForeignPtrToPtr fptr) offset
        segmentReader =
            SegmentReader
                arena
                (castForeignPtr fptr)
                ptr
                (fromIntegral len `div` fromIntegral bytesPerWord)
    in segmentReader

tryGetSegment :: Arena -> SegmentId -> SegmentReader
tryGetSegment (ReaderArena segment0 segments) id =
    if id == 0
      then segment0
      else segments !! (fromIntegral (id - 1))

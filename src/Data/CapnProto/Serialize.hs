module Data.CapnProto.Serialize where

import           Control.Monad
import           Data.ByteString             (useAsCStringLen)
import qualified Data.ByteString             as BS
import           Data.ByteString.Internal
import           Data.Word
import           Foreign.C.String            (CStringLen)
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO                   (Handle, IOMode (..), openFile)

import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Mutable.Dynamic as VMD

import           Data.CapnProto.Arena
import           Data.CapnProto.Layout       as L
import           Data.CapnProto.Units

readHandle :: Handle -> IO MessageReader
readHandle handle =
    readMessage get
  where
    get = BS.hGet handle

readFile :: String -> IO MessageReader
readFile path =
    openFile path ReadMode >>= readHandle

-- TODO: take fn that writes to Ptr, then allocated segments of the appropriate
-- alignment.
readMessage :: (Int -> IO ByteString) -> IO MessageReader
readMessage read = do
    lengths <- readSegmentTable readWord32
    strs <- mapM (\n -> read $ n * bytesPerWord) lengths
    let arena = arenaFromByteStrings strs
    return $ MessageReader arena
  where
    readWord32 = do
        str <- read 4
        useAsCStringLen str $ \(ptr, len) ->
            peek (castPtr ptr :: Ptr Word32)

-- | Read a segment table, giving the lengths of each segment in words.
readSegmentTable :: Monad m => m Word32 -> m [Int]
readSegmentTable read = do
    numSegs <- fmap (+1) read
    segments <- readSegments (fromIntegral numSegs :: Int) []
    -- read the remaining half-word of padding
    when (numSegs `rem` 2 == 0) (void read)
    return segments
  where
    readSegments 0 segments = return (reverse segments)
    readSegments n segments = do
        len <- fmap fromIntegral read
        readSegments (n-1) (len:segments)

writeMessage :: MessageBuilder -> (CStringLen -> IO ()) -> IO ()
writeMessage message write = do
    segments <- messageBuilderGetSegmentsForOutput message
    alloca $ \ptr -> do
        let writeWord32 val = poke ptr val >> write (castPtr ptr, sizeOf val)
        writeSegmentTable segments writeWord32
    writeSegments segments write

writeSegments :: V.Vector (ForeignPtr Word8, WordCount32) -> (CStringLen -> IO ()) -> IO ()
writeSegments segments write =
    V.forM_ segments $ \(fptr, len) ->
        write (castPtr $ unsafeForeignPtrToPtr fptr, fromIntegral len)

writeSegmentTable :: V.Vector (ForeignPtr Word8, WordCount32) -> (Word32 -> IO ()) -> IO ()
writeSegmentTable segments write = do
    let segmentCount = V.length segments
    write (fromIntegral segmentCount)
    flip V.imapM_ segments $ \idx (_, len) ->
        write len
    when (segmentCount `rem` 2 == 0) (write 0)

getRoot :: FromStructReader a => MessageReader -> IO a
getRoot (MessageReader arena) = do
    let segment = readerArenaSegment0 arena
    withSegmentReader segment $ \ptr -> do
        pointerReader <- L.getRoot segment (unsafeSegmentReaderPtr segment)
        struct <- getReaderStruct pointerReader nullPtr
        fromStructReader struct

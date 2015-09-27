module Data.CapnProto.Serialize where

import           Control.Monad
import           Data.ByteString          (useAsCStringLen)
import qualified Data.ByteString          as BS
import           Data.ByteString.Internal
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO                (Handle, IOMode (..), openFile)

import           Data.CapnProto.Arena
import           Data.CapnProto.Layout    as L
import           Data.CapnProto.Units

data MessageReader = MessageReader
  { messageReaderArena :: ReaderArena
  }

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
    lengths <- readSegmentTable readByte
    strs <- mapM (\n -> read $ n * bytesPerWord) lengths
    let arena = arenaFromByteStrings strs
    return $ MessageReader arena
  where
    readByte = do
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

getRoot :: FromStructReader a => MessageReader -> IO a
getRoot (MessageReader arena) = do
    segment <- segmentReaderGetFirstSegment arena
    withSegmentReader segment $ \ptr -> do
        pointerReader <- L.getRoot segment (unsafeSegmentReaderPtr segment)
        struct <- getReaderStruct pointerReader nullPtr
        fromStructReader struct

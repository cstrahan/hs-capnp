module Data.CapnProto.Units where

import Data.Word
import Data.Int
import Foreign.Ptr

--------------------------------------------------------------------------------

type BitCount = Word
type BitCount8 = Word8
type BitCount16 = Word16
type BitCount32 = Word32
type BitCount64 = Word64

type ByteCount = Word
type ByteCount8 = Word8
type ByteCount16 = Word16
type ByteCount32 = Word32
type ByteCount64 = Word64

type WordCount = Word
type WordCount8 = Word8
type WordCount16 = Word16
type WordCount32 = Word32
type WordCount64 = Word64

type ElementCount = Word
type ElementCount8 = Word8
type ElementCount16 = Word16
type ElementCount32 = Word32
type ElementCount64 = Word64

type WirePointerCount = Word
type WirePointerCount8 = Word8
type WirePointerCount16 = Word16
type WirePointerCount32 = Word32
type WirePointerCount64 = Word64

--------------------------------------------------------------------------------

bytesPerWord :: Int
bytesPerWord = 8

pointerSizeInWords :: Int
pointerSizeInWords = 1

bitsPerByte :: Int
bitsPerByte = 8

bitsPerWord :: Int
bitsPerWord = 64

bitsPerPointer :: Int
bitsPerPointer = 64

--------------------------------------------------------------------------------
-- TODO: move elsewhere

class Nullable a where
    isNull :: a -> Bool

instance Nullable (Ptr a) where
    isNull ptr = ptr == nullPtr


class Zero a where
    zero :: a


instance Zero Bool where
    zero = False


instance Zero Word8 where
    zero = 0

instance Zero Word16 where
    zero = 0

instance Zero Word32 where
    zero = 0

instance Zero Word64 where
    zero = 0


instance Zero Int8 where
    zero = 0

instance Zero Int16 where
    zero = 0

instance Zero Int32 where
    zero = 0

instance Zero Int64 where
    zero = 0


instance Zero Float where
    zero = 0

instance Zero Double where
    zero = 0

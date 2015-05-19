{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.CapnProto.Mask where

import           Data.Array.ST     (MArray, STUArray, newArray, readArray)
import           Data.Array.Unsafe (castSTUArray)
import           Data.Bits
import           Data.Int
import           Data.Word
import           Control.Monad.ST  (ST, runST)

--------------------------------------------------------------------------------

class Mask n where
    type MaskTy n :: *
    mask :: n -> MaskTy n -> n

--------------------------------------------------------------------------------

instance Mask () where
    type MaskTy () = ()
    mask _ _ = ()

--------------------------------------------------------------------------------

instance Mask Bool where
    type MaskTy Bool = Bool
    mask x y = (x || y) && (not (x && y))

--------------------------------------------------------------------------------

instance Mask Int where
    type MaskTy Int = Int
    mask = xor

instance Mask Int8 where
    type MaskTy Int8 = Int8
    mask = xor

instance Mask Int16 where
    type MaskTy Int16 = Int16
    mask = xor

instance Mask Int32 where
    type MaskTy Int32 = Int32
    mask = xor

instance Mask Int64 where
    type MaskTy Int64 = Int64
    mask = xor

--------------------------------------------------------------------------------

instance Mask Word where
    type MaskTy Word = Word
    mask = xor

instance Mask Word8 where
    type MaskTy Word8 = Word8
    mask = xor

instance Mask Word16 where
    type MaskTy Word16 = Word16
    mask = xor

instance Mask Word32 where
    type MaskTy Word32 = Word32
    mask = xor

instance Mask Word64 where
    type MaskTy Word64 = Word64
    mask = xor

--------------------------------------------------------------------------------

instance Mask Float where
    type MaskTy Float = Word32
    mask n m = wordToFloat (floatToWord n `xor` m)

instance Mask Double where
    type MaskTy Double = Word64
    mask n m = wordToDouble (doubleToWord n `xor` m)

--------------------------------------------------------------------------------

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

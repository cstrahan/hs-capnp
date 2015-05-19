{-# LANGUAGE OverloadedStrings #-}


module LayoutSpec where

import qualified Data.ByteString          as BS
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO
import           Test.Hspec
import           Text.Printf              (printf)

import qualified Data.CapnProto.Layout    as L
import qualified Data.CapnProto.Serialize as S

--------------------------------------------------------------------------------

readStruct :: String -> IO L.StructReader
readStruct path = do
    message <- S.readFile path
    S.getRoot message

readSegmentTable :: String -> IO [Int]
readSegmentTable path = do
    handle <- openFile path ReadMode
    let get = BS.hGet handle
    S.readSegmentTable (readByte get)
  where
    readByte get = do
        str <- get 4
        BS.useAsCStringLen str $ \(ptr, _) ->
            peek (castPtr ptr :: Ptr Word32)

--------------------------------------------------------------------------------

-- TODO
-- * followFars

hex8 :: Word8 -> String
hex8 = printf "%02x"

hex16 :: Word16 -> String
hex16 = printf "%04x"

hex32 :: Word32 -> String
hex32 = printf "%08x"

hex64 :: Word64 -> String
hex64 = printf "%016x"

spec :: Spec
spec = do
    describe "segment table" $ do
        it "parses correctly" $ do
            table <- readSegmentTable "test-data/double_far_one_uint32"
            table `shouldBe` [1, 5, 5]

    describe "struct" $ do
        describe "with one bool field" $ do
            it "set to false" $ do
                struct <- readStruct "test-data/one_bool_false"
                value <- L.getField struct 0
                value `shouldBe` False

                defaulted <- L.getField struct 1
                defaulted `shouldBe` False

            it "set to true" $ do
                struct <- readStruct "test-data/one_bool_true"
                value <- L.getField struct 0
                value `shouldBe` True

                defaulted <- L.getField struct 1
                defaulted `shouldBe` False

        it "with 64 bool fields" $ do
            struct <- readStruct "test-data/many_bool"
            let expected = replicate 64 True
            bytes <- traverse (\n -> L.getField struct n) [0..63]
            bytes `shouldBe` expected

            defaulted <- L.getField struct 64
            defaulted `shouldBe` False

        it "with 8 bytes" $ do
            struct <- readStruct "test-data/many_uint8"
            let expected = [1..8] :: [Word8]
            bytes <- traverse (L.getField struct) [0..7]
            bytes `shouldBe` expected

            defaulted <- L.getField struct 8
            defaulted `shouldBe` (0 :: Word8)

        it "with 3 structs" $ do
            struct <- readStruct "test-data/three_structs"

            struct0 <- L.getField struct 0
            num0 <- L.getField struct0 0

            struct1 <- L.getField struct 1
            num1 <- L.getField struct1 0

            struct2 <- L.getField struct 2
            num2 <- L.getField struct2 0

            let expected = [0x11223344, 0x55667788, 0x99AABBCC] :: [Word32]
            [num0, num1, num2] `shouldBe` expected

            structDefault <- L.getField struct 3
            numDefault <- L.getField structDefault 0
            numDefault `shouldBe` (0 :: Word32)

        it "far pointer" $ do
            struct <- readStruct "test-data/far_one_uint32"
            num0 <- L.getField struct 0
            num0 `shouldBe` (0x11223344 :: Word32)

        it "double far pointer" $ do
            struct <- readStruct "test-data/double_far_one_uint32"
            num0 <- L.getField struct 0
            num0 `shouldBe` (0x11223344 :: Word32)

        it "text" $ do
            struct <- readStruct "test-data/one_text"
            text <- L.getField struct 0 :: IO L.TextReader

            L.textReaderData text `shouldBe` ("This is some text." :: BS.ByteString)

        it "data" $ do
            struct <- readStruct "test-data/one_data"
            text <- L.getField struct 0 :: IO L.DataReader

            L.dataReaderData text `shouldBe` ("This is some data." :: BS.ByteString)

        describe "list of" $ do
            it "bool" $ do
                struct <- readStruct "test-data/list_of_bool"
                list <- L.getField struct 0

                let expected = [True, True, False, True]
                bools <- traverse (L.getElement list) [0..3]

                bools `shouldBe` expected

            it "uint32" $ do
                struct <- readStruct "test-data/list_of_uint32"
                list <- L.getField struct 0

                let expected = [0x11223344, 0x55667788, 0x99AABBCC] :: [Word32]
                nums <- traverse (L.getElement list) [0..2]

                nums `shouldBe` expected

            it "uint64" $ do
                struct <- readStruct "test-data/list_of_uint64"
                list <- L.getField struct 0
                nums <- traverse (L.getElement list) [0..2]

                let expected = [0x1020304050607080, 0x1121314151617181, 0x1222324252627282] :: [Word64]
                nums `shouldBe` expected

            it "struct of uint32" $ do
                struct <- readStruct "test-data/list_of_one_uint32"

                list <- L.getField struct 0
                structs <- traverse (L.getElement list) [0..2] :: IO [L.StructReader]
                nums <- traverse (flip L.getField 0) structs

                let expected = [0x11223344, 0x55667788, 0x99AABBCC] :: [Word32]
                nums `shouldBe` expected

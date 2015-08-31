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
-- * mapElements et al
--   * when empty

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

        it "with 3 lists" $ do
            struct <- readStruct "test-data/three_lists"

            list0 <- L.getField struct 0
            list1 <- L.getField struct 1
            list2 <- L.getField struct 2

            elem0 <- L.getReaderElement list0 0 :: IO Word32
            elem1 <- L.getReaderElement list1 0 :: IO Word32
            elem2 <- L.getReaderElement list1 1 :: IO Word32
            elem3 <- L.getReaderElement list2 0 :: IO Word32
            elem4 <- L.getReaderElement list2 1 :: IO Word32
            elem5 <- L.getReaderElement list2 2 :: IO Word32

            [elem0] `shouldBe` [0]
            [elem1, elem2] `shouldBe` [1, 2]
            [elem3, elem4, elem5] `shouldBe` [3, 4, 5]

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

            L.textReaderData text `shouldBe` "This is some text."

        it "data" $ do
            struct <- readStruct "test-data/one_data"
            text <- L.getField struct 0 :: IO L.DataReader

            L.dataReaderData text `shouldBe` "This is some data."

        it "mixed" $ do
            struct <- readStruct "test-data/mixed"
            num0 <- L.getField struct 0 :: IO Word32

            struct0 <- L.getField struct 0 :: IO L.StructReader
            num1 <- L.getField struct0 0 :: IO Word32

            list0 <- L.getField struct 1
            num3 <- L.getReaderElement list0 0
            num4 <- L.getReaderElement list0 1
            num5 <- L.getReaderElement list0 2

            [num0, num1, num3, num4, num5] `shouldBe` [12345, 67890, 20304, 30405, 50607]

        it "unininitialized list of uint32" $ do
            struct <- readStruct "test-data/uninitialized_list_of_uint32"
            list <- L.getField struct 0 :: IO (L.ListReader Word32)
            L.listLength list `shouldBe` 0

        describe "list of" $ do
            it "bool" $ do
                struct <- readStruct "test-data/list_of_bool"
                list <- L.getField struct 0

                let expected = [True, True, False, True]
                bools <- traverse (L.getReaderElement list) [0..3]

                bools `shouldBe` expected

            it "uint32" $ do
                struct <- readStruct "test-data/list_of_uint32"
                list <- L.getField struct 0

                let expected = [0x11223344, 0x55667788, 0x99AABBCC] :: [Word32]
                nums <- traverse (L.getReaderElement list) [0..2]

                nums `shouldBe` expected

            it "uint64" $ do
                struct <- readStruct "test-data/list_of_uint64"
                list <- L.getField struct 0
                nums <- traverse (L.getReaderElement list) [0..2]

                let expected = [0x1020304050607080, 0x1121314151617181, 0x1222324252627282] :: [Word64]
                nums `shouldBe` expected

            it "struct of uint32" $ do
                struct <- readStruct "test-data/list_of_one_uint32"

                list <- L.getField struct 0
                structs <- traverse (L.getReaderElement list) [0..2] :: IO [L.StructReader]
                nums <- traverse (flip L.getField 0) structs

                let expected = [0x11223344, 0x55667788, 0x99AABBCC] :: [Word32]
                nums `shouldBe` expected

            it "list of uint32" $ do
                struct <- readStruct "test-data/list_of_list_of_uint32"

                lists <- L.getField struct 0
                list0 <- L.getReaderElement lists 0
                list1 <- L.getReaderElement lists 1
                list2 <- L.getReaderElement lists 2

                elem0 <- L.getReaderElement list0 0 :: IO Word32
                elem1 <- L.getReaderElement list1 0 :: IO Word32
                elem2 <- L.getReaderElement list1 1 :: IO Word32
                elem3 <- L.getReaderElement list2 0 :: IO Word32
                elem4 <- L.getReaderElement list2 1 :: IO Word32
                elem5 <- L.getReaderElement list2 2 :: IO Word32

                [elem0] `shouldBe` [0]
                [elem1, elem2] `shouldBe` [1, 2]
                [elem3, elem4, elem5] `shouldBe` [3, 4, 5]

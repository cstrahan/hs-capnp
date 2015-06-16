module Main where

import           System.IO
import           Text.Show.Pretty                (ppShow)

import           Data.CapnProto.Schema           (readSchema)
import           Data.CapnProto.Schema.Generator (generateCode)

main :: IO ()
main = do
    schema <- readSchema stdin
    {- putStrLn . ppShow $ schema -}
    putStrLn $ generateCode schema
    return ()

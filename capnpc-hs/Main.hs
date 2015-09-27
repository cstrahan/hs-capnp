module Main where

import           System.IO
import           System.Process
import           Text.Show.Pretty                (ppShow)

import           Data.CapnProto.Schema           (readSchema)
import           Data.CapnProto.Schema.Generator (generateCode)

main :: IO ()
main = do
    schema <- readSchema stdin
    {- putStrLn . ppShow $ schema -}
    putStrLn $ generateCode schema
    return ()

compileSchema :: String -> IO Handle
compileSchema schemaPath = do
    (_, Just hout, _, _) <- createProcess (proc "capnp" ["compile", "-o", "-", schemaPath]){ std_out = CreatePipe }
    return hout

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative   hiding (Const)
import           Control.Monad         (unless)
import           Data.Aeson            hiding (Value)
import qualified Data.Aeson            as J
import           Data.Aeson.Types      (Parser, parseEither, parseMaybe)
import qualified Data.ByteString       as BS
import           Data.HashMap.Strict   (member)
import           Data.Int
import           Data.Monoid
import qualified Data.Text             as T
import           Data.Traversable
import           Data.Word
import           Text.Show.Pretty      (ppShow)

import           Data.CapnProto.Parser
import           Data.CapnProto.Schema

main :: IO ()
main = putStrLn . ppShow $ schema

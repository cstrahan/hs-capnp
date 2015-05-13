module Data.CapnProto.Parser
  ( parser
  ) where

import Data.Aeson hiding (object)
import Data.Text (pack)
import qualified Data.Vector as V
import Text.Parsec
import Data.HashMap.Strict (fromList)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.ByteString.Lazy.Char8 as BLC

-- This parses Cap'n Proto's textual encoding.

-- NOTE:
-- Cap'n Proto uses 64-bt ids,
-- and json (or, at least, JavaScript) only supports integers <= 2^53,
-- as they must fit within the representation of a 64-bit float.

--------------------------------------------------------------------------------

type Parser a = Parsec [Char] () a

parser :: Parser Value
parser = whiteSpace *> value

object = parens attrs
array = brackets . commaSep $ value

attrs = commaSep attr

attr = do
    ident <- identifier
    equals
    val <- value
    return (ident, val)

value =
    Object . toObject <$> object <|>
    Number <$> number <|>
    Array . V.fromList <$> array <|>
    String . pack <$> stringLit <|>
    Bool <$> bool <|>
    return Null <* opaque <|>
    String . pack <$> identifier
  where
    toObject = fromList . map (\(id, val) -> (pack id, val))

number = lexeme (read <$> many1 (oneOf ['0'..'9']))

bool = symbol "true" *> return True
   <|> symbol "false" *> return False

--------------------------------------------------------------------------------

lexeme p = p <* whiteSpace
whiteSpace = skipMany (satisfy isSpace)
commaSep p = sepBy p comma

quotes = between  (symbol "\"")  (symbol "\"")
parens = between  (symbol "(")  (symbol ")")
brackets = between  (symbol "[")  (symbol "]")

comma = symbol ","
equals = symbol "="
opaque = symbol "<opaque pointer>"

symbol = lexeme . string
identifier = lexeme . many1 . satisfy $ isAlphaNum
stringLit = quotes . many . satisfy $ (/= '"')

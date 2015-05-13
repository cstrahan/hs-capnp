import Text.Parsec
import Data.Aeson
import Text.Show.Pretty (ppShow)

import Data.CapnProto.Parser
import Data.CapnProto.Schema

--------------------------------------------------------------------------------

parseSchema :: IO Value
parseSchema = do
    request <- readFile "schema.txt"
    let result = runParser parser () "schema.txt" request
    let val = either (error . show) (id) result
    return val

--------------------------------------------------------------------------------

type Id = Int

data Node = Node
  { nodeId :: Id
  , nodeDisplayName :: String
  , nodeScopeId :: Id
  , nodeIsGeneric :: Bool
  }

data Type =
    Void
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float32
  | Float64
  | Text
  | Data

  | List
    { typeElementType :: Type
    }

  | Enum
    { typeTypeId :: Id
    , typeBrand :: Brand
    }
  | Struct
    { typeTypeId :: Id
    , typeBrand :: Brand
    }
  | Interface
    { typeTypeId :: Id
    , typeBrand :: Brand
    }

  | AnyPointer AnyPointer

data AnyPointer =
    Unconstrained
  | Parameter
    { apScopeId :: Id
    , apParameterIndex :: Int
    }
  | ImplicitMethodParameter
    { apParameterIndex :: Int
    }

data Brand = Brand

--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn . show $ schema

{-# LANGUAGE OverloadedStrings #-}

module Data.CapnProto.Schema
  ( schema

  , Id
  , Text
  , Data

  , ElementSize(..)
  , AnyPointer(..)
  , Value(..)
  , Node(..)
  , NodeKind(..)
  , Field(..)
  , Annotation(..)
  , FieldKind(..)
  , FieldOrdinal(..)
  , Enumerant(..)
  , Type(..)
  , AnyPointerKind(..)
  , Brand(..)
  ) where

import           Control.Applicative   hiding (Const)
import           Control.Monad         (unless)
import           Data.Aeson            hiding (Value)
import qualified Data.Aeson            as J
import           Data.Aeson.Types      (Parser, parseEither, parseMaybe)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import           Data.HashMap.Strict   (member)
import           Data.Int
import           Data.Monoid
import qualified Data.Text             as T
import           Data.Traversable
import           Data.Word

import           Data.CapnProto.Parser

--------------------------------------------------------------------------------
-- The parsed Schema schema

schema :: [Node]
schema = decodeNodes parsedJson

-- This is the json produced after parsing capnp's textual encoding of schema.capnp
-- (see Data.CapnProto.Parser)
asJsonString :: BL.ByteString
asJsonString = "{\"requestedFiles\":[{\"imports\":[{\"name\":\"/capnp/c++.capnp\",\"id\":13688829037717245569}],\"id\":12195682960037147353,\"filename\":\"schema.capnp\"}],\"nodes\":[{\"scopeId\":14981803260258615394,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":1,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"id\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"name\",\"ordinal\":{\"explicit\":1}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:CodeGeneratorRequest.RequestedFile.Import\",\"id\":12560611460656617445,\"nestedNodes\":[],\"displayNamePrefixLength\":48},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":0,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":2,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":16610026722781537303}}}}},\"name\":\"nodes\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":14981803260258615394}}}}},\"name\":\"requestedFiles\",\"ordinal\":{\"explicit\":1}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:CodeGeneratorRequest\",\"id\":13818529054586492878,\"nestedNodes\":[{\"name\":\"RequestedFile\",\"id\":14981803260258615394}],\"displayNamePrefixLength\":13},{\"scopeId\":13818529054586492878,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":1,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":2,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"id\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"filename\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":12560611460656617445}}}}},\"name\":\"imports\",\"ordinal\":{\"explicit\":2}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:CodeGeneratorRequest.RequestedFile\",\"id\":14981803260258615394,\"nestedNodes\":[{\"name\":\"Import\",\"id\":12560611460656617445}],\"displayNamePrefixLength\":34},{\"scopeId\":15020482145304562784,\"struct\":{\"discriminantOffset\":4,\"dataWordCount\":3,\"discriminantCount\":3,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":0,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"unconstrained\",\"ordinal\":{\"explicit\":18}},{\"group\":{\"typeId\":11372142272178113157},\"codeOrder\":1,\"discriminantValue\":1,\"name\":\"parameter\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":13470206089842057844},\"codeOrder\":2,\"discriminantValue\":2,\"name\":\"implicitMethodParameter\",\"ordinal\":{\"implicit\":\"void\"}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Type.anyPointer\",\"id\":14003731834718800369,\"displayNamePrefixLength\":18},{\"scopeId\":15020482145304562784,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"typeId\",\"ordinal\":{\"explicit\":16}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":10391024731148337707}}},\"name\":\"brand\",\"ordinal\":{\"explicit\":22}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Type.struct\",\"id\":12410354185295152851,\"displayNamePrefixLength\":18},{\"scopeId\":15020482145304562784,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"typeId\",\"ordinal\":{\"explicit\":15}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":10391024731148337707}}},\"name\":\"brand\",\"ordinal\":{\"explicit\":21}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Type.enum\",\"id\":11389172934837766057,\"displayNamePrefixLength\":18},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":5,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"name\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"codeOrder\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":3,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"paramStructType\",\"ordinal\":{\"explicit\":2}},{\"codeOrder\":5,\"discriminantValue\":65535,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"resultStructType\",\"ordinal\":{\"explicit\":3}},{\"codeOrder\":7,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":17422339044421236034}}}}},\"name\":\"annotations\",\"ordinal\":{\"explicit\":4}},{\"codeOrder\":4,\"discriminantValue\":65535,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":10391024731148337707}}},\"name\":\"paramBrand\",\"ordinal\":{\"explicit\":5}},{\"codeOrder\":6,\"discriminantValue\":65535,\"slot\":{\"offset\":3,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":10391024731148337707}}},\"name\":\"resultBrand\",\"ordinal\":{\"explicit\":6}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":4,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":13353766412138554289}}}}},\"name\":\"implicitParameters\",\"ordinal\":{\"explicit\":7}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Method\",\"id\":10736806783679155584,\"nestedNodes\":[],\"displayNamePrefixLength\":13},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":1,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":2,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"name\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"codeOrder\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":17422339044421236034}}}}},\"name\":\"annotations\",\"ordinal\":{\"explicit\":2}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Enumerant\",\"id\":10919677598968879693,\"nestedNodes\":[],\"displayNamePrefixLength\":13},{\"scopeId\":11145653318641710175,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":4,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"typeId\",\"ordinal\":{\"explicit\":7}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Field.group\",\"id\":14626792032033250577,\"displayNamePrefixLength\":19},{\"scopeId\":16610026722781537303,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":5,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":6,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":3,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":10736806783679155584}}}}},\"name\":\"methods\",\"ordinal\":{\"explicit\":15}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":4,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":12220001500510083064}}}}},\"name\":\"superclasses\",\"ordinal\":{\"explicit\":31}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Node.interface\",\"id\":16728431493453586831,\"displayNamePrefixLength\":18},{\"scopeId\":16610026722781537303,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":5,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":6,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":3,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":15020482145304562784}}},\"name\":\"type\",\"ordinal\":{\"explicit\":16}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":4,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":14853958794117909659}}},\"name\":\"value\",\"ordinal\":{\"explicit\":17}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Node.const\",\"id\":12793219851699983392,\"displayNamePrefixLength\":18},{\"scopeId\":16610026722781537303,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":5,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":6,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":7,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"dataWordCount\",\"ordinal\":{\"explicit\":7}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":12,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"pointerCount\",\"ordinal\":{\"explicit\":8}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":13,\"hadExplicitDefault\":false,\"defaultValue\":{\"enum\":0},\"type\":{\"enum\":{\"typeId\":15102134695616452902}}},\"name\":\"preferredListEncoding\",\"ordinal\":{\"explicit\":9}},{\"codeOrder\":3,\"discriminantValue\":65535,\"slot\":{\"offset\":224,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"isGroup\",\"ordinal\":{\"explicit\":10}},{\"codeOrder\":4,\"discriminantValue\":65535,\"slot\":{\"offset\":15,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"discriminantCount\",\"ordinal\":{\"explicit\":11}},{\"codeOrder\":5,\"discriminantValue\":65535,\"slot\":{\"offset\":8,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint32\":0},\"type\":{\"uint32\":\"void\"}},\"name\":\"discriminantOffset\",\"ordinal\":{\"explicit\":12}},{\"codeOrder\":6,\"discriminantValue\":65535,\"slot\":{\"offset\":3,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":11145653318641710175}}}}},\"name\":\"fields\",\"ordinal\":{\"explicit\":13}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Node.struct\",\"id\":11430331134483579957,\"displayNamePrefixLength\":18},{\"scopeId\":16610026722781537303,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":5,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":6,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":3,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":15020482145304562784}}},\"name\":\"type\",\"ordinal\":{\"explicit\":18}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":112,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsFile\",\"ordinal\":{\"explicit\":19}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":113,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsConst\",\"ordinal\":{\"explicit\":20}},{\"codeOrder\":3,\"discriminantValue\":65535,\"slot\":{\"offset\":114,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsEnum\",\"ordinal\":{\"explicit\":21}},{\"codeOrder\":4,\"discriminantValue\":65535,\"slot\":{\"offset\":115,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsEnumerant\",\"ordinal\":{\"explicit\":22}},{\"codeOrder\":5,\"discriminantValue\":65535,\"slot\":{\"offset\":116,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsStruct\",\"ordinal\":{\"explicit\":23}},{\"codeOrder\":6,\"discriminantValue\":65535,\"slot\":{\"offset\":117,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsField\",\"ordinal\":{\"explicit\":24}},{\"codeOrder\":7,\"discriminantValue\":65535,\"slot\":{\"offset\":118,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsUnion\",\"ordinal\":{\"explicit\":25}},{\"codeOrder\":8,\"discriminantValue\":65535,\"slot\":{\"offset\":119,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsGroup\",\"ordinal\":{\"explicit\":26}},{\"codeOrder\":9,\"discriminantValue\":65535,\"slot\":{\"offset\":120,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsInterface\",\"ordinal\":{\"explicit\":27}},{\"codeOrder\":10,\"discriminantValue\":65535,\"slot\":{\"offset\":121,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsMethod\",\"ordinal\":{\"explicit\":28}},{\"codeOrder\":11,\"discriminantValue\":65535,\"slot\":{\"offset\":122,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsParam\",\"ordinal\":{\"explicit\":29}},{\"codeOrder\":12,\"discriminantValue\":65535,\"slot\":{\"offset\":123,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"targetsAnnotation\",\"ordinal\":{\"explicit\":30}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Node.annotation\",\"id\":17011813041836786320,\"displayNamePrefixLength\":18},{\"scopeId\":11145653318641710175,\"struct\":{\"discriminantOffset\":5,\"dataWordCount\":3,\"discriminantCount\":2,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":4,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":0,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"implicit\",\"ordinal\":{\"explicit\":8}},{\"codeOrder\":1,\"discriminantValue\":1,\"slot\":{\"offset\":6,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"explicit\",\"ordinal\":{\"explicit\":9}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Field.ordinal\",\"id\":13515537513213004774,\"displayNamePrefixLength\":19},{\"scopeId\":11145653318641710175,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":4,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint32\":0},\"type\":{\"uint32\":\"void\"}},\"name\":\"offset\",\"ordinal\":{\"explicit\":4}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":15020482145304562784}}},\"name\":\"type\",\"ordinal\":{\"explicit\":5}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":3,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":14853958794117909659}}},\"name\":\"defaultValue\",\"ordinal\":{\"explicit\":6}},{\"codeOrder\":3,\"discriminantValue\":65535,\"slot\":{\"offset\":128,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"hadExplicitDefault\",\"ordinal\":{\"explicit\":10}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Field.slot\",\"id\":14133145859926553711,\"displayNamePrefixLength\":19},{\"scopeId\":16610026722781537303,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":5,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":6,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":3,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":10919677598968879693}}}}},\"name\":\"enumerants\",\"ordinal\":{\"explicit\":14}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Node.enum\",\"id\":13063450714778629528,\"displayNamePrefixLength\":18},{\"scopeId\":12195682960037147353,\"isGeneric\":false,\"displayName\":\"schema.capnp:ElementSize\",\"id\":15102134695616452902,\"enum\":{\"enumerants\":[{\"codeOrder\":0,\"name\":\"empty\"},{\"codeOrder\":1,\"name\":\"bit\"},{\"codeOrder\":2,\"name\":\"byte\"},{\"codeOrder\":3,\"name\":\"twoBytes\"},{\"codeOrder\":4,\"name\":\"fourBytes\"},{\"codeOrder\":5,\"name\":\"eightBytes\"},{\"codeOrder\":6,\"name\":\"pointer\"},{\"codeOrder\":7,\"name\":\"inlineComposite\"}]},\"nestedNodes\":[],\"displayNamePrefixLength\":13},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":19,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":0,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"void\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":1,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"bool\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":2,\"discriminantValue\":2,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"int8\",\"ordinal\":{\"explicit\":2}},{\"codeOrder\":3,\"discriminantValue\":3,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"int16\",\"ordinal\":{\"explicit\":3}},{\"codeOrder\":4,\"discriminantValue\":4,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"int32\",\"ordinal\":{\"explicit\":4}},{\"codeOrder\":5,\"discriminantValue\":5,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"int64\",\"ordinal\":{\"explicit\":5}},{\"codeOrder\":6,\"discriminantValue\":6,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"uint8\",\"ordinal\":{\"explicit\":6}},{\"codeOrder\":7,\"discriminantValue\":7,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"uint16\",\"ordinal\":{\"explicit\":7}},{\"codeOrder\":8,\"discriminantValue\":8,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"uint32\",\"ordinal\":{\"explicit\":8}},{\"codeOrder\":9,\"discriminantValue\":9,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"uint64\",\"ordinal\":{\"explicit\":9}},{\"codeOrder\":10,\"discriminantValue\":10,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"float32\",\"ordinal\":{\"explicit\":10}},{\"codeOrder\":11,\"discriminantValue\":11,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"float64\",\"ordinal\":{\"explicit\":11}},{\"codeOrder\":12,\"discriminantValue\":12,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"text\",\"ordinal\":{\"explicit\":12}},{\"codeOrder\":13,\"discriminantValue\":13,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"data\",\"ordinal\":{\"explicit\":13}},{\"group\":{\"typeId\":9792858745991129751},\"codeOrder\":14,\"discriminantValue\":14,\"name\":\"list\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":11389172934837766057},\"codeOrder\":15,\"discriminantValue\":15,\"name\":\"enum\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":12410354185295152851},\"codeOrder\":16,\"discriminantValue\":16,\"name\":\"struct\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":17116997365232503999},\"codeOrder\":17,\"discriminantValue\":17,\"name\":\"interface\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":14003731834718800369},\"codeOrder\":18,\"discriminantValue\":18,\"name\":\"anyPointer\",\"ordinal\":{\"implicit\":\"void\"}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Type\",\"id\":15020482145304562784,\"nestedNodes\":[],\"displayNamePrefixLength\":13},{\"annotations\":[{\"value\":{\"text\":\"capnp::annotations\"},\"brand\":{},\"id\":13386661402618388268}],\"scopeId\":0,\"isGeneric\":false,\"displayName\":\"capnp/c++.capnp\",\"id\":13688829037717245569,\"file\":\"void\",\"nestedNodes\":[{\"name\":\"namespace\",\"id\":13386661402618388268},{\"name\":\"name\",\"id\":17466269397259751886}],\"displayNamePrefixLength\":10},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":4,\"dataWordCount\":3,\"discriminantCount\":2,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":4,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"name\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"codeOrder\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":17422339044421236034}}}}},\"name\":\"annotations\",\"ordinal\":{\"explicit\":2}},{\"codeOrder\":3,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":true,\"defaultValue\":{\"uint16\":65535},\"type\":{\"uint16\":\"void\"}},\"name\":\"discriminantValue\",\"ordinal\":{\"explicit\":3}},{\"group\":{\"typeId\":14133145859926553711},\"codeOrder\":4,\"discriminantValue\":0,\"name\":\"slot\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":14626792032033250577},\"codeOrder\":5,\"discriminantValue\":1,\"name\":\"group\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":13515537513213004774},\"codeOrder\":6,\"discriminantValue\":65535,\"name\":\"ordinal\",\"ordinal\":{\"implicit\":\"void\"}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Field\",\"id\":11145653318641710175,\"nestedNodes\":[{\"name\":\"noDiscriminant\",\"id\":10930602151629473554}],\"displayNamePrefixLength\":13},{\"annotation\":{\"targetsInterface\":false,\"targetsConst\":false,\"targetsMethod\":false,\"targetsParam\":false,\"targetsEnum\":false,\"targetsFile\":true,\"targetsEnumerant\":false,\"targetsGroup\":false,\"targetsAnnotation\":false,\"targetsStruct\":false,\"targetsField\":false,\"type\":{\"text\":\"void\"},\"targetsUnion\":false},\"scopeId\":13688829037717245569,\"isGeneric\":false,\"displayName\":\"capnp/c++.capnp:namespace\",\"id\":13386661402618388268,\"nestedNodes\":[],\"displayNamePrefixLength\":16},{\"scopeId\":15020482145304562784,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"typeId\",\"ordinal\":{\"explicit\":17}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":10391024731148337707}}},\"name\":\"brand\",\"ordinal\":{\"explicit\":23}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Type.interface\",\"id\":17116997365232503999,\"displayNamePrefixLength\":18},{\"scopeId\":10391024731148337707,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":1,\"discriminantCount\":2,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":0,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"unbound\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":1,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":15020482145304562784}}},\"name\":\"type\",\"ordinal\":{\"explicit\":1}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Brand.Binding\",\"id\":14439610327179913212,\"nestedNodes\":[],\"displayNamePrefixLength\":19},{\"scopeId\":14003731834718800369,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"scopeId\",\"ordinal\":{\"explicit\":19}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":5,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"parameterIndex\",\"ordinal\":{\"explicit\":20}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Type.anyPointer.parameter\",\"id\":11372142272178113157,\"displayNamePrefixLength\":29},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":1,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"id\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":10391024731148337707}}},\"name\":\"brand\",\"ordinal\":{\"explicit\":1}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Superclass\",\"id\":12220001500510083064,\"nestedNodes\":[],\"displayNamePrefixLength\":13},{\"annotations\":[{\"value\":{\"text\":\"capnp::schema\"},\"brand\":{},\"id\":13386661402618388268}],\"scopeId\":0,\"isGeneric\":false,\"displayName\":\"schema.capnp\",\"id\":12195682960037147353,\"file\":\"void\",\"nestedNodes\":[{\"name\":\"Node\",\"id\":16610026722781537303},{\"name\":\"Field\",\"id\":11145653318641710175},{\"name\":\"Enumerant\",\"id\":10919677598968879693},{\"name\":\"Superclass\",\"id\":12220001500510083064},{\"name\":\"Method\",\"id\":10736806783679155584},{\"name\":\"Type\",\"id\":15020482145304562784},{\"name\":\"Brand\",\"id\":10391024731148337707},{\"name\":\"Value\",\"id\":14853958794117909659},{\"name\":\"Annotation\",\"id\":17422339044421236034},{\"name\":\"ElementSize\",\"id\":15102134695616452902},{\"name\":\"CodeGeneratorRequest\",\"id\":13818529054586492878}],\"displayNamePrefixLength\":7},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":2,\"discriminantCount\":19,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":0,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"void\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":1,\"slot\":{\"offset\":16,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"bool\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":2,\"discriminantValue\":2,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"int8\":0},\"type\":{\"int8\":\"void\"}},\"name\":\"int8\",\"ordinal\":{\"explicit\":2}},{\"codeOrder\":3,\"discriminantValue\":3,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"int16\":0},\"type\":{\"int16\":\"void\"}},\"name\":\"int16\",\"ordinal\":{\"explicit\":3}},{\"codeOrder\":4,\"discriminantValue\":4,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"int32\":0},\"type\":{\"int32\":\"void\"}},\"name\":\"int32\",\"ordinal\":{\"explicit\":4}},{\"codeOrder\":5,\"discriminantValue\":5,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"int64\":0},\"type\":{\"int64\":\"void\"}},\"name\":\"int64\",\"ordinal\":{\"explicit\":5}},{\"codeOrder\":6,\"discriminantValue\":6,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint8\":0},\"type\":{\"uint8\":\"void\"}},\"name\":\"uint8\",\"ordinal\":{\"explicit\":6}},{\"codeOrder\":7,\"discriminantValue\":7,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"uint16\",\"ordinal\":{\"explicit\":7}},{\"codeOrder\":8,\"discriminantValue\":8,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint32\":0},\"type\":{\"uint32\":\"void\"}},\"name\":\"uint32\",\"ordinal\":{\"explicit\":8}},{\"codeOrder\":9,\"discriminantValue\":9,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"uint64\",\"ordinal\":{\"explicit\":9}},{\"codeOrder\":10,\"discriminantValue\":10,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"float32\":0},\"type\":{\"float32\":\"void\"}},\"name\":\"float32\",\"ordinal\":{\"explicit\":10}},{\"codeOrder\":11,\"discriminantValue\":11,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"float64\":0},\"type\":{\"float64\":\"void\"}},\"name\":\"float64\",\"ordinal\":{\"explicit\":11}},{\"codeOrder\":12,\"discriminantValue\":12,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"text\",\"ordinal\":{\"explicit\":12}},{\"codeOrder\":13,\"discriminantValue\":13,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"data\":\"\"},\"type\":{\"data\":\"void\"}},\"name\":\"data\",\"ordinal\":{\"explicit\":13}},{\"codeOrder\":14,\"discriminantValue\":14,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"anyPointer\":null},\"type\":{\"anyPointer\":{\"unconstrained\":\"void\"}}},\"name\":\"list\",\"ordinal\":{\"explicit\":14}},{\"codeOrder\":15,\"discriminantValue\":15,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"enum\",\"ordinal\":{\"explicit\":15}},{\"codeOrder\":16,\"discriminantValue\":16,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"anyPointer\":null},\"type\":{\"anyPointer\":{\"unconstrained\":\"void\"}}},\"name\":\"struct\",\"ordinal\":{\"explicit\":16}},{\"codeOrder\":17,\"discriminantValue\":17,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"interface\",\"ordinal\":{\"explicit\":17}},{\"codeOrder\":18,\"discriminantValue\":18,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"anyPointer\":null},\"type\":{\"anyPointer\":{\"unconstrained\":\"void\"}}},\"name\":\"anyPointer\",\"ordinal\":{\"explicit\":18}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Value\",\"id\":14853958794117909659,\"nestedNodes\":[],\"displayNamePrefixLength\":13},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":0,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":12382423449155627977}}}}},\"name\":\"scopes\",\"ordinal\":{\"explicit\":0}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Brand\",\"id\":10391024731148337707,\"nestedNodes\":[{\"name\":\"Scope\",\"id\":12382423449155627977},{\"name\":\"Binding\",\"id\":14439610327179913212}],\"displayNamePrefixLength\":13},{\"scopeId\":14003731834718800369,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":5,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint16\":0},\"type\":{\"uint16\":\"void\"}},\"name\":\"parameterIndex\",\"ordinal\":{\"explicit\":24}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Type.anyPointer.implicitMethodParameter\",\"id\":13470206089842057844,\"displayNamePrefixLength\":29},{\"scopeId\":16610026722781537303,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":1,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"name\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"id\",\"ordinal\":{\"explicit\":1}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Node.NestedNode\",\"id\":16050641862814319170,\"nestedNodes\":[],\"displayNamePrefixLength\":18},{\"scopeId\":15020482145304562784,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":3,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":true,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":15020482145304562784}}},\"name\":\"elementType\",\"ordinal\":{\"explicit\":14}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Type.list\",\"id\":9792858745991129751,\"displayNamePrefixLength\":18},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":1,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":2,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"id\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":14853958794117909659}}},\"name\":\"value\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"struct\":null},\"type\":{\"struct\":{\"typeId\":10391024731148337707}}},\"name\":\"brand\",\"ordinal\":{\"explicit\":2}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Annotation\",\"id\":17422339044421236034,\"nestedNodes\":[],\"displayNamePrefixLength\":13},{\"scopeId\":16610026722781537303,\"struct\":{\"discriminantOffset\":0,\"dataWordCount\":0,\"discriminantCount\":0,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"name\",\"ordinal\":{\"explicit\":0}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Node.Parameter\",\"id\":13353766412138554289,\"nestedNodes\":[],\"displayNamePrefixLength\":18},{\"scopeId\":11145653318641710175,\"isGeneric\":false,\"displayName\":\"schema.capnp:Field.noDiscriminant\",\"id\":10930602151629473554,\"const\":{\"value\":{\"uint16\":65535},\"type\":{\"uint16\":\"void\"}},\"nestedNodes\":[],\"displayNamePrefixLength\":19},{\"scopeId\":12195682960037147353,\"struct\":{\"discriminantOffset\":6,\"dataWordCount\":5,\"discriminantCount\":6,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":6,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"id\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"text\":\"\"},\"type\":{\"text\":\"void\"}},\"name\":\"displayName\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":2,\"discriminantValue\":65535,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint32\":0},\"type\":{\"uint32\":\"void\"}},\"name\":\"displayNamePrefixLength\",\"ordinal\":{\"explicit\":2}},{\"codeOrder\":3,\"discriminantValue\":65535,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"scopeId\",\"ordinal\":{\"explicit\":3}},{\"codeOrder\":6,\"discriminantValue\":65535,\"slot\":{\"offset\":1,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":16050641862814319170}}}}},\"name\":\"nestedNodes\",\"ordinal\":{\"explicit\":4}},{\"codeOrder\":7,\"discriminantValue\":65535,\"slot\":{\"offset\":2,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":17422339044421236034}}}}},\"name\":\"annotations\",\"ordinal\":{\"explicit\":5}},{\"codeOrder\":8,\"discriminantValue\":0,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"file\",\"ordinal\":{\"explicit\":6}},{\"group\":{\"typeId\":11430331134483579957},\"codeOrder\":9,\"discriminantValue\":1,\"name\":\"struct\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":13063450714778629528},\"codeOrder\":10,\"discriminantValue\":2,\"name\":\"enum\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":16728431493453586831},\"codeOrder\":11,\"discriminantValue\":3,\"name\":\"interface\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":12793219851699983392},\"codeOrder\":12,\"discriminantValue\":4,\"name\":\"const\",\"ordinal\":{\"implicit\":\"void\"}},{\"group\":{\"typeId\":17011813041836786320},\"codeOrder\":13,\"discriminantValue\":5,\"name\":\"annotation\",\"ordinal\":{\"implicit\":\"void\"}},{\"codeOrder\":4,\"discriminantValue\":65535,\"slot\":{\"offset\":5,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":13353766412138554289}}}}},\"name\":\"parameters\",\"ordinal\":{\"explicit\":32}},{\"codeOrder\":5,\"discriminantValue\":65535,\"slot\":{\"offset\":288,\"hadExplicitDefault\":false,\"defaultValue\":{\"bool\":false},\"type\":{\"bool\":\"void\"}},\"name\":\"isGeneric\",\"ordinal\":{\"explicit\":33}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Node\",\"id\":16610026722781537303,\"nestedNodes\":[{\"name\":\"Parameter\",\"id\":13353766412138554289},{\"name\":\"NestedNode\",\"id\":16050641862814319170}],\"displayNamePrefixLength\":13},{\"scopeId\":10391024731148337707,\"struct\":{\"discriminantOffset\":4,\"dataWordCount\":2,\"discriminantCount\":2,\"preferredListEncoding\":\"inlineComposite\",\"isGroup\":false,\"pointerCount\":1,\"fields\":[{\"codeOrder\":0,\"discriminantValue\":65535,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"uint64\":0},\"type\":{\"uint64\":\"void\"}},\"name\":\"scopeId\",\"ordinal\":{\"explicit\":0}},{\"codeOrder\":1,\"discriminantValue\":0,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"list\":null},\"type\":{\"list\":{\"elementType\":{\"struct\":{\"typeId\":14439610327179913212}}}}},\"name\":\"bind\",\"ordinal\":{\"explicit\":1}},{\"codeOrder\":2,\"discriminantValue\":1,\"slot\":{\"offset\":0,\"hadExplicitDefault\":false,\"defaultValue\":{\"void\":\"void\"},\"type\":{\"void\":\"void\"}},\"name\":\"inherit\",\"ordinal\":{\"explicit\":2}}]},\"isGeneric\":false,\"displayName\":\"schema.capnp:Brand.Scope\",\"id\":12382423449155627977,\"nestedNodes\":[],\"displayNamePrefixLength\":19}]}"
-- " silly vim

parsedJson :: J.Value
parsedJson = either error id . eitherDecode $ asJsonString

--------------------------------------------------------------------------------
-- types

type Id = Word64
type Text = String
type Data = String

data ElementSize =
    SzEmpty
  | SzBit
  | SzByte
  | SzTwoBytes
  | SzFourBytes
  | SzEightBytes
  | SzPointer
  | SzInlineComposite
  deriving (Show)

data AnyPointer = AnyPointer
  deriving (Show)

data Value =
    ValVoid
  | ValBool Bool
  | ValInt8 Int8
  | ValInt16 Int16
  | ValInt32 Int32
  | ValInt64 Int64
  | ValUint8 Word8
  | ValUint16 Word16
  | ValUint32 Word32
  | ValUint64 Word64
  | ValFloat32 Float
  | ValFloat64 Double
  | ValText Text -- UTF-8, NUL-terminated
  | ValData Data -- completely arbitrary sequence of bytes
  | ValList AnyPointer
  | ValEnum Word16
  | ValStruct AnyPointer
  | ValInterface
  | ValAnyPointer AnyPointer
  deriving (Show)

data Node = Node
  { nodeId          :: Id
  , nodeDisplayName :: String
  , nodeScopeId     :: Id
  , nodeIsGeneric   :: Bool
  , nodeKind        :: NodeKind
  }
  deriving (Show)

data NodeKind =
    File
  | Struct
    { dataWordCount         :: Word16
    , pointerCount          :: Word16
    , preferredListEncoding :: ElementSize
    , isGroup               :: Bool
    , discriminantCount     :: Word16
    , discriminantOffset    :: Word32
    , fields                :: [Field]
    }
  | Enum
    { enumerants :: [Enumerant]
    }
  {- | Interface -}
    {- { methods :: List [Method] -}
    {- , superclasses :: List [Superclass] -}
    {- } -}
  | Const
    { nodeType :: Type
    , value    :: Value
    }
  | Annotation
    { nodeType          :: Type
    , targetsFile       :: Bool
    , targetsConst      :: Bool
    , targetsEnum       :: Bool
    , targetsEnumerant  :: Bool
    , targetsStruct     :: Bool
    , targetsField      :: Bool
    , targetsUnion      :: Bool
    , targetsGroup      :: Bool
    , targetsInterface  :: Bool
    , targetsMethod     :: Bool
    , targetsParam      :: Bool
    , targetsAnnotation :: Bool
    }
  deriving (Show)

-- TODO: use Maybe instead
noDiscriminant :: Word16
noDiscriminant = 0xffff

data Field = Field
  { fieldName         :: String
  , fieldCodeOrder    :: Word16
  , fieldAnnotations  :: [Annotation]
  , discriminantValue :: Word16
  , fieldKind         :: FieldKind
  , ordinal           :: FieldOrdinal
  }
  deriving (Show)

data Annotation = Ann
  deriving (Show)

data FieldKind =
    SlotField
    { offset             :: Word32
    , fieldType          :: Type
    , defaultValue       :: Value
    , hadExplicitDefault :: Bool
    }
  | GroupField
    { fieldTypeId :: Id
    }
  deriving (Show)

data FieldOrdinal =
    OrdImplicit
  | OrdExplicit Word16
  deriving (Show)

data Enumerant = Enumerant
  { enumerantName        :: Text
  , enumerantCodeOrder   :: Word16
  , enumerantAnnotations :: [Annotation]
  }
  deriving (Show)

data Type =
    TyVoid
  | TyBool
  | TyInt8
  | TyInt16
  | TyInt32
  | TyInt64
  | TyUint8
  | TyUint16
  | TyUint32
  | TyUint64
  | TyFloat32
  | TyFloat64
  | TyText
  | TyData
  | TyList
    { typeElementType :: Type
    }
  | TyEnum
    { typeId :: Id
    , brand  :: Brand
    }
  | TyStruct
    { typeId :: Id
    , brand  :: Brand
    }
  | TyInterface
    { typeId :: Id
    , brand  :: Brand
    }
  | TyAnyPointer AnyPointer
  deriving (Show)

data AnyPointerKind =
    Unconstrained
  | Parameter
    { apScopeId        :: Id
    , apParameterIndex :: Word16
    }
  | ImplicitMethodParameter
    { apParameterIndex :: Word16
    }
  deriving (Show)

data Brand = Brand
  deriving (Show)

--------------------------------------------------------------------------------
-- after converting the textual dump of schema.capnp to json,
-- translate the json into a list of Nodes.

decodeNodes :: J.Value -> [Node]
decodeNodes (Object obj) =
   either error id . flip parseEither obj $ \obj ->
       traverse parseNode =<< obj .: "nodes"

withKey :: Object -> T.Text -> Parser ()
withKey obj key =
    unless (member key obj) $
      fail . T.unpack $ "Key \"" <> key <> "\" not found. \n"

parseNode :: Object -> Parser Node
parseNode obj =
    Node
        <$> obj .: "id"
        <*> obj .: "displayName"
        <*> obj .: "scopeId"
        <*> obj .: "isGeneric"
        <*> parseNodeKind obj

parseNodeKind :: Object -> Parser NodeKind
parseNodeKind obj =
    parseFile <|> parseStruct <|> parseEnum <|> parseConst <|> parseAnnotation
  where
    parseFile = return File <* withKey obj "file"
    parseStruct = obj .: "struct" >>= \obj ->
        Struct
            <$> obj .: "dataWordCount"
            <*> obj .: "pointerCount"
            <*> (readElementSize <$> obj .: "preferredListEncoding")
            <*> obj .: "isGroup"
            <*> obj .: "discriminantCount"
            <*> obj .: "discriminantOffset"
            <*> (traverse parseField =<< obj .: "fields")
    parseEnum = obj .: "enum" >>= \obj ->
        Enum <$> (traverse parseEnumerant =<< obj .: "enumerants")
    parseConst = obj .: "const" >>= \obj ->
        Const
            <$> (parseType =<< obj .: "type")
            <*> (parseValue =<< obj .: "value")
    parseAnnotation = obj .: "annotation" >>= \obj ->
        Annotation
            <$> (parseType =<< obj .: "type")
            <*> obj .: "targetsFile"
            <*> obj .: "targetsConst"
            <*> obj .: "targetsEnum"
            <*> obj .: "targetsEnumerant"
            <*> obj .: "targetsStruct"
            <*> obj .: "targetsField"
            <*> obj .: "targetsUnion"
            <*> obj .: "targetsGroup"
            <*> obj .: "targetsInterface"
            <*> obj .: "targetsMethod"
            <*> obj .: "targetsParam"
            <*> obj .: "targetsAnnotation"

readElementSize :: String -> ElementSize
readElementSize txt =
    case txt of
        "empty" -> SzEmpty
        "bit" -> SzBit
        "byte" -> SzByte
        "twoBytes" -> SzTwoBytes
        "fourBytes" -> SzFourBytes
        "eightBytes" -> SzEightBytes
        "pointer" -> SzPointer
        "inlineComposite" -> SzInlineComposite

parseType :: Object -> Parser Type
parseType obj =
        TyVoid `named` "void"
    <|> TyBool `named` "bool"
    <|> TyInt8 `named` "int8"
    <|> TyInt16 `named` "int16"
    <|> TyInt32 `named` "int32"
    <|> TyInt64 `named` "int64"
    <|> TyUint8 `named` "uint8"
    <|> TyUint16 `named` "uint16"
    <|> TyUint32 `named` "uint32"
    <|> TyUint64 `named` "uint64"
    <|> TyFloat32 `named` "float32"
    <|> TyFloat64 `named` "float64"
    <|> TyText `named` "text"
    <|> TyData `named` "data"
    <|> (parseList =<< obj .: "list")
    <|> (parseEnum =<< obj .: "enum")
    <|> (parseStruct =<< obj .: "struct")
    <|> (parseInterface =<< obj .: "Interface")
    <|> (return $ TyAnyPointer AnyPointer) <* withKey obj "anyPointer"
  where
    named t k = withKey obj k *> return t
    parseList obj = TyList <$> (parseType =<< obj .: "elementType")
    parseEnum obj = TyEnum <$> obj .: "typeId" <*> return Brand
    parseStruct obj = TyStruct <$> obj .: "typeId" <*> return Brand
    parseInterface obj = TyInterface <$> obj .: "typeId" <*> return Brand

parseField :: Object -> Parser Field
parseField obj = Field
    <$> obj .: "name"
    <*> obj .: "codeOrder"
    <*> ((traverse parseAnnotation =<< obj .: "annotations") <|> return [])
    <*> obj .: "discriminantValue"
    <*> (parseFieldKind obj)
    <*> (parseFieldOrdinal =<< obj .: "ordinal")

parseFieldKind :: Object -> Parser FieldKind
parseFieldKind obj =
        (parseSlot =<< obj .: "slot")
    <|> (parseGroup =<< obj .: "group")
  where
    parseSlot obj = SlotField
        <$> (obj .: "offset")
        <*> (parseType =<< obj .: "type")
        <*> (parseValue =<< obj .: "defaultValue")
        <*> (obj .: "hadExplicitDefault")

    parseGroup obj = GroupField
        <$> obj .: "typeId"

parseValue :: Object -> Parser Value
parseValue obj =
        return ValVoid <* withKey obj "void"
    <|> ValBool `named` "bool"
    <|> ValInt8 `named` "int8"
    <|> ValInt16 `named` "int16"
    <|> ValInt32 `named` "int32"
    <|> ValInt64 `named` "int64"
    <|> ValUint8 `named` "uint8"
    <|> ValUint16 `named` "uint16"
    <|> ValUint32 `named` "uint32"
    <|> ValUint64 `named` "uint64"
    <|> ValFloat32 `named` "float32"
    <|> ValFloat64 `named` "float64"
    <|> ValText `named` "text"
    <|> ValData `named` "data"
    <|> (return $ ValList AnyPointer) <* withKey obj "list"
    <|> ValEnum `named` "enum"
    <|> (return $ ValStruct AnyPointer) <* withKey obj "struct"
    <|> (return $ ValInterface) <* withKey obj "interface"
    <|> (return $ ValAnyPointer AnyPointer) <* withKey obj "anyPointer"
  where
    named t n = t <$> obj .: n

parseFieldOrdinal :: Object -> Parser FieldOrdinal
parseFieldOrdinal obj =
    explicit <|> implicit
  where
    explicit = OrdExplicit <$> obj .: "explicit"
    implicit = return OrdImplicit

parseEnumerant :: Object -> Parser Enumerant
parseEnumerant obj =
    Enumerant <$> obj .: "name"
              <*> obj .: "codeOrder"
              <*> ((traverse parseAnnotation =<< obj .: "annotations") <|> return [])

parseAnnotation :: Object -> Parser Annotation
parseAnnotation obj = return Ann

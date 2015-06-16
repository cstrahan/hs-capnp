{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecursiveDo      #-}
{-# LANGUAGE TypeFamilies     #-}

module Data.CapnProto.Schema
  ( readSchema
  , noDiscriminant

  , Id
  , Text
  , Data

  , ElementSize(..)
  , AnyPointer(..)
  , Value(..)

  , Node(..)
  , FileNode(..)
  , StructNode(..)
  , EnumNode(..)
  , ConstNode(..)
  , AnnotationNode(..)

  , Field(..)
  , Ann(..)
  , FieldKind(..)
  , FieldOrdinal(..)
  , Enumerant(..)
  , Type(..)
  , AnyPointerKind(..)
  , Brand(..)
  ) where

import qualified Data.ByteString.Char8           as BSC
import           Data.Int
import qualified Data.Map                        as Map
import           Data.Monoid
import           Data.Word
import           System.IO

import qualified Data.CapnProto.Layout           as L
import qualified Data.CapnProto.Schema.Generated as G
import qualified Data.CapnProto.Serialize        as S

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

data AnyPointer = AnyPointer

data Value =
    ValVoid
  | ValBool Bool
  | ValInt8 Int8
  | ValInt16 Int16
  | ValInt32 Int32
  | ValInt64 Int64
  | ValUInt8 Word8
  | ValUInt16 Word16
  | ValUInt32 Word32
  | ValUInt64 Word64
  | ValFloat32 Float
  | ValFloat64 Double
  | ValText (Maybe Text) -- UTF-8, NUL-terminated
  | ValData (Maybe Data) -- completely arbitrary sequence of bytes
  | ValList AnyPointer
  | ValEnum Word16
  | ValStruct AnyPointer
  | ValInterface
  | ValAnyPointer AnyPointer

data Node
  = File FileNode
  | Struct StructNode
  | Enum EnumNode
  | Const ConstNode
  | Annotation AnnotationNode
  | Interface InterfaceNode

data FileNode = FileNode
  { fileNodeId          :: Id
  , fileNodeDisplayName :: String
  , fileNodeScopeId     :: Id
  , fileNodeIsGeneric   :: Bool
  }

data StructNode = StructNode
  { structNodeId                    :: Id
  , structNodeDisplayName           :: String
  , structNodeScopeId               :: Id
  , structNodeIsGeneric             :: Bool
  , structNodeDataWordCount         :: Word16
  , structNodePointerCount          :: Word16
  , structNodePreferredListEncoding :: ElementSize
  , structNodeIsGroup               :: Bool
  , structNodeDiscriminantCount     :: Word16
  , structNodeDiscriminantOffset    :: Word32
  , structNodeFields                :: [Field]
  }

data EnumNode = EnumNode
  { enumNodeId          :: Id
  , enumNodeDisplayName :: String
  , enumNodeScopeId     :: Id
  , enumNodeIsGeneric   :: Bool
  , enumNodeEnumerants  :: [Enumerant]
  }

data ConstNode = ConstNode
  { constNodeId          :: Id
  , constNodeDisplayName :: String
  , constNodeScopeId     :: Id
  , constNodeIsGeneric   :: Bool
  , constNodeType        :: Type
  , constNodeValue       :: Value
  }

data AnnotationNode = AnnotationNode
  { annotationNodeId                :: Id
  , annotationNodeDisplayName       :: String
  , annotationNodeScopeId           :: Id
  , annotationNodeIsGeneric         :: Bool
  , annotationNodeNodeType          :: Type
  , annotationNodeTargetsFile       :: Bool
  , annotationNodeTargetsConst      :: Bool
  , annotationNodeTargetsEnum       :: Bool
  , annotationNodeTargetsEnumerant  :: Bool
  , annotationNodeTargetsStruct     :: Bool
  , annotationNodeTargetsField      :: Bool
  , annotationNodeTargetsUnion      :: Bool
  , annotationNodeTargetsGroup      :: Bool
  , annotationNodeTargetsInterface  :: Bool
  , annotationNodeTargetsMethod     :: Bool
  , annotationNodeTargetsParam      :: Bool
  , annotationNodeTargetsAnnotation :: Bool
  }

data InterfaceNode = InterfaceNode

noDiscriminant :: Word16
noDiscriminant = 0xffff

data Field = Field
  { fieldName              :: String
  , fieldCodeOrder         :: Word16
  , fieldAnnotations       :: [Ann]
  , fieldDiscriminantValue :: Maybe Word16
  , fieldKind              :: FieldKind
  , fieldOrdinal           :: FieldOrdinal
  }

data Ann = Ann

data FieldKind =
    SlotField
    { fieldKindOffset             :: Word32
    , fieldKindType               :: Type
    , fieldKindDefaultValue       :: Value
    , fieldKindHadExplicitDefault :: Bool
    }
  | GroupField
    { fieldKindTypeNode :: StructNode
    }

data FieldOrdinal =
    OrdImplicit
  | OrdExplicit Word16

data Enumerant = Enumerant
  { enumerantName        :: Text
  , enumerantCodeOrder   :: Word16
  , enumerantAnnotations :: [Ann]
  }

data Type =
    TyVoid
  | TyBool
  | TyInt8
  | TyInt16
  | TyInt32
  | TyInt64
  | TyUInt8
  | TyUInt16
  | TyUInt32
  | TyUInt64
  | TyFloat32
  | TyFloat64
  | TyText
  | TyData
  | TyList
    { typeElementType :: Type
    }
  | TyEnum
    { typeEnumNode :: EnumNode
    , typeBrand    :: Brand
    }
  | TyStruct
    { typeStructNode :: StructNode
    , typeBrand      :: Brand
    }
  | TyInterface
    { typeInterfaceNode :: InterfaceNode
    , typeBrand         :: Brand
    }
  | TyAnyPointer AnyPointer

data AnyPointerKind =
    Unconstrained
  | Parameter
    { anyPointerKindScopeNode      :: Node
    , anyPointerKindParameterIndex :: Word16
    }
  | ImplicitMethodParameter
    { anyPointerKindParameterIndex :: Word16
    }

data Brand = Brand

--------------------------------------------------------------------------------

type NodeMap = Map.Map Id Node

readSchema :: Handle -> IO [Node]
readSchema handle = do
    msg <- S.readHandle handle
    request <- S.getRoot msg :: IO G.CodeGeneratorRequest_Reader
    rec let nodeMap = Map.fromList $ map (\n -> (nodeId n, n)) nodes :: NodeMap
        nodes <- L.mapElements (readNode nodeMap) =<< G.getNodes request
    return nodes
  where
    readNode :: NodeMap -> G.Node_Reader -> IO Node
    readNode nm node =
        L.which node >>= \case
            G.Node_NotInSchema _ -> fail "UNKOWN NODE KIND"
            G.Node_file -> fmap File $
                FileNode
                    <$> G.getId node
                    <*> (textToString <$> G.getDisplayName node)
                    <*> G.getScopeId node
                    <*> G.getIsGeneric node
            G.Node_struct struct -> fmap Struct $
                StructNode
                    <$> G.getId node
                    <*> (textToString <$> G.getDisplayName node)
                    <*> G.getScopeId node
                    <*> G.getIsGeneric node
                    <*> G.getDataWordCount struct
                    <*> G.getPointerCount struct
                    <*> (toElementSize =<< G.getPreferredListEncoding struct)
                    <*> G.getIsGroup struct
                    <*> G.getDiscriminantCount struct
                    <*> G.getDiscriminantOffset struct
                    <*> (readFields nm =<< G.getFields struct)
            G.Node_enum enum -> fmap Enum $
                EnumNode
                    <$> G.getId node
                    <*> (textToString <$> G.getDisplayName node)
                    <*> G.getScopeId node
                    <*> G.getIsGeneric node
                    <*> (L.mapElements readEnumerant =<< G.getEnumerants enum)
            G.Node_const const -> fmap Const $
                ConstNode
                    <$> G.getId node
                    <*> (textToString <$> G.getDisplayName node)
                    <*> G.getScopeId node
                    <*> G.getIsGeneric node
                    <*> (readType nm =<< G.getType const)
                    <*> (readValue =<< G.getValue const)
            G.Node_annotation ann -> fmap Annotation $
                AnnotationNode
                    <$> G.getId node
                    <*> (textToString <$> G.getDisplayName node)
                    <*> G.getScopeId node
                    <*> G.getIsGeneric node
                    <*> (readType nm =<< G.getType ann)
                    <*> G.getTargetsFile ann
                    <*> G.getTargetsConst ann
                    <*> G.getTargetsEnum ann
                    <*> G.getTargetsEnumerant ann
                    <*> G.getTargetsStruct ann
                    <*> G.getTargetsField ann
                    <*> G.getTargetsUnion ann
                    <*> G.getTargetsGroup ann
                    <*> G.getTargetsInterface ann
                    <*> G.getTargetsMethod ann
                    <*> G.getTargetsParam ann
                    <*> G.getTargetsAnnotation ann

    readEnumerant enumerant =
        Enumerant
            <$> (textToString <$> G.getName enumerant)
            <*> G.getCodeOrder enumerant
            <*> (L.mapElements readAnnotation =<< G.getAnnotations enumerant)

    readFields nm fields = L.eachElement fields $ \field ->
        Field
            <$> (textToString <$> G.getName field)
            <*> G.getCodeOrder field
            <*> (L.mapElements readAnnotation =<< G.getAnnotations field)
            <*> (readDiscriminant <$> G.getDiscriminantValue field)
            <*> readFieldKind nm field
            <*> (readOrdinal =<< G.getOrdinal field)

    readDiscriminant discriminant =
        if discriminant == noDiscriminant
          then Nothing
          else Just discriminant

    readOrdinal ordinal =
        L.which ordinal >>= \case
            G.Field_ordinal_NotInSchema _ -> fail "UNKOWN ORDINAL"
            G.Field_ordinal_implicit ->
                return OrdImplicit
            G.Field_ordinal_explicit val ->
                return $ OrdExplicit val

    readFieldKind nm field = L.which field >>= \case
        G.Field_NotInSchema _ -> fail "UNKOWN FIELD KIND"
        G.Field_slot slot ->
            SlotField
                <$> G.getOffset slot
                <*> (readType nm =<< G.getType slot)
                <*> (readValue =<< G.getDefaultValue slot)
                <*> G.getHadExplicitDefault slot
        G.Field_group group ->
            GroupField
                <$> (lookupStructNode nm <$> G.getTypeId group)

    readType nm ty =
        L.which ty >>= \case
            G.Type_NotInSchema _ -> fail "UNKOWN TYPE"
            G.Type_void -> return TyVoid
            G.Type_bool -> return TyBool
            G.Type_int8 -> return TyInt8
            G.Type_int16 -> return TyInt16
            G.Type_int32 -> return TyInt32
            G.Type_int64 -> return TyInt64
            G.Type_uint8 -> return TyUInt8
            G.Type_uint16 -> return TyUInt16
            G.Type_uint32 -> return TyUInt32
            G.Type_uint64 -> return TyUInt64
            G.Type_float32 -> return TyFloat32
            G.Type_float64 -> return TyFloat64
            G.Type_text -> return TyText
            G.Type_data -> return TyData
            G.Type_list list -> TyList <$> (readType nm =<< G.getElementType list)
            G.Type_enum enum -> TyEnum <$> (lookupEnumNode nm <$> G.getTypeId enum) <*> return Brand
            G.Type_struct struct -> TyStruct <$> (lookupStructNode nm <$> G.getTypeId struct) <*> return Brand
            G.Type_interface interface -> TyInterface <$> (lookupInterfaceNode nm <$> G.getTypeId interface) <*> return Brand
            G.Type_anyPointer anyPointer -> TyAnyPointer <$> return AnyPointer

    readValue v =
        L.which v >>= \case
            G.Value_NotInSchema _ -> fail "UNKOWN VALUE"
            G.Value_void -> return ValVoid
            G.Value_bool val -> return $ ValBool val
            G.Value_int8 val -> return $ ValInt8 val
            G.Value_int16 val -> return $ ValInt16 val
            G.Value_int32 val -> return $ ValInt32 val
            G.Value_int64 val -> return $ ValInt64 val
            G.Value_uint8 val -> return $ ValUInt8 val
            G.Value_uint16 val -> return $ ValUInt16 val
            G.Value_uint32 val -> return $ ValUInt32 val
            G.Value_uint64 val -> return $ ValUInt64 val
            G.Value_float32 val -> return $ ValFloat32 val
            G.Value_float64 val -> return $ ValFloat64 val
            G.Value_text val -> return $ ValText (textToMaybeString val)
            G.Value_data val -> return $ ValData Nothing -- XXX
            G.Value_list val -> return $ ValList AnyPointer
            G.Value_enum val -> return $ ValEnum val
            G.Value_struct val -> return $ ValStruct AnyPointer
            G.Value_interface -> return $ ValInterface
            G.Value_anyPointer val -> return $ ValAnyPointer AnyPointer

    lookupNode :: NodeMap -> Id -> Node
    lookupNode nm id = Map.findWithDefault (error $ "Failed to find node with id "<>show id) id nm

    lookupStructNode nm id = let (Struct struct) = lookupNode nm id in struct
    lookupEnumNode nm id = let (Enum enum) = lookupNode nm id in enum
    lookupInterfaceNode nm id = let (Interface interface) = lookupNode nm id in interface

    lookupScope nm id =
        if id == 0
          then Nothing
          else Just $ lookupNode nm id

    readAnnotation a = return Ann

    toElementSize sz =
        case sz of
            G.ElementSize_NotInSchema _ -> fail "UNKOWN ELEMENT SIZE"
            G.ElementSize_empty -> return SzEmpty
            G.ElementSize_bit -> return SzBit
            G.ElementSize_byte -> return SzByte
            G.ElementSize_twoBytes -> return SzTwoBytes
            G.ElementSize_fourBytes -> return SzFourBytes
            G.ElementSize_eightBytes -> return SzEightBytes
            G.ElementSize_pointer -> return SzPointer
            G.ElementSize_inlineComposite -> return SzInlineComposite

    textToString (L.TextReader (Just name)) = BSC.unpack name
    textToString _ = ""

    textToMaybeString (L.TextReader (Just name)) = Just $ BSC.unpack name
    textToMaybeString _ = Nothing

    nodeId node =
        case node of
            File file -> fileNodeId file
            Struct struct -> structNodeId struct
            Enum enum -> enumNodeId enum
            Const const -> constNodeId const
            Annotation ann -> annotationNodeId ann

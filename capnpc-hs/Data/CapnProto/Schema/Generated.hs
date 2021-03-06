{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Data.CapnProto.Schema.Generated where

import           Data.Int
import           Data.Word
import           GHC.Exts (Ptr(..))
import           GHC.Prim (Addr#)

import qualified Data.CapnProto.Layout as L

data Annotation_Reader = Annotation_Reader L.StructReader

instance L.FromStructReader Annotation_Reader where
    fromStructReader = return . Annotation_Reader

instance L.ListElement Annotation_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Annotation_Reader $ L.getUntypedElement reader index

instance HasId Annotation_Reader where
    type IdTy Annotation_Reader = Word64
    getId (Annotation_Reader reader) = L.getField reader 0

instance HasValue Annotation_Reader where
    type ValueTy Annotation_Reader = Value_Reader
    getValue (Annotation_Reader reader) = fmap Value_Reader $ L.getField reader 0

instance HasBrand Annotation_Reader where
    type BrandTy Annotation_Reader = Brand_Reader
    getBrand (Annotation_Reader reader) = fmap Brand_Reader $ L.getField reader 1


data Brand_Reader = Brand_Reader L.StructReader

instance L.FromStructReader Brand_Reader where
    fromStructReader = return . Brand_Reader

instance L.ListElement Brand_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Brand_Reader $ L.getUntypedElement reader index

instance HasScopes Brand_Reader where
    type ScopesTy Brand_Reader = (L.ListReader Brand_Scope_Reader)
    getScopes (Brand_Reader reader) = L.getField reader 0


data Brand_Binding_Reader = Brand_Binding_Reader L.StructReader

instance L.FromStructReader Brand_Binding_Reader where
    fromStructReader = return . Brand_Binding_Reader

instance L.ListElement Brand_Binding_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Brand_Binding_Reader $ L.getUntypedElement reader index

data Brand_Binding_Which_Reader
  = Brand_Binding_NotInSchema Word16
  | Brand_Binding_unbound
  | Brand_Binding_type Type_Reader

instance L.Union Brand_Binding_Reader where
    type UnionTy Brand_Binding_Reader = Brand_Binding_Which_Reader
    which (Brand_Binding_Reader reader) = do
        d <- L.getField reader 0 :: IO Word16
        case d of
            0 -> return Brand_Binding_unbound
            1 -> fmap Brand_Binding_type $ fmap Type_Reader $ L.getField reader 0
            _ -> return $ Brand_Binding_NotInSchema d


data Brand_Scope_Reader = Brand_Scope_Reader L.StructReader

instance L.FromStructReader Brand_Scope_Reader where
    fromStructReader = return . Brand_Scope_Reader

instance L.ListElement Brand_Scope_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Brand_Scope_Reader $ L.getUntypedElement reader index

instance HasScopeId Brand_Scope_Reader where
    type ScopeIdTy Brand_Scope_Reader = Word64
    getScopeId (Brand_Scope_Reader reader) = L.getField reader 0

data Brand_Scope_Which_Reader
  = Brand_Scope_NotInSchema Word16
  | Brand_Scope_bind (L.ListReader Brand_Binding_Reader)
  | Brand_Scope_inherit

instance L.Union Brand_Scope_Reader where
    type UnionTy Brand_Scope_Reader = Brand_Scope_Which_Reader
    which (Brand_Scope_Reader reader) = do
        d <- L.getField reader 4 :: IO Word16
        case d of
            0 -> fmap Brand_Scope_bind $ L.getField reader 0
            1 -> return Brand_Scope_inherit
            _ -> return $ Brand_Scope_NotInSchema d


data CodeGeneratorRequest_Reader = CodeGeneratorRequest_Reader L.StructReader

instance L.FromStructReader CodeGeneratorRequest_Reader where
    fromStructReader = return . CodeGeneratorRequest_Reader

instance L.ListElement CodeGeneratorRequest_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap CodeGeneratorRequest_Reader $ L.getUntypedElement reader index

instance HasNodes CodeGeneratorRequest_Reader where
    type NodesTy CodeGeneratorRequest_Reader = (L.ListReader Node_Reader)
    getNodes (CodeGeneratorRequest_Reader reader) = L.getField reader 0

instance HasRequestedFiles CodeGeneratorRequest_Reader where
    type RequestedFilesTy CodeGeneratorRequest_Reader = (L.ListReader CodeGeneratorRequest_RequestedFile_Reader)
    getRequestedFiles (CodeGeneratorRequest_Reader reader) = L.getField reader 1


data CodeGeneratorRequest_RequestedFile_Reader = CodeGeneratorRequest_RequestedFile_Reader L.StructReader

instance L.FromStructReader CodeGeneratorRequest_RequestedFile_Reader where
    fromStructReader = return . CodeGeneratorRequest_RequestedFile_Reader

instance L.ListElement CodeGeneratorRequest_RequestedFile_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap CodeGeneratorRequest_RequestedFile_Reader $ L.getUntypedElement reader index

instance HasId CodeGeneratorRequest_RequestedFile_Reader where
    type IdTy CodeGeneratorRequest_RequestedFile_Reader = Word64
    getId (CodeGeneratorRequest_RequestedFile_Reader reader) = L.getField reader 0

instance HasFilename CodeGeneratorRequest_RequestedFile_Reader where
    type FilenameTy CodeGeneratorRequest_RequestedFile_Reader = L.TextReader
    getFilename (CodeGeneratorRequest_RequestedFile_Reader reader) = L.getField reader 0

instance HasImports CodeGeneratorRequest_RequestedFile_Reader where
    type ImportsTy CodeGeneratorRequest_RequestedFile_Reader = (L.ListReader CodeGeneratorRequest_RequestedFile_Import_Reader)
    getImports (CodeGeneratorRequest_RequestedFile_Reader reader) = L.getField reader 1


data CodeGeneratorRequest_RequestedFile_Import_Reader = CodeGeneratorRequest_RequestedFile_Import_Reader L.StructReader

instance L.FromStructReader CodeGeneratorRequest_RequestedFile_Import_Reader where
    fromStructReader = return . CodeGeneratorRequest_RequestedFile_Import_Reader

instance L.ListElement CodeGeneratorRequest_RequestedFile_Import_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap CodeGeneratorRequest_RequestedFile_Import_Reader $ L.getUntypedElement reader index

instance HasId CodeGeneratorRequest_RequestedFile_Import_Reader where
    type IdTy CodeGeneratorRequest_RequestedFile_Import_Reader = Word64
    getId (CodeGeneratorRequest_RequestedFile_Import_Reader reader) = L.getField reader 0

instance HasName CodeGeneratorRequest_RequestedFile_Import_Reader where
    type NameTy CodeGeneratorRequest_RequestedFile_Import_Reader = L.TextReader
    getName (CodeGeneratorRequest_RequestedFile_Import_Reader reader) = L.getField reader 0


data Enumerant_Reader = Enumerant_Reader L.StructReader

instance L.FromStructReader Enumerant_Reader where
    fromStructReader = return . Enumerant_Reader

instance L.ListElement Enumerant_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Enumerant_Reader $ L.getUntypedElement reader index

instance HasName Enumerant_Reader where
    type NameTy Enumerant_Reader = L.TextReader
    getName (Enumerant_Reader reader) = L.getField reader 0

instance HasCodeOrder Enumerant_Reader where
    type CodeOrderTy Enumerant_Reader = Word16
    getCodeOrder (Enumerant_Reader reader) = L.getField reader 0

instance HasAnnotations Enumerant_Reader where
    type AnnotationsTy Enumerant_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Enumerant_Reader reader) = L.getField reader 1


data Field_Reader = Field_Reader L.StructReader

instance L.FromStructReader Field_Reader where
    fromStructReader = return . Field_Reader

instance L.ListElement Field_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Field_Reader $ L.getUntypedElement reader index

instance HasName Field_Reader where
    type NameTy Field_Reader = L.TextReader
    getName (Field_Reader reader) = L.getField reader 0

instance HasCodeOrder Field_Reader where
    type CodeOrderTy Field_Reader = Word16
    getCodeOrder (Field_Reader reader) = L.getField reader 0

instance HasAnnotations Field_Reader where
    type AnnotationsTy Field_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Field_Reader reader) = L.getField reader 1

instance HasDiscriminantValue Field_Reader where
    type DiscriminantValueTy Field_Reader = Word16
    getDiscriminantValue (Field_Reader reader) = L.getField' reader 1 65535

instance HasOrdinal Field_Reader where
    type OrdinalTy Field_Reader = Field_ordinal_Reader
    getOrdinal (Field_Reader reader) = return . Field_ordinal_Reader $ reader

data Field_Which_Reader
  = Field_NotInSchema Word16
  | Field_slot Field_slot_Reader
  | Field_group Field_group_Reader

instance L.Union Field_Reader where
    type UnionTy Field_Reader = Field_Which_Reader
    which (Field_Reader reader) = do
        d <- L.getField reader 4 :: IO Word16
        case d of
            0 -> fmap Field_slot $ return . Field_slot_Reader $ reader
            1 -> fmap Field_group $ return . Field_group_Reader $ reader
            _ -> return $ Field_NotInSchema d


data Field_group_Reader = Field_group_Reader L.StructReader

instance HasTypeId Field_group_Reader where
    type TypeIdTy Field_group_Reader = Word64
    getTypeId (Field_group_Reader reader) = L.getField reader 2


data Field_ordinal_Reader = Field_ordinal_Reader L.StructReader

data Field_ordinal_Which_Reader
  = Field_ordinal_NotInSchema Word16
  | Field_ordinal_implicit
  | Field_ordinal_explicit Word16

instance L.Union Field_ordinal_Reader where
    type UnionTy Field_ordinal_Reader = Field_ordinal_Which_Reader
    which (Field_ordinal_Reader reader) = do
        d <- L.getField reader 5 :: IO Word16
        case d of
            0 -> return Field_ordinal_implicit
            1 -> fmap Field_ordinal_explicit $ L.getField reader 6
            _ -> return $ Field_ordinal_NotInSchema d


data Field_slot_Reader = Field_slot_Reader L.StructReader

instance HasOffset Field_slot_Reader where
    type OffsetTy Field_slot_Reader = Word32
    getOffset (Field_slot_Reader reader) = L.getField reader 1

instance HasType Field_slot_Reader where
    type TypeTy Field_slot_Reader = Type_Reader
    getType (Field_slot_Reader reader) = fmap Type_Reader $ L.getField reader 2

instance HasDefaultValue Field_slot_Reader where
    type DefaultValueTy Field_slot_Reader = Value_Reader
    getDefaultValue (Field_slot_Reader reader) = fmap Value_Reader $ L.getField reader 3

instance HasHadExplicitDefault Field_slot_Reader where
    type HadExplicitDefaultTy Field_slot_Reader = Bool
    getHadExplicitDefault (Field_slot_Reader reader) = L.getField reader 128


data Method_Reader = Method_Reader L.StructReader

instance L.FromStructReader Method_Reader where
    fromStructReader = return . Method_Reader

instance L.ListElement Method_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Method_Reader $ L.getUntypedElement reader index

instance HasName Method_Reader where
    type NameTy Method_Reader = L.TextReader
    getName (Method_Reader reader) = L.getField reader 0

instance HasCodeOrder Method_Reader where
    type CodeOrderTy Method_Reader = Word16
    getCodeOrder (Method_Reader reader) = L.getField reader 0

instance HasParamStructType Method_Reader where
    type ParamStructTypeTy Method_Reader = Word64
    getParamStructType (Method_Reader reader) = L.getField reader 1

instance HasResultStructType Method_Reader where
    type ResultStructTypeTy Method_Reader = Word64
    getResultStructType (Method_Reader reader) = L.getField reader 2

instance HasAnnotations Method_Reader where
    type AnnotationsTy Method_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Method_Reader reader) = L.getField reader 1

instance HasParamBrand Method_Reader where
    type ParamBrandTy Method_Reader = Brand_Reader
    getParamBrand (Method_Reader reader) = fmap Brand_Reader $ L.getField reader 2

instance HasResultBrand Method_Reader where
    type ResultBrandTy Method_Reader = Brand_Reader
    getResultBrand (Method_Reader reader) = fmap Brand_Reader $ L.getField reader 3

instance HasImplicitParameters Method_Reader where
    type ImplicitParametersTy Method_Reader = (L.ListReader Node_Parameter_Reader)
    getImplicitParameters (Method_Reader reader) = L.getField reader 4


data Node_Reader = Node_Reader L.StructReader

instance L.FromStructReader Node_Reader where
    fromStructReader = return . Node_Reader

instance L.ListElement Node_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Node_Reader $ L.getUntypedElement reader index

instance HasId Node_Reader where
    type IdTy Node_Reader = Word64
    getId (Node_Reader reader) = L.getField reader 0

instance HasDisplayName Node_Reader where
    type DisplayNameTy Node_Reader = L.TextReader
    getDisplayName (Node_Reader reader) = L.getField reader 0

instance HasDisplayNamePrefixLength Node_Reader where
    type DisplayNamePrefixLengthTy Node_Reader = Word32
    getDisplayNamePrefixLength (Node_Reader reader) = L.getField reader 2

instance HasScopeId Node_Reader where
    type ScopeIdTy Node_Reader = Word64
    getScopeId (Node_Reader reader) = L.getField reader 2

instance HasNestedNodes Node_Reader where
    type NestedNodesTy Node_Reader = (L.ListReader Node_NestedNode_Reader)
    getNestedNodes (Node_Reader reader) = L.getField reader 1

instance HasAnnotations Node_Reader where
    type AnnotationsTy Node_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Node_Reader reader) = L.getField reader 2

instance HasParameters Node_Reader where
    type ParametersTy Node_Reader = (L.ListReader Node_Parameter_Reader)
    getParameters (Node_Reader reader) = L.getField reader 5

instance HasIsGeneric Node_Reader where
    type IsGenericTy Node_Reader = Bool
    getIsGeneric (Node_Reader reader) = L.getField reader 288

data Node_Which_Reader
  = Node_NotInSchema Word16
  | Node_file
  | Node_struct Node_struct_Reader
  | Node_enum Node_enum_Reader
  | Node_interface Node_interface_Reader
  | Node_const Node_const_Reader
  | Node_annotation Node_annotation_Reader

instance L.Union Node_Reader where
    type UnionTy Node_Reader = Node_Which_Reader
    which (Node_Reader reader) = do
        d <- L.getField reader 6 :: IO Word16
        case d of
            0 -> return Node_file
            1 -> fmap Node_struct $ return . Node_struct_Reader $ reader
            2 -> fmap Node_enum $ return . Node_enum_Reader $ reader
            3 -> fmap Node_interface $ return . Node_interface_Reader $ reader
            4 -> fmap Node_const $ return . Node_const_Reader $ reader
            5 -> fmap Node_annotation $ return . Node_annotation_Reader $ reader
            _ -> return $ Node_NotInSchema d


data Node_NestedNode_Reader = Node_NestedNode_Reader L.StructReader

instance L.FromStructReader Node_NestedNode_Reader where
    fromStructReader = return . Node_NestedNode_Reader

instance L.ListElement Node_NestedNode_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Node_NestedNode_Reader $ L.getUntypedElement reader index

instance HasName Node_NestedNode_Reader where
    type NameTy Node_NestedNode_Reader = L.TextReader
    getName (Node_NestedNode_Reader reader) = L.getField reader 0

instance HasId Node_NestedNode_Reader where
    type IdTy Node_NestedNode_Reader = Word64
    getId (Node_NestedNode_Reader reader) = L.getField reader 0


data Node_Parameter_Reader = Node_Parameter_Reader L.StructReader

instance L.FromStructReader Node_Parameter_Reader where
    fromStructReader = return . Node_Parameter_Reader

instance L.ListElement Node_Parameter_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Node_Parameter_Reader $ L.getUntypedElement reader index

instance HasName Node_Parameter_Reader where
    type NameTy Node_Parameter_Reader = L.TextReader
    getName (Node_Parameter_Reader reader) = L.getField reader 0


data Node_annotation_Reader = Node_annotation_Reader L.StructReader

instance HasType Node_annotation_Reader where
    type TypeTy Node_annotation_Reader = Type_Reader
    getType (Node_annotation_Reader reader) = fmap Type_Reader $ L.getField reader 3

instance HasTargetsFile Node_annotation_Reader where
    type TargetsFileTy Node_annotation_Reader = Bool
    getTargetsFile (Node_annotation_Reader reader) = L.getField reader 112

instance HasTargetsConst Node_annotation_Reader where
    type TargetsConstTy Node_annotation_Reader = Bool
    getTargetsConst (Node_annotation_Reader reader) = L.getField reader 113

instance HasTargetsEnum Node_annotation_Reader where
    type TargetsEnumTy Node_annotation_Reader = Bool
    getTargetsEnum (Node_annotation_Reader reader) = L.getField reader 114

instance HasTargetsEnumerant Node_annotation_Reader where
    type TargetsEnumerantTy Node_annotation_Reader = Bool
    getTargetsEnumerant (Node_annotation_Reader reader) = L.getField reader 115

instance HasTargetsStruct Node_annotation_Reader where
    type TargetsStructTy Node_annotation_Reader = Bool
    getTargetsStruct (Node_annotation_Reader reader) = L.getField reader 116

instance HasTargetsField Node_annotation_Reader where
    type TargetsFieldTy Node_annotation_Reader = Bool
    getTargetsField (Node_annotation_Reader reader) = L.getField reader 117

instance HasTargetsUnion Node_annotation_Reader where
    type TargetsUnionTy Node_annotation_Reader = Bool
    getTargetsUnion (Node_annotation_Reader reader) = L.getField reader 118

instance HasTargetsGroup Node_annotation_Reader where
    type TargetsGroupTy Node_annotation_Reader = Bool
    getTargetsGroup (Node_annotation_Reader reader) = L.getField reader 119

instance HasTargetsInterface Node_annotation_Reader where
    type TargetsInterfaceTy Node_annotation_Reader = Bool
    getTargetsInterface (Node_annotation_Reader reader) = L.getField reader 120

instance HasTargetsMethod Node_annotation_Reader where
    type TargetsMethodTy Node_annotation_Reader = Bool
    getTargetsMethod (Node_annotation_Reader reader) = L.getField reader 121

instance HasTargetsParam Node_annotation_Reader where
    type TargetsParamTy Node_annotation_Reader = Bool
    getTargetsParam (Node_annotation_Reader reader) = L.getField reader 122

instance HasTargetsAnnotation Node_annotation_Reader where
    type TargetsAnnotationTy Node_annotation_Reader = Bool
    getTargetsAnnotation (Node_annotation_Reader reader) = L.getField reader 123


data Node_const_Reader = Node_const_Reader L.StructReader

instance HasType Node_const_Reader where
    type TypeTy Node_const_Reader = Type_Reader
    getType (Node_const_Reader reader) = fmap Type_Reader $ L.getField reader 3

instance HasValue Node_const_Reader where
    type ValueTy Node_const_Reader = Value_Reader
    getValue (Node_const_Reader reader) = fmap Value_Reader $ L.getField reader 4


data Node_enum_Reader = Node_enum_Reader L.StructReader

instance HasEnumerants Node_enum_Reader where
    type EnumerantsTy Node_enum_Reader = (L.ListReader Enumerant_Reader)
    getEnumerants (Node_enum_Reader reader) = L.getField reader 3


data Node_interface_Reader = Node_interface_Reader L.StructReader

instance HasMethods Node_interface_Reader where
    type MethodsTy Node_interface_Reader = (L.ListReader Method_Reader)
    getMethods (Node_interface_Reader reader) = L.getField reader 3

instance HasSuperclasses Node_interface_Reader where
    type SuperclassesTy Node_interface_Reader = (L.ListReader Superclass_Reader)
    getSuperclasses (Node_interface_Reader reader) = L.getField reader 4


data Node_struct_Reader = Node_struct_Reader L.StructReader

instance HasDataWordCount Node_struct_Reader where
    type DataWordCountTy Node_struct_Reader = Word16
    getDataWordCount (Node_struct_Reader reader) = L.getField reader 7

instance HasPointerCount Node_struct_Reader where
    type PointerCountTy Node_struct_Reader = Word16
    getPointerCount (Node_struct_Reader reader) = L.getField reader 12

instance HasPreferredListEncoding Node_struct_Reader where
    type PreferredListEncodingTy Node_struct_Reader = ElementSize
    getPreferredListEncoding (Node_struct_Reader reader) = fmap (toEnum . fromIntegral) (L.getField reader 13 :: IO Word16)

instance HasIsGroup Node_struct_Reader where
    type IsGroupTy Node_struct_Reader = Bool
    getIsGroup (Node_struct_Reader reader) = L.getField reader 224

instance HasDiscriminantCount Node_struct_Reader where
    type DiscriminantCountTy Node_struct_Reader = Word16
    getDiscriminantCount (Node_struct_Reader reader) = L.getField reader 15

instance HasDiscriminantOffset Node_struct_Reader where
    type DiscriminantOffsetTy Node_struct_Reader = Word32
    getDiscriminantOffset (Node_struct_Reader reader) = L.getField reader 8

instance HasFields Node_struct_Reader where
    type FieldsTy Node_struct_Reader = (L.ListReader Field_Reader)
    getFields (Node_struct_Reader reader) = L.getField reader 3


data Superclass_Reader = Superclass_Reader L.StructReader

instance L.FromStructReader Superclass_Reader where
    fromStructReader = return . Superclass_Reader

instance L.ListElement Superclass_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Superclass_Reader $ L.getUntypedElement reader index

instance HasId Superclass_Reader where
    type IdTy Superclass_Reader = Word64
    getId (Superclass_Reader reader) = L.getField reader 0

instance HasBrand Superclass_Reader where
    type BrandTy Superclass_Reader = Brand_Reader
    getBrand (Superclass_Reader reader) = fmap Brand_Reader $ L.getField reader 0


data Type_Reader = Type_Reader L.StructReader

instance L.FromStructReader Type_Reader where
    fromStructReader = return . Type_Reader

instance L.ListElement Type_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Type_Reader $ L.getUntypedElement reader index

data Type_Which_Reader
  = Type_NotInSchema Word16
  | Type_void
  | Type_bool
  | Type_int8
  | Type_int16
  | Type_int32
  | Type_int64
  | Type_uint8
  | Type_uint16
  | Type_uint32
  | Type_uint64
  | Type_float32
  | Type_float64
  | Type_text
  | Type_data
  | Type_list Type_list_Reader
  | Type_enum Type_enum_Reader
  | Type_struct Type_struct_Reader
  | Type_interface Type_interface_Reader
  | Type_anyPointer Type_anyPointer_Reader

instance L.Union Type_Reader where
    type UnionTy Type_Reader = Type_Which_Reader
    which (Type_Reader reader) = do
        d <- L.getField reader 0 :: IO Word16
        case d of
            0 -> return Type_void
            1 -> return Type_bool
            2 -> return Type_int8
            3 -> return Type_int16
            4 -> return Type_int32
            5 -> return Type_int64
            6 -> return Type_uint8
            7 -> return Type_uint16
            8 -> return Type_uint32
            9 -> return Type_uint64
            10 -> return Type_float32
            11 -> return Type_float64
            12 -> return Type_text
            13 -> return Type_data
            14 -> fmap Type_list $ return . Type_list_Reader $ reader
            15 -> fmap Type_enum $ return . Type_enum_Reader $ reader
            16 -> fmap Type_struct $ return . Type_struct_Reader $ reader
            17 -> fmap Type_interface $ return . Type_interface_Reader $ reader
            18 -> fmap Type_anyPointer $ return . Type_anyPointer_Reader $ reader
            _ -> return $ Type_NotInSchema d


data Type_anyPointer_Reader = Type_anyPointer_Reader L.StructReader

data Type_anyPointer_Which_Reader
  = Type_anyPointer_NotInSchema Word16
  | Type_anyPointer_unconstrained
  | Type_anyPointer_parameter Type_anyPointer_parameter_Reader
  | Type_anyPointer_implicitMethodParameter Type_anyPointer_implicitMethodParameter_Reader

instance L.Union Type_anyPointer_Reader where
    type UnionTy Type_anyPointer_Reader = Type_anyPointer_Which_Reader
    which (Type_anyPointer_Reader reader) = do
        d <- L.getField reader 4 :: IO Word16
        case d of
            0 -> return Type_anyPointer_unconstrained
            1 -> fmap Type_anyPointer_parameter $ return . Type_anyPointer_parameter_Reader $ reader
            2 -> fmap Type_anyPointer_implicitMethodParameter $ return . Type_anyPointer_implicitMethodParameter_Reader $ reader
            _ -> return $ Type_anyPointer_NotInSchema d


data Type_anyPointer_implicitMethodParameter_Reader = Type_anyPointer_implicitMethodParameter_Reader L.StructReader

instance HasParameterIndex Type_anyPointer_implicitMethodParameter_Reader where
    type ParameterIndexTy Type_anyPointer_implicitMethodParameter_Reader = Word16
    getParameterIndex (Type_anyPointer_implicitMethodParameter_Reader reader) = L.getField reader 5


data Type_anyPointer_parameter_Reader = Type_anyPointer_parameter_Reader L.StructReader

instance HasScopeId Type_anyPointer_parameter_Reader where
    type ScopeIdTy Type_anyPointer_parameter_Reader = Word64
    getScopeId (Type_anyPointer_parameter_Reader reader) = L.getField reader 2

instance HasParameterIndex Type_anyPointer_parameter_Reader where
    type ParameterIndexTy Type_anyPointer_parameter_Reader = Word16
    getParameterIndex (Type_anyPointer_parameter_Reader reader) = L.getField reader 5


data Type_enum_Reader = Type_enum_Reader L.StructReader

instance HasTypeId Type_enum_Reader where
    type TypeIdTy Type_enum_Reader = Word64
    getTypeId (Type_enum_Reader reader) = L.getField reader 1

instance HasBrand Type_enum_Reader where
    type BrandTy Type_enum_Reader = Brand_Reader
    getBrand (Type_enum_Reader reader) = fmap Brand_Reader $ L.getField reader 0


data Type_interface_Reader = Type_interface_Reader L.StructReader

instance HasTypeId Type_interface_Reader where
    type TypeIdTy Type_interface_Reader = Word64
    getTypeId (Type_interface_Reader reader) = L.getField reader 1

instance HasBrand Type_interface_Reader where
    type BrandTy Type_interface_Reader = Brand_Reader
    getBrand (Type_interface_Reader reader) = fmap Brand_Reader $ L.getField reader 0


data Type_list_Reader = Type_list_Reader L.StructReader

instance HasElementType Type_list_Reader where
    type ElementTypeTy Type_list_Reader = Type_Reader
    getElementType (Type_list_Reader reader) = fmap Type_Reader $ L.getField reader 0


data Type_struct_Reader = Type_struct_Reader L.StructReader

instance HasTypeId Type_struct_Reader where
    type TypeIdTy Type_struct_Reader = Word64
    getTypeId (Type_struct_Reader reader) = L.getField reader 1

instance HasBrand Type_struct_Reader where
    type BrandTy Type_struct_Reader = Brand_Reader
    getBrand (Type_struct_Reader reader) = fmap Brand_Reader $ L.getField reader 0


data Value_Reader = Value_Reader L.StructReader

instance L.FromStructReader Value_Reader where
    fromStructReader = return . Value_Reader

instance L.ListElement Value_Reader where
    elementSize _ = L.SzInlineComposite
    getUntypedElement reader index =
        fmap Value_Reader $ L.getUntypedElement reader index

data Value_Which_Reader
  = Value_NotInSchema Word16
  | Value_void
  | Value_bool Bool
  | Value_int8 Int8
  | Value_int16 Int16
  | Value_int32 Int32
  | Value_int64 Int64
  | Value_uint8 Word8
  | Value_uint16 Word16
  | Value_uint32 Word32
  | Value_uint64 Word64
  | Value_float32 Float
  | Value_float64 Double
  | Value_text L.TextReader
  | Value_data L.DataReader
  | Value_list ()
  | Value_enum Word16
  | Value_struct ()
  | Value_interface
  | Value_anyPointer ()

instance L.Union Value_Reader where
    type UnionTy Value_Reader = Value_Which_Reader
    which (Value_Reader reader) = do
        d <- L.getField reader 0 :: IO Word16
        case d of
            0 -> return Value_void
            1 -> fmap Value_bool $ L.getField reader 16
            2 -> fmap Value_int8 $ L.getField reader 2
            3 -> fmap Value_int16 $ L.getField reader 1
            4 -> fmap Value_int32 $ L.getField reader 1
            5 -> fmap Value_int64 $ L.getField reader 1
            6 -> fmap Value_uint8 $ L.getField reader 2
            7 -> fmap Value_uint16 $ L.getField reader 1
            8 -> fmap Value_uint32 $ L.getField reader 1
            9 -> fmap Value_uint64 $ L.getField reader 1
            10 -> fmap Value_float32 $ L.getField reader 1
            11 -> fmap Value_float64 $ L.getField reader 1
            12 -> fmap Value_text $ L.getField reader 0
            13 -> fmap Value_data $ L.getField reader 0
            14 -> fmap Value_list $ L.getField reader 0
            15 -> fmap Value_enum $ L.getField reader 1
            16 -> fmap Value_struct $ L.getField reader 0
            17 -> return Value_interface
            18 -> fmap Value_anyPointer $ L.getField reader 0
            _ -> return $ Value_NotInSchema d



data ElementSize
  = ElementSize_NotInSchema Word16
  | ElementSize_empty
  | ElementSize_bit
  | ElementSize_byte
  | ElementSize_twoBytes
  | ElementSize_fourBytes
  | ElementSize_eightBytes
  | ElementSize_pointer
  | ElementSize_inlineComposite

instance Enum ElementSize where
    toEnum num =
        case num of
            0 -> ElementSize_empty
            1 -> ElementSize_bit
            2 -> ElementSize_byte
            3 -> ElementSize_twoBytes
            4 -> ElementSize_fourBytes
            5 -> ElementSize_eightBytes
            6 -> ElementSize_pointer
            7 -> ElementSize_inlineComposite
            _ -> ElementSize_NotInSchema $ fromIntegral num

    fromEnum enum =
        case enum of
            ElementSize_empty -> 0
            ElementSize_bit -> 1
            ElementSize_byte -> 2
            ElementSize_twoBytes -> 3
            ElementSize_fourBytes -> 4
            ElementSize_eightBytes -> 5
            ElementSize_pointer -> 6
            ElementSize_inlineComposite -> 7
            ElementSize_NotInSchema num -> fromIntegral num

instance Eq ElementSize where
    x == y = fromEnum x == fromEnum y

instance L.ListElement ElementSize where
    elementSize _ = L.SzTwoBytes
    getUntypedElement reader index =
        fmap (toEnum . fromIntegral) (L.getUntypedElement reader index :: IO Word16)


class HasAnnotations a where
    type AnnotationsTy a :: *
    getAnnotations :: a -> IO (AnnotationsTy a)

class HasBrand a where
    type BrandTy a :: *
    getBrand :: a -> IO (BrandTy a)

class HasCodeOrder a where
    type CodeOrderTy a :: *
    getCodeOrder :: a -> IO (CodeOrderTy a)

class HasDataWordCount a where
    type DataWordCountTy a :: *
    getDataWordCount :: a -> IO (DataWordCountTy a)

class HasDefaultValue a where
    type DefaultValueTy a :: *
    getDefaultValue :: a -> IO (DefaultValueTy a)

class HasDiscriminantCount a where
    type DiscriminantCountTy a :: *
    getDiscriminantCount :: a -> IO (DiscriminantCountTy a)

class HasDiscriminantOffset a where
    type DiscriminantOffsetTy a :: *
    getDiscriminantOffset :: a -> IO (DiscriminantOffsetTy a)

class HasDiscriminantValue a where
    type DiscriminantValueTy a :: *
    getDiscriminantValue :: a -> IO (DiscriminantValueTy a)

class HasDisplayName a where
    type DisplayNameTy a :: *
    getDisplayName :: a -> IO (DisplayNameTy a)

class HasDisplayNamePrefixLength a where
    type DisplayNamePrefixLengthTy a :: *
    getDisplayNamePrefixLength :: a -> IO (DisplayNamePrefixLengthTy a)

class HasElementType a where
    type ElementTypeTy a :: *
    getElementType :: a -> IO (ElementTypeTy a)

class HasEnumerants a where
    type EnumerantsTy a :: *
    getEnumerants :: a -> IO (EnumerantsTy a)

class HasFields a where
    type FieldsTy a :: *
    getFields :: a -> IO (FieldsTy a)

class HasFilename a where
    type FilenameTy a :: *
    getFilename :: a -> IO (FilenameTy a)

class HasHadExplicitDefault a where
    type HadExplicitDefaultTy a :: *
    getHadExplicitDefault :: a -> IO (HadExplicitDefaultTy a)

class HasId a where
    type IdTy a :: *
    getId :: a -> IO (IdTy a)

class HasImplicitParameters a where
    type ImplicitParametersTy a :: *
    getImplicitParameters :: a -> IO (ImplicitParametersTy a)

class HasImports a where
    type ImportsTy a :: *
    getImports :: a -> IO (ImportsTy a)

class HasIsGeneric a where
    type IsGenericTy a :: *
    getIsGeneric :: a -> IO (IsGenericTy a)

class HasIsGroup a where
    type IsGroupTy a :: *
    getIsGroup :: a -> IO (IsGroupTy a)

class HasMethods a where
    type MethodsTy a :: *
    getMethods :: a -> IO (MethodsTy a)

class HasName a where
    type NameTy a :: *
    getName :: a -> IO (NameTy a)

class HasNestedNodes a where
    type NestedNodesTy a :: *
    getNestedNodes :: a -> IO (NestedNodesTy a)

class HasNodes a where
    type NodesTy a :: *
    getNodes :: a -> IO (NodesTy a)

class HasOffset a where
    type OffsetTy a :: *
    getOffset :: a -> IO (OffsetTy a)

class HasOrdinal a where
    type OrdinalTy a :: *
    getOrdinal :: a -> IO (OrdinalTy a)

class HasParamBrand a where
    type ParamBrandTy a :: *
    getParamBrand :: a -> IO (ParamBrandTy a)

class HasParamStructType a where
    type ParamStructTypeTy a :: *
    getParamStructType :: a -> IO (ParamStructTypeTy a)

class HasParameterIndex a where
    type ParameterIndexTy a :: *
    getParameterIndex :: a -> IO (ParameterIndexTy a)

class HasParameters a where
    type ParametersTy a :: *
    getParameters :: a -> IO (ParametersTy a)

class HasPointerCount a where
    type PointerCountTy a :: *
    getPointerCount :: a -> IO (PointerCountTy a)

class HasPreferredListEncoding a where
    type PreferredListEncodingTy a :: *
    getPreferredListEncoding :: a -> IO (PreferredListEncodingTy a)

class HasRequestedFiles a where
    type RequestedFilesTy a :: *
    getRequestedFiles :: a -> IO (RequestedFilesTy a)

class HasResultBrand a where
    type ResultBrandTy a :: *
    getResultBrand :: a -> IO (ResultBrandTy a)

class HasResultStructType a where
    type ResultStructTypeTy a :: *
    getResultStructType :: a -> IO (ResultStructTypeTy a)

class HasScopeId a where
    type ScopeIdTy a :: *
    getScopeId :: a -> IO (ScopeIdTy a)

class HasScopes a where
    type ScopesTy a :: *
    getScopes :: a -> IO (ScopesTy a)

class HasSuperclasses a where
    type SuperclassesTy a :: *
    getSuperclasses :: a -> IO (SuperclassesTy a)

class HasTargetsAnnotation a where
    type TargetsAnnotationTy a :: *
    getTargetsAnnotation :: a -> IO (TargetsAnnotationTy a)

class HasTargetsConst a where
    type TargetsConstTy a :: *
    getTargetsConst :: a -> IO (TargetsConstTy a)

class HasTargetsEnum a where
    type TargetsEnumTy a :: *
    getTargetsEnum :: a -> IO (TargetsEnumTy a)

class HasTargetsEnumerant a where
    type TargetsEnumerantTy a :: *
    getTargetsEnumerant :: a -> IO (TargetsEnumerantTy a)

class HasTargetsField a where
    type TargetsFieldTy a :: *
    getTargetsField :: a -> IO (TargetsFieldTy a)

class HasTargetsFile a where
    type TargetsFileTy a :: *
    getTargetsFile :: a -> IO (TargetsFileTy a)

class HasTargetsGroup a where
    type TargetsGroupTy a :: *
    getTargetsGroup :: a -> IO (TargetsGroupTy a)

class HasTargetsInterface a where
    type TargetsInterfaceTy a :: *
    getTargetsInterface :: a -> IO (TargetsInterfaceTy a)

class HasTargetsMethod a where
    type TargetsMethodTy a :: *
    getTargetsMethod :: a -> IO (TargetsMethodTy a)

class HasTargetsParam a where
    type TargetsParamTy a :: *
    getTargetsParam :: a -> IO (TargetsParamTy a)

class HasTargetsStruct a where
    type TargetsStructTy a :: *
    getTargetsStruct :: a -> IO (TargetsStructTy a)

class HasTargetsUnion a where
    type TargetsUnionTy a :: *
    getTargetsUnion :: a -> IO (TargetsUnionTy a)

class HasType a where
    type TypeTy a :: *
    getType :: a -> IO (TypeTy a)

class HasTypeId a where
    type TypeIdTy a :: *
    getTypeId :: a -> IO (TypeIdTy a)

class HasValue a where
    type ValueTy a :: *
    getValue :: a -> IO (ValueTy a)




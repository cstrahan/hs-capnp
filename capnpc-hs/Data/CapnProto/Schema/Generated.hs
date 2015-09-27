{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.CapnProto.Schema.Generated where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Coerce (coerce)
import           Data.Int
import           Data.Word
import           Foreign.Ptr (nullPtr)
import           GHC.Exts (Ptr(..))
import           GHC.Prim (Addr#)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.CapnProto.Layout as L

{-# NOINLINE emptyString #-}
emptyString :: BS.ByteString
emptyString = unsafePerformIO $ BS.unsafePackAddressLen 0 "\NULL"#

data Annotation_Reader = Annotation_Reader L.StructReader
data Annotation_Builder = Annotation_Builder L.StructBuilder

instance L.FromStructReader Annotation_Reader where
    fromStructReader = return . Annotation_Reader

instance L.ListElement Annotation_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Annotation_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Annotation_Reader where
    getReaderElement list index =
        fmap Annotation_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Annotation_Builder where
    getBuilderElement list index =
        fmap Annotation_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Annotation_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasId Annotation_Reader where
    type IdTy Annotation_Reader = Word64
    getId (Annotation_Reader struct) = L.getReaderNumericField struct 0

instance HasId Annotation_Builder where
    type IdTy Annotation_Builder = Word64
    getId (Annotation_Builder struct) = L.getBuilderNumericField struct 0

instance HasValue Annotation_Reader where
    type ValueTy Annotation_Reader = Value_Reader
    getValue (Annotation_Reader struct) = fmap Value_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance HasValue Annotation_Builder where
    type ValueTy Annotation_Builder = Value_Builder
    getValue (Annotation_Builder struct) = fmap Value_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 1 2) nullPtr

instance HasBrand Annotation_Reader where
    type BrandTy Annotation_Reader = Brand_Reader
    getBrand (Annotation_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 1) nullPtr

instance HasBrand Annotation_Builder where
    type BrandTy Annotation_Builder = Brand_Builder
    getBrand (Annotation_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 1) (L.StructSize 1 2) nullPtr


data Brand_Reader = Brand_Reader L.StructReader
data Brand_Builder = Brand_Builder L.StructBuilder

instance L.FromStructReader Brand_Reader where
    fromStructReader = return . Brand_Reader

instance L.ListElement Brand_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Brand_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Brand_Reader where
    getReaderElement list index =
        fmap Brand_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Brand_Builder where
    getBuilderElement list index =
        fmap Brand_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Brand_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasScopes Brand_Reader where
    type ScopesTy Brand_Reader = (L.ListReader Brand_Scope_Reader)
    getScopes (Brand_Reader struct) = L.getReaderList (L.getReaderPointerField struct 0) nullPtr

instance HasScopes Brand_Builder where
    type ScopesTy Brand_Builder = (L.ListBuilder Brand_Scope_Builder)
    getScopes (Brand_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 0) nullPtr


data Brand_Binding_Reader = Brand_Binding_Reader L.StructReader
data Brand_Binding_Builder = Brand_Binding_Builder L.StructBuilder

instance L.FromStructReader Brand_Binding_Reader where
    fromStructReader = return . Brand_Binding_Reader

instance L.ListElement Brand_Binding_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Brand_Binding_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Brand_Binding_Reader where
    getReaderElement list index =
        fmap Brand_Binding_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Brand_Binding_Builder where
    getBuilderElement list index =
        fmap Brand_Binding_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Brand_Binding_Builder struct) =
        L.setBuilderElement (coerce list) index struct

data Brand_Binding_Which_Reader
  = Brand_Binding_NotInSchema_Reader_ Word16
  | Brand_Binding_unbound_Reader_
  | Brand_Binding_type_Reader_ Type_Reader

data Brand_Binding_Which_Builder
  = Brand_Binding_NotInSchema_Builder_ Word16
  | Brand_Binding_unbound_Builder_
  | Brand_Binding_type_Builder_ Type_Builder

instance L.Union Brand_Binding_Reader where
    type UnionTy Brand_Binding_Reader = Brand_Binding_Which_Reader
    which (Brand_Binding_Reader struct) = do
        d <- L.getReaderNumericField struct 0 :: IO Word16
        case d of
            0 -> return Brand_Binding_unbound_Reader_
            1 -> fmap Brand_Binding_type_Reader_ $ fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr
            _ -> return $ Brand_Binding_NotInSchema_Reader_ d

instance L.Union Brand_Binding_Builder where
    type UnionTy Brand_Binding_Builder = Brand_Binding_Which_Builder
    which (Brand_Binding_Builder struct) = do
        d <- L.getBuilderNumericField struct 0 :: IO Word16
        case d of
            0 -> return Brand_Binding_unbound_Builder_
            1 -> fmap Brand_Binding_type_Builder_ $ fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 1 1) nullPtr
            _ -> return $ Brand_Binding_NotInSchema_Builder_ d


data Brand_Scope_Reader = Brand_Scope_Reader L.StructReader
data Brand_Scope_Builder = Brand_Scope_Builder L.StructBuilder

instance L.FromStructReader Brand_Scope_Reader where
    fromStructReader = return . Brand_Scope_Reader

instance L.ListElement Brand_Scope_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Brand_Scope_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Brand_Scope_Reader where
    getReaderElement list index =
        fmap Brand_Scope_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Brand_Scope_Builder where
    getBuilderElement list index =
        fmap Brand_Scope_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Brand_Scope_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasScopeId Brand_Scope_Reader where
    type ScopeIdTy Brand_Scope_Reader = Word64
    getScopeId (Brand_Scope_Reader struct) = L.getReaderNumericField struct 0

instance HasScopeId Brand_Scope_Builder where
    type ScopeIdTy Brand_Scope_Builder = Word64
    getScopeId (Brand_Scope_Builder struct) = L.getBuilderNumericField struct 0

data Brand_Scope_Which_Reader
  = Brand_Scope_NotInSchema_Reader_ Word16
  | Brand_Scope_bind_Reader_ (L.ListReader Brand_Binding_Reader)
  | Brand_Scope_inherit_Reader_

data Brand_Scope_Which_Builder
  = Brand_Scope_NotInSchema_Builder_ Word16
  | Brand_Scope_bind_Builder_ (L.ListBuilder Brand_Binding_Builder)
  | Brand_Scope_inherit_Builder_

instance L.Union Brand_Scope_Reader where
    type UnionTy Brand_Scope_Reader = Brand_Scope_Which_Reader
    which (Brand_Scope_Reader struct) = do
        d <- L.getReaderNumericField struct 4 :: IO Word16
        case d of
            0 -> fmap Brand_Scope_bind_Reader_ $ L.getReaderList (L.getReaderPointerField struct 0) nullPtr
            1 -> return Brand_Scope_inherit_Reader_
            _ -> return $ Brand_Scope_NotInSchema_Reader_ d

instance L.Union Brand_Scope_Builder where
    type UnionTy Brand_Scope_Builder = Brand_Scope_Which_Builder
    which (Brand_Scope_Builder struct) = do
        d <- L.getBuilderNumericField struct 4 :: IO Word16
        case d of
            0 -> fmap Brand_Scope_bind_Builder_ $ L.getBuilderList (L.getBuilderPointerField struct 0) nullPtr
            1 -> return Brand_Scope_inherit_Builder_
            _ -> return $ Brand_Scope_NotInSchema_Builder_ d


data CodeGeneratorRequest_Reader = CodeGeneratorRequest_Reader L.StructReader
data CodeGeneratorRequest_Builder = CodeGeneratorRequest_Builder L.StructBuilder

instance L.FromStructReader CodeGeneratorRequest_Reader where
    fromStructReader = return . CodeGeneratorRequest_Reader

instance L.ListElement CodeGeneratorRequest_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement CodeGeneratorRequest_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement CodeGeneratorRequest_Reader where
    getReaderElement list index =
        fmap CodeGeneratorRequest_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement CodeGeneratorRequest_Builder where
    getBuilderElement list index =
        fmap CodeGeneratorRequest_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (CodeGeneratorRequest_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasNodes CodeGeneratorRequest_Reader where
    type NodesTy CodeGeneratorRequest_Reader = (L.ListReader Node_Reader)
    getNodes (CodeGeneratorRequest_Reader struct) = L.getReaderList (L.getReaderPointerField struct 0) nullPtr

instance HasNodes CodeGeneratorRequest_Builder where
    type NodesTy CodeGeneratorRequest_Builder = (L.ListBuilder Node_Builder)
    getNodes (CodeGeneratorRequest_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 0) nullPtr

instance HasRequestedFiles CodeGeneratorRequest_Reader where
    type RequestedFilesTy CodeGeneratorRequest_Reader = (L.ListReader CodeGeneratorRequest_RequestedFile_Reader)
    getRequestedFiles (CodeGeneratorRequest_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance HasRequestedFiles CodeGeneratorRequest_Builder where
    type RequestedFilesTy CodeGeneratorRequest_Builder = (L.ListBuilder CodeGeneratorRequest_RequestedFile_Builder)
    getRequestedFiles (CodeGeneratorRequest_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr


data CodeGeneratorRequest_RequestedFile_Reader = CodeGeneratorRequest_RequestedFile_Reader L.StructReader
data CodeGeneratorRequest_RequestedFile_Builder = CodeGeneratorRequest_RequestedFile_Builder L.StructBuilder

instance L.FromStructReader CodeGeneratorRequest_RequestedFile_Reader where
    fromStructReader = return . CodeGeneratorRequest_RequestedFile_Reader

instance L.ListElement CodeGeneratorRequest_RequestedFile_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement CodeGeneratorRequest_RequestedFile_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement CodeGeneratorRequest_RequestedFile_Reader where
    getReaderElement list index =
        fmap CodeGeneratorRequest_RequestedFile_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement CodeGeneratorRequest_RequestedFile_Builder where
    getBuilderElement list index =
        fmap CodeGeneratorRequest_RequestedFile_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (CodeGeneratorRequest_RequestedFile_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasId CodeGeneratorRequest_RequestedFile_Reader where
    type IdTy CodeGeneratorRequest_RequestedFile_Reader = Word64
    getId (CodeGeneratorRequest_RequestedFile_Reader struct) = L.getReaderNumericField struct 0

instance HasId CodeGeneratorRequest_RequestedFile_Builder where
    type IdTy CodeGeneratorRequest_RequestedFile_Builder = Word64
    getId (CodeGeneratorRequest_RequestedFile_Builder struct) = L.getBuilderNumericField struct 0

instance HasFilename CodeGeneratorRequest_RequestedFile_Reader where
    type FilenameTy CodeGeneratorRequest_RequestedFile_Reader = L.TextReader
    getFilename (CodeGeneratorRequest_RequestedFile_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance HasFilename CodeGeneratorRequest_RequestedFile_Builder where
    type FilenameTy CodeGeneratorRequest_RequestedFile_Builder = L.TextBuilder
    getFilename (CodeGeneratorRequest_RequestedFile_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance HasImports CodeGeneratorRequest_RequestedFile_Reader where
    type ImportsTy CodeGeneratorRequest_RequestedFile_Reader = (L.ListReader CodeGeneratorRequest_RequestedFile_Import_Reader)
    getImports (CodeGeneratorRequest_RequestedFile_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance HasImports CodeGeneratorRequest_RequestedFile_Builder where
    type ImportsTy CodeGeneratorRequest_RequestedFile_Builder = (L.ListBuilder CodeGeneratorRequest_RequestedFile_Import_Builder)
    getImports (CodeGeneratorRequest_RequestedFile_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr


data CodeGeneratorRequest_RequestedFile_Import_Reader = CodeGeneratorRequest_RequestedFile_Import_Reader L.StructReader
data CodeGeneratorRequest_RequestedFile_Import_Builder = CodeGeneratorRequest_RequestedFile_Import_Builder L.StructBuilder

instance L.FromStructReader CodeGeneratorRequest_RequestedFile_Import_Reader where
    fromStructReader = return . CodeGeneratorRequest_RequestedFile_Import_Reader

instance L.ListElement CodeGeneratorRequest_RequestedFile_Import_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement CodeGeneratorRequest_RequestedFile_Import_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement CodeGeneratorRequest_RequestedFile_Import_Reader where
    getReaderElement list index =
        fmap CodeGeneratorRequest_RequestedFile_Import_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement CodeGeneratorRequest_RequestedFile_Import_Builder where
    getBuilderElement list index =
        fmap CodeGeneratorRequest_RequestedFile_Import_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (CodeGeneratorRequest_RequestedFile_Import_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasId CodeGeneratorRequest_RequestedFile_Import_Reader where
    type IdTy CodeGeneratorRequest_RequestedFile_Import_Reader = Word64
    getId (CodeGeneratorRequest_RequestedFile_Import_Reader struct) = L.getReaderNumericField struct 0

instance HasId CodeGeneratorRequest_RequestedFile_Import_Builder where
    type IdTy CodeGeneratorRequest_RequestedFile_Import_Builder = Word64
    getId (CodeGeneratorRequest_RequestedFile_Import_Builder struct) = L.getBuilderNumericField struct 0

instance HasName CodeGeneratorRequest_RequestedFile_Import_Reader where
    type NameTy CodeGeneratorRequest_RequestedFile_Import_Reader = L.TextReader
    getName (CodeGeneratorRequest_RequestedFile_Import_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance HasName CodeGeneratorRequest_RequestedFile_Import_Builder where
    type NameTy CodeGeneratorRequest_RequestedFile_Import_Builder = L.TextBuilder
    getName (CodeGeneratorRequest_RequestedFile_Import_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString


data Enumerant_Reader = Enumerant_Reader L.StructReader
data Enumerant_Builder = Enumerant_Builder L.StructBuilder

instance L.FromStructReader Enumerant_Reader where
    fromStructReader = return . Enumerant_Reader

instance L.ListElement Enumerant_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Enumerant_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Enumerant_Reader where
    getReaderElement list index =
        fmap Enumerant_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Enumerant_Builder where
    getBuilderElement list index =
        fmap Enumerant_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Enumerant_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasName Enumerant_Reader where
    type NameTy Enumerant_Reader = L.TextReader
    getName (Enumerant_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance HasName Enumerant_Builder where
    type NameTy Enumerant_Builder = L.TextBuilder
    getName (Enumerant_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance HasCodeOrder Enumerant_Reader where
    type CodeOrderTy Enumerant_Reader = Word16
    getCodeOrder (Enumerant_Reader struct) = L.getReaderNumericField struct 0

instance HasCodeOrder Enumerant_Builder where
    type CodeOrderTy Enumerant_Builder = Word16
    getCodeOrder (Enumerant_Builder struct) = L.getBuilderNumericField struct 0

instance HasAnnotations Enumerant_Reader where
    type AnnotationsTy Enumerant_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Enumerant_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance HasAnnotations Enumerant_Builder where
    type AnnotationsTy Enumerant_Builder = (L.ListBuilder Annotation_Builder)
    getAnnotations (Enumerant_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr


data Field_Reader = Field_Reader L.StructReader
data Field_Builder = Field_Builder L.StructBuilder

instance L.FromStructReader Field_Reader where
    fromStructReader = return . Field_Reader

instance L.ListElement Field_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Field_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Field_Reader where
    getReaderElement list index =
        fmap Field_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Field_Builder where
    getBuilderElement list index =
        fmap Field_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Field_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasName Field_Reader where
    type NameTy Field_Reader = L.TextReader
    getName (Field_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance HasName Field_Builder where
    type NameTy Field_Builder = L.TextBuilder
    getName (Field_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance HasCodeOrder Field_Reader where
    type CodeOrderTy Field_Reader = Word16
    getCodeOrder (Field_Reader struct) = L.getReaderNumericField struct 0

instance HasCodeOrder Field_Builder where
    type CodeOrderTy Field_Builder = Word16
    getCodeOrder (Field_Builder struct) = L.getBuilderNumericField struct 0

instance HasAnnotations Field_Reader where
    type AnnotationsTy Field_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Field_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance HasAnnotations Field_Builder where
    type AnnotationsTy Field_Builder = (L.ListBuilder Annotation_Builder)
    getAnnotations (Field_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr

instance HasDiscriminantValue Field_Reader where
    type DiscriminantValueTy Field_Reader = Word16
    getDiscriminantValue (Field_Reader struct) = L.getReaderNumericFieldMasked struct 1 65535

instance HasDiscriminantValue Field_Builder where
    type DiscriminantValueTy Field_Builder = Word16
    getDiscriminantValue (Field_Builder struct) = L.getBuilderNumericFieldMasked struct 1 65535

instance HasOrdinal Field_Reader where
    type OrdinalTy Field_Reader = Field_ordinal_Reader
    getOrdinal (Field_Reader struct) = return . Field_ordinal_Reader $ struct

instance HasOrdinal Field_Builder where
    type OrdinalTy Field_Builder = Field_ordinal_Builder
    getOrdinal (Field_Builder struct) = return . Field_ordinal_Builder $ struct

data Field_Which_Reader
  = Field_NotInSchema_Reader_ Word16
  | Field_slot_Reader_ Field_slot_Reader
  | Field_group_Reader_ Field_group_Reader

data Field_Which_Builder
  = Field_NotInSchema_Builder_ Word16
  | Field_slot_Builder_ Field_slot_Builder
  | Field_group_Builder_ Field_group_Builder

instance L.Union Field_Reader where
    type UnionTy Field_Reader = Field_Which_Reader
    which (Field_Reader struct) = do
        d <- L.getReaderNumericField struct 4 :: IO Word16
        case d of
            0 -> fmap Field_slot_Reader_ $ return . Field_slot_Reader $ struct
            1 -> fmap Field_group_Reader_ $ return . Field_group_Reader $ struct
            _ -> return $ Field_NotInSchema_Reader_ d

instance L.Union Field_Builder where
    type UnionTy Field_Builder = Field_Which_Builder
    which (Field_Builder struct) = do
        d <- L.getBuilderNumericField struct 4 :: IO Word16
        case d of
            0 -> fmap Field_slot_Builder_ $ return . Field_slot_Builder $ struct
            1 -> fmap Field_group_Builder_ $ return . Field_group_Builder $ struct
            _ -> return $ Field_NotInSchema_Builder_ d


data Field_group_Reader = Field_group_Reader L.StructReader
data Field_group_Builder = Field_group_Builder L.StructBuilder

instance HasTypeId Field_group_Reader where
    type TypeIdTy Field_group_Reader = Word64
    getTypeId (Field_group_Reader struct) = L.getReaderNumericField struct 2

instance HasTypeId Field_group_Builder where
    type TypeIdTy Field_group_Builder = Word64
    getTypeId (Field_group_Builder struct) = L.getBuilderNumericField struct 2


data Field_ordinal_Reader = Field_ordinal_Reader L.StructReader
data Field_ordinal_Builder = Field_ordinal_Builder L.StructBuilder

data Field_ordinal_Which_Reader
  = Field_ordinal_NotInSchema_Reader_ Word16
  | Field_ordinal_implicit_Reader_
  | Field_ordinal_explicit_Reader_ Word16

data Field_ordinal_Which_Builder
  = Field_ordinal_NotInSchema_Builder_ Word16
  | Field_ordinal_implicit_Builder_
  | Field_ordinal_explicit_Builder_ Word16

instance L.Union Field_ordinal_Reader where
    type UnionTy Field_ordinal_Reader = Field_ordinal_Which_Reader
    which (Field_ordinal_Reader struct) = do
        d <- L.getReaderNumericField struct 5 :: IO Word16
        case d of
            0 -> return Field_ordinal_implicit_Reader_
            1 -> fmap Field_ordinal_explicit_Reader_ $ L.getReaderNumericField struct 6
            _ -> return $ Field_ordinal_NotInSchema_Reader_ d

instance L.Union Field_ordinal_Builder where
    type UnionTy Field_ordinal_Builder = Field_ordinal_Which_Builder
    which (Field_ordinal_Builder struct) = do
        d <- L.getBuilderNumericField struct 5 :: IO Word16
        case d of
            0 -> return Field_ordinal_implicit_Builder_
            1 -> fmap Field_ordinal_explicit_Builder_ $ L.getBuilderNumericField struct 6
            _ -> return $ Field_ordinal_NotInSchema_Builder_ d


data Field_slot_Reader = Field_slot_Reader L.StructReader
data Field_slot_Builder = Field_slot_Builder L.StructBuilder

instance HasOffset Field_slot_Reader where
    type OffsetTy Field_slot_Reader = Word32
    getOffset (Field_slot_Reader struct) = L.getReaderNumericField struct 1

instance HasOffset Field_slot_Builder where
    type OffsetTy Field_slot_Builder = Word32
    getOffset (Field_slot_Builder struct) = L.getBuilderNumericField struct 1

instance HasType Field_slot_Reader where
    type TypeTy Field_slot_Reader = Type_Reader
    getType (Field_slot_Reader struct) = fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 2) nullPtr

instance HasType Field_slot_Builder where
    type TypeTy Field_slot_Builder = Type_Builder
    getType (Field_slot_Builder struct) = fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 2) (L.StructSize 3 4) nullPtr

instance HasDefaultValue Field_slot_Reader where
    type DefaultValueTy Field_slot_Reader = Value_Reader
    getDefaultValue (Field_slot_Reader struct) = fmap Value_Reader $ L.getReaderStruct (L.getReaderPointerField struct 3) nullPtr

instance HasDefaultValue Field_slot_Builder where
    type DefaultValueTy Field_slot_Builder = Value_Builder
    getDefaultValue (Field_slot_Builder struct) = fmap Value_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 3) (L.StructSize 3 4) nullPtr

instance HasHadExplicitDefault Field_slot_Reader where
    type HadExplicitDefaultTy Field_slot_Reader = Bool
    getHadExplicitDefault (Field_slot_Reader struct) = L.getReaderBoolField struct 128

instance HasHadExplicitDefault Field_slot_Builder where
    type HadExplicitDefaultTy Field_slot_Builder = Bool
    getHadExplicitDefault (Field_slot_Builder struct) = L.getBuilderBoolField struct 128


data Method_Reader = Method_Reader L.StructReader
data Method_Builder = Method_Builder L.StructBuilder

instance L.FromStructReader Method_Reader where
    fromStructReader = return . Method_Reader

instance L.ListElement Method_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Method_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Method_Reader where
    getReaderElement list index =
        fmap Method_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Method_Builder where
    getBuilderElement list index =
        fmap Method_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Method_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasName Method_Reader where
    type NameTy Method_Reader = L.TextReader
    getName (Method_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance HasName Method_Builder where
    type NameTy Method_Builder = L.TextBuilder
    getName (Method_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance HasCodeOrder Method_Reader where
    type CodeOrderTy Method_Reader = Word16
    getCodeOrder (Method_Reader struct) = L.getReaderNumericField struct 0

instance HasCodeOrder Method_Builder where
    type CodeOrderTy Method_Builder = Word16
    getCodeOrder (Method_Builder struct) = L.getBuilderNumericField struct 0

instance HasParamStructType Method_Reader where
    type ParamStructTypeTy Method_Reader = Word64
    getParamStructType (Method_Reader struct) = L.getReaderNumericField struct 1

instance HasParamStructType Method_Builder where
    type ParamStructTypeTy Method_Builder = Word64
    getParamStructType (Method_Builder struct) = L.getBuilderNumericField struct 1

instance HasResultStructType Method_Reader where
    type ResultStructTypeTy Method_Reader = Word64
    getResultStructType (Method_Reader struct) = L.getReaderNumericField struct 2

instance HasResultStructType Method_Builder where
    type ResultStructTypeTy Method_Builder = Word64
    getResultStructType (Method_Builder struct) = L.getBuilderNumericField struct 2

instance HasAnnotations Method_Reader where
    type AnnotationsTy Method_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Method_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance HasAnnotations Method_Builder where
    type AnnotationsTy Method_Builder = (L.ListBuilder Annotation_Builder)
    getAnnotations (Method_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr

instance HasParamBrand Method_Reader where
    type ParamBrandTy Method_Reader = Brand_Reader
    getParamBrand (Method_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 2) nullPtr

instance HasParamBrand Method_Builder where
    type ParamBrandTy Method_Builder = Brand_Builder
    getParamBrand (Method_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 2) (L.StructSize 3 5) nullPtr

instance HasResultBrand Method_Reader where
    type ResultBrandTy Method_Reader = Brand_Reader
    getResultBrand (Method_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 3) nullPtr

instance HasResultBrand Method_Builder where
    type ResultBrandTy Method_Builder = Brand_Builder
    getResultBrand (Method_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 3) (L.StructSize 3 5) nullPtr

instance HasImplicitParameters Method_Reader where
    type ImplicitParametersTy Method_Reader = (L.ListReader Node_Parameter_Reader)
    getImplicitParameters (Method_Reader struct) = L.getReaderList (L.getReaderPointerField struct 4) nullPtr

instance HasImplicitParameters Method_Builder where
    type ImplicitParametersTy Method_Builder = (L.ListBuilder Node_Parameter_Builder)
    getImplicitParameters (Method_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 4) nullPtr


data Node_Reader = Node_Reader L.StructReader
data Node_Builder = Node_Builder L.StructBuilder

instance L.FromStructReader Node_Reader where
    fromStructReader = return . Node_Reader

instance L.ListElement Node_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Node_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Node_Reader where
    getReaderElement list index =
        fmap Node_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Node_Builder where
    getBuilderElement list index =
        fmap Node_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Node_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasId Node_Reader where
    type IdTy Node_Reader = Word64
    getId (Node_Reader struct) = L.getReaderNumericField struct 0

instance HasId Node_Builder where
    type IdTy Node_Builder = Word64
    getId (Node_Builder struct) = L.getBuilderNumericField struct 0

instance HasDisplayName Node_Reader where
    type DisplayNameTy Node_Reader = L.TextReader
    getDisplayName (Node_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance HasDisplayName Node_Builder where
    type DisplayNameTy Node_Builder = L.TextBuilder
    getDisplayName (Node_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance HasDisplayNamePrefixLength Node_Reader where
    type DisplayNamePrefixLengthTy Node_Reader = Word32
    getDisplayNamePrefixLength (Node_Reader struct) = L.getReaderNumericField struct 2

instance HasDisplayNamePrefixLength Node_Builder where
    type DisplayNamePrefixLengthTy Node_Builder = Word32
    getDisplayNamePrefixLength (Node_Builder struct) = L.getBuilderNumericField struct 2

instance HasScopeId Node_Reader where
    type ScopeIdTy Node_Reader = Word64
    getScopeId (Node_Reader struct) = L.getReaderNumericField struct 2

instance HasScopeId Node_Builder where
    type ScopeIdTy Node_Builder = Word64
    getScopeId (Node_Builder struct) = L.getBuilderNumericField struct 2

instance HasNestedNodes Node_Reader where
    type NestedNodesTy Node_Reader = (L.ListReader Node_NestedNode_Reader)
    getNestedNodes (Node_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance HasNestedNodes Node_Builder where
    type NestedNodesTy Node_Builder = (L.ListBuilder Node_NestedNode_Builder)
    getNestedNodes (Node_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr

instance HasAnnotations Node_Reader where
    type AnnotationsTy Node_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Node_Reader struct) = L.getReaderList (L.getReaderPointerField struct 2) nullPtr

instance HasAnnotations Node_Builder where
    type AnnotationsTy Node_Builder = (L.ListBuilder Annotation_Builder)
    getAnnotations (Node_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 2) nullPtr

instance HasParameters Node_Reader where
    type ParametersTy Node_Reader = (L.ListReader Node_Parameter_Reader)
    getParameters (Node_Reader struct) = L.getReaderList (L.getReaderPointerField struct 5) nullPtr

instance HasParameters Node_Builder where
    type ParametersTy Node_Builder = (L.ListBuilder Node_Parameter_Builder)
    getParameters (Node_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 5) nullPtr

instance HasIsGeneric Node_Reader where
    type IsGenericTy Node_Reader = Bool
    getIsGeneric (Node_Reader struct) = L.getReaderBoolField struct 288

instance HasIsGeneric Node_Builder where
    type IsGenericTy Node_Builder = Bool
    getIsGeneric (Node_Builder struct) = L.getBuilderBoolField struct 288

data Node_Which_Reader
  = Node_NotInSchema_Reader_ Word16
  | Node_file_Reader_
  | Node_struct_Reader_ Node_struct_Reader
  | Node_enum_Reader_ Node_enum_Reader
  | Node_interface_Reader_ Node_interface_Reader
  | Node_const_Reader_ Node_const_Reader
  | Node_annotation_Reader_ Node_annotation_Reader

data Node_Which_Builder
  = Node_NotInSchema_Builder_ Word16
  | Node_file_Builder_
  | Node_struct_Builder_ Node_struct_Builder
  | Node_enum_Builder_ Node_enum_Builder
  | Node_interface_Builder_ Node_interface_Builder
  | Node_const_Builder_ Node_const_Builder
  | Node_annotation_Builder_ Node_annotation_Builder

instance L.Union Node_Reader where
    type UnionTy Node_Reader = Node_Which_Reader
    which (Node_Reader struct) = do
        d <- L.getReaderNumericField struct 6 :: IO Word16
        case d of
            0 -> return Node_file_Reader_
            1 -> fmap Node_struct_Reader_ $ return . Node_struct_Reader $ struct
            2 -> fmap Node_enum_Reader_ $ return . Node_enum_Reader $ struct
            3 -> fmap Node_interface_Reader_ $ return . Node_interface_Reader $ struct
            4 -> fmap Node_const_Reader_ $ return . Node_const_Reader $ struct
            5 -> fmap Node_annotation_Reader_ $ return . Node_annotation_Reader $ struct
            _ -> return $ Node_NotInSchema_Reader_ d

instance L.Union Node_Builder where
    type UnionTy Node_Builder = Node_Which_Builder
    which (Node_Builder struct) = do
        d <- L.getBuilderNumericField struct 6 :: IO Word16
        case d of
            0 -> return Node_file_Builder_
            1 -> fmap Node_struct_Builder_ $ return . Node_struct_Builder $ struct
            2 -> fmap Node_enum_Builder_ $ return . Node_enum_Builder $ struct
            3 -> fmap Node_interface_Builder_ $ return . Node_interface_Builder $ struct
            4 -> fmap Node_const_Builder_ $ return . Node_const_Builder $ struct
            5 -> fmap Node_annotation_Builder_ $ return . Node_annotation_Builder $ struct
            _ -> return $ Node_NotInSchema_Builder_ d


data Node_NestedNode_Reader = Node_NestedNode_Reader L.StructReader
data Node_NestedNode_Builder = Node_NestedNode_Builder L.StructBuilder

instance L.FromStructReader Node_NestedNode_Reader where
    fromStructReader = return . Node_NestedNode_Reader

instance L.ListElement Node_NestedNode_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Node_NestedNode_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Node_NestedNode_Reader where
    getReaderElement list index =
        fmap Node_NestedNode_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Node_NestedNode_Builder where
    getBuilderElement list index =
        fmap Node_NestedNode_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Node_NestedNode_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasName Node_NestedNode_Reader where
    type NameTy Node_NestedNode_Reader = L.TextReader
    getName (Node_NestedNode_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance HasName Node_NestedNode_Builder where
    type NameTy Node_NestedNode_Builder = L.TextBuilder
    getName (Node_NestedNode_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance HasId Node_NestedNode_Reader where
    type IdTy Node_NestedNode_Reader = Word64
    getId (Node_NestedNode_Reader struct) = L.getReaderNumericField struct 0

instance HasId Node_NestedNode_Builder where
    type IdTy Node_NestedNode_Builder = Word64
    getId (Node_NestedNode_Builder struct) = L.getBuilderNumericField struct 0


data Node_Parameter_Reader = Node_Parameter_Reader L.StructReader
data Node_Parameter_Builder = Node_Parameter_Builder L.StructBuilder

instance L.FromStructReader Node_Parameter_Reader where
    fromStructReader = return . Node_Parameter_Reader

instance L.ListElement Node_Parameter_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Node_Parameter_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Node_Parameter_Reader where
    getReaderElement list index =
        fmap Node_Parameter_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Node_Parameter_Builder where
    getBuilderElement list index =
        fmap Node_Parameter_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Node_Parameter_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasName Node_Parameter_Reader where
    type NameTy Node_Parameter_Reader = L.TextReader
    getName (Node_Parameter_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance HasName Node_Parameter_Builder where
    type NameTy Node_Parameter_Builder = L.TextBuilder
    getName (Node_Parameter_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString


data Node_annotation_Reader = Node_annotation_Reader L.StructReader
data Node_annotation_Builder = Node_annotation_Builder L.StructBuilder

instance HasType Node_annotation_Reader where
    type TypeTy Node_annotation_Reader = Type_Reader
    getType (Node_annotation_Reader struct) = fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 3) nullPtr

instance HasType Node_annotation_Builder where
    type TypeTy Node_annotation_Builder = Type_Builder
    getType (Node_annotation_Builder struct) = fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 3) (L.StructSize 5 6) nullPtr

instance HasTargetsFile Node_annotation_Reader where
    type TargetsFileTy Node_annotation_Reader = Bool
    getTargetsFile (Node_annotation_Reader struct) = L.getReaderBoolField struct 112

instance HasTargetsFile Node_annotation_Builder where
    type TargetsFileTy Node_annotation_Builder = Bool
    getTargetsFile (Node_annotation_Builder struct) = L.getBuilderBoolField struct 112

instance HasTargetsConst Node_annotation_Reader where
    type TargetsConstTy Node_annotation_Reader = Bool
    getTargetsConst (Node_annotation_Reader struct) = L.getReaderBoolField struct 113

instance HasTargetsConst Node_annotation_Builder where
    type TargetsConstTy Node_annotation_Builder = Bool
    getTargetsConst (Node_annotation_Builder struct) = L.getBuilderBoolField struct 113

instance HasTargetsEnum Node_annotation_Reader where
    type TargetsEnumTy Node_annotation_Reader = Bool
    getTargetsEnum (Node_annotation_Reader struct) = L.getReaderBoolField struct 114

instance HasTargetsEnum Node_annotation_Builder where
    type TargetsEnumTy Node_annotation_Builder = Bool
    getTargetsEnum (Node_annotation_Builder struct) = L.getBuilderBoolField struct 114

instance HasTargetsEnumerant Node_annotation_Reader where
    type TargetsEnumerantTy Node_annotation_Reader = Bool
    getTargetsEnumerant (Node_annotation_Reader struct) = L.getReaderBoolField struct 115

instance HasTargetsEnumerant Node_annotation_Builder where
    type TargetsEnumerantTy Node_annotation_Builder = Bool
    getTargetsEnumerant (Node_annotation_Builder struct) = L.getBuilderBoolField struct 115

instance HasTargetsStruct Node_annotation_Reader where
    type TargetsStructTy Node_annotation_Reader = Bool
    getTargetsStruct (Node_annotation_Reader struct) = L.getReaderBoolField struct 116

instance HasTargetsStruct Node_annotation_Builder where
    type TargetsStructTy Node_annotation_Builder = Bool
    getTargetsStruct (Node_annotation_Builder struct) = L.getBuilderBoolField struct 116

instance HasTargetsField Node_annotation_Reader where
    type TargetsFieldTy Node_annotation_Reader = Bool
    getTargetsField (Node_annotation_Reader struct) = L.getReaderBoolField struct 117

instance HasTargetsField Node_annotation_Builder where
    type TargetsFieldTy Node_annotation_Builder = Bool
    getTargetsField (Node_annotation_Builder struct) = L.getBuilderBoolField struct 117

instance HasTargetsUnion Node_annotation_Reader where
    type TargetsUnionTy Node_annotation_Reader = Bool
    getTargetsUnion (Node_annotation_Reader struct) = L.getReaderBoolField struct 118

instance HasTargetsUnion Node_annotation_Builder where
    type TargetsUnionTy Node_annotation_Builder = Bool
    getTargetsUnion (Node_annotation_Builder struct) = L.getBuilderBoolField struct 118

instance HasTargetsGroup Node_annotation_Reader where
    type TargetsGroupTy Node_annotation_Reader = Bool
    getTargetsGroup (Node_annotation_Reader struct) = L.getReaderBoolField struct 119

instance HasTargetsGroup Node_annotation_Builder where
    type TargetsGroupTy Node_annotation_Builder = Bool
    getTargetsGroup (Node_annotation_Builder struct) = L.getBuilderBoolField struct 119

instance HasTargetsInterface Node_annotation_Reader where
    type TargetsInterfaceTy Node_annotation_Reader = Bool
    getTargetsInterface (Node_annotation_Reader struct) = L.getReaderBoolField struct 120

instance HasTargetsInterface Node_annotation_Builder where
    type TargetsInterfaceTy Node_annotation_Builder = Bool
    getTargetsInterface (Node_annotation_Builder struct) = L.getBuilderBoolField struct 120

instance HasTargetsMethod Node_annotation_Reader where
    type TargetsMethodTy Node_annotation_Reader = Bool
    getTargetsMethod (Node_annotation_Reader struct) = L.getReaderBoolField struct 121

instance HasTargetsMethod Node_annotation_Builder where
    type TargetsMethodTy Node_annotation_Builder = Bool
    getTargetsMethod (Node_annotation_Builder struct) = L.getBuilderBoolField struct 121

instance HasTargetsParam Node_annotation_Reader where
    type TargetsParamTy Node_annotation_Reader = Bool
    getTargetsParam (Node_annotation_Reader struct) = L.getReaderBoolField struct 122

instance HasTargetsParam Node_annotation_Builder where
    type TargetsParamTy Node_annotation_Builder = Bool
    getTargetsParam (Node_annotation_Builder struct) = L.getBuilderBoolField struct 122

instance HasTargetsAnnotation Node_annotation_Reader where
    type TargetsAnnotationTy Node_annotation_Reader = Bool
    getTargetsAnnotation (Node_annotation_Reader struct) = L.getReaderBoolField struct 123

instance HasTargetsAnnotation Node_annotation_Builder where
    type TargetsAnnotationTy Node_annotation_Builder = Bool
    getTargetsAnnotation (Node_annotation_Builder struct) = L.getBuilderBoolField struct 123


data Node_const_Reader = Node_const_Reader L.StructReader
data Node_const_Builder = Node_const_Builder L.StructBuilder

instance HasType Node_const_Reader where
    type TypeTy Node_const_Reader = Type_Reader
    getType (Node_const_Reader struct) = fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 3) nullPtr

instance HasType Node_const_Builder where
    type TypeTy Node_const_Builder = Type_Builder
    getType (Node_const_Builder struct) = fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 3) (L.StructSize 5 6) nullPtr

instance HasValue Node_const_Reader where
    type ValueTy Node_const_Reader = Value_Reader
    getValue (Node_const_Reader struct) = fmap Value_Reader $ L.getReaderStruct (L.getReaderPointerField struct 4) nullPtr

instance HasValue Node_const_Builder where
    type ValueTy Node_const_Builder = Value_Builder
    getValue (Node_const_Builder struct) = fmap Value_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 4) (L.StructSize 5 6) nullPtr


data Node_enum_Reader = Node_enum_Reader L.StructReader
data Node_enum_Builder = Node_enum_Builder L.StructBuilder

instance HasEnumerants Node_enum_Reader where
    type EnumerantsTy Node_enum_Reader = (L.ListReader Enumerant_Reader)
    getEnumerants (Node_enum_Reader struct) = L.getReaderList (L.getReaderPointerField struct 3) nullPtr

instance HasEnumerants Node_enum_Builder where
    type EnumerantsTy Node_enum_Builder = (L.ListBuilder Enumerant_Builder)
    getEnumerants (Node_enum_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 3) nullPtr


data Node_interface_Reader = Node_interface_Reader L.StructReader
data Node_interface_Builder = Node_interface_Builder L.StructBuilder

instance HasMethods Node_interface_Reader where
    type MethodsTy Node_interface_Reader = (L.ListReader Method_Reader)
    getMethods (Node_interface_Reader struct) = L.getReaderList (L.getReaderPointerField struct 3) nullPtr

instance HasMethods Node_interface_Builder where
    type MethodsTy Node_interface_Builder = (L.ListBuilder Method_Builder)
    getMethods (Node_interface_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 3) nullPtr

instance HasSuperclasses Node_interface_Reader where
    type SuperclassesTy Node_interface_Reader = (L.ListReader Superclass_Reader)
    getSuperclasses (Node_interface_Reader struct) = L.getReaderList (L.getReaderPointerField struct 4) nullPtr

instance HasSuperclasses Node_interface_Builder where
    type SuperclassesTy Node_interface_Builder = (L.ListBuilder Superclass_Builder)
    getSuperclasses (Node_interface_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 4) nullPtr


data Node_struct_Reader = Node_struct_Reader L.StructReader
data Node_struct_Builder = Node_struct_Builder L.StructBuilder

instance HasDataWordCount Node_struct_Reader where
    type DataWordCountTy Node_struct_Reader = Word16
    getDataWordCount (Node_struct_Reader struct) = L.getReaderNumericField struct 7

instance HasDataWordCount Node_struct_Builder where
    type DataWordCountTy Node_struct_Builder = Word16
    getDataWordCount (Node_struct_Builder struct) = L.getBuilderNumericField struct 7

instance HasPointerCount Node_struct_Reader where
    type PointerCountTy Node_struct_Reader = Word16
    getPointerCount (Node_struct_Reader struct) = L.getReaderNumericField struct 12

instance HasPointerCount Node_struct_Builder where
    type PointerCountTy Node_struct_Builder = Word16
    getPointerCount (Node_struct_Builder struct) = L.getBuilderNumericField struct 12

instance HasPreferredListEncoding Node_struct_Reader where
    type PreferredListEncodingTy Node_struct_Reader = ElementSize
    getPreferredListEncoding (Node_struct_Reader struct) = fmap (toEnum . fromIntegral) (L.getReaderNumericField struct 13 :: IO Word16)

instance HasPreferredListEncoding Node_struct_Builder where
    type PreferredListEncodingTy Node_struct_Builder = ElementSize
    getPreferredListEncoding (Node_struct_Builder struct) = fmap (toEnum . fromIntegral) (L.getBuilderNumericField struct 13 :: IO Word16)

instance HasIsGroup Node_struct_Reader where
    type IsGroupTy Node_struct_Reader = Bool
    getIsGroup (Node_struct_Reader struct) = L.getReaderBoolField struct 224

instance HasIsGroup Node_struct_Builder where
    type IsGroupTy Node_struct_Builder = Bool
    getIsGroup (Node_struct_Builder struct) = L.getBuilderBoolField struct 224

instance HasDiscriminantCount Node_struct_Reader where
    type DiscriminantCountTy Node_struct_Reader = Word16
    getDiscriminantCount (Node_struct_Reader struct) = L.getReaderNumericField struct 15

instance HasDiscriminantCount Node_struct_Builder where
    type DiscriminantCountTy Node_struct_Builder = Word16
    getDiscriminantCount (Node_struct_Builder struct) = L.getBuilderNumericField struct 15

instance HasDiscriminantOffset Node_struct_Reader where
    type DiscriminantOffsetTy Node_struct_Reader = Word32
    getDiscriminantOffset (Node_struct_Reader struct) = L.getReaderNumericField struct 8

instance HasDiscriminantOffset Node_struct_Builder where
    type DiscriminantOffsetTy Node_struct_Builder = Word32
    getDiscriminantOffset (Node_struct_Builder struct) = L.getBuilderNumericField struct 8

instance HasFields Node_struct_Reader where
    type FieldsTy Node_struct_Reader = (L.ListReader Field_Reader)
    getFields (Node_struct_Reader struct) = L.getReaderList (L.getReaderPointerField struct 3) nullPtr

instance HasFields Node_struct_Builder where
    type FieldsTy Node_struct_Builder = (L.ListBuilder Field_Builder)
    getFields (Node_struct_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 3) nullPtr


data Superclass_Reader = Superclass_Reader L.StructReader
data Superclass_Builder = Superclass_Builder L.StructBuilder

instance L.FromStructReader Superclass_Reader where
    fromStructReader = return . Superclass_Reader

instance L.ListElement Superclass_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Superclass_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Superclass_Reader where
    getReaderElement list index =
        fmap Superclass_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Superclass_Builder where
    getBuilderElement list index =
        fmap Superclass_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Superclass_Builder struct) =
        L.setBuilderElement (coerce list) index struct

instance HasId Superclass_Reader where
    type IdTy Superclass_Reader = Word64
    getId (Superclass_Reader struct) = L.getReaderNumericField struct 0

instance HasId Superclass_Builder where
    type IdTy Superclass_Builder = Word64
    getId (Superclass_Builder struct) = L.getBuilderNumericField struct 0

instance HasBrand Superclass_Reader where
    type BrandTy Superclass_Reader = Brand_Reader
    getBrand (Superclass_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance HasBrand Superclass_Builder where
    type BrandTy Superclass_Builder = Brand_Builder
    getBrand (Superclass_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 1 1) nullPtr


data Type_Reader = Type_Reader L.StructReader
data Type_Builder = Type_Builder L.StructBuilder

instance L.FromStructReader Type_Reader where
    fromStructReader = return . Type_Reader

instance L.ListElement Type_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Type_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Type_Reader where
    getReaderElement list index =
        fmap Type_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Type_Builder where
    getBuilderElement list index =
        fmap Type_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Type_Builder struct) =
        L.setBuilderElement (coerce list) index struct

data Type_Which_Reader
  = Type_NotInSchema_Reader_ Word16
  | Type_void_Reader_
  | Type_bool_Reader_
  | Type_int8_Reader_
  | Type_int16_Reader_
  | Type_int32_Reader_
  | Type_int64_Reader_
  | Type_uint8_Reader_
  | Type_uint16_Reader_
  | Type_uint32_Reader_
  | Type_uint64_Reader_
  | Type_float32_Reader_
  | Type_float64_Reader_
  | Type_text_Reader_
  | Type_data_Reader_
  | Type_list_Reader_ Type_list_Reader
  | Type_enum_Reader_ Type_enum_Reader
  | Type_struct_Reader_ Type_struct_Reader
  | Type_interface_Reader_ Type_interface_Reader
  | Type_anyPointer_Reader_ Type_anyPointer_Reader

data Type_Which_Builder
  = Type_NotInSchema_Builder_ Word16
  | Type_void_Builder_
  | Type_bool_Builder_
  | Type_int8_Builder_
  | Type_int16_Builder_
  | Type_int32_Builder_
  | Type_int64_Builder_
  | Type_uint8_Builder_
  | Type_uint16_Builder_
  | Type_uint32_Builder_
  | Type_uint64_Builder_
  | Type_float32_Builder_
  | Type_float64_Builder_
  | Type_text_Builder_
  | Type_data_Builder_
  | Type_list_Builder_ Type_list_Builder
  | Type_enum_Builder_ Type_enum_Builder
  | Type_struct_Builder_ Type_struct_Builder
  | Type_interface_Builder_ Type_interface_Builder
  | Type_anyPointer_Builder_ Type_anyPointer_Builder

instance L.Union Type_Reader where
    type UnionTy Type_Reader = Type_Which_Reader
    which (Type_Reader struct) = do
        d <- L.getReaderNumericField struct 0 :: IO Word16
        case d of
            0 -> return Type_void_Reader_
            1 -> return Type_bool_Reader_
            2 -> return Type_int8_Reader_
            3 -> return Type_int16_Reader_
            4 -> return Type_int32_Reader_
            5 -> return Type_int64_Reader_
            6 -> return Type_uint8_Reader_
            7 -> return Type_uint16_Reader_
            8 -> return Type_uint32_Reader_
            9 -> return Type_uint64_Reader_
            10 -> return Type_float32_Reader_
            11 -> return Type_float64_Reader_
            12 -> return Type_text_Reader_
            13 -> return Type_data_Reader_
            14 -> fmap Type_list_Reader_ $ return . Type_list_Reader $ struct
            15 -> fmap Type_enum_Reader_ $ return . Type_enum_Reader $ struct
            16 -> fmap Type_struct_Reader_ $ return . Type_struct_Reader $ struct
            17 -> fmap Type_interface_Reader_ $ return . Type_interface_Reader $ struct
            18 -> fmap Type_anyPointer_Reader_ $ return . Type_anyPointer_Reader $ struct
            _ -> return $ Type_NotInSchema_Reader_ d

instance L.Union Type_Builder where
    type UnionTy Type_Builder = Type_Which_Builder
    which (Type_Builder struct) = do
        d <- L.getBuilderNumericField struct 0 :: IO Word16
        case d of
            0 -> return Type_void_Builder_
            1 -> return Type_bool_Builder_
            2 -> return Type_int8_Builder_
            3 -> return Type_int16_Builder_
            4 -> return Type_int32_Builder_
            5 -> return Type_int64_Builder_
            6 -> return Type_uint8_Builder_
            7 -> return Type_uint16_Builder_
            8 -> return Type_uint32_Builder_
            9 -> return Type_uint64_Builder_
            10 -> return Type_float32_Builder_
            11 -> return Type_float64_Builder_
            12 -> return Type_text_Builder_
            13 -> return Type_data_Builder_
            14 -> fmap Type_list_Builder_ $ return . Type_list_Builder $ struct
            15 -> fmap Type_enum_Builder_ $ return . Type_enum_Builder $ struct
            16 -> fmap Type_struct_Builder_ $ return . Type_struct_Builder $ struct
            17 -> fmap Type_interface_Builder_ $ return . Type_interface_Builder $ struct
            18 -> fmap Type_anyPointer_Builder_ $ return . Type_anyPointer_Builder $ struct
            _ -> return $ Type_NotInSchema_Builder_ d


data Type_anyPointer_Reader = Type_anyPointer_Reader L.StructReader
data Type_anyPointer_Builder = Type_anyPointer_Builder L.StructBuilder

data Type_anyPointer_Which_Reader
  = Type_anyPointer_NotInSchema_Reader_ Word16
  | Type_anyPointer_unconstrained_Reader_
  | Type_anyPointer_parameter_Reader_ Type_anyPointer_parameter_Reader
  | Type_anyPointer_implicitMethodParameter_Reader_ Type_anyPointer_implicitMethodParameter_Reader

data Type_anyPointer_Which_Builder
  = Type_anyPointer_NotInSchema_Builder_ Word16
  | Type_anyPointer_unconstrained_Builder_
  | Type_anyPointer_parameter_Builder_ Type_anyPointer_parameter_Builder
  | Type_anyPointer_implicitMethodParameter_Builder_ Type_anyPointer_implicitMethodParameter_Builder

instance L.Union Type_anyPointer_Reader where
    type UnionTy Type_anyPointer_Reader = Type_anyPointer_Which_Reader
    which (Type_anyPointer_Reader struct) = do
        d <- L.getReaderNumericField struct 4 :: IO Word16
        case d of
            0 -> return Type_anyPointer_unconstrained_Reader_
            1 -> fmap Type_anyPointer_parameter_Reader_ $ return . Type_anyPointer_parameter_Reader $ struct
            2 -> fmap Type_anyPointer_implicitMethodParameter_Reader_ $ return . Type_anyPointer_implicitMethodParameter_Reader $ struct
            _ -> return $ Type_anyPointer_NotInSchema_Reader_ d

instance L.Union Type_anyPointer_Builder where
    type UnionTy Type_anyPointer_Builder = Type_anyPointer_Which_Builder
    which (Type_anyPointer_Builder struct) = do
        d <- L.getBuilderNumericField struct 4 :: IO Word16
        case d of
            0 -> return Type_anyPointer_unconstrained_Builder_
            1 -> fmap Type_anyPointer_parameter_Builder_ $ return . Type_anyPointer_parameter_Builder $ struct
            2 -> fmap Type_anyPointer_implicitMethodParameter_Builder_ $ return . Type_anyPointer_implicitMethodParameter_Builder $ struct
            _ -> return $ Type_anyPointer_NotInSchema_Builder_ d


data Type_anyPointer_implicitMethodParameter_Reader = Type_anyPointer_implicitMethodParameter_Reader L.StructReader
data Type_anyPointer_implicitMethodParameter_Builder = Type_anyPointer_implicitMethodParameter_Builder L.StructBuilder

instance HasParameterIndex Type_anyPointer_implicitMethodParameter_Reader where
    type ParameterIndexTy Type_anyPointer_implicitMethodParameter_Reader = Word16
    getParameterIndex (Type_anyPointer_implicitMethodParameter_Reader struct) = L.getReaderNumericField struct 5

instance HasParameterIndex Type_anyPointer_implicitMethodParameter_Builder where
    type ParameterIndexTy Type_anyPointer_implicitMethodParameter_Builder = Word16
    getParameterIndex (Type_anyPointer_implicitMethodParameter_Builder struct) = L.getBuilderNumericField struct 5


data Type_anyPointer_parameter_Reader = Type_anyPointer_parameter_Reader L.StructReader
data Type_anyPointer_parameter_Builder = Type_anyPointer_parameter_Builder L.StructBuilder

instance HasScopeId Type_anyPointer_parameter_Reader where
    type ScopeIdTy Type_anyPointer_parameter_Reader = Word64
    getScopeId (Type_anyPointer_parameter_Reader struct) = L.getReaderNumericField struct 2

instance HasScopeId Type_anyPointer_parameter_Builder where
    type ScopeIdTy Type_anyPointer_parameter_Builder = Word64
    getScopeId (Type_anyPointer_parameter_Builder struct) = L.getBuilderNumericField struct 2

instance HasParameterIndex Type_anyPointer_parameter_Reader where
    type ParameterIndexTy Type_anyPointer_parameter_Reader = Word16
    getParameterIndex (Type_anyPointer_parameter_Reader struct) = L.getReaderNumericField struct 5

instance HasParameterIndex Type_anyPointer_parameter_Builder where
    type ParameterIndexTy Type_anyPointer_parameter_Builder = Word16
    getParameterIndex (Type_anyPointer_parameter_Builder struct) = L.getBuilderNumericField struct 5


data Type_enum_Reader = Type_enum_Reader L.StructReader
data Type_enum_Builder = Type_enum_Builder L.StructBuilder

instance HasTypeId Type_enum_Reader where
    type TypeIdTy Type_enum_Reader = Word64
    getTypeId (Type_enum_Reader struct) = L.getReaderNumericField struct 1

instance HasTypeId Type_enum_Builder where
    type TypeIdTy Type_enum_Builder = Word64
    getTypeId (Type_enum_Builder struct) = L.getBuilderNumericField struct 1

instance HasBrand Type_enum_Reader where
    type BrandTy Type_enum_Reader = Brand_Reader
    getBrand (Type_enum_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance HasBrand Type_enum_Builder where
    type BrandTy Type_enum_Builder = Brand_Builder
    getBrand (Type_enum_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 3 1) nullPtr


data Type_interface_Reader = Type_interface_Reader L.StructReader
data Type_interface_Builder = Type_interface_Builder L.StructBuilder

instance HasTypeId Type_interface_Reader where
    type TypeIdTy Type_interface_Reader = Word64
    getTypeId (Type_interface_Reader struct) = L.getReaderNumericField struct 1

instance HasTypeId Type_interface_Builder where
    type TypeIdTy Type_interface_Builder = Word64
    getTypeId (Type_interface_Builder struct) = L.getBuilderNumericField struct 1

instance HasBrand Type_interface_Reader where
    type BrandTy Type_interface_Reader = Brand_Reader
    getBrand (Type_interface_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance HasBrand Type_interface_Builder where
    type BrandTy Type_interface_Builder = Brand_Builder
    getBrand (Type_interface_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 3 1) nullPtr


data Type_list_Reader = Type_list_Reader L.StructReader
data Type_list_Builder = Type_list_Builder L.StructBuilder

instance HasElementType Type_list_Reader where
    type ElementTypeTy Type_list_Reader = Type_Reader
    getElementType (Type_list_Reader struct) = fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance HasElementType Type_list_Builder where
    type ElementTypeTy Type_list_Builder = Type_Builder
    getElementType (Type_list_Builder struct) = fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 3 1) nullPtr


data Type_struct_Reader = Type_struct_Reader L.StructReader
data Type_struct_Builder = Type_struct_Builder L.StructBuilder

instance HasTypeId Type_struct_Reader where
    type TypeIdTy Type_struct_Reader = Word64
    getTypeId (Type_struct_Reader struct) = L.getReaderNumericField struct 1

instance HasTypeId Type_struct_Builder where
    type TypeIdTy Type_struct_Builder = Word64
    getTypeId (Type_struct_Builder struct) = L.getBuilderNumericField struct 1

instance HasBrand Type_struct_Reader where
    type BrandTy Type_struct_Reader = Brand_Reader
    getBrand (Type_struct_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance HasBrand Type_struct_Builder where
    type BrandTy Type_struct_Builder = Brand_Builder
    getBrand (Type_struct_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 3 1) nullPtr


data Value_Reader = Value_Reader L.StructReader
data Value_Builder = Value_Builder L.StructBuilder

instance L.FromStructReader Value_Reader where
    fromStructReader = return . Value_Reader

instance L.ListElement Value_Reader where
    elementSize _ = L.SzInlineComposite

instance L.ListElement Value_Builder where
    elementSize _ = L.SzInlineComposite

instance L.ListReaderElement Value_Reader where
    getReaderElement list index =
        fmap Value_Reader $ L.getReaderElement (coerce list) index

instance L.ListBuilderElement Value_Builder where
    getBuilderElement list index =
        fmap Value_Builder $ L.getBuilderElement (coerce list) index
    setBuilderElement list index (Value_Builder struct) =
        L.setBuilderElement (coerce list) index struct

data Value_Which_Reader
  = Value_NotInSchema_Reader_ Word16
  | Value_void_Reader_
  | Value_bool_Reader_ Bool
  | Value_int8_Reader_ Int8
  | Value_int16_Reader_ Int16
  | Value_int32_Reader_ Int32
  | Value_int64_Reader_ Int64
  | Value_uint8_Reader_ Word8
  | Value_uint16_Reader_ Word16
  | Value_uint32_Reader_ Word32
  | Value_uint64_Reader_ Word64
  | Value_float32_Reader_ Float
  | Value_float64_Reader_ Double
  | Value_text_Reader_ L.TextReader
  | Value_data_Reader_ L.DataReader
  | Value_list_Reader_ L.PointerReader
  | Value_enum_Reader_ Word16
  | Value_struct_Reader_ L.PointerReader
  | Value_interface_Reader_
  | Value_anyPointer_Reader_ L.PointerReader

data Value_Which_Builder
  = Value_NotInSchema_Builder_ Word16
  | Value_void_Builder_
  | Value_bool_Builder_ Bool
  | Value_int8_Builder_ Int8
  | Value_int16_Builder_ Int16
  | Value_int32_Builder_ Int32
  | Value_int64_Builder_ Int64
  | Value_uint8_Builder_ Word8
  | Value_uint16_Builder_ Word16
  | Value_uint32_Builder_ Word32
  | Value_uint64_Builder_ Word64
  | Value_float32_Builder_ Float
  | Value_float64_Builder_ Double
  | Value_text_Builder_ L.TextBuilder
  | Value_data_Builder_ L.DataBuilder
  | Value_list_Builder_ L.PointerBuilder
  | Value_enum_Builder_ Word16
  | Value_struct_Builder_ L.PointerBuilder
  | Value_interface_Builder_
  | Value_anyPointer_Builder_ L.PointerBuilder

instance L.Union Value_Reader where
    type UnionTy Value_Reader = Value_Which_Reader
    which (Value_Reader struct) = do
        d <- L.getReaderNumericField struct 0 :: IO Word16
        case d of
            0 -> return Value_void_Reader_
            1 -> fmap Value_bool_Reader_ $ L.getReaderBoolField struct 16
            2 -> fmap Value_int8_Reader_ $ L.getReaderNumericField struct 2
            3 -> fmap Value_int16_Reader_ $ L.getReaderNumericField struct 1
            4 -> fmap Value_int32_Reader_ $ L.getReaderNumericField struct 1
            5 -> fmap Value_int64_Reader_ $ L.getReaderNumericField struct 1
            6 -> fmap Value_uint8_Reader_ $ L.getReaderNumericField struct 2
            7 -> fmap Value_uint16_Reader_ $ L.getReaderNumericField struct 1
            8 -> fmap Value_uint32_Reader_ $ L.getReaderNumericField struct 1
            9 -> fmap Value_uint64_Reader_ $ L.getReaderNumericField struct 1
            10 -> fmap Value_float32_Reader_ $ L.getReaderNumericField struct 1
            11 -> fmap Value_float64_Reader_ $ L.getReaderNumericField struct 1
            12 -> fmap Value_text_Reader_ $ L.getReaderText (L.getReaderPointerField struct 0) emptyString
            13 -> fmap Value_data_Reader_ $ L.getReaderData (L.getReaderPointerField struct 0) ""
            14 -> fmap Value_list_Reader_ $ return $ L.getReaderPointerField struct 0
            15 -> fmap Value_enum_Reader_ $ L.getReaderNumericField struct 1
            16 -> fmap Value_struct_Reader_ $ return $ L.getReaderPointerField struct 0
            17 -> return Value_interface_Reader_
            18 -> fmap Value_anyPointer_Reader_ $ return $ L.getReaderPointerField struct 0
            _ -> return $ Value_NotInSchema_Reader_ d

instance L.Union Value_Builder where
    type UnionTy Value_Builder = Value_Which_Builder
    which (Value_Builder struct) = do
        d <- L.getBuilderNumericField struct 0 :: IO Word16
        case d of
            0 -> return Value_void_Builder_
            1 -> fmap Value_bool_Builder_ $ L.getBuilderBoolField struct 16
            2 -> fmap Value_int8_Builder_ $ L.getBuilderNumericField struct 2
            3 -> fmap Value_int16_Builder_ $ L.getBuilderNumericField struct 1
            4 -> fmap Value_int32_Builder_ $ L.getBuilderNumericField struct 1
            5 -> fmap Value_int64_Builder_ $ L.getBuilderNumericField struct 1
            6 -> fmap Value_uint8_Builder_ $ L.getBuilderNumericField struct 2
            7 -> fmap Value_uint16_Builder_ $ L.getBuilderNumericField struct 1
            8 -> fmap Value_uint32_Builder_ $ L.getBuilderNumericField struct 1
            9 -> fmap Value_uint64_Builder_ $ L.getBuilderNumericField struct 1
            10 -> fmap Value_float32_Builder_ $ L.getBuilderNumericField struct 1
            11 -> fmap Value_float64_Builder_ $ L.getBuilderNumericField struct 1
            12 -> fmap Value_text_Builder_ $ L.getBuilderText (L.getBuilderPointerField struct 0) emptyString
            13 -> fmap Value_data_Builder_ $ L.getBuilderData (L.getBuilderPointerField struct 0) ""
            14 -> fmap Value_list_Builder_ $ return $ L.getBuilderPointerField struct 0
            15 -> fmap Value_enum_Builder_ $ L.getBuilderNumericField struct 1
            16 -> fmap Value_struct_Builder_ $ return $ L.getBuilderPointerField struct 0
            17 -> return Value_interface_Builder_
            18 -> fmap Value_anyPointer_Builder_ $ return $ L.getBuilderPointerField struct 0
            _ -> return $ Value_NotInSchema_Builder_ d



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

instance L.ListReaderElement ElementSize where
    getReaderElement list index =
        fmap (toEnum . fromIntegral) (L.getReaderElement (coerce list) index :: IO Word16)

instance L.ListBuilderElement ElementSize where
    getBuilderElement list index =
        fmap (toEnum . fromIntegral) (L.getBuilderElement (coerce list) index :: IO Word16)
    setBuilderElement list index val =
        L.setBuilderElement (coerce list) index (fromIntegral . fromEnum $ val :: Word16)


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





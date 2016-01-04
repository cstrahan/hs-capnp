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

instance Get_Id Annotation_Reader where
    type Get_Id_Ty Annotation_Reader = Word64
    getId (Annotation_Reader struct) = L.getReaderNumericField struct 0

instance Get_Id Annotation_Builder where
    type Get_Id_Ty Annotation_Builder = Word64
    getId (Annotation_Builder struct) = L.getBuilderNumericField struct 0

instance Get_Value Annotation_Reader where
    type Get_Value_Ty Annotation_Reader = Value_Reader
    getValue (Annotation_Reader struct) = fmap Value_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance Get_Value Annotation_Builder where
    type Get_Value_Ty Annotation_Builder = Value_Builder
    getValue (Annotation_Builder struct) = fmap Value_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 1 2) nullPtr

instance Get_Brand Annotation_Reader where
    type Get_Brand_Ty Annotation_Reader = Brand_Reader
    getBrand (Annotation_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 1) nullPtr

instance Get_Brand Annotation_Builder where
    type Get_Brand_Ty Annotation_Builder = Brand_Builder
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

instance Get_Scopes Brand_Reader where
    type Get_Scopes_Ty Brand_Reader = (L.ListReader Brand_Scope_Reader)
    getScopes (Brand_Reader struct) = L.getReaderList (L.getReaderPointerField struct 0) nullPtr

instance Get_Scopes Brand_Builder where
    type Get_Scopes_Ty Brand_Builder = (L.ListBuilder Brand_Scope_Builder)
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

instance Get_ScopeId Brand_Scope_Reader where
    type Get_ScopeId_Ty Brand_Scope_Reader = Word64
    getScopeId (Brand_Scope_Reader struct) = L.getReaderNumericField struct 0

instance Get_ScopeId Brand_Scope_Builder where
    type Get_ScopeId_Ty Brand_Scope_Builder = Word64
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

instance Get_Nodes CodeGeneratorRequest_Reader where
    type Get_Nodes_Ty CodeGeneratorRequest_Reader = (L.ListReader Node_Reader)
    getNodes (CodeGeneratorRequest_Reader struct) = L.getReaderList (L.getReaderPointerField struct 0) nullPtr

instance Get_Nodes CodeGeneratorRequest_Builder where
    type Get_Nodes_Ty CodeGeneratorRequest_Builder = (L.ListBuilder Node_Builder)
    getNodes (CodeGeneratorRequest_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 0) nullPtr

instance Get_RequestedFiles CodeGeneratorRequest_Reader where
    type Get_RequestedFiles_Ty CodeGeneratorRequest_Reader = (L.ListReader CodeGeneratorRequest_RequestedFile_Reader)
    getRequestedFiles (CodeGeneratorRequest_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance Get_RequestedFiles CodeGeneratorRequest_Builder where
    type Get_RequestedFiles_Ty CodeGeneratorRequest_Builder = (L.ListBuilder CodeGeneratorRequest_RequestedFile_Builder)
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

instance Get_Id CodeGeneratorRequest_RequestedFile_Reader where
    type Get_Id_Ty CodeGeneratorRequest_RequestedFile_Reader = Word64
    getId (CodeGeneratorRequest_RequestedFile_Reader struct) = L.getReaderNumericField struct 0

instance Get_Id CodeGeneratorRequest_RequestedFile_Builder where
    type Get_Id_Ty CodeGeneratorRequest_RequestedFile_Builder = Word64
    getId (CodeGeneratorRequest_RequestedFile_Builder struct) = L.getBuilderNumericField struct 0

instance Get_Filename CodeGeneratorRequest_RequestedFile_Reader where
    type Get_Filename_Ty CodeGeneratorRequest_RequestedFile_Reader = L.TextReader
    getFilename (CodeGeneratorRequest_RequestedFile_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance Get_Filename CodeGeneratorRequest_RequestedFile_Builder where
    type Get_Filename_Ty CodeGeneratorRequest_RequestedFile_Builder = L.TextBuilder
    getFilename (CodeGeneratorRequest_RequestedFile_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance Get_Imports CodeGeneratorRequest_RequestedFile_Reader where
    type Get_Imports_Ty CodeGeneratorRequest_RequestedFile_Reader = (L.ListReader CodeGeneratorRequest_RequestedFile_Import_Reader)
    getImports (CodeGeneratorRequest_RequestedFile_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance Get_Imports CodeGeneratorRequest_RequestedFile_Builder where
    type Get_Imports_Ty CodeGeneratorRequest_RequestedFile_Builder = (L.ListBuilder CodeGeneratorRequest_RequestedFile_Import_Builder)
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

instance Get_Id CodeGeneratorRequest_RequestedFile_Import_Reader where
    type Get_Id_Ty CodeGeneratorRequest_RequestedFile_Import_Reader = Word64
    getId (CodeGeneratorRequest_RequestedFile_Import_Reader struct) = L.getReaderNumericField struct 0

instance Get_Id CodeGeneratorRequest_RequestedFile_Import_Builder where
    type Get_Id_Ty CodeGeneratorRequest_RequestedFile_Import_Builder = Word64
    getId (CodeGeneratorRequest_RequestedFile_Import_Builder struct) = L.getBuilderNumericField struct 0

instance Get_Name CodeGeneratorRequest_RequestedFile_Import_Reader where
    type Get_Name_Ty CodeGeneratorRequest_RequestedFile_Import_Reader = L.TextReader
    getName (CodeGeneratorRequest_RequestedFile_Import_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance Get_Name CodeGeneratorRequest_RequestedFile_Import_Builder where
    type Get_Name_Ty CodeGeneratorRequest_RequestedFile_Import_Builder = L.TextBuilder
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

instance Get_Name Enumerant_Reader where
    type Get_Name_Ty Enumerant_Reader = L.TextReader
    getName (Enumerant_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance Get_Name Enumerant_Builder where
    type Get_Name_Ty Enumerant_Builder = L.TextBuilder
    getName (Enumerant_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance Get_CodeOrder Enumerant_Reader where
    type Get_CodeOrder_Ty Enumerant_Reader = Word16
    getCodeOrder (Enumerant_Reader struct) = L.getReaderNumericField struct 0

instance Get_CodeOrder Enumerant_Builder where
    type Get_CodeOrder_Ty Enumerant_Builder = Word16
    getCodeOrder (Enumerant_Builder struct) = L.getBuilderNumericField struct 0

instance Get_Annotations Enumerant_Reader where
    type Get_Annotations_Ty Enumerant_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Enumerant_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance Get_Annotations Enumerant_Builder where
    type Get_Annotations_Ty Enumerant_Builder = (L.ListBuilder Annotation_Builder)
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

instance Get_Name Field_Reader where
    type Get_Name_Ty Field_Reader = L.TextReader
    getName (Field_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance Get_Name Field_Builder where
    type Get_Name_Ty Field_Builder = L.TextBuilder
    getName (Field_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance Get_CodeOrder Field_Reader where
    type Get_CodeOrder_Ty Field_Reader = Word16
    getCodeOrder (Field_Reader struct) = L.getReaderNumericField struct 0

instance Get_CodeOrder Field_Builder where
    type Get_CodeOrder_Ty Field_Builder = Word16
    getCodeOrder (Field_Builder struct) = L.getBuilderNumericField struct 0

instance Get_Annotations Field_Reader where
    type Get_Annotations_Ty Field_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Field_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance Get_Annotations Field_Builder where
    type Get_Annotations_Ty Field_Builder = (L.ListBuilder Annotation_Builder)
    getAnnotations (Field_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr

instance Get_DiscriminantValue Field_Reader where
    type Get_DiscriminantValue_Ty Field_Reader = Word16
    getDiscriminantValue (Field_Reader struct) = L.getReaderNumericFieldMasked struct 1 65535

instance Get_DiscriminantValue Field_Builder where
    type Get_DiscriminantValue_Ty Field_Builder = Word16
    getDiscriminantValue (Field_Builder struct) = L.getBuilderNumericFieldMasked struct 1 65535

instance Get_Ordinal Field_Reader where
    type Get_Ordinal_Ty Field_Reader = Field_ordinal_Reader
    getOrdinal (Field_Reader struct) = return . Field_ordinal_Reader $ struct

instance Get_Ordinal Field_Builder where
    type Get_Ordinal_Ty Field_Builder = Field_ordinal_Builder
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

instance Get_TypeId Field_group_Reader where
    type Get_TypeId_Ty Field_group_Reader = Word64
    getTypeId (Field_group_Reader struct) = L.getReaderNumericField struct 2

instance Get_TypeId Field_group_Builder where
    type Get_TypeId_Ty Field_group_Builder = Word64
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

instance Get_Offset Field_slot_Reader where
    type Get_Offset_Ty Field_slot_Reader = Word32
    getOffset (Field_slot_Reader struct) = L.getReaderNumericField struct 1

instance Get_Offset Field_slot_Builder where
    type Get_Offset_Ty Field_slot_Builder = Word32
    getOffset (Field_slot_Builder struct) = L.getBuilderNumericField struct 1

instance Get_Type Field_slot_Reader where
    type Get_Type_Ty Field_slot_Reader = Type_Reader
    getType (Field_slot_Reader struct) = fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 2) nullPtr

instance Get_Type Field_slot_Builder where
    type Get_Type_Ty Field_slot_Builder = Type_Builder
    getType (Field_slot_Builder struct) = fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 2) (L.StructSize 3 4) nullPtr

instance Get_DefaultValue Field_slot_Reader where
    type Get_DefaultValue_Ty Field_slot_Reader = Value_Reader
    getDefaultValue (Field_slot_Reader struct) = fmap Value_Reader $ L.getReaderStruct (L.getReaderPointerField struct 3) nullPtr

instance Get_DefaultValue Field_slot_Builder where
    type Get_DefaultValue_Ty Field_slot_Builder = Value_Builder
    getDefaultValue (Field_slot_Builder struct) = fmap Value_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 3) (L.StructSize 3 4) nullPtr

instance Get_HadExplicitDefault Field_slot_Reader where
    type Get_HadExplicitDefault_Ty Field_slot_Reader = Bool
    getHadExplicitDefault (Field_slot_Reader struct) = L.getReaderBoolField struct 128

instance Get_HadExplicitDefault Field_slot_Builder where
    type Get_HadExplicitDefault_Ty Field_slot_Builder = Bool
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

instance Get_Name Method_Reader where
    type Get_Name_Ty Method_Reader = L.TextReader
    getName (Method_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance Get_Name Method_Builder where
    type Get_Name_Ty Method_Builder = L.TextBuilder
    getName (Method_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance Get_CodeOrder Method_Reader where
    type Get_CodeOrder_Ty Method_Reader = Word16
    getCodeOrder (Method_Reader struct) = L.getReaderNumericField struct 0

instance Get_CodeOrder Method_Builder where
    type Get_CodeOrder_Ty Method_Builder = Word16
    getCodeOrder (Method_Builder struct) = L.getBuilderNumericField struct 0

instance Get_ParamStructType Method_Reader where
    type Get_ParamStructType_Ty Method_Reader = Word64
    getParamStructType (Method_Reader struct) = L.getReaderNumericField struct 1

instance Get_ParamStructType Method_Builder where
    type Get_ParamStructType_Ty Method_Builder = Word64
    getParamStructType (Method_Builder struct) = L.getBuilderNumericField struct 1

instance Get_ResultStructType Method_Reader where
    type Get_ResultStructType_Ty Method_Reader = Word64
    getResultStructType (Method_Reader struct) = L.getReaderNumericField struct 2

instance Get_ResultStructType Method_Builder where
    type Get_ResultStructType_Ty Method_Builder = Word64
    getResultStructType (Method_Builder struct) = L.getBuilderNumericField struct 2

instance Get_Annotations Method_Reader where
    type Get_Annotations_Ty Method_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Method_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance Get_Annotations Method_Builder where
    type Get_Annotations_Ty Method_Builder = (L.ListBuilder Annotation_Builder)
    getAnnotations (Method_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr

instance Get_ParamBrand Method_Reader where
    type Get_ParamBrand_Ty Method_Reader = Brand_Reader
    getParamBrand (Method_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 2) nullPtr

instance Get_ParamBrand Method_Builder where
    type Get_ParamBrand_Ty Method_Builder = Brand_Builder
    getParamBrand (Method_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 2) (L.StructSize 3 5) nullPtr

instance Get_ResultBrand Method_Reader where
    type Get_ResultBrand_Ty Method_Reader = Brand_Reader
    getResultBrand (Method_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 3) nullPtr

instance Get_ResultBrand Method_Builder where
    type Get_ResultBrand_Ty Method_Builder = Brand_Builder
    getResultBrand (Method_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 3) (L.StructSize 3 5) nullPtr

instance Get_ImplicitParameters Method_Reader where
    type Get_ImplicitParameters_Ty Method_Reader = (L.ListReader Node_Parameter_Reader)
    getImplicitParameters (Method_Reader struct) = L.getReaderList (L.getReaderPointerField struct 4) nullPtr

instance Get_ImplicitParameters Method_Builder where
    type Get_ImplicitParameters_Ty Method_Builder = (L.ListBuilder Node_Parameter_Builder)
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

instance Get_Id Node_Reader where
    type Get_Id_Ty Node_Reader = Word64
    getId (Node_Reader struct) = L.getReaderNumericField struct 0

instance Get_Id Node_Builder where
    type Get_Id_Ty Node_Builder = Word64
    getId (Node_Builder struct) = L.getBuilderNumericField struct 0

instance Get_DisplayName Node_Reader where
    type Get_DisplayName_Ty Node_Reader = L.TextReader
    getDisplayName (Node_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance Get_DisplayName Node_Builder where
    type Get_DisplayName_Ty Node_Builder = L.TextBuilder
    getDisplayName (Node_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance Get_DisplayNamePrefixLength Node_Reader where
    type Get_DisplayNamePrefixLength_Ty Node_Reader = Word32
    getDisplayNamePrefixLength (Node_Reader struct) = L.getReaderNumericField struct 2

instance Get_DisplayNamePrefixLength Node_Builder where
    type Get_DisplayNamePrefixLength_Ty Node_Builder = Word32
    getDisplayNamePrefixLength (Node_Builder struct) = L.getBuilderNumericField struct 2

instance Get_ScopeId Node_Reader where
    type Get_ScopeId_Ty Node_Reader = Word64
    getScopeId (Node_Reader struct) = L.getReaderNumericField struct 2

instance Get_ScopeId Node_Builder where
    type Get_ScopeId_Ty Node_Builder = Word64
    getScopeId (Node_Builder struct) = L.getBuilderNumericField struct 2

instance Get_NestedNodes Node_Reader where
    type Get_NestedNodes_Ty Node_Reader = (L.ListReader Node_NestedNode_Reader)
    getNestedNodes (Node_Reader struct) = L.getReaderList (L.getReaderPointerField struct 1) nullPtr

instance Get_NestedNodes Node_Builder where
    type Get_NestedNodes_Ty Node_Builder = (L.ListBuilder Node_NestedNode_Builder)
    getNestedNodes (Node_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 1) nullPtr

instance Get_Annotations Node_Reader where
    type Get_Annotations_Ty Node_Reader = (L.ListReader Annotation_Reader)
    getAnnotations (Node_Reader struct) = L.getReaderList (L.getReaderPointerField struct 2) nullPtr

instance Get_Annotations Node_Builder where
    type Get_Annotations_Ty Node_Builder = (L.ListBuilder Annotation_Builder)
    getAnnotations (Node_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 2) nullPtr

instance Get_Parameters Node_Reader where
    type Get_Parameters_Ty Node_Reader = (L.ListReader Node_Parameter_Reader)
    getParameters (Node_Reader struct) = L.getReaderList (L.getReaderPointerField struct 5) nullPtr

instance Get_Parameters Node_Builder where
    type Get_Parameters_Ty Node_Builder = (L.ListBuilder Node_Parameter_Builder)
    getParameters (Node_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 5) nullPtr

instance Get_IsGeneric Node_Reader where
    type Get_IsGeneric_Ty Node_Reader = Bool
    getIsGeneric (Node_Reader struct) = L.getReaderBoolField struct 288

instance Get_IsGeneric Node_Builder where
    type Get_IsGeneric_Ty Node_Builder = Bool
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

instance Get_Name Node_NestedNode_Reader where
    type Get_Name_Ty Node_NestedNode_Reader = L.TextReader
    getName (Node_NestedNode_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance Get_Name Node_NestedNode_Builder where
    type Get_Name_Ty Node_NestedNode_Builder = L.TextBuilder
    getName (Node_NestedNode_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString

instance Get_Id Node_NestedNode_Reader where
    type Get_Id_Ty Node_NestedNode_Reader = Word64
    getId (Node_NestedNode_Reader struct) = L.getReaderNumericField struct 0

instance Get_Id Node_NestedNode_Builder where
    type Get_Id_Ty Node_NestedNode_Builder = Word64
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

instance Get_Name Node_Parameter_Reader where
    type Get_Name_Ty Node_Parameter_Reader = L.TextReader
    getName (Node_Parameter_Reader struct) = L.getReaderText (L.getReaderPointerField struct 0) emptyString

instance Get_Name Node_Parameter_Builder where
    type Get_Name_Ty Node_Parameter_Builder = L.TextBuilder
    getName (Node_Parameter_Builder struct) = L.getBuilderText (L.getBuilderPointerField struct 0) emptyString


data Node_annotation_Reader = Node_annotation_Reader L.StructReader
data Node_annotation_Builder = Node_annotation_Builder L.StructBuilder

instance Get_Type Node_annotation_Reader where
    type Get_Type_Ty Node_annotation_Reader = Type_Reader
    getType (Node_annotation_Reader struct) = fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 3) nullPtr

instance Get_Type Node_annotation_Builder where
    type Get_Type_Ty Node_annotation_Builder = Type_Builder
    getType (Node_annotation_Builder struct) = fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 3) (L.StructSize 5 6) nullPtr

instance Get_TargetsFile Node_annotation_Reader where
    type Get_TargetsFile_Ty Node_annotation_Reader = Bool
    getTargetsFile (Node_annotation_Reader struct) = L.getReaderBoolField struct 112

instance Get_TargetsFile Node_annotation_Builder where
    type Get_TargetsFile_Ty Node_annotation_Builder = Bool
    getTargetsFile (Node_annotation_Builder struct) = L.getBuilderBoolField struct 112

instance Get_TargetsConst Node_annotation_Reader where
    type Get_TargetsConst_Ty Node_annotation_Reader = Bool
    getTargetsConst (Node_annotation_Reader struct) = L.getReaderBoolField struct 113

instance Get_TargetsConst Node_annotation_Builder where
    type Get_TargetsConst_Ty Node_annotation_Builder = Bool
    getTargetsConst (Node_annotation_Builder struct) = L.getBuilderBoolField struct 113

instance Get_TargetsEnum Node_annotation_Reader where
    type Get_TargetsEnum_Ty Node_annotation_Reader = Bool
    getTargetsEnum (Node_annotation_Reader struct) = L.getReaderBoolField struct 114

instance Get_TargetsEnum Node_annotation_Builder where
    type Get_TargetsEnum_Ty Node_annotation_Builder = Bool
    getTargetsEnum (Node_annotation_Builder struct) = L.getBuilderBoolField struct 114

instance Get_TargetsEnumerant Node_annotation_Reader where
    type Get_TargetsEnumerant_Ty Node_annotation_Reader = Bool
    getTargetsEnumerant (Node_annotation_Reader struct) = L.getReaderBoolField struct 115

instance Get_TargetsEnumerant Node_annotation_Builder where
    type Get_TargetsEnumerant_Ty Node_annotation_Builder = Bool
    getTargetsEnumerant (Node_annotation_Builder struct) = L.getBuilderBoolField struct 115

instance Get_TargetsStruct Node_annotation_Reader where
    type Get_TargetsStruct_Ty Node_annotation_Reader = Bool
    getTargetsStruct (Node_annotation_Reader struct) = L.getReaderBoolField struct 116

instance Get_TargetsStruct Node_annotation_Builder where
    type Get_TargetsStruct_Ty Node_annotation_Builder = Bool
    getTargetsStruct (Node_annotation_Builder struct) = L.getBuilderBoolField struct 116

instance Get_TargetsField Node_annotation_Reader where
    type Get_TargetsField_Ty Node_annotation_Reader = Bool
    getTargetsField (Node_annotation_Reader struct) = L.getReaderBoolField struct 117

instance Get_TargetsField Node_annotation_Builder where
    type Get_TargetsField_Ty Node_annotation_Builder = Bool
    getTargetsField (Node_annotation_Builder struct) = L.getBuilderBoolField struct 117

instance Get_TargetsUnion Node_annotation_Reader where
    type Get_TargetsUnion_Ty Node_annotation_Reader = Bool
    getTargetsUnion (Node_annotation_Reader struct) = L.getReaderBoolField struct 118

instance Get_TargetsUnion Node_annotation_Builder where
    type Get_TargetsUnion_Ty Node_annotation_Builder = Bool
    getTargetsUnion (Node_annotation_Builder struct) = L.getBuilderBoolField struct 118

instance Get_TargetsGroup Node_annotation_Reader where
    type Get_TargetsGroup_Ty Node_annotation_Reader = Bool
    getTargetsGroup (Node_annotation_Reader struct) = L.getReaderBoolField struct 119

instance Get_TargetsGroup Node_annotation_Builder where
    type Get_TargetsGroup_Ty Node_annotation_Builder = Bool
    getTargetsGroup (Node_annotation_Builder struct) = L.getBuilderBoolField struct 119

instance Get_TargetsInterface Node_annotation_Reader where
    type Get_TargetsInterface_Ty Node_annotation_Reader = Bool
    getTargetsInterface (Node_annotation_Reader struct) = L.getReaderBoolField struct 120

instance Get_TargetsInterface Node_annotation_Builder where
    type Get_TargetsInterface_Ty Node_annotation_Builder = Bool
    getTargetsInterface (Node_annotation_Builder struct) = L.getBuilderBoolField struct 120

instance Get_TargetsMethod Node_annotation_Reader where
    type Get_TargetsMethod_Ty Node_annotation_Reader = Bool
    getTargetsMethod (Node_annotation_Reader struct) = L.getReaderBoolField struct 121

instance Get_TargetsMethod Node_annotation_Builder where
    type Get_TargetsMethod_Ty Node_annotation_Builder = Bool
    getTargetsMethod (Node_annotation_Builder struct) = L.getBuilderBoolField struct 121

instance Get_TargetsParam Node_annotation_Reader where
    type Get_TargetsParam_Ty Node_annotation_Reader = Bool
    getTargetsParam (Node_annotation_Reader struct) = L.getReaderBoolField struct 122

instance Get_TargetsParam Node_annotation_Builder where
    type Get_TargetsParam_Ty Node_annotation_Builder = Bool
    getTargetsParam (Node_annotation_Builder struct) = L.getBuilderBoolField struct 122

instance Get_TargetsAnnotation Node_annotation_Reader where
    type Get_TargetsAnnotation_Ty Node_annotation_Reader = Bool
    getTargetsAnnotation (Node_annotation_Reader struct) = L.getReaderBoolField struct 123

instance Get_TargetsAnnotation Node_annotation_Builder where
    type Get_TargetsAnnotation_Ty Node_annotation_Builder = Bool
    getTargetsAnnotation (Node_annotation_Builder struct) = L.getBuilderBoolField struct 123


data Node_const_Reader = Node_const_Reader L.StructReader
data Node_const_Builder = Node_const_Builder L.StructBuilder

instance Get_Type Node_const_Reader where
    type Get_Type_Ty Node_const_Reader = Type_Reader
    getType (Node_const_Reader struct) = fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 3) nullPtr

instance Get_Type Node_const_Builder where
    type Get_Type_Ty Node_const_Builder = Type_Builder
    getType (Node_const_Builder struct) = fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 3) (L.StructSize 5 6) nullPtr

instance Get_Value Node_const_Reader where
    type Get_Value_Ty Node_const_Reader = Value_Reader
    getValue (Node_const_Reader struct) = fmap Value_Reader $ L.getReaderStruct (L.getReaderPointerField struct 4) nullPtr

instance Get_Value Node_const_Builder where
    type Get_Value_Ty Node_const_Builder = Value_Builder
    getValue (Node_const_Builder struct) = fmap Value_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 4) (L.StructSize 5 6) nullPtr


data Node_enum_Reader = Node_enum_Reader L.StructReader
data Node_enum_Builder = Node_enum_Builder L.StructBuilder

instance Get_Enumerants Node_enum_Reader where
    type Get_Enumerants_Ty Node_enum_Reader = (L.ListReader Enumerant_Reader)
    getEnumerants (Node_enum_Reader struct) = L.getReaderList (L.getReaderPointerField struct 3) nullPtr

instance Get_Enumerants Node_enum_Builder where
    type Get_Enumerants_Ty Node_enum_Builder = (L.ListBuilder Enumerant_Builder)
    getEnumerants (Node_enum_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 3) nullPtr


data Node_interface_Reader = Node_interface_Reader L.StructReader
data Node_interface_Builder = Node_interface_Builder L.StructBuilder

instance Get_Methods Node_interface_Reader where
    type Get_Methods_Ty Node_interface_Reader = (L.ListReader Method_Reader)
    getMethods (Node_interface_Reader struct) = L.getReaderList (L.getReaderPointerField struct 3) nullPtr

instance Get_Methods Node_interface_Builder where
    type Get_Methods_Ty Node_interface_Builder = (L.ListBuilder Method_Builder)
    getMethods (Node_interface_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 3) nullPtr

instance Get_Superclasses Node_interface_Reader where
    type Get_Superclasses_Ty Node_interface_Reader = (L.ListReader Superclass_Reader)
    getSuperclasses (Node_interface_Reader struct) = L.getReaderList (L.getReaderPointerField struct 4) nullPtr

instance Get_Superclasses Node_interface_Builder where
    type Get_Superclasses_Ty Node_interface_Builder = (L.ListBuilder Superclass_Builder)
    getSuperclasses (Node_interface_Builder struct) = L.getBuilderList (L.getBuilderPointerField struct 4) nullPtr


data Node_struct_Reader = Node_struct_Reader L.StructReader
data Node_struct_Builder = Node_struct_Builder L.StructBuilder

instance Get_DataWordCount Node_struct_Reader where
    type Get_DataWordCount_Ty Node_struct_Reader = Word16
    getDataWordCount (Node_struct_Reader struct) = L.getReaderNumericField struct 7

instance Get_DataWordCount Node_struct_Builder where
    type Get_DataWordCount_Ty Node_struct_Builder = Word16
    getDataWordCount (Node_struct_Builder struct) = L.getBuilderNumericField struct 7

instance Get_PointerCount Node_struct_Reader where
    type Get_PointerCount_Ty Node_struct_Reader = Word16
    getPointerCount (Node_struct_Reader struct) = L.getReaderNumericField struct 12

instance Get_PointerCount Node_struct_Builder where
    type Get_PointerCount_Ty Node_struct_Builder = Word16
    getPointerCount (Node_struct_Builder struct) = L.getBuilderNumericField struct 12

instance Get_PreferredListEncoding Node_struct_Reader where
    type Get_PreferredListEncoding_Ty Node_struct_Reader = ElementSize
    getPreferredListEncoding (Node_struct_Reader struct) = fmap (toEnum . fromIntegral) (L.getReaderNumericField struct 13 :: IO Word16)

instance Get_PreferredListEncoding Node_struct_Builder where
    type Get_PreferredListEncoding_Ty Node_struct_Builder = ElementSize
    getPreferredListEncoding (Node_struct_Builder struct) = fmap (toEnum . fromIntegral) (L.getBuilderNumericField struct 13 :: IO Word16)

instance Get_IsGroup Node_struct_Reader where
    type Get_IsGroup_Ty Node_struct_Reader = Bool
    getIsGroup (Node_struct_Reader struct) = L.getReaderBoolField struct 224

instance Get_IsGroup Node_struct_Builder where
    type Get_IsGroup_Ty Node_struct_Builder = Bool
    getIsGroup (Node_struct_Builder struct) = L.getBuilderBoolField struct 224

instance Get_DiscriminantCount Node_struct_Reader where
    type Get_DiscriminantCount_Ty Node_struct_Reader = Word16
    getDiscriminantCount (Node_struct_Reader struct) = L.getReaderNumericField struct 15

instance Get_DiscriminantCount Node_struct_Builder where
    type Get_DiscriminantCount_Ty Node_struct_Builder = Word16
    getDiscriminantCount (Node_struct_Builder struct) = L.getBuilderNumericField struct 15

instance Get_DiscriminantOffset Node_struct_Reader where
    type Get_DiscriminantOffset_Ty Node_struct_Reader = Word32
    getDiscriminantOffset (Node_struct_Reader struct) = L.getReaderNumericField struct 8

instance Get_DiscriminantOffset Node_struct_Builder where
    type Get_DiscriminantOffset_Ty Node_struct_Builder = Word32
    getDiscriminantOffset (Node_struct_Builder struct) = L.getBuilderNumericField struct 8

instance Get_Fields Node_struct_Reader where
    type Get_Fields_Ty Node_struct_Reader = (L.ListReader Field_Reader)
    getFields (Node_struct_Reader struct) = L.getReaderList (L.getReaderPointerField struct 3) nullPtr

instance Get_Fields Node_struct_Builder where
    type Get_Fields_Ty Node_struct_Builder = (L.ListBuilder Field_Builder)
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

instance Get_Id Superclass_Reader where
    type Get_Id_Ty Superclass_Reader = Word64
    getId (Superclass_Reader struct) = L.getReaderNumericField struct 0

instance Get_Id Superclass_Builder where
    type Get_Id_Ty Superclass_Builder = Word64
    getId (Superclass_Builder struct) = L.getBuilderNumericField struct 0

instance Get_Brand Superclass_Reader where
    type Get_Brand_Ty Superclass_Reader = Brand_Reader
    getBrand (Superclass_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance Get_Brand Superclass_Builder where
    type Get_Brand_Ty Superclass_Builder = Brand_Builder
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

instance Get_ParameterIndex Type_anyPointer_implicitMethodParameter_Reader where
    type Get_ParameterIndex_Ty Type_anyPointer_implicitMethodParameter_Reader = Word16
    getParameterIndex (Type_anyPointer_implicitMethodParameter_Reader struct) = L.getReaderNumericField struct 5

instance Get_ParameterIndex Type_anyPointer_implicitMethodParameter_Builder where
    type Get_ParameterIndex_Ty Type_anyPointer_implicitMethodParameter_Builder = Word16
    getParameterIndex (Type_anyPointer_implicitMethodParameter_Builder struct) = L.getBuilderNumericField struct 5


data Type_anyPointer_parameter_Reader = Type_anyPointer_parameter_Reader L.StructReader
data Type_anyPointer_parameter_Builder = Type_anyPointer_parameter_Builder L.StructBuilder

instance Get_ScopeId Type_anyPointer_parameter_Reader where
    type Get_ScopeId_Ty Type_anyPointer_parameter_Reader = Word64
    getScopeId (Type_anyPointer_parameter_Reader struct) = L.getReaderNumericField struct 2

instance Get_ScopeId Type_anyPointer_parameter_Builder where
    type Get_ScopeId_Ty Type_anyPointer_parameter_Builder = Word64
    getScopeId (Type_anyPointer_parameter_Builder struct) = L.getBuilderNumericField struct 2

instance Get_ParameterIndex Type_anyPointer_parameter_Reader where
    type Get_ParameterIndex_Ty Type_anyPointer_parameter_Reader = Word16
    getParameterIndex (Type_anyPointer_parameter_Reader struct) = L.getReaderNumericField struct 5

instance Get_ParameterIndex Type_anyPointer_parameter_Builder where
    type Get_ParameterIndex_Ty Type_anyPointer_parameter_Builder = Word16
    getParameterIndex (Type_anyPointer_parameter_Builder struct) = L.getBuilderNumericField struct 5


data Type_enum_Reader = Type_enum_Reader L.StructReader
data Type_enum_Builder = Type_enum_Builder L.StructBuilder

instance Get_TypeId Type_enum_Reader where
    type Get_TypeId_Ty Type_enum_Reader = Word64
    getTypeId (Type_enum_Reader struct) = L.getReaderNumericField struct 1

instance Get_TypeId Type_enum_Builder where
    type Get_TypeId_Ty Type_enum_Builder = Word64
    getTypeId (Type_enum_Builder struct) = L.getBuilderNumericField struct 1

instance Get_Brand Type_enum_Reader where
    type Get_Brand_Ty Type_enum_Reader = Brand_Reader
    getBrand (Type_enum_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance Get_Brand Type_enum_Builder where
    type Get_Brand_Ty Type_enum_Builder = Brand_Builder
    getBrand (Type_enum_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 3 1) nullPtr


data Type_interface_Reader = Type_interface_Reader L.StructReader
data Type_interface_Builder = Type_interface_Builder L.StructBuilder

instance Get_TypeId Type_interface_Reader where
    type Get_TypeId_Ty Type_interface_Reader = Word64
    getTypeId (Type_interface_Reader struct) = L.getReaderNumericField struct 1

instance Get_TypeId Type_interface_Builder where
    type Get_TypeId_Ty Type_interface_Builder = Word64
    getTypeId (Type_interface_Builder struct) = L.getBuilderNumericField struct 1

instance Get_Brand Type_interface_Reader where
    type Get_Brand_Ty Type_interface_Reader = Brand_Reader
    getBrand (Type_interface_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance Get_Brand Type_interface_Builder where
    type Get_Brand_Ty Type_interface_Builder = Brand_Builder
    getBrand (Type_interface_Builder struct) = fmap Brand_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 3 1) nullPtr


data Type_list_Reader = Type_list_Reader L.StructReader
data Type_list_Builder = Type_list_Builder L.StructBuilder

instance Get_ElementType Type_list_Reader where
    type Get_ElementType_Ty Type_list_Reader = Type_Reader
    getElementType (Type_list_Reader struct) = fmap Type_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance Get_ElementType Type_list_Builder where
    type Get_ElementType_Ty Type_list_Builder = Type_Builder
    getElementType (Type_list_Builder struct) = fmap Type_Builder $ L.getBuilderStruct (L.getBuilderPointerField struct 0) (L.StructSize 3 1) nullPtr


data Type_struct_Reader = Type_struct_Reader L.StructReader
data Type_struct_Builder = Type_struct_Builder L.StructBuilder

instance Get_TypeId Type_struct_Reader where
    type Get_TypeId_Ty Type_struct_Reader = Word64
    getTypeId (Type_struct_Reader struct) = L.getReaderNumericField struct 1

instance Get_TypeId Type_struct_Builder where
    type Get_TypeId_Ty Type_struct_Builder = Word64
    getTypeId (Type_struct_Builder struct) = L.getBuilderNumericField struct 1

instance Get_Brand Type_struct_Reader where
    type Get_Brand_Ty Type_struct_Reader = Brand_Reader
    getBrand (Type_struct_Reader struct) = fmap Brand_Reader $ L.getReaderStruct (L.getReaderPointerField struct 0) nullPtr

instance Get_Brand Type_struct_Builder where
    type Get_Brand_Ty Type_struct_Builder = Brand_Builder
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


class Get_Annotations a where
    type Get_Annotations_Ty a :: *
    getAnnotations :: a -> IO (Get_Annotations_Ty a)

class Set_Annotations a where
    type Set_Annotations_Ty a :: *
    setAnnotations :: a -> Set_Annotations_Ty a -> IO ()

class Init_Annotations a where
    type Init_Annotations_Ty a :: *
    initAnnotations :: a -> IO (Init_Annotations_Ty a)

class Initn_Annotations a where
    type Initn_Annotations_Ty a :: *
    initnAnnotations :: a -> Word32 -> IO (Initn_Annotations_Ty a)

class Get_Brand a where
    type Get_Brand_Ty a :: *
    getBrand :: a -> IO (Get_Brand_Ty a)

class Set_Brand a where
    type Set_Brand_Ty a :: *
    setBrand :: a -> Set_Brand_Ty a -> IO ()

class Init_Brand a where
    type Init_Brand_Ty a :: *
    initBrand :: a -> IO (Init_Brand_Ty a)

class Initn_Brand a where
    type Initn_Brand_Ty a :: *
    initnBrand :: a -> Word32 -> IO (Initn_Brand_Ty a)

class Get_CodeOrder a where
    type Get_CodeOrder_Ty a :: *
    getCodeOrder :: a -> IO (Get_CodeOrder_Ty a)

class Set_CodeOrder a where
    type Set_CodeOrder_Ty a :: *
    setCodeOrder :: a -> Set_CodeOrder_Ty a -> IO ()

class Init_CodeOrder a where
    type Init_CodeOrder_Ty a :: *
    initCodeOrder :: a -> IO (Init_CodeOrder_Ty a)

class Initn_CodeOrder a where
    type Initn_CodeOrder_Ty a :: *
    initnCodeOrder :: a -> Word32 -> IO (Initn_CodeOrder_Ty a)

class Get_DataWordCount a where
    type Get_DataWordCount_Ty a :: *
    getDataWordCount :: a -> IO (Get_DataWordCount_Ty a)

class Set_DataWordCount a where
    type Set_DataWordCount_Ty a :: *
    setDataWordCount :: a -> Set_DataWordCount_Ty a -> IO ()

class Init_DataWordCount a where
    type Init_DataWordCount_Ty a :: *
    initDataWordCount :: a -> IO (Init_DataWordCount_Ty a)

class Initn_DataWordCount a where
    type Initn_DataWordCount_Ty a :: *
    initnDataWordCount :: a -> Word32 -> IO (Initn_DataWordCount_Ty a)

class Get_DefaultValue a where
    type Get_DefaultValue_Ty a :: *
    getDefaultValue :: a -> IO (Get_DefaultValue_Ty a)

class Set_DefaultValue a where
    type Set_DefaultValue_Ty a :: *
    setDefaultValue :: a -> Set_DefaultValue_Ty a -> IO ()

class Init_DefaultValue a where
    type Init_DefaultValue_Ty a :: *
    initDefaultValue :: a -> IO (Init_DefaultValue_Ty a)

class Initn_DefaultValue a where
    type Initn_DefaultValue_Ty a :: *
    initnDefaultValue :: a -> Word32 -> IO (Initn_DefaultValue_Ty a)

class Get_DiscriminantCount a where
    type Get_DiscriminantCount_Ty a :: *
    getDiscriminantCount :: a -> IO (Get_DiscriminantCount_Ty a)

class Set_DiscriminantCount a where
    type Set_DiscriminantCount_Ty a :: *
    setDiscriminantCount :: a -> Set_DiscriminantCount_Ty a -> IO ()

class Init_DiscriminantCount a where
    type Init_DiscriminantCount_Ty a :: *
    initDiscriminantCount :: a -> IO (Init_DiscriminantCount_Ty a)

class Initn_DiscriminantCount a where
    type Initn_DiscriminantCount_Ty a :: *
    initnDiscriminantCount :: a -> Word32 -> IO (Initn_DiscriminantCount_Ty a)

class Get_DiscriminantOffset a where
    type Get_DiscriminantOffset_Ty a :: *
    getDiscriminantOffset :: a -> IO (Get_DiscriminantOffset_Ty a)

class Set_DiscriminantOffset a where
    type Set_DiscriminantOffset_Ty a :: *
    setDiscriminantOffset :: a -> Set_DiscriminantOffset_Ty a -> IO ()

class Init_DiscriminantOffset a where
    type Init_DiscriminantOffset_Ty a :: *
    initDiscriminantOffset :: a -> IO (Init_DiscriminantOffset_Ty a)

class Initn_DiscriminantOffset a where
    type Initn_DiscriminantOffset_Ty a :: *
    initnDiscriminantOffset :: a -> Word32 -> IO (Initn_DiscriminantOffset_Ty a)

class Get_DiscriminantValue a where
    type Get_DiscriminantValue_Ty a :: *
    getDiscriminantValue :: a -> IO (Get_DiscriminantValue_Ty a)

class Set_DiscriminantValue a where
    type Set_DiscriminantValue_Ty a :: *
    setDiscriminantValue :: a -> Set_DiscriminantValue_Ty a -> IO ()

class Init_DiscriminantValue a where
    type Init_DiscriminantValue_Ty a :: *
    initDiscriminantValue :: a -> IO (Init_DiscriminantValue_Ty a)

class Initn_DiscriminantValue a where
    type Initn_DiscriminantValue_Ty a :: *
    initnDiscriminantValue :: a -> Word32 -> IO (Initn_DiscriminantValue_Ty a)

class Get_DisplayName a where
    type Get_DisplayName_Ty a :: *
    getDisplayName :: a -> IO (Get_DisplayName_Ty a)

class Set_DisplayName a where
    type Set_DisplayName_Ty a :: *
    setDisplayName :: a -> Set_DisplayName_Ty a -> IO ()

class Init_DisplayName a where
    type Init_DisplayName_Ty a :: *
    initDisplayName :: a -> IO (Init_DisplayName_Ty a)

class Initn_DisplayName a where
    type Initn_DisplayName_Ty a :: *
    initnDisplayName :: a -> Word32 -> IO (Initn_DisplayName_Ty a)

class Get_DisplayNamePrefixLength a where
    type Get_DisplayNamePrefixLength_Ty a :: *
    getDisplayNamePrefixLength :: a -> IO (Get_DisplayNamePrefixLength_Ty a)

class Set_DisplayNamePrefixLength a where
    type Set_DisplayNamePrefixLength_Ty a :: *
    setDisplayNamePrefixLength :: a -> Set_DisplayNamePrefixLength_Ty a -> IO ()

class Init_DisplayNamePrefixLength a where
    type Init_DisplayNamePrefixLength_Ty a :: *
    initDisplayNamePrefixLength :: a -> IO (Init_DisplayNamePrefixLength_Ty a)

class Initn_DisplayNamePrefixLength a where
    type Initn_DisplayNamePrefixLength_Ty a :: *
    initnDisplayNamePrefixLength :: a -> Word32 -> IO (Initn_DisplayNamePrefixLength_Ty a)

class Get_ElementType a where
    type Get_ElementType_Ty a :: *
    getElementType :: a -> IO (Get_ElementType_Ty a)

class Set_ElementType a where
    type Set_ElementType_Ty a :: *
    setElementType :: a -> Set_ElementType_Ty a -> IO ()

class Init_ElementType a where
    type Init_ElementType_Ty a :: *
    initElementType :: a -> IO (Init_ElementType_Ty a)

class Initn_ElementType a where
    type Initn_ElementType_Ty a :: *
    initnElementType :: a -> Word32 -> IO (Initn_ElementType_Ty a)

class Get_Enumerants a where
    type Get_Enumerants_Ty a :: *
    getEnumerants :: a -> IO (Get_Enumerants_Ty a)

class Set_Enumerants a where
    type Set_Enumerants_Ty a :: *
    setEnumerants :: a -> Set_Enumerants_Ty a -> IO ()

class Init_Enumerants a where
    type Init_Enumerants_Ty a :: *
    initEnumerants :: a -> IO (Init_Enumerants_Ty a)

class Initn_Enumerants a where
    type Initn_Enumerants_Ty a :: *
    initnEnumerants :: a -> Word32 -> IO (Initn_Enumerants_Ty a)

class Get_Fields a where
    type Get_Fields_Ty a :: *
    getFields :: a -> IO (Get_Fields_Ty a)

class Set_Fields a where
    type Set_Fields_Ty a :: *
    setFields :: a -> Set_Fields_Ty a -> IO ()

class Init_Fields a where
    type Init_Fields_Ty a :: *
    initFields :: a -> IO (Init_Fields_Ty a)

class Initn_Fields a where
    type Initn_Fields_Ty a :: *
    initnFields :: a -> Word32 -> IO (Initn_Fields_Ty a)

class Get_Filename a where
    type Get_Filename_Ty a :: *
    getFilename :: a -> IO (Get_Filename_Ty a)

class Set_Filename a where
    type Set_Filename_Ty a :: *
    setFilename :: a -> Set_Filename_Ty a -> IO ()

class Init_Filename a where
    type Init_Filename_Ty a :: *
    initFilename :: a -> IO (Init_Filename_Ty a)

class Initn_Filename a where
    type Initn_Filename_Ty a :: *
    initnFilename :: a -> Word32 -> IO (Initn_Filename_Ty a)

class Get_HadExplicitDefault a where
    type Get_HadExplicitDefault_Ty a :: *
    getHadExplicitDefault :: a -> IO (Get_HadExplicitDefault_Ty a)

class Set_HadExplicitDefault a where
    type Set_HadExplicitDefault_Ty a :: *
    setHadExplicitDefault :: a -> Set_HadExplicitDefault_Ty a -> IO ()

class Init_HadExplicitDefault a where
    type Init_HadExplicitDefault_Ty a :: *
    initHadExplicitDefault :: a -> IO (Init_HadExplicitDefault_Ty a)

class Initn_HadExplicitDefault a where
    type Initn_HadExplicitDefault_Ty a :: *
    initnHadExplicitDefault :: a -> Word32 -> IO (Initn_HadExplicitDefault_Ty a)

class Get_Id a where
    type Get_Id_Ty a :: *
    getId :: a -> IO (Get_Id_Ty a)

class Set_Id a where
    type Set_Id_Ty a :: *
    setId :: a -> Set_Id_Ty a -> IO ()

class Init_Id a where
    type Init_Id_Ty a :: *
    initId :: a -> IO (Init_Id_Ty a)

class Initn_Id a where
    type Initn_Id_Ty a :: *
    initnId :: a -> Word32 -> IO (Initn_Id_Ty a)

class Get_ImplicitParameters a where
    type Get_ImplicitParameters_Ty a :: *
    getImplicitParameters :: a -> IO (Get_ImplicitParameters_Ty a)

class Set_ImplicitParameters a where
    type Set_ImplicitParameters_Ty a :: *
    setImplicitParameters :: a -> Set_ImplicitParameters_Ty a -> IO ()

class Init_ImplicitParameters a where
    type Init_ImplicitParameters_Ty a :: *
    initImplicitParameters :: a -> IO (Init_ImplicitParameters_Ty a)

class Initn_ImplicitParameters a where
    type Initn_ImplicitParameters_Ty a :: *
    initnImplicitParameters :: a -> Word32 -> IO (Initn_ImplicitParameters_Ty a)

class Get_Imports a where
    type Get_Imports_Ty a :: *
    getImports :: a -> IO (Get_Imports_Ty a)

class Set_Imports a where
    type Set_Imports_Ty a :: *
    setImports :: a -> Set_Imports_Ty a -> IO ()

class Init_Imports a where
    type Init_Imports_Ty a :: *
    initImports :: a -> IO (Init_Imports_Ty a)

class Initn_Imports a where
    type Initn_Imports_Ty a :: *
    initnImports :: a -> Word32 -> IO (Initn_Imports_Ty a)

class Get_IsGeneric a where
    type Get_IsGeneric_Ty a :: *
    getIsGeneric :: a -> IO (Get_IsGeneric_Ty a)

class Set_IsGeneric a where
    type Set_IsGeneric_Ty a :: *
    setIsGeneric :: a -> Set_IsGeneric_Ty a -> IO ()

class Init_IsGeneric a where
    type Init_IsGeneric_Ty a :: *
    initIsGeneric :: a -> IO (Init_IsGeneric_Ty a)

class Initn_IsGeneric a where
    type Initn_IsGeneric_Ty a :: *
    initnIsGeneric :: a -> Word32 -> IO (Initn_IsGeneric_Ty a)

class Get_IsGroup a where
    type Get_IsGroup_Ty a :: *
    getIsGroup :: a -> IO (Get_IsGroup_Ty a)

class Set_IsGroup a where
    type Set_IsGroup_Ty a :: *
    setIsGroup :: a -> Set_IsGroup_Ty a -> IO ()

class Init_IsGroup a where
    type Init_IsGroup_Ty a :: *
    initIsGroup :: a -> IO (Init_IsGroup_Ty a)

class Initn_IsGroup a where
    type Initn_IsGroup_Ty a :: *
    initnIsGroup :: a -> Word32 -> IO (Initn_IsGroup_Ty a)

class Get_Methods a where
    type Get_Methods_Ty a :: *
    getMethods :: a -> IO (Get_Methods_Ty a)

class Set_Methods a where
    type Set_Methods_Ty a :: *
    setMethods :: a -> Set_Methods_Ty a -> IO ()

class Init_Methods a where
    type Init_Methods_Ty a :: *
    initMethods :: a -> IO (Init_Methods_Ty a)

class Initn_Methods a where
    type Initn_Methods_Ty a :: *
    initnMethods :: a -> Word32 -> IO (Initn_Methods_Ty a)

class Get_Name a where
    type Get_Name_Ty a :: *
    getName :: a -> IO (Get_Name_Ty a)

class Set_Name a where
    type Set_Name_Ty a :: *
    setName :: a -> Set_Name_Ty a -> IO ()

class Init_Name a where
    type Init_Name_Ty a :: *
    initName :: a -> IO (Init_Name_Ty a)

class Initn_Name a where
    type Initn_Name_Ty a :: *
    initnName :: a -> Word32 -> IO (Initn_Name_Ty a)

class Get_NestedNodes a where
    type Get_NestedNodes_Ty a :: *
    getNestedNodes :: a -> IO (Get_NestedNodes_Ty a)

class Set_NestedNodes a where
    type Set_NestedNodes_Ty a :: *
    setNestedNodes :: a -> Set_NestedNodes_Ty a -> IO ()

class Init_NestedNodes a where
    type Init_NestedNodes_Ty a :: *
    initNestedNodes :: a -> IO (Init_NestedNodes_Ty a)

class Initn_NestedNodes a where
    type Initn_NestedNodes_Ty a :: *
    initnNestedNodes :: a -> Word32 -> IO (Initn_NestedNodes_Ty a)

class Get_Nodes a where
    type Get_Nodes_Ty a :: *
    getNodes :: a -> IO (Get_Nodes_Ty a)

class Set_Nodes a where
    type Set_Nodes_Ty a :: *
    setNodes :: a -> Set_Nodes_Ty a -> IO ()

class Init_Nodes a where
    type Init_Nodes_Ty a :: *
    initNodes :: a -> IO (Init_Nodes_Ty a)

class Initn_Nodes a where
    type Initn_Nodes_Ty a :: *
    initnNodes :: a -> Word32 -> IO (Initn_Nodes_Ty a)

class Get_Offset a where
    type Get_Offset_Ty a :: *
    getOffset :: a -> IO (Get_Offset_Ty a)

class Set_Offset a where
    type Set_Offset_Ty a :: *
    setOffset :: a -> Set_Offset_Ty a -> IO ()

class Init_Offset a where
    type Init_Offset_Ty a :: *
    initOffset :: a -> IO (Init_Offset_Ty a)

class Initn_Offset a where
    type Initn_Offset_Ty a :: *
    initnOffset :: a -> Word32 -> IO (Initn_Offset_Ty a)

class Get_Ordinal a where
    type Get_Ordinal_Ty a :: *
    getOrdinal :: a -> IO (Get_Ordinal_Ty a)

class Set_Ordinal a where
    type Set_Ordinal_Ty a :: *
    setOrdinal :: a -> Set_Ordinal_Ty a -> IO ()

class Init_Ordinal a where
    type Init_Ordinal_Ty a :: *
    initOrdinal :: a -> IO (Init_Ordinal_Ty a)

class Initn_Ordinal a where
    type Initn_Ordinal_Ty a :: *
    initnOrdinal :: a -> Word32 -> IO (Initn_Ordinal_Ty a)

class Get_ParamBrand a where
    type Get_ParamBrand_Ty a :: *
    getParamBrand :: a -> IO (Get_ParamBrand_Ty a)

class Set_ParamBrand a where
    type Set_ParamBrand_Ty a :: *
    setParamBrand :: a -> Set_ParamBrand_Ty a -> IO ()

class Init_ParamBrand a where
    type Init_ParamBrand_Ty a :: *
    initParamBrand :: a -> IO (Init_ParamBrand_Ty a)

class Initn_ParamBrand a where
    type Initn_ParamBrand_Ty a :: *
    initnParamBrand :: a -> Word32 -> IO (Initn_ParamBrand_Ty a)

class Get_ParamStructType a where
    type Get_ParamStructType_Ty a :: *
    getParamStructType :: a -> IO (Get_ParamStructType_Ty a)

class Set_ParamStructType a where
    type Set_ParamStructType_Ty a :: *
    setParamStructType :: a -> Set_ParamStructType_Ty a -> IO ()

class Init_ParamStructType a where
    type Init_ParamStructType_Ty a :: *
    initParamStructType :: a -> IO (Init_ParamStructType_Ty a)

class Initn_ParamStructType a where
    type Initn_ParamStructType_Ty a :: *
    initnParamStructType :: a -> Word32 -> IO (Initn_ParamStructType_Ty a)

class Get_ParameterIndex a where
    type Get_ParameterIndex_Ty a :: *
    getParameterIndex :: a -> IO (Get_ParameterIndex_Ty a)

class Set_ParameterIndex a where
    type Set_ParameterIndex_Ty a :: *
    setParameterIndex :: a -> Set_ParameterIndex_Ty a -> IO ()

class Init_ParameterIndex a where
    type Init_ParameterIndex_Ty a :: *
    initParameterIndex :: a -> IO (Init_ParameterIndex_Ty a)

class Initn_ParameterIndex a where
    type Initn_ParameterIndex_Ty a :: *
    initnParameterIndex :: a -> Word32 -> IO (Initn_ParameterIndex_Ty a)

class Get_Parameters a where
    type Get_Parameters_Ty a :: *
    getParameters :: a -> IO (Get_Parameters_Ty a)

class Set_Parameters a where
    type Set_Parameters_Ty a :: *
    setParameters :: a -> Set_Parameters_Ty a -> IO ()

class Init_Parameters a where
    type Init_Parameters_Ty a :: *
    initParameters :: a -> IO (Init_Parameters_Ty a)

class Initn_Parameters a where
    type Initn_Parameters_Ty a :: *
    initnParameters :: a -> Word32 -> IO (Initn_Parameters_Ty a)

class Get_PointerCount a where
    type Get_PointerCount_Ty a :: *
    getPointerCount :: a -> IO (Get_PointerCount_Ty a)

class Set_PointerCount a where
    type Set_PointerCount_Ty a :: *
    setPointerCount :: a -> Set_PointerCount_Ty a -> IO ()

class Init_PointerCount a where
    type Init_PointerCount_Ty a :: *
    initPointerCount :: a -> IO (Init_PointerCount_Ty a)

class Initn_PointerCount a where
    type Initn_PointerCount_Ty a :: *
    initnPointerCount :: a -> Word32 -> IO (Initn_PointerCount_Ty a)

class Get_PreferredListEncoding a where
    type Get_PreferredListEncoding_Ty a :: *
    getPreferredListEncoding :: a -> IO (Get_PreferredListEncoding_Ty a)

class Set_PreferredListEncoding a where
    type Set_PreferredListEncoding_Ty a :: *
    setPreferredListEncoding :: a -> Set_PreferredListEncoding_Ty a -> IO ()

class Init_PreferredListEncoding a where
    type Init_PreferredListEncoding_Ty a :: *
    initPreferredListEncoding :: a -> IO (Init_PreferredListEncoding_Ty a)

class Initn_PreferredListEncoding a where
    type Initn_PreferredListEncoding_Ty a :: *
    initnPreferredListEncoding :: a -> Word32 -> IO (Initn_PreferredListEncoding_Ty a)

class Get_RequestedFiles a where
    type Get_RequestedFiles_Ty a :: *
    getRequestedFiles :: a -> IO (Get_RequestedFiles_Ty a)

class Set_RequestedFiles a where
    type Set_RequestedFiles_Ty a :: *
    setRequestedFiles :: a -> Set_RequestedFiles_Ty a -> IO ()

class Init_RequestedFiles a where
    type Init_RequestedFiles_Ty a :: *
    initRequestedFiles :: a -> IO (Init_RequestedFiles_Ty a)

class Initn_RequestedFiles a where
    type Initn_RequestedFiles_Ty a :: *
    initnRequestedFiles :: a -> Word32 -> IO (Initn_RequestedFiles_Ty a)

class Get_ResultBrand a where
    type Get_ResultBrand_Ty a :: *
    getResultBrand :: a -> IO (Get_ResultBrand_Ty a)

class Set_ResultBrand a where
    type Set_ResultBrand_Ty a :: *
    setResultBrand :: a -> Set_ResultBrand_Ty a -> IO ()

class Init_ResultBrand a where
    type Init_ResultBrand_Ty a :: *
    initResultBrand :: a -> IO (Init_ResultBrand_Ty a)

class Initn_ResultBrand a where
    type Initn_ResultBrand_Ty a :: *
    initnResultBrand :: a -> Word32 -> IO (Initn_ResultBrand_Ty a)

class Get_ResultStructType a where
    type Get_ResultStructType_Ty a :: *
    getResultStructType :: a -> IO (Get_ResultStructType_Ty a)

class Set_ResultStructType a where
    type Set_ResultStructType_Ty a :: *
    setResultStructType :: a -> Set_ResultStructType_Ty a -> IO ()

class Init_ResultStructType a where
    type Init_ResultStructType_Ty a :: *
    initResultStructType :: a -> IO (Init_ResultStructType_Ty a)

class Initn_ResultStructType a where
    type Initn_ResultStructType_Ty a :: *
    initnResultStructType :: a -> Word32 -> IO (Initn_ResultStructType_Ty a)

class Get_ScopeId a where
    type Get_ScopeId_Ty a :: *
    getScopeId :: a -> IO (Get_ScopeId_Ty a)

class Set_ScopeId a where
    type Set_ScopeId_Ty a :: *
    setScopeId :: a -> Set_ScopeId_Ty a -> IO ()

class Init_ScopeId a where
    type Init_ScopeId_Ty a :: *
    initScopeId :: a -> IO (Init_ScopeId_Ty a)

class Initn_ScopeId a where
    type Initn_ScopeId_Ty a :: *
    initnScopeId :: a -> Word32 -> IO (Initn_ScopeId_Ty a)

class Get_Scopes a where
    type Get_Scopes_Ty a :: *
    getScopes :: a -> IO (Get_Scopes_Ty a)

class Set_Scopes a where
    type Set_Scopes_Ty a :: *
    setScopes :: a -> Set_Scopes_Ty a -> IO ()

class Init_Scopes a where
    type Init_Scopes_Ty a :: *
    initScopes :: a -> IO (Init_Scopes_Ty a)

class Initn_Scopes a where
    type Initn_Scopes_Ty a :: *
    initnScopes :: a -> Word32 -> IO (Initn_Scopes_Ty a)

class Get_Superclasses a where
    type Get_Superclasses_Ty a :: *
    getSuperclasses :: a -> IO (Get_Superclasses_Ty a)

class Set_Superclasses a where
    type Set_Superclasses_Ty a :: *
    setSuperclasses :: a -> Set_Superclasses_Ty a -> IO ()

class Init_Superclasses a where
    type Init_Superclasses_Ty a :: *
    initSuperclasses :: a -> IO (Init_Superclasses_Ty a)

class Initn_Superclasses a where
    type Initn_Superclasses_Ty a :: *
    initnSuperclasses :: a -> Word32 -> IO (Initn_Superclasses_Ty a)

class Get_TargetsAnnotation a where
    type Get_TargetsAnnotation_Ty a :: *
    getTargetsAnnotation :: a -> IO (Get_TargetsAnnotation_Ty a)

class Set_TargetsAnnotation a where
    type Set_TargetsAnnotation_Ty a :: *
    setTargetsAnnotation :: a -> Set_TargetsAnnotation_Ty a -> IO ()

class Init_TargetsAnnotation a where
    type Init_TargetsAnnotation_Ty a :: *
    initTargetsAnnotation :: a -> IO (Init_TargetsAnnotation_Ty a)

class Initn_TargetsAnnotation a where
    type Initn_TargetsAnnotation_Ty a :: *
    initnTargetsAnnotation :: a -> Word32 -> IO (Initn_TargetsAnnotation_Ty a)

class Get_TargetsConst a where
    type Get_TargetsConst_Ty a :: *
    getTargetsConst :: a -> IO (Get_TargetsConst_Ty a)

class Set_TargetsConst a where
    type Set_TargetsConst_Ty a :: *
    setTargetsConst :: a -> Set_TargetsConst_Ty a -> IO ()

class Init_TargetsConst a where
    type Init_TargetsConst_Ty a :: *
    initTargetsConst :: a -> IO (Init_TargetsConst_Ty a)

class Initn_TargetsConst a where
    type Initn_TargetsConst_Ty a :: *
    initnTargetsConst :: a -> Word32 -> IO (Initn_TargetsConst_Ty a)

class Get_TargetsEnum a where
    type Get_TargetsEnum_Ty a :: *
    getTargetsEnum :: a -> IO (Get_TargetsEnum_Ty a)

class Set_TargetsEnum a where
    type Set_TargetsEnum_Ty a :: *
    setTargetsEnum :: a -> Set_TargetsEnum_Ty a -> IO ()

class Init_TargetsEnum a where
    type Init_TargetsEnum_Ty a :: *
    initTargetsEnum :: a -> IO (Init_TargetsEnum_Ty a)

class Initn_TargetsEnum a where
    type Initn_TargetsEnum_Ty a :: *
    initnTargetsEnum :: a -> Word32 -> IO (Initn_TargetsEnum_Ty a)

class Get_TargetsEnumerant a where
    type Get_TargetsEnumerant_Ty a :: *
    getTargetsEnumerant :: a -> IO (Get_TargetsEnumerant_Ty a)

class Set_TargetsEnumerant a where
    type Set_TargetsEnumerant_Ty a :: *
    setTargetsEnumerant :: a -> Set_TargetsEnumerant_Ty a -> IO ()

class Init_TargetsEnumerant a where
    type Init_TargetsEnumerant_Ty a :: *
    initTargetsEnumerant :: a -> IO (Init_TargetsEnumerant_Ty a)

class Initn_TargetsEnumerant a where
    type Initn_TargetsEnumerant_Ty a :: *
    initnTargetsEnumerant :: a -> Word32 -> IO (Initn_TargetsEnumerant_Ty a)

class Get_TargetsField a where
    type Get_TargetsField_Ty a :: *
    getTargetsField :: a -> IO (Get_TargetsField_Ty a)

class Set_TargetsField a where
    type Set_TargetsField_Ty a :: *
    setTargetsField :: a -> Set_TargetsField_Ty a -> IO ()

class Init_TargetsField a where
    type Init_TargetsField_Ty a :: *
    initTargetsField :: a -> IO (Init_TargetsField_Ty a)

class Initn_TargetsField a where
    type Initn_TargetsField_Ty a :: *
    initnTargetsField :: a -> Word32 -> IO (Initn_TargetsField_Ty a)

class Get_TargetsFile a where
    type Get_TargetsFile_Ty a :: *
    getTargetsFile :: a -> IO (Get_TargetsFile_Ty a)

class Set_TargetsFile a where
    type Set_TargetsFile_Ty a :: *
    setTargetsFile :: a -> Set_TargetsFile_Ty a -> IO ()

class Init_TargetsFile a where
    type Init_TargetsFile_Ty a :: *
    initTargetsFile :: a -> IO (Init_TargetsFile_Ty a)

class Initn_TargetsFile a where
    type Initn_TargetsFile_Ty a :: *
    initnTargetsFile :: a -> Word32 -> IO (Initn_TargetsFile_Ty a)

class Get_TargetsGroup a where
    type Get_TargetsGroup_Ty a :: *
    getTargetsGroup :: a -> IO (Get_TargetsGroup_Ty a)

class Set_TargetsGroup a where
    type Set_TargetsGroup_Ty a :: *
    setTargetsGroup :: a -> Set_TargetsGroup_Ty a -> IO ()

class Init_TargetsGroup a where
    type Init_TargetsGroup_Ty a :: *
    initTargetsGroup :: a -> IO (Init_TargetsGroup_Ty a)

class Initn_TargetsGroup a where
    type Initn_TargetsGroup_Ty a :: *
    initnTargetsGroup :: a -> Word32 -> IO (Initn_TargetsGroup_Ty a)

class Get_TargetsInterface a where
    type Get_TargetsInterface_Ty a :: *
    getTargetsInterface :: a -> IO (Get_TargetsInterface_Ty a)

class Set_TargetsInterface a where
    type Set_TargetsInterface_Ty a :: *
    setTargetsInterface :: a -> Set_TargetsInterface_Ty a -> IO ()

class Init_TargetsInterface a where
    type Init_TargetsInterface_Ty a :: *
    initTargetsInterface :: a -> IO (Init_TargetsInterface_Ty a)

class Initn_TargetsInterface a where
    type Initn_TargetsInterface_Ty a :: *
    initnTargetsInterface :: a -> Word32 -> IO (Initn_TargetsInterface_Ty a)

class Get_TargetsMethod a where
    type Get_TargetsMethod_Ty a :: *
    getTargetsMethod :: a -> IO (Get_TargetsMethod_Ty a)

class Set_TargetsMethod a where
    type Set_TargetsMethod_Ty a :: *
    setTargetsMethod :: a -> Set_TargetsMethod_Ty a -> IO ()

class Init_TargetsMethod a where
    type Init_TargetsMethod_Ty a :: *
    initTargetsMethod :: a -> IO (Init_TargetsMethod_Ty a)

class Initn_TargetsMethod a where
    type Initn_TargetsMethod_Ty a :: *
    initnTargetsMethod :: a -> Word32 -> IO (Initn_TargetsMethod_Ty a)

class Get_TargetsParam a where
    type Get_TargetsParam_Ty a :: *
    getTargetsParam :: a -> IO (Get_TargetsParam_Ty a)

class Set_TargetsParam a where
    type Set_TargetsParam_Ty a :: *
    setTargetsParam :: a -> Set_TargetsParam_Ty a -> IO ()

class Init_TargetsParam a where
    type Init_TargetsParam_Ty a :: *
    initTargetsParam :: a -> IO (Init_TargetsParam_Ty a)

class Initn_TargetsParam a where
    type Initn_TargetsParam_Ty a :: *
    initnTargetsParam :: a -> Word32 -> IO (Initn_TargetsParam_Ty a)

class Get_TargetsStruct a where
    type Get_TargetsStruct_Ty a :: *
    getTargetsStruct :: a -> IO (Get_TargetsStruct_Ty a)

class Set_TargetsStruct a where
    type Set_TargetsStruct_Ty a :: *
    setTargetsStruct :: a -> Set_TargetsStruct_Ty a -> IO ()

class Init_TargetsStruct a where
    type Init_TargetsStruct_Ty a :: *
    initTargetsStruct :: a -> IO (Init_TargetsStruct_Ty a)

class Initn_TargetsStruct a where
    type Initn_TargetsStruct_Ty a :: *
    initnTargetsStruct :: a -> Word32 -> IO (Initn_TargetsStruct_Ty a)

class Get_TargetsUnion a where
    type Get_TargetsUnion_Ty a :: *
    getTargetsUnion :: a -> IO (Get_TargetsUnion_Ty a)

class Set_TargetsUnion a where
    type Set_TargetsUnion_Ty a :: *
    setTargetsUnion :: a -> Set_TargetsUnion_Ty a -> IO ()

class Init_TargetsUnion a where
    type Init_TargetsUnion_Ty a :: *
    initTargetsUnion :: a -> IO (Init_TargetsUnion_Ty a)

class Initn_TargetsUnion a where
    type Initn_TargetsUnion_Ty a :: *
    initnTargetsUnion :: a -> Word32 -> IO (Initn_TargetsUnion_Ty a)

class Get_Type a where
    type Get_Type_Ty a :: *
    getType :: a -> IO (Get_Type_Ty a)

class Set_Type a where
    type Set_Type_Ty a :: *
    setType :: a -> Set_Type_Ty a -> IO ()

class Init_Type a where
    type Init_Type_Ty a :: *
    initType :: a -> IO (Init_Type_Ty a)

class Initn_Type a where
    type Initn_Type_Ty a :: *
    initnType :: a -> Word32 -> IO (Initn_Type_Ty a)

class Get_TypeId a where
    type Get_TypeId_Ty a :: *
    getTypeId :: a -> IO (Get_TypeId_Ty a)

class Set_TypeId a where
    type Set_TypeId_Ty a :: *
    setTypeId :: a -> Set_TypeId_Ty a -> IO ()

class Init_TypeId a where
    type Init_TypeId_Ty a :: *
    initTypeId :: a -> IO (Init_TypeId_Ty a)

class Initn_TypeId a where
    type Initn_TypeId_Ty a :: *
    initnTypeId :: a -> Word32 -> IO (Initn_TypeId_Ty a)

class Get_Value a where
    type Get_Value_Ty a :: *
    getValue :: a -> IO (Get_Value_Ty a)

class Set_Value a where
    type Set_Value_Ty a :: *
    setValue :: a -> Set_Value_Ty a -> IO ()

class Init_Value a where
    type Init_Value_Ty a :: *
    initValue :: a -> IO (Init_Value_Ty a)

class Initn_Value a where
    type Initn_Value_Ty a :: *
    initnValue :: a -> Word32 -> IO (Initn_Value_Ty a)





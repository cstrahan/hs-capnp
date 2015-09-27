{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}

module Data.CapnProto.Schema.Generator where

import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Text.Show.Pretty      (ppShow)

import qualified Data.CapnProto.Layout as L
import           Data.CapnProto.Schema

generateCode :: [Node] -> String
generateCode schema =
    unlines [ prelude "Data.CapnProto.Schema.Generated"
            , unlines $ map renderStruct structs
            , unlines $ map renderEnum enums
            , unlines (map mkClass nonUnionFieldNames)
            , unlines [ def | struct <- structs
                            , field <- structNodeFields struct
                            , def <- maybeToList $ renderFieldDefault struct field ]
            ]
  where
    maybeToList (Just x) = [x]
    maybeToList _ = []

    structs :: [StructNode]
    structs = sortBy (compare `on` structNodeDisplayName) $ mapMaybe getStruct $ schema
      where
        getStruct (Struct struct) = Just struct
        getStruct _ = Nothing

    enums :: [EnumNode]
    enums = sortBy (compare `on` enumNodeDisplayName) $ mapMaybe getEnum $ schema
      where
        getEnum (Enum enum) = Just enum
        getEnum _ = Nothing

    allFields :: [Field]
    allFields = join $ map (sortBy (compare `on` fieldName) . structNodeFields) structs

    nonUnionFields :: [Field]
    nonUnionFields = filter (not . isUnionMember) allFields

    nonUnionFieldNames :: [String]
    nonUnionFieldNames = sort $ nub $ map fieldName nonUnionFields

renderStruct :: StructNode -> String
renderStruct node =
    unlines $
      [ mkData node ] ++
      map (renderField node) nonUnionFields ++
      (if hasUnion node then [ renderUnion node ] else [])
  where
    fields = structNodeFields node
    unionFields = filter isUnionMember fields
    nonUnionFields = filter (not . isUnionMember) fields

mkStructName :: StructNode -> String
mkStructName node =
    intercalate "_" parts
  where
    parts = (displayNameParts (structNodeDisplayName node))

mkEnumName :: EnumNode -> String
mkEnumName node =
    intercalate "_" parts
  where
    parts = displayNameParts (enumNodeDisplayName node)

displayNameParts :: String -> [String]
displayNameParts displayName = parts
  where
    _:dottedName:_ = (splitOn ":" displayName)
    parts = splitOn "." dottedName

renderFieldDefault :: StructNode -> Field -> Maybe String
renderFieldDefault node (Field name _ _ _ kind _ ) =
    case kind of
        GroupField _ -> Nothing
        SlotField offset ty def explicitDefault ->
            if not explicitDefault
              then Nothing
              else case def of
                       ValData datum ->
                           case datum of
                               "" -> Nothing
                               _ -> Just $ defName<>" :: Ptr L.WirePointer\n"<>
                                           defName<>" = Ptr "<>show datum<>"#"
                       ValText txt ->
                           case txt of
                               "" -> Nothing
                               _ -> Just $ defName<>" :: Ptr L.WirePointer\n"<>
                                           defName<>" = Ptr "<>show (txt<>"\NUL")<>"#"
                       ValList list -> notImplemented
                       ValStruct struct -> notImplemented
                       ValAnyPointer ptr -> notImplemented
                       _ -> Nothing
          where
            defName = "default_"<>mkStructName node<>"_"<>name
            notImplemented =
                Just $ defName<>" :: Ptr L.WirePointer\n"<>
                       defName<>" = nullPtr" -- XXX

renderFieldGetter :: String -> StructNode -> Field -> String
renderFieldGetter k node (Field name _ _ _ kind _ ) =
    case kind of
        GroupField typeNode ->
            "return . "<>mkStructName typeNode <> "_"<>k<>" $ struct"
        SlotField offset ty def explicitDefault ->
            if explicitDefault
              then case ty of
                       TyVoid -> "return ()"
                       TyText{} -> "L.get"<>k<>"Text (L.get"<>k<>"PointerField struct "<>show offset<>") emptyString"
                       TyData{} -> "L.get"<>k<>"Data (L.get"<>k<>"PointerField struct "<>show offset<>") \"\""
                       TyList{} -> "L.get"<>k<>"List (L.get"<>k<>"PointerField struct "<>show offset<>") nullPtr"
                       TyStruct n _ -> "fmap "<>mkStructName n <> "_"<>k<>" $ "<>"L.get"<>k<>"Struct (L.get"<>k<>"PointerField struct "<>show offset<>")"<>structSize<>"nullPtr"
                       TyInterface{} -> "error \"not implemented\""
                       TyAnyPointer{} -> "return $ L.get"<>k<>"PointerField struct"<>show offset
                       TyBool{} -> "L.get"<>k<>"BoolFieldMasked struct "<>show offset<>" "<>defaultValueTerm
                       TyEnum{} -> "fmap (toEnum . fromIntegral) ("<>"L.get"<>k<>"NumericFieldMasked struct "<>show offset<>" "<>show defaultValueTerm<>" :: IO Word16)"
                       _ -> "L.get"<>k<>"NumericFieldMasked struct "<>show offset<>" "<>defaultValueTerm
              else case ty of
                       TyVoid -> "return ()"
                       TyText{} -> "L.get"<>k<>"Text (L.get"<>k<>"PointerField struct "<>show offset<>") emptyString"
                       TyData{} -> "L.get"<>k<>"Data (L.get"<>k<>"PointerField struct "<>show offset<>") \"\""
                       TyList{} -> "L.get"<>k<>"List (L.get"<>k<>"PointerField struct "<>show offset<>") nullPtr"
                       TyStruct n _ -> "fmap "<>mkStructName n <> "_"<>k<>" $ "<>"L.get"<>k<>"Struct (L.get"<>k<>"PointerField struct "<>show offset<>")"<>structSize<>"nullPtr"
                       TyInterface{} -> "error \"not implemented\""
                       TyAnyPointer{} -> "return $ L.get"<>k<>"PointerField struct "<>show offset
                       TyBool{} -> "L.get"<>k<>"BoolField struct "<>show offset
                       TyEnum{} -> "fmap (toEnum . fromIntegral) ("<>"L.get"<>k<>"NumericField struct "<>show offset<>" :: IO Word16)"
                       _ -> "L.get"<>k<>"NumericField struct "<>show offset
          where
            dataWords = structNodeDataWordCount node
            pointers = structNodePointerCount node
            structSize =
                if k == "Reader"
                  then " "
                  else " (L.StructSize "<>show dataWords<>" "<>show pointers<>") "
            defaultValueTerm =
                case def of
                    ValVoid -> "()"
                    ValText val -> defName
                    ValData val -> defName
                    ValList val -> defName
                    ValStruct val -> defName
                    ValAnyPointer val -> defName
                    ValInterface -> "()"
                    ValBool val -> show val
                    ValInt8 val -> show val
                    ValInt16 val -> show val
                    ValInt32 val -> show val
                    ValInt64 val -> show val
                    ValUInt8 val -> show val
                    ValUInt16 val -> show val
                    ValUInt32 val -> show val
                    ValUInt64 val -> show val
                    ValFloat32 val -> show val
                    ValFloat64 val -> show val
                    ValEnum val -> show val
              where
                defName = "default_"<>mkStructName node<>"_"<>name

renderUnion :: StructNode -> String
renderUnion node =
    unlines $ [ "data "<>whichReaderName
              , "  = "<> mkStructName node<>"_NotInSchema_Reader_ Word16"
              , "  | "<>intercalate "\n  | " (map (renderVariant "Reader") unionFields)
              , ""
              , "data "<>whichBuilderName
              , "  = "<> mkStructName node<>"_NotInSchema_Builder_ Word16"
              , "  | "<>intercalate "\n  | " (map (renderVariant "Builder") unionFields)
              , ""
              , "instance L.Union "<>readerName<>" where"
              , "    type UnionTy "<>readerName<>" = "<>whichReaderName
              , "    which ("<>readerName<>" struct) = do"
              , "        d <- L.getReaderNumericField struct "<>show discriminantOffset<>" :: IO Word16"
              , "        case d of"
              ] ++ map (renderCase "Reader") unionFields ++ [
                "            _ -> return $ "<>mkStructName node<>"_NotInSchema_Reader_ d"
              , ""
              , "instance L.Union "<>builderName<>" where"
              , "    type UnionTy "<>builderName<>" = "<>whichBuilderName
              , "    which ("<>builderName<>" struct) = do"
              , "        d <- L.getBuilderNumericField struct "<>show discriminantOffset<>" :: IO Word16"
              , "        case d of"
              ] ++ map (renderCase "Builder") unionFields ++ [
                "            _ -> return $ "<>mkStructName node<>"_NotInSchema_Builder_ d"
              ]
  where
    whichReaderName = mkStructName node<>"_Which_Reader"
    readerName = mkStructName node<>"_Reader"
    whichBuilderName = mkStructName node<>"_Which_Builder"
    builderName = mkStructName node<>"_Builder"
    discriminantOffset = structNodeDiscriminantOffset node
    fields = structNodeFields node
    unionFields = filter isUnionMember fields
    renderVariant k field =
        if isVoidField field
          then mkStructName node <>"_"<>fieldName field<>"_"<>k<>"_"
          else mkStructName node <>"_"<>fieldName field<>"_"<>k<>"_ "<>fieldToHsType k field
    renderCase k field@(Field name _ _ (Just discriminant) kind _ ) =
        if isVoidField field
          then "            "<>show discriminant<>" -> return "<>mkStructName node <>"_"<>name<>"_"<>k<>"_"
          else "            "<>show discriminant<>" -> fmap "<>mkStructName node <>"_"<>name<>"_"<>k<>"_ $ "<>renderFieldGetter k node field

isVoidField :: Field -> Bool
isVoidField field =
    case fieldKind field of
        SlotField _ TyVoid _ _ -> True
        _ -> False

hasUnion :: StructNode -> Bool
hasUnion node = structNodeDiscriminantCount node /= 0

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c:cs

prelude :: String -> String
prelude mod =
    unlines [ "{-# LANGUAGE MagicHash         #-}"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "{-# LANGUAGE TypeFamilies      #-}"
            , ""
            , "module "<>mod<>" where"
            , ""
            , "import qualified Data.ByteString        as BS"
            , "import qualified Data.ByteString.Unsafe as BS"
            , "import           Data.Coerce (coerce)"
            , "import           Data.Int"
            , "import           Data.Word"
            , "import           Foreign.Ptr (nullPtr)"
            , "import           GHC.Exts (Ptr(..))"
            , "import           GHC.Prim (Addr#)"
            , "import           System.IO.Unsafe (unsafePerformIO)"
            , ""
            , "import qualified Data.CapnProto.Layout as L"
            , ""
            , "{-# NOINLINE emptyString #-}"
            , "emptyString :: BS.ByteString"
            , "emptyString = unsafePerformIO $ BS.unsafePackAddressLen 0 \"\\NULL\"#"
            ]

isUnionMember :: Field -> Bool
isUnionMember field =
    case fieldDiscriminantValue field of
        Just _ -> True
        _ -> False

renderEnum :: EnumNode -> String
renderEnum node =
    unlines $ [ "data "<>enumName
              , "  = "<>enumName<>"_NotInSchema Word16"
              , "  | "<>intercalate "\n  | " (map renderOne enums)
              , ""
              , "instance Enum "<>enumName<>" where"
              , "    toEnum num ="
              , "        case num of"
              ] ++ toEnumCases ++
              [ "            _ -> "<>enumName<>"_NotInSchema $ fromIntegral num"
              , ""
              , "    fromEnum enum ="
              , "        case enum of"
              ] ++ fromEnumCases ++
              [ "            "<>enumName<>"_NotInSchema num -> fromIntegral num"
              , ""
              , "instance Eq "<>enumName<>" where"
              , "    x == y = fromEnum x == fromEnum y"
              , ""
              , "instance L.ListElement "<>enumName<>" where"
              , "    elementSize _ = L.SzTwoBytes"
              , ""
              , "instance L.ListReaderElement "<>enumName<>" where"
              , "    getReaderElement list index ="
              , "        fmap (toEnum . fromIntegral) (L.getReaderElement (coerce list) index :: IO Word16)"
              , ""
              , "instance L.ListBuilderElement "<>enumName<>" where"
              , "    getBuilderElement list index ="
              , "        fmap (toEnum . fromIntegral) (L.getBuilderElement (coerce list) index :: IO Word16)"
              , "    setBuilderElement list index val ="
              , "        L.setBuilderElement (coerce list) index (fromIntegral . fromEnum $ val :: Word16)"
              ]
  where
    enumName = mkEnumName node
    enums = enumNodeEnumerants node
    renderOne e = enumName<>"_"<>enumerantName e
    toEnumCases = map toEnumCase (zip enums [0..])
    toEnumCase (enum, i) =
        "            "<>show i<>" -> "<>enumName<>"_"<>enumerantName enum
    fromEnumCases = map fromEnumCase (zip enums [0..])
    fromEnumCase (enum, i) =
        "            "<>enumName<>"_"<>enumerantName enum<>" -> "<>show i

fieldToHsType :: String -> Field -> String
fieldToHsType k field =
    case fieldKind field of
        SlotField offset fieldType defaultValue hadExplicitDefault ->
            typeToHsType k fieldType
        GroupField typeNode ->
            -- we cheat a little bit here, by wrapping the group node in a Type
            typeToHsType k (TyStruct typeNode Brand)

typeToHsType :: String -> Type -> String
typeToHsType k ty =
    case ty of
        TyVoid -> "()"
        TyBool -> "Bool"
        TyInt8 -> "Int8"
        TyInt16 -> "Int16"
        TyInt32 -> "Int32"
        TyInt64 -> "Int64"
        TyUInt8 -> "Word8"
        TyUInt16 -> "Word16"
        TyUInt32 -> "Word32"
        TyUInt64 -> "Word64"
        TyFloat32 -> "Float"
        TyFloat64 -> "Double"
        TyText -> "L.Text"<>k
        TyData -> "L.Data"<>k
        TyList ty -> "(L.List"<>k<>" "<>typeToHsType k ty<>")"
        TyEnum n _ -> mkEnumName n
        TyStruct n _ -> mkStructName n<>"_"<>k
        TyInterface _ _ -> "()" -- TODO: actually implement this
        TyAnyPointer _ -> "L.Pointer"<>k

mkData :: StructNode -> String
mkData node =
    unlines $ [ "data "<>readerName<>" = "<>readerName<>" L.StructReader"
              , "data "<>builderName<>" = "<>builderName<>" L.StructBuilder"
              ] ++ if isGroup then [] else
              [ ""
              , "instance L.FromStructReader "<>readerName<>" where"
              , "    fromStructReader = return . "<>readerName
              , ""
              , "instance L.ListElement "<>readerName<>" where"
              , "    elementSize _ = L.SzInlineComposite"
              , ""
              , "instance L.ListElement "<>builderName<>" where"
              , "    elementSize _ = L.SzInlineComposite"
              , ""
              , "instance L.ListReaderElement "<>readerName<>" where"
              , "    getReaderElement list index ="
              , "        fmap "<>readerName<>" $ L.getReaderElement (coerce list) index"
              , ""
              , "instance L.ListBuilderElement "<>builderName<>" where"
              , "    getBuilderElement list index ="
              , "        fmap "<>builderName<>" $ L.getBuilderElement (coerce list) index"
              , "    setBuilderElement list index ("<>builderName<>" struct) ="
              , "        L.setBuilderElement (coerce list) index struct"
              ]
  where
    readerName = mkStructName node<>"_Reader"
    builderName = mkStructName node<>"_Builder"
    isGroup = structNodeIsGroup node

mkClass :: String -> String
mkClass field =
    unlines [ "class Has"<>capField<>" a where"
            , "    type "<>capField<>"Ty a :: *"
            , "    get"<>capField<>" :: a -> IO ("<>capField<>"Ty a)"
            ]
  where
    capField = capitalize field

renderField :: StructNode -> Field -> String
renderField node field@(Field name _ _ _ kind _) =
    unlines [ "instance Has"<>capField<>" "<>readerName<>" where"
            , "    type "<>capField<>"Ty "<>readerName<>" = "<>fieldToHsType "Reader" field
            , "    get"<>capField<>" ("<>readerName<>" struct) = "<>renderFieldGetter "Reader" node field
            , ""
            , "instance Has"<>capField<>" "<>builderName<>" where"
            , "    type "<>capField<>"Ty "<>builderName<>" = "<>fieldToHsType "Builder" field
            , "    get"<>capField<>" ("<>builderName<>" struct) = "<>renderFieldGetter "Builder" node field
            ]
  where
    capField = capitalize name
    readerName = mkStructName node<>"_Reader"
    builderName = mkStructName node<>"_Builder"

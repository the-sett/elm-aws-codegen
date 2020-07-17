module Templates.KVDecode exposing (..)

{-| Code generators for URL paths, query arguments or header fields in formats acceptable
to AWS services.
-}

import Dict
import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)
import Elm.FunDecl as FunDecl exposing (FunDecl, FunGen)
import Elm.Helper as Util
import Errors exposing (Error, ErrorBuilder)
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, PropSpec(..), Properties, Property(..), Restricted(..), Type(..))
import L2 exposing (RefChecked(..))
import L3 exposing (DefaultProperties, L3, L3Error(..), ProcessorImpl, PropertiesAPI)
import List.Nonempty
import Maybe.Extra
import Naming
import ResultME exposing (ResultME)
import Set exposing (Set)


type KVDecodeError
    = L3Error L3.L3Error
    | UnsupportedType String
    | UnsupportedDeclaration String


errorCatalogue =
    Dict.fromList
        [ ( 305
          , { title = "Unsupported Type for a Key-Value String decoder."
            , body = "Cannot generate a Key-Value String decoder for the Type []{arg|key=name }."
            }
          )
        , ( 306
          , { title = "Unsupported Declarable for a Key-Value String decoder."
            , body = "Cannot generate a Key-Value String decoder for the Declarable []{arg|key=name }."
            }
          )
        ]


errorBuilder : ErrorBuilder pos KVDecodeError
errorBuilder posFn err =
    case err of
        L3Error l3error ->
            L3.errorBuilder posFn l3error

        UnsupportedType name ->
            Errors.lookupError errorCatalogue
                305
                (Dict.fromList [ ( "name", name ) ])
                []

        UnsupportedDeclaration name ->
            Errors.lookupError errorCatalogue
                306
                (Dict.fromList [ ( "name", name ) ])
                []


check : L3 pos -> ResultME err (L3 pos)
check l3 =
    l3 |> Ok


processorImpl : ProcessorImpl pos KVDecodeError
processorImpl =
    { name = "KVDecode"
    , defaults = defaultProperties
    , check = check
    , buildError = errorBuilder
    }


defaultProperties : DefaultProperties
defaultProperties =
    { top = L1.defineProperties [] []
    , alias = L1.defineProperties [] []
    , sum = L1.defineProperties [] []
    , enum = L1.defineProperties [] []
    , restricted = L1.defineProperties [] []
    , fields =
        L1.defineProperties
            [ ( "serializedName", PSOptional PSString ) ]
            []
    , unit = L1.defineProperties [] []
    , basic = L1.defineProperties [] []
    , named = L1.defineProperties [] []
    , product = L1.defineProperties [] []
    , emptyProduct = L1.defineProperties [] []
    , container = L1.defineProperties [] []
    , function = L1.defineProperties [] []
    }



--===  Conversion of simple types to strings as Elm expressions.


{-| Converts a value of a basic type to a string.

The value is given as an expression and the to-string logic is applied to that
expression.

-}
basicToString : Basic -> Expression -> ( Expression, Linkage )
basicToString basic expr =
    case basic of
        L1.BBool ->
            ( CG.apply
                [ CG.fqFun awsKVDecodeMod "bool"
                , expr
                ]
            , CG.emptyLinkage
                |> CG.addImport awsKVDecodeImport
            )

        L1.BInt ->
            ( CG.apply
                [ CG.fqFun stringMod "fromInt"
                , expr
                ]
            , CG.emptyLinkage
            )

        L1.BReal ->
            ( CG.apply
                [ CG.fqFun stringMod "fromFloat"
                , expr
                ]
            , CG.emptyLinkage
            )

        L1.BString ->
            ( expr, CG.emptyLinkage )


{-| Converts a value of a simple type to a string.

The value is given as an expression and the to-string logic is applied to that
expression.

Only simple types can be handled by this function, which is why its result is a
`Maybe`. Compound types such as sums or products or containers are not supported
and will produce `Nothing`.

-}
typeToString : Type pos L2.RefChecked -> Expression -> Result KVDecodeError ( Expression, Linkage )
typeToString l2type expr =
    case l2type of
        TBasic _ _ basic ->
            basicToString basic expr
                |> Ok

        TNamed _ _ refName ref ->
            case ref of
                L2.RcTBasic basic ->
                    basicToString basic expr
                        |> Ok

                L2.RcRestricted basic ->
                    CG.apply
                        [ CG.fqFun refinedMod "unbox"
                        , Naming.safeCCL refName |> CG.val
                        , expr
                        ]
                        |> basicToString basic
                        |> Tuple.mapSecond (CG.addImport refinedImport)
                        |> Ok

                L2.RcEnum ->
                    CG.apply
                        [ CG.fqFun enumMod "toString"
                        , Naming.safeCCL refName |> CG.val
                        , expr
                        ]
                        |> basicToString BString
                        |> Tuple.mapSecond (CG.addImport enumImport)
                        |> Ok

                _ ->
                    L2.refCheckedConsName ref
                        |> UnsupportedType
                        |> Err

        _ ->
            L1.typeConsName l2type
                |> UnsupportedType
                |> Err



--===  Conversion of all types (except functions) to key-value string pairs.


{-| Generates a KVDecoder for a type declaration.
-}
kvDecoder :
    PropertiesAPI pos
    -> String
    -> Declarable pos RefChecked
    -> ResultME KVDecodeError FunGen
kvDecoder propertiesApi name decl =
    case decl of
        DAlias _ _ l1Type ->
            typeAliasKVDecoder propertiesApi name l1Type

        DSum _ _ constructors ->
            -- customTypeKVDecoder name (List.Nonempty.toList constructors)
            L1.declarableConsName decl
                |> UnsupportedDeclaration
                |> ResultME.error

        DEnum _ _ labels ->
            enumKVDecoder name (List.Nonempty.toList labels) |> Ok

        DRestricted _ _ res ->
            restrictedKVDecoder name res |> Ok


partialKVDecoder :
    PropertiesAPI pos
    -> String
    -> List (Field pos RefChecked)
    -> ResultME KVDecodeError FunGen
partialKVDecoder propertiesApi name fields =
    ResultME.map
        (\impl ->
            let
                decodeFnName =
                    Naming.safeCCL (name ++ "Decoder")

                typeName =
                    Naming.safeCCU name

                sig =
                    CG.typed "Decoder" [ CG.typed typeName [] ]

                doc =
                    CG.emptyDocComment
                        |> CG.markdown ("Decoder for " ++ typeName ++ ".")
            in
            ( FunDecl
                (Just doc)
                (Just sig)
                decodeFnName
                [ CG.varPattern "val" ]
                impl
            , CG.emptyLinkage
                |> CG.addImport awsKVDecodeImport
                |> CG.addExposing (CG.funExpose decodeFnName)
            )
        )
        (kvDecoderNamedProduct propertiesApi name fields)


{-| Generates a KVDecoder for an L1 type alias.
-}
typeAliasKVDecoder : PropertiesAPI pos -> String -> Type pos RefChecked -> ResultME KVDecodeError ( FunDecl, Linkage )
typeAliasKVDecoder propertiesApi name l1Type =
    ResultME.map
        (\impl ->
            let
                kvDecoderFnName =
                    Naming.safeCCL name ++ "KVDecoder"

                typeName =
                    Naming.safeCCU name

                sig =
                    CG.funAnn
                        (CG.typed typeName [])
                        (CG.typed "KVPairs" [])

                doc =
                    CG.emptyDocComment
                        |> CG.markdown ("KVDecoder for " ++ typeName ++ ".")
            in
            ( FunDecl
                (Just doc)
                (Just sig)
                kvDecoderFnName
                [ CG.varPattern "val" ]
                impl
            , CG.emptyLinkage
                |> CG.addImport awsKVDecodeImport
                |> CG.addExposing (CG.funExpose kvDecoderFnName)
            )
        )
        (kvDecoderNamedType propertiesApi name l1Type)


enumKVDecoder : String -> List String -> ( FunDecl, Linkage )
enumKVDecoder name constructors =
    let
        kvDecoderFnName =
            Naming.safeCCL name ++ "KVDecoder"

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.funAnn
                (CG.typed typeName [])
                (CG.typed "KVPairs" [])

        ( impl, implLinkage ) =
            basicToKVFun BString
                |> Tuple.mapFirst
                    (\kvfun ->
                        CG.pipe
                            (CG.apply
                                [ CG.fqFun enumMod "toString"
                                , CG.val enumName
                                , CG.val "val"
                                ]
                            )
                            [ kvfun ]
                    )

        doc =
            CG.emptyDocComment
                |> CG.markdown ("KVDecoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        kvDecoderFnName
        [ CG.varPattern "val" ]
        impl
    , CG.combineLinkage
        [ CG.emptyLinkage
            |> CG.addImport enumImport
            |> CG.addExposing (CG.funExpose kvDecoderFnName)
        , implLinkage
        ]
    )


restrictedKVDecoder : String -> Restricted -> ( FunDecl, Linkage )
restrictedKVDecoder name res =
    let
        basic =
            case res of
                RInt _ ->
                    BInt

                RString _ ->
                    BString

        kvDecoderFnName =
            Naming.safeCCL name ++ "KVDecoder"

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.funAnn
                (CG.typed typeName [])
                (CG.typed "KVPairs" [])

        -- Implementation needs to look like this:
        --
        -- AWS.KVDecode.andThen
        --     (\sval ->
        --         case Refined.build version sval of
        --             Ok val ->
        --                 AWS.KVDecode.succeed val
        --
        --             Err err ->
        --                 Refined.stringErrorToString err |> AWS.KVDecode.fail
        --     )
        --     AWS.KVDecode.string
        --
        ( impl, implLinkage ) =
            basicToKVFun basic
                |> Tuple.mapFirst
                    (\kvfun ->
                        CG.pipe
                            (CG.apply
                                [ CG.fqFun refinedMod "unbox"
                                , enumName |> CG.val
                                , CG.val "val"
                                ]
                            )
                            [ kvfun ]
                    )

        doc =
            CG.emptyDocComment
                |> CG.markdown ("KVDecoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        kvDecoderFnName
        [ CG.varPattern "val" ]
        impl
    , CG.combineLinkage
        [ CG.emptyLinkage
            |> CG.addImport enumImport
            |> CG.addExposing (CG.funExpose kvDecoderFnName)
        , implLinkage
        ]
    )


{-| Generates a KVDecoder for an L1 type that has been named as an alias.
-}
kvDecoderNamedType : PropertiesAPI pos -> String -> Type pos RefChecked -> ResultME KVDecodeError Expression
kvDecoderNamedType propertiesApi name l1Type =
    case l1Type of
        TUnit _ _ ->
            kvDecoderUnit |> Ok

        TBasic _ _ basic ->
            kvDecoderType l1Type |> Ok

        TNamed _ _ named _ ->
            CG.string "kvDecoderNamedType_TNamed" |> Ok

        TProduct _ _ fields ->
            kvDecoderNamedProduct propertiesApi name (List.Nonempty.toList fields)

        TEmptyProduct _ _ ->
            kvDecoderNamedProduct propertiesApi name []

        TContainer _ _ container ->
            kvDecoderType l1Type |> Ok

        TFunction _ _ arg res ->
            CG.unit |> Ok


{-| Generates a KVDecoder for an L1 type.
-}
kvDecoderType : Type pos RefChecked -> Expression
kvDecoderType l1Type =
    case l1Type of
        TBasic _ _ basic ->
            kvDecoderBasic basic

        TNamed _ _ named _ ->
            kvDecoderNamed named

        TProduct _ _ fields ->
            kvDecoderProduct (List.Nonempty.toList fields)

        TContainer _ _ container ->
            kvDecoderContainer container

        _ ->
            CG.unit


{-| Generates a field kvDecoder for a named field with an L1 type.
-}
kvDecoderTypeField : String -> String -> Type pos RefChecked -> Expression
kvDecoderTypeField name serializedName l1Type =
    case l1Type of
        TUnit _ _ ->
            kvDecoderUnit |> kvDecoderField name serializedName

        TBasic _ _ basic ->
            kvDecoderBasic basic
                |> kvDecoderField name serializedName

        TNamed _ _ named _ ->
            kvDecoderNamed named
                |> kvDecoderField name serializedName

        TProduct _ _ fields ->
            kvDecoderProduct (List.Nonempty.toList fields)
                |> kvDecoderField name serializedName

        TEmptyProduct _ _ ->
            kvDecoderProduct []
                |> kvDecoderField name serializedName

        TContainer _ _ container ->
            kvDecoderContainerField name serializedName container

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a kvDecoder for unit.

Decodes `()`, and decodes to JSON `null`.

-}
kvDecoderUnit =
    CG.apply
        [ awsKVDecodeFn "constant"
        , CG.unit
        ]


{-| Generates a kvDecoder for a basic L1 type.
-}
kvDecoderBasic : Basic -> Expression
kvDecoderBasic basic =
    case basic of
        BBool ->
            CG.apply
                [ awsKVDecodeFn "bool"
                , CG.val "val"
                ]

        BInt ->
            CG.apply
                [ awsKVDecodeFn "int"
                , CG.val "val"
                ]

        BReal ->
            CG.apply
                [ awsKVDecodeFn "float"
                , CG.val "val"
                ]

        BString ->
            CG.apply
                [ awsKVDecodeFn "string"
                , CG.val "val"
                ]


kvDecoderNamed named =
    CG.fun (Naming.safeCCL named ++ "KVDecoder")


kvDecoderContainer : Container pos RefChecked -> Expression
kvDecoderContainer container =
    case container of
        CList l1Type ->
            CG.apply [ awsKVDecodeFn "list", kvDecoderType l1Type, CG.val "val" ]
                |> CG.parens

        CSet l1Type ->
            CG.apply [ awsKVDecodeFn "set", kvDecoderType l1Type, CG.val "val" ]
                |> CG.parens

        CDict l1keyType l1valType ->
            kvDecoderDict l1keyType l1valType

        COptional l1Type ->
            CG.apply [ awsKVDecodeFn "maybe", kvDecoderType l1Type, CG.val "val" ]
                |> CG.parens


kvDecoderDict : Type pos RefChecked -> Type pos RefChecked -> Expression
kvDecoderDict l1keyType l1valType =
    case l1keyType of
        TNamed _ _ name (RcRestricted basic) ->
            CG.apply
                [ awsKVDecodeFn "dict"
                , kvDecoderType l1valType
                , CG.apply
                    [ CG.fqFun refinedMod "unboxedDict"
                    , CG.val (Naming.safeCCL name)
                    , CG.val "identity"
                    , CG.val "val"
                    ]
                    |> CG.parens
                ]

        TNamed _ _ name RcEnum ->
            CG.apply
                [ awsKVDecodeFn "dict"
                , kvDecoderType l1valType
                , CG.apply
                    [ CG.fqFun refinedMod "stringDict"
                    , CG.val (Naming.safeCCL name)
                    , CG.val "identity"
                    , CG.val "val"
                    ]
                    |> CG.parens
                ]

        _ ->
            CG.apply [ awsKVDecodeFn "dict", kvDecoderType l1valType, CG.val "val" ]
                |> CG.parens


{-| Generates a kvDecoder for an L1 product type that has been named as an alias.
The alias name is also the constructor function for the type.
-}
kvDecoderNamedProduct : PropertiesAPI pos -> String -> List ( String, Type pos RefChecked, L1.Properties ) -> ResultME KVDecodeError Expression
kvDecoderNamedProduct propertiesApi name fields =
    ResultME.map
        (\fieldDecoders ->
            let
                typeName =
                    Naming.safeCCU name

                impl =
                    CG.pipe (CG.fqFun awsKVDecodeMod "object")
                        fieldDecoders
            in
            impl
        )
        (kvDecoderFields propertiesApi fields)


{-| Generates a kvDecoder for an L1 product type that does not have a name.
Without a name there is no constructor function for the product, so it must be
built explicitly by its fields.
-}
kvDecoderProduct : List ( String, Type pos RefChecked, L1.Properties ) -> Expression
kvDecoderProduct fields =
    CG.string "kvDecoderProduct"


{-| Generates a field kvDecoder for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `KVDecoder.optionalField`.
-}
kvDecoderContainerField : String -> String -> Container pos RefChecked -> Expression
kvDecoderContainerField name serializedName container =
    case container of
        CList l1Type ->
            CG.apply [ awsKVDecodeFn "list", kvDecoderType l1Type ]
                |> CG.parens
                |> kvDecoderField name serializedName

        CSet l1Type ->
            CG.apply [ awsKVDecodeFn "set", kvDecoderType l1Type ]
                |> CG.parens
                |> kvDecoderField name serializedName

        CDict l1keyType l1valType ->
            kvDecoderDict l1keyType l1valType
                |> kvDecoderField name serializedName

        COptional l1Type ->
            kvDecoderType l1Type
                |> decoderOptionalField name serializedName


{-| Outputs decoders for a list of fields and terminates the list with `Decoder.buildObject`.
Helper function useful when building record decoders.
-}
kvDecoderFields : PropertiesAPI pos -> List ( String, Type pos RefChecked, L1.Properties ) -> ResultME KVDecodeError (List Expression)
kvDecoderFields propertiesApi fields =
    let
        fieldMapperFn ( fieldName, l1Type, fprops ) accum =
            let
                maybeSerName =
                    (propertiesApi.field fprops).getOptionalStringProperty "serializedName"
            in
            case maybeSerName of
                Ok (Just serName) ->
                    (kvDecoderTypeField fieldName serName l1Type |> Ok) :: accum

                Ok Nothing ->
                    (kvDecoderTypeField fieldName fieldName l1Type |> Ok) :: accum

                Err err ->
                    Err err :: accum
    in
    List.foldl fieldMapperFn [] fields
        |> ResultME.combineList
        |> ResultME.mapError L3Error


{-| Helper function for building field decoders.
-}
kvDecoderField : String -> String -> Expression -> Expression
kvDecoderField name serializedName expr =
    CG.apply
        [ CG.fqFun awsKVDecodeMod "field"
        , CG.string serializedName
        , expr
        ]


{-| Helper function for building optional field decoders.
-}
decoderOptionalField : String -> String -> Expression -> Expression
decoderOptionalField name serializedName expr =
    CG.apply
        [ CG.fqFun awsKVDecodeMod "optional"
        , CG.string serializedName
        , expr
        ]


basicToKVFun : Basic -> ( Expression, Linkage )
basicToKVFun basic =
    case basic of
        L1.BBool ->
            ( CG.fqFun awsKVDecodeMod "bool"
            , CG.emptyLinkage
                |> CG.addImport awsKVDecodeImport
            )

        L1.BInt ->
            ( CG.fqFun awsKVDecodeMod "int"
            , CG.emptyLinkage
            )

        L1.BReal ->
            ( CG.fqFun awsKVDecodeMod "float"
            , CG.emptyLinkage
            )

        L1.BString ->
            ( CG.fqFun awsKVDecodeMod "string"
            , CG.emptyLinkage
            )



--== Helper Functions


dummyFn : String -> ( Declaration, Linkage )
dummyFn name =
    ( CG.funDecl Nothing Nothing name [] CG.unit, CG.emptyLinkage )


awsKVDecodeMod : List String
awsKVDecodeMod =
    [ "AWS", "KVDecode" ]


dictEnumMod : List String
dictEnumMod =
    [ "Dict", "Enum" ]


enumMod : List String
enumMod =
    [ "Enum" ]


maybeMod : List String
maybeMod =
    [ "Maybe" ]


dictRefinedMod : List String
dictRefinedMod =
    [ "Dict", "Refined" ]


refinedMod : List String
refinedMod =
    [ "Refined" ]


resultMod : List String
resultMod =
    [ "Result" ]


stringMod : List String
stringMod =
    [ "String" ]


awsKVDecodeFn : String -> Expression
awsKVDecodeFn =
    CG.fqFun awsKVDecodeMod


awsKVDecodeImport : Import
awsKVDecodeImport =
    CG.importStmt awsKVDecodeMod Nothing Nothing


setImport : Import
setImport =
    CG.importStmt [ "Set" ] Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Set" ])


dictImport : Import
dictImport =
    CG.importStmt [ "Dict" ] Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Dict" ])


enumImport : Import
enumImport =
    CG.importStmt enumMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Enum" ])


dictEnumImport : Import
dictEnumImport =
    CG.importStmt dictEnumMod Nothing Nothing


refinedImport : Import
refinedImport =
    CG.importStmt refinedMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Refined" ])


dictRefinedImport : Import
dictRefinedImport =
    CG.importStmt dictRefinedMod Nothing Nothing


guardedImportExposing : List String -> Import
guardedImportExposing exposings =
    CG.importStmt refinedMod Nothing (Just <| CG.exposeExplicit (List.map CG.typeOrAliasExpose exposings))

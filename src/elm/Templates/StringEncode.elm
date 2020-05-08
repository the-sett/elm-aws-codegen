module Templates.StringEncode exposing (..)

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


type StringEncodeError
    = L3Error L3.L3Error
    | UnsupportedType String
    | UnsupportedDeclaration String


errorCatalogue =
    Dict.fromList
        [ ( 305
          , { title = "Unsupported Type for a Key-Value String encoder."
            , body = "Cannot generate a Key-Value String encoder for the Type []{arg|key=name }."
            }
          )
        , ( 306
          , { title = "Unsupported Declarable for a Key-Value String encoder."
            , body = "Cannot generate a Key-Value String encoder for the Declarable []{arg|key=name }."
            }
          )
        ]


errorBuilder : ErrorBuilder pos StringEncodeError
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


processorImpl : ProcessorImpl pos StringEncodeError
processorImpl =
    { name = "StringEncode"
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
                [ CG.fqFun awsCoreKVEncodeMod "bool"
                , expr
                ]
            , CG.emptyLinkage
                |> CG.addImport awsCoreKVEncodeImport
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
typeToString : Type pos L2.RefChecked -> Expression -> Result StringEncodeError ( Expression, Linkage )
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


{-| Generates a KVEncoder for a type declaration.
-}
kvEncoder :
    PropertiesAPI pos
    -> String
    -> Declarable pos RefChecked
    -> ResultME StringEncodeError FunGen
kvEncoder propertiesApi name decl =
    case decl of
        DAlias _ _ l1Type ->
            typeAliasKVEncoder propertiesApi name l1Type

        DSum _ _ constructors ->
            -- customTypeKVEncoder name (List.Nonempty.toList constructors)
            L1.declarableConsName decl
                |> UnsupportedDeclaration
                |> ResultME.error

        DEnum _ _ labels ->
            enumKVEncoder name (List.Nonempty.toList labels) |> Ok

        DRestricted _ _ res ->
            restrictedKVEncoder name res |> Ok


partialKVEncoder :
    PropertiesAPI pos
    -> String
    -> List (Field pos RefChecked)
    -> ResultME StringEncodeError FunGen
partialKVEncoder propertiesApi name fields =
    ResultME.map
        (\impl ->
            let
                encodeFnName =
                    Naming.safeCCL (name ++ "Encoder")

                typeName =
                    Naming.safeCCU name

                sig =
                    CG.typed "Encoder" [ CG.typed typeName [] ]

                doc =
                    CG.emptyDocComment
                        |> CG.markdown ("Encoder for " ++ typeName ++ ".")
            in
            ( FunDecl
                (Just doc)
                (Just sig)
                encodeFnName
                [ CG.varPattern "val" ]
                impl
            , CG.emptyLinkage
                |> CG.addImport awsCoreKVEncodeImport
                |> CG.addExposing (CG.funExpose encodeFnName)
            )
        )
        (codecNamedProduct propertiesApi name fields)


{-| Generates a KVEncoder for an L1 type alias.
-}
typeAliasKVEncoder : PropertiesAPI pos -> String -> Type pos RefChecked -> ResultME StringEncodeError ( FunDecl, Linkage )
typeAliasKVEncoder propertiesApi name l1Type =
    ResultME.map
        (\impl ->
            let
                codecFnName =
                    Naming.safeCCL name ++ "KVEncoder"

                typeName =
                    Naming.safeCCU name

                sig =
                    CG.funAnn
                        (CG.typed typeName [])
                        (CG.typed "KVPairs" [])

                doc =
                    CG.emptyDocComment
                        |> CG.markdown ("KVEncoder for " ++ typeName ++ ".")
            in
            ( FunDecl
                (Just doc)
                (Just sig)
                codecFnName
                [ CG.varPattern "val" ]
                impl
            , CG.emptyLinkage
                |> CG.addImport awsCoreKVEncodeImport
                |> CG.addExposing (CG.funExpose codecFnName)
            )
        )
        (codecNamedType propertiesApi name l1Type)



-- customTypeKVEncoder : String -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> ( FunDecl, Linkage )
-- customTypeKVEncoder name constructors =
--     let
--         codecFnName =
--             Naming.safeCCL name ++ "KVEncoder"
--
--         typeName =
--             Naming.safeCCU name
--
--         sig =
--             CG.funAnn
--                 (CG.typed typeName [])
--                 (CG.listAnn (CG.tupleAnn [ CG.stringAnn, CG.stringAnn ]))
--
--         impl =
--             codecCustomType constructors
--
--         doc =
--             CG.emptyDocComment
--                 |> CG.markdown ("KVEncoder for " ++ typeName ++ ".")
--     in
--     ( FunDecl
--         (Just doc)
--         (Just sig)
--         codecFnName
--         [ CG.varPattern "val" ]
--         impl
--     , CG.emptyLinkage
--         |> CG.addImport awsCoreKVEncodeImport
--         |> CG.addExposing (CG.funExpose codecFnName)
--     )


enumKVEncoder : String -> List String -> ( FunDecl, Linkage )
enumKVEncoder name constructors =
    let
        codecFnName =
            Naming.safeCCL name ++ "KVEncoder"

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
                |> CG.markdown ("KVEncoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        codecFnName
        [ CG.varPattern "val" ]
        impl
    , CG.combineLinkage
        [ CG.emptyLinkage
            |> CG.addImport enumImport
            |> CG.addExposing (CG.funExpose codecFnName)
        , implLinkage
        ]
    )


restrictedKVEncoder : String -> Restricted -> ( FunDecl, Linkage )
restrictedKVEncoder name res =
    let
        basic =
            case res of
                RInt _ ->
                    BInt

                RString _ ->
                    BString

        codecFnName =
            Naming.safeCCL name ++ "KVEncoder"

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.funAnn
                (CG.typed typeName [])
                (CG.typed "KVPairs" [])

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
                |> CG.markdown ("KVEncoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        codecFnName
        [ CG.varPattern "val" ]
        impl
    , CG.combineLinkage
        [ CG.emptyLinkage
            |> CG.addImport enumImport
            |> CG.addExposing (CG.funExpose codecFnName)
        , implLinkage
        ]
    )



-- codecCustomType : List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
-- codecCustomType constructors =
--     let
--         codecVariant name args =
--             List.foldr
--                 (\( _, l1Type, _ ) accum -> codecType l1Type :: accum)
--                 [ Naming.safeCCU name |> CG.fun
--                 , Naming.safeCCU name |> CG.string
--                 , awsCoreKVEncodeFn ("variant" ++ String.fromInt (List.length args))
--                 ]
--                 args
--                 |> List.reverse
--                 |> CG.apply
--     in
--     List.foldr (\( name, consArgs ) accum -> codecVariant name consArgs :: accum)
--         [ CG.apply [ awsCoreKVEncodeFn "buildCustom" ] ]
--         constructors
--         |> CG.pipe
--             (CG.apply
--                 [ awsCoreKVEncodeFn "custom"
--                 , codecMatchFn constructors
--                 ]
--             )


codecMatchFn : List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
codecMatchFn constructors =
    let
        consFnName name =
            "f" ++ Naming.safeCCL name

        args =
            List.foldr (\( name, _ ) accum -> (consFnName name |> CG.varPattern) :: accum)
                [ CG.varPattern "value" ]
                constructors

        consPattern ( name, consArgs ) =
            ( CG.namedPattern (Naming.safeCCU name)
                (List.map (\( argName, _, _ ) -> CG.varPattern argName) consArgs)
            , List.foldr (\( argName, _, _ ) accum -> CG.val argName :: accum)
                [ consFnName name |> CG.fun ]
                consArgs
                |> List.reverse
                |> CG.apply
            )

        matchFnBody =
            List.map consPattern constructors
                |> CG.caseExpr (CG.val "value")
    in
    CG.lambda args matchFnBody


{-| Generates a KVEncoder for an L1 type that has been named as an alias.
-}
codecNamedType : PropertiesAPI pos -> String -> Type pos RefChecked -> ResultME StringEncodeError Expression
codecNamedType propertiesApi name l1Type =
    case l1Type of
        TUnit _ _ ->
            codecUnit |> Ok

        TBasic _ _ basic ->
            codecType l1Type |> Ok

        TNamed _ _ named _ ->
            CG.string "codecNamedType_TNamed" |> Ok

        TProduct _ _ fields ->
            codecNamedProduct propertiesApi name (List.Nonempty.toList fields)

        TEmptyProduct _ _ ->
            codecNamedProduct propertiesApi name []

        TContainer _ _ container ->
            codecType l1Type |> Ok

        TFunction _ _ arg res ->
            CG.unit |> Ok


{-| Generates a KVEncoder for an L1 type.
-}
codecType : Type pos RefChecked -> Expression
codecType l1Type =
    case l1Type of
        TBasic _ _ basic ->
            codecBasic basic

        TNamed _ _ named _ ->
            codecNamed named

        TProduct _ _ fields ->
            codecProduct (List.Nonempty.toList fields)

        TContainer _ _ container ->
            codecContainer container

        _ ->
            CG.unit


{-| Generates a field codec for a named field with an L1 type.
-}
codecTypeField : String -> String -> Type pos RefChecked -> Expression
codecTypeField name serializedName l1Type =
    case l1Type of
        TUnit _ _ ->
            codecUnit |> encoderField name serializedName

        TBasic _ _ basic ->
            codecBasic basic
                |> encoderField name serializedName

        TNamed _ _ named _ ->
            codecNamed named
                |> encoderField name serializedName

        TProduct _ _ fields ->
            codecProduct (List.Nonempty.toList fields)
                |> encoderField name serializedName

        TEmptyProduct _ _ ->
            codecProduct []
                |> encoderField name serializedName

        TContainer _ _ container ->
            codecContainerField name serializedName container

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a codec for unit.

Decodes `()`, and encodes to JSON `null`.

-}
codecUnit =
    CG.apply
        [ awsCoreKVEncodeFn "constant"
        , CG.unit
        ]


{-| Generates a codec for a basic L1 type.
-}
codecBasic : Basic -> Expression
codecBasic basic =
    case basic of
        BBool ->
            CG.apply
                [ awsCoreKVEncodeFn "bool"
                , CG.val "val"
                ]

        BInt ->
            CG.apply
                [ awsCoreKVEncodeFn "int"
                , CG.val "val"
                ]

        BReal ->
            CG.apply
                [ awsCoreKVEncodeFn "float"
                , CG.val "val"
                ]

        BString ->
            CG.apply
                [ awsCoreKVEncodeFn "string"
                , CG.val "val"
                ]


codecNamed named =
    CG.fun (Naming.safeCCL named ++ "KVEncoder")


codecContainer : Container pos RefChecked -> Expression
codecContainer container =
    case container of
        CList l1Type ->
            CG.apply [ awsCoreKVEncodeFn "list", codecType l1Type, CG.val "val" ]
                |> CG.parens

        CSet l1Type ->
            CG.apply [ awsCoreKVEncodeFn "set", codecType l1Type, CG.val "val" ]
                |> CG.parens

        CDict l1keyType l1valType ->
            codecDict l1keyType l1valType

        COptional l1Type ->
            CG.apply [ awsCoreKVEncodeFn "maybe", codecType l1Type, CG.val "val" ]
                |> CG.parens


codecDict : Type pos RefChecked -> Type pos RefChecked -> Expression
codecDict l1keyType l1valType =
    case l1keyType of
        TNamed _ _ name (RcRestricted basic) ->
            CG.apply
                [ awsCoreKVEncodeFn "dict"
                , codecType l1valType
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
                [ awsCoreKVEncodeFn "dict"
                , codecType l1valType
                , CG.apply
                    [ CG.fqFun refinedMod "stringDict"
                    , CG.val (Naming.safeCCL name)
                    , CG.val "identity"
                    , CG.val "val"
                    ]
                    |> CG.parens
                ]

        _ ->
            CG.apply [ awsCoreKVEncodeFn "dict", codecType l1valType, CG.val "val" ]
                |> CG.parens


{-| Generates a codec for an L1 product type that has been named as an alias.
The alias name is also the constructor function for the type.
-}
codecNamedProduct : PropertiesAPI pos -> String -> List ( String, Type pos RefChecked, L1.Properties ) -> ResultME StringEncodeError Expression
codecNamedProduct propertiesApi name fields =
    ResultME.map
        (\fieldEncoders ->
            let
                typeName =
                    Naming.safeCCU name

                impl =
                    CG.pipe
                        (fieldEncoders |> CG.list)
                        [ CG.fqFun awsCoreKVEncodeMod "object"
                        ]
            in
            impl
        )
        (encoderFields propertiesApi fields)


{-| Generates a codec for an L1 product type that does not have a name.
Without a name there is no constructor function for the product, so it must be
built explicitly by its fields.
-}
codecProduct : List ( String, Type pos RefChecked, L1.Properties ) -> Expression
codecProduct fields =
    CG.string "codecProduct"


{-| Generates a field codec for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `KVEncoder.optionalField`.
-}
codecContainerField : String -> String -> Container pos RefChecked -> Expression
codecContainerField name serializedName container =
    case container of
        CList l1Type ->
            CG.apply [ awsCoreKVEncodeFn "list", codecType l1Type ]
                |> CG.parens
                |> encoderField name serializedName

        CSet l1Type ->
            CG.apply [ awsCoreKVEncodeFn "set", codecType l1Type ]
                |> CG.parens
                |> encoderField name serializedName

        CDict l1keyType l1valType ->
            codecDict l1keyType l1valType
                |> encoderField name serializedName

        COptional l1Type ->
            codecType l1Type
                |> encoderOptionalField name serializedName


{-| Outputs encoders for a list of fields and terminates the list with `Encoder.buildObject`.
Helper function useful when building record encoders.
-}
encoderFields : PropertiesAPI pos -> List ( String, Type pos RefChecked, L1.Properties ) -> ResultME StringEncodeError (List Expression)
encoderFields propertiesApi fields =
    let
        fieldMapperFn ( fieldName, l1Type, fprops ) accum =
            let
                maybeSerName =
                    (propertiesApi.field fprops).getOptionalStringProperty "serializedName"
            in
            case maybeSerName of
                Ok (Just serName) ->
                    (codecTypeField fieldName serName l1Type |> Ok) :: accum

                Ok Nothing ->
                    (codecTypeField fieldName fieldName l1Type |> Ok) :: accum

                Err err ->
                    Err err :: accum
    in
    List.foldl fieldMapperFn [] fields
        |> ResultME.combineList
        |> ResultME.mapError L3Error


{-| Helper function for building field encoders.
-}
encoderField : String -> String -> Expression -> Expression
encoderField name serializedName expr =
    CG.applyBinOp
        (CG.tuple
            [ CG.string serializedName
            , CG.access (CG.val "val") (Naming.safeCCL name)
            ]
        )
        CG.piper
        (CG.apply [ CG.fqFun awsCoreKVEncodeMod "field", expr ])


{-| Helper function for building optional field encoders.
-}
encoderOptionalField : String -> String -> Expression -> Expression
encoderOptionalField name serializedName expr =
    CG.applyBinOp
        (CG.tuple
            [ CG.string serializedName
            , CG.access (CG.val "val") (Naming.safeCCL name)
            ]
        )
        CG.piper
        (CG.apply [ CG.fqFun awsCoreKVEncodeMod "optional", expr ])


basicToKVFun : Basic -> ( Expression, Linkage )
basicToKVFun basic =
    case basic of
        L1.BBool ->
            ( CG.fqFun awsCoreKVEncodeMod "bool"
            , CG.emptyLinkage
                |> CG.addImport awsCoreKVEncodeImport
            )

        L1.BInt ->
            ( CG.fqFun awsCoreKVEncodeMod "int"
            , CG.emptyLinkage
            )

        L1.BReal ->
            ( CG.fqFun awsCoreKVEncodeMod "float"
            , CG.emptyLinkage
            )

        L1.BString ->
            ( CG.fqFun awsCoreKVEncodeMod "string"
            , CG.emptyLinkage
            )



--== Helper Functions


dummyFn : String -> ( Declaration, Linkage )
dummyFn name =
    ( CG.funDecl Nothing Nothing name [] CG.unit, CG.emptyLinkage )


awsCoreKVEncodeMod : List String
awsCoreKVEncodeMod =
    [ "AWS", "Core", "KVEncode" ]


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


awsCoreKVEncodeFn : String -> Expression
awsCoreKVEncodeFn =
    CG.fqFun awsCoreKVEncodeMod


awsCoreKVEncodeImport : Import
awsCoreKVEncodeImport =
    CG.importStmt awsCoreKVEncodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "KVPairs" ])


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

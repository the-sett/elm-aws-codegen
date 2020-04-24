module Templates.StringEncode exposing (..)

{-| Code generators for URL paths, query arguments or header fields in formats acceptable
to AWS services.
-}

import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)
import Elm.FunDecl as FunDecl exposing (FunDecl, FunGen)
import Elm.Helper as Util
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, Restricted(..), Type(..))
import L2 exposing (RefChecked(..))
import List.Nonempty
import Maybe.Extra
import Naming
import Set exposing (Set)



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
                [ CG.fqFun awsCoreEncodeMod "bool"
                , expr
                ]
            , CG.emptyLinkage
                |> CG.addImport awsCoreEncodeImport
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
typeToString : Type pos L2.RefChecked -> Expression -> Maybe ( Expression, Linkage )
typeToString l2type expr =
    case l2type of
        TBasic _ _ basic ->
            basicToString basic expr
                |> Just

        TNamed _ _ refName ref ->
            case ref of
                L2.RcTBasic basic ->
                    basicToString basic expr
                        |> Just

                L2.RcRestricted basic ->
                    CG.apply
                        [ CG.fqFun refinedMod "unbox"
                        , Naming.safeCCL refName |> CG.val
                        , expr
                        ]
                        |> basicToString basic
                        |> Tuple.mapSecond (CG.addImport refinedImport)
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing



--===  Conversion of all types (except functions) to key-value string pairs.
-- An example encoder to the query format - list of string tuples.
--
-- type alias CreateInstanceProfileRequest =
--     { instanceProfileName : String
--     , path : Maybe String
--     }
--
-- createInstanceProfileRequestEncoder : CreateInstanceProfileRequest -> List ( String, String )
-- createInstanceProfileRequestEncoder data =
--     []
--         |> AWS.Core.Encode.addOneToQueryArgs identity "InstanceProfileName" data.instanceProfileName
--         |> (case data.path of
--                 Just value ->
--                     AWS.Core.Encode.addOneToQueryArgs identity "Path" value
--
--                 Nothing ->
--                     AWS.Core.Encode.unchangedQueryArgs
--            )
--
--
--== Helper Functions


dummyFn : String -> ( Declaration, Linkage )
dummyFn name =
    ( CG.funDecl Nothing Nothing name [] CG.unit, CG.emptyLinkage )


awsCoreEncodeMod : List String
awsCoreEncodeMod =
    [ "AWS", "Core", "Encode" ]


codecMod : List String
codecMod =
    [ "Codec" ]


decodeMod : List String
decodeMod =
    [ "Json", "Decode" ]


encodeMod : List String
encodeMod =
    [ "Json", "Encode" ]


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


codecFn : String -> Expression
codecFn =
    CG.fqFun codecMod


awsCoreEncodeImport : Import
awsCoreEncodeImport =
    CG.importStmt awsCoreEncodeMod Nothing Nothing


codecImport : Import
codecImport =
    CG.importStmt codecMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Codec" ])


decodeImport : Import
decodeImport =
    CG.importStmt decodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Decoder" ])


encodeImport : Import
encodeImport =
    CG.importStmt encodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Value" ])


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

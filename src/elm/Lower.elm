module Lower exposing (..)

{-| Lower defines a transformation to lower the AWS API model into a model that
is better suited to code generation of stubs for calling the AWS APIs. This does
the following things:

1.  The AWS service model uses refined type to describe restrictions on parameters
    in requests and responses of AWS services. For the purposes of generated stub
    code,those are ignored, and the underlying base type, such as String or Int
    is used.

    (Note: Enums are left in since they can be represented by custom types.)

2.  Shorten aliases to basic types. Sometimes basic types are declared in the AWS
    stubs and sometimes aliases to basic types are created by 1. Aliases to basic
    types are removed,and usages of them as types are replaced by the basic type
    directly.

-}

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, Restricted(..), Type(..))
import L2 exposing (RefChecked)
import L3
import List.Nonempty as Nonempty


transform : L3.L3 () -> L3.L3 ()
transform awsModel =
    { properties = awsModel.properties
    , declarations =
        deRestrict awsModel.declarations
            |> deAlias
    }



-- Replace restricted types by Int and String.


deRestrict : Dict String (Declarable () RefChecked) -> Dict String (Declarable () RefChecked)
deRestrict decls =
    Dict.map (\k v -> deRestrictDecl v) decls


deRestrictDecl : Declarable () RefChecked -> Declarable () RefChecked
deRestrictDecl decl =
    case decl of
        DAlias pos props alias ->
            DAlias pos props alias

        DSum pos props fields ->
            DSum pos props fields

        DEnum pos props val ->
            DEnum pos props val

        DRestricted pos props res ->
            DAlias pos props (deRestrictRestricted pos res)


deRestrictRestricted : () -> Restricted -> Type () RefChecked
deRestrictRestricted pos res =
    case res of
        RInt _ ->
            TBasic pos BInt

        RString _ ->
            TBasic pos BString



-- Remove aliases to basic types.


deAlias : Dict String (Declarable () RefChecked) -> Dict String (Declarable () RefChecked)
deAlias decls =
    let
        ( declsWithoutBasicAliases, basicAliases ) =
            findBasicAliases decls
    in
    deAliasDecls basicAliases declsWithoutBasicAliases


findBasicAliases : Dict String (Declarable () RefChecked) -> ( Dict String (Declarable () RefChecked), Dict String (Type () RefChecked) )
findBasicAliases decls =
    Dict.foldl
        (\name decl ( accumDecls, accumBasicAliases ) ->
            case decl of
                DAlias pos props (TUnit upos) ->
                    ( accumDecls, Dict.insert name (TUnit upos) accumBasicAliases )

                DAlias pos props (TBasic bpos basic) ->
                    ( accumDecls, Dict.insert name (TBasic bpos basic) accumBasicAliases )

                _ ->
                    ( Dict.insert name decl accumDecls, accumBasicAliases )
        )
        ( Dict.empty, Dict.empty )
        decls


deAliasDecls : Dict String (Type () RefChecked) -> Dict String (Declarable () RefChecked) -> Dict String (Declarable () RefChecked)
deAliasDecls basicAliases decls =
    Dict.map (\name decl -> deAliasDecl basicAliases decl) decls


deAliasDecl : Dict String (Type () RefChecked) -> Declarable () RefChecked -> Declarable () RefChecked
deAliasDecl basicAliases decl =
    case decl of
        DAlias pos props alias ->
            DAlias pos props (deAliasType basicAliases alias)

        DSum pos props constructors ->
            DSum pos props (Nonempty.map (deAliasConstructor basicAliases) constructors)

        DEnum pos props val ->
            DEnum pos props val

        DRestricted pos props res ->
            DRestricted pos props res


deAliasConstructor : Dict String (Type () RefChecked) -> ( String, List (Field () RefChecked) ) -> ( String, List (Field () RefChecked) )
deAliasConstructor basicAliases ( name, fields ) =
    ( name, List.map (deAliasField basicAliases) fields )


deAliasType : Dict String (Type () RefChecked) -> Type () RefChecked -> Type () RefChecked
deAliasType basicAliases type_ =
    case type_ of
        TNamed pos name ref ->
            Dict.get name basicAliases
                |> Maybe.withDefault (TNamed pos name ref)

        TProduct pos fields ->
            TProduct pos (Nonempty.map (deAliasField basicAliases) fields)

        TContainer pos container ->
            TContainer pos (deAliasContainer basicAliases container)

        TFunction pos argType resType ->
            TFunction pos (deAliasType basicAliases argType) (deAliasType basicAliases resType)

        _ ->
            type_


deAliasField : Dict String (Type () RefChecked) -> Field () RefChecked -> Field () RefChecked
deAliasField basicAliases ( name, ftype, props ) =
    ( name, deAliasType basicAliases ftype, props )


deAliasContainer : Dict String (Type () RefChecked) -> Container () RefChecked -> Container () RefChecked
deAliasContainer basicAliases container =
    case container of
        CList ltype ->
            CList (deAliasType basicAliases ltype)

        CSet stype ->
            CSet (deAliasType basicAliases stype)

        CDict ktype vtype ->
            CDict (deAliasType basicAliases ktype) (deAliasType basicAliases vtype)

        COptional otype ->
            COptional (deAliasType basicAliases otype)

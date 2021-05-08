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
import L1 exposing (Basic(..), Declarable(..), Restricted(..), Type(..))


transform awsModel =
    { properties = awsModel.properties
    , declarations = deRestrictDecls awsModel.declarations
    }


deRestrictDecls decls =
    Dict.map (\k v -> deRestrictDecl v) decls


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


deRestrictRestricted pos res =
    case res of
        RInt _ ->
            TBasic pos BInt

        RString _ ->
            TBasic pos BString

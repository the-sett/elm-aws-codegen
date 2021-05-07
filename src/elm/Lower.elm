module Lower exposing (..)

{-| The AWS service model uses refined type to describe restrictions on parameters
in requests and responses of AWS services. For the purposes of generated stub code,
those are ignored, and the underlying base type, such as String or Int is used.

If bad parametes are passed to an AWS service, the service will response with an
HTTP error code.

This lowering tranforms all restricted types into their underlying base types.

-}

import Dict exposing (Dict)
import L1 exposing (Basic(..), Declarable(..), Restricted(..), Type(..))


transform awsModel =
    { properties = awsModel.properties
    , declarations = lowerDecls awsModel.declarations
    }


lowerDecls decls =
    Dict.map (\k v -> lowerDecl v) decls


lowerDecl decl =
    case decl of
        DAlias pos props alias ->
            DAlias pos props alias

        DSum pos props fields ->
            DSum pos props fields

        DEnum pos props val ->
            DEnum pos props val

        DRestricted pos props res ->
            DAlias pos props (lowerRestricted pos res)


lowerRestricted pos res =
    case res of
        RInt _ ->
            TBasic pos BInt

        RString _ ->
            TBasic pos BString

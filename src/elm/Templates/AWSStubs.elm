module Templates.AWSStubs exposing
    ( elmEnumStyleEnum
    , generate
    , locationEnum
    , processorImpl
    , protocolEnum
    , signerEnum
    , topLevelEnum
    )

import AWS.Core.Service exposing (Protocol(..), Signer(..))
import Dict exposing (Dict)
import Documentation
import Elm.CodeGen as CG exposing (Declaration, Expression, File, Import, LetDeclaration, Linkage, Module, Pattern, TopLevelExpose, TypeAnnotation)
import Elm.Codec
import Elm.Encode
import Elm.FunDecl as FunDecl exposing (defaultOptions)
import Elm.Lang
import Enum exposing (Enum)
import Errors exposing (Error, ErrorBuilder)
import HttpMethod exposing (HttpMethod)
import L1 exposing (Declarable(..), PropSpec(..), Properties, Property(..), Type(..))
import L2 exposing (L2)
import L3 exposing (DefaultProperties, L3, L3Error(..), ProcessorImpl, PropertiesAPI)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Maybe.Extra
import Naming
import ResultME exposing (ResultME)
import SourcePos exposing (SourceLines)
import UrlParser exposing (UrlPart(..))


type AWSStubsError
    = CheckedPropertyMissing String PropSpec
    | CheckedPropertyWrongKind String PropSpec
    | DerefDeclMissing String
    | UrlDidNotParse String


l3ToAwsStubsError : L3.L3Error -> AWSStubsError
l3ToAwsStubsError err =
    case err of
        L3.CheckedPropertyMissing name propSpec ->
            CheckedPropertyMissing name propSpec

        L3.CheckedPropertyWrongKind name propSpec ->
            CheckedPropertyWrongKind name propSpec

        L3.DerefDeclMissing name ->
            DerefDeclMissing name


{-| The error catalogue for this processor.
-}
errorCatalogue =
    Dict.fromList
        [ ( 301
          , { title = "Required Property Missing"
            , body = "The required property []{arg|key=name } was not set."
            }
          )
        , ( 302
          , { title = "Property is the Wrong Kind"
            , body = "The required property []{arg|key=name } is the wrong kind."
            }
          )
        , ( 303
          , { title = "Name Type Alias Could Not Be Found"
            , body = "The type alias []{arg|key=name } could not be found."
            }
          )
        , ( 304
          , { title = "Unable to Parse URL Spec"
            , body = "Parsing the URL specification gave this error: []{arg|key=errMsg }."
            }
          )
        ]


errorBuilder : ErrorBuilder pos AWSStubsError
errorBuilder posFn err =
    case err of
        CheckedPropertyMissing name propSpec ->
            Errors.lookupError errorCatalogue
                301
                (Dict.fromList [ ( "name", name ) ])
                []

        CheckedPropertyWrongKind name propSpec ->
            Errors.lookupError errorCatalogue
                302
                (Dict.fromList [ ( "name", name ) ])
                []

        DerefDeclMissing name ->
            Errors.lookupError errorCatalogue
                303
                (Dict.fromList [ ( "name", name ) ])
                []

        UrlDidNotParse errMsg ->
            Errors.lookupError errorCatalogue
                304
                (Dict.fromList
                    [ ( "errMsg", errMsg )
                    ]
                )
                []


processorImpl : ProcessorImpl pos AWSStubsError
processorImpl =
    { name = "AWSStubs"
    , defaults = defaultProperties
    , check = check
    , buildError = errorBuilder
    }


protocolEnum : Enum String
protocolEnum =
    Enum.define
        [ "EC2"
        , "JSON"
        , "QUERY"
        , "REST_JSON"
        , "REST_XML"
        ]
        identity


signerEnum : Enum String
signerEnum =
    Enum.define
        [ "SignS3"
        , "SignV4"
        ]
        identity


elmEnumStyleEnum : Enum String
elmEnumStyleEnum =
    Enum.define
        [ "customType"
        , "guardedType"
        , "listOfStrings"
        ]
        identity


locationEnum : Enum String
locationEnum =
    Enum.define
        [ "header"
        , "queryString"
        , "statusCode"
        , "uri"
        , "body"
        ]
        identity


topLevelEnum : Enum String
topLevelEnum =
    Enum.define
        [ "request"
        , "response"
        ]
        identity


defaultProperties : DefaultProperties
defaultProperties =
    { top =
        L1.defineProperties
            [ ( "name", PSQName )
            , ( "endpointPrefix", PSString )
            , ( "apiVersion", PSString )
            , ( "protocol", PSEnum protocolEnum )
            , ( "signer", PSEnum signerEnum )
            ]
            [ ( "isRegional", PBool False )
            , ( "xmlNamespace", POptional PSString Nothing )
            , ( "targetPrefix", POptional PSString Nothing )
            , ( "signingName", POptional PSString Nothing )
            , ( "jsonVersion", POptional PSString Nothing )
            , ( "documentation", POptional PSString Nothing )
            ]
    , alias =
        L1.defineProperties
            []
            [ ( "exclude", PBool False )
            , ( "documentation", POptional PSString Nothing )
            , ( "topLevel", POptional (PSEnum topLevelEnum) Nothing )
            ]
    , sum =
        L1.defineProperties []
            [ ( "exclude", PBool False )
            , ( "documentation", POptional PSString Nothing )
            , ( "topLevel", POptional (PSEnum topLevelEnum) Nothing )
            ]
    , enum =
        L1.defineProperties
            []
            [ ( "exclude", PBool False )
            , ( "elmEnumStyle", PEnum elmEnumStyleEnum "customType" )
            , ( "documentation", POptional PSString Nothing )
            , ( "topLevel", POptional (PSEnum topLevelEnum) Nothing )
            ]
    , restricted =
        L1.defineProperties
            []
            [ ( "exclude", PBool False )
            , ( "documentation", POptional PSString Nothing )
            , ( "topLevel", POptional (PSEnum topLevelEnum) Nothing )
            ]
    , fields =
        L1.defineProperties
            [ ( "location", PSEnum locationEnum ) ]
            []
    , unit = L1.defineProperties [] []
    , basic = L1.defineProperties [] []
    , named = L1.defineProperties [] []
    , product = L1.defineProperties [] []
    , emptyProduct = L1.defineProperties [] []
    , container = L1.defineProperties [] []
    , function =
        L1.defineProperties
            [ ( "url", PSString )
            , ( "httpMethod", PSString )
            ]
            []
    }


check : L3 pos -> ResultME err (L3 pos)
check l3 =
    -- Debug.todo "check"
    l3 |> Ok


generate : (pos -> SourceLines) -> PropertiesAPI pos -> L3 pos -> ResultME Error File
generate posFn propertiesApi model =
    ResultME.map5
        (\( serviceFn, serviceLinkage ) ( endpoints, operationsLinkage ) ( types, typeDeclLinkage ) ( codecs, codecsLinkage ) documentation ->
            let
                declarations =
                    codecs
                        |> List.append types
                        |> List.append endpoints
                        |> (::) serviceFn

                linkages =
                    [ serviceLinkage, operationsLinkage, typeDeclLinkage, codecsLinkage ]

                ( imports, exposings ) =
                    CG.combineLinkage linkages

                doc =
                    documentation
                        |> Maybe.map Documentation.htmlToFileComment
                        |> Maybe.withDefault CG.emptyFileComment
                        |> CG.markdown "# Service definition."
                        |> CG.docTagsFromExposings (Tuple.second serviceLinkage)
                        |> CG.markdown "# Service endpoints."
                        |> CG.docTagsFromExposings (Tuple.second operationsLinkage)
                        |> CG.markdown "# API data model."
                        |> CG.docTagsFromExposings (Tuple.second typeDeclLinkage)
                        |> CG.markdown "# Codecs for the data model."
                        |> CG.docTagsFromExposings (Tuple.second codecsLinkage)
            in
            module_ propertiesApi model exposings
                |> ResultME.map (\moduleSpec -> CG.file moduleSpec imports declarations (Just doc))
        )
        (service propertiesApi model)
        (operations propertiesApi model)
        (typeDeclarations propertiesApi model)
        (jsonCodecs propertiesApi model)
        (propertiesApi.top.getOptionalStringProperty "documentation"
            |> ResultME.mapError l3ToAwsStubsError
        )
        |> ResultME.flatten
        |> ResultME.mapError (errorBuilder posFn)



--== Module Specification (with exposing).


module_ : PropertiesAPI pos -> L3 pos -> List TopLevelExpose -> ResultME AWSStubsError Module
module_ propertiesApi model exposings =
    propertiesApi.top.getQNameProperty "name"
        |> ResultME.map (\path -> CG.normalModule path exposings)
        |> ResultME.mapError l3ToAwsStubsError



--== Service Definition


service : PropertiesAPI pos -> L3 pos -> ResultME AWSStubsError ( Declaration, Linkage )
service propertiesApi model =
    propertiesApi.top.getBoolProperty "isRegional"
        |> ResultME.andThen
            (\isRegional ->
                if isRegional then
                    regionalService propertiesApi model

                else
                    globalService propertiesApi model
            )
        |> ResultME.mapError l3ToAwsStubsError


optionsFn : PropertiesAPI pos -> L3 pos -> ResultME L3.L3Error LetDeclaration
optionsFn propertiesApi model =
    ResultME.map4
        (\jsonVersion signingName targetPrefix xmlNamespace ->
            let
                jsonVersionOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun coreServiceMod "setJsonVersion", CG.string name ])
                        jsonVersion

                signingNameOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun coreServiceMod "setSigningName", CG.string name ])
                        signingName

                targetPrefixOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun coreServiceMod "setTargetPrefix", CG.string name ])
                        targetPrefix

                xmlNamespaceOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun coreServiceMod "setXmlNamespace", CG.string name ])
                        xmlNamespace

                options =
                    [ jsonVersionOption, signingNameOption, targetPrefixOption, xmlNamespaceOption ] |> Maybe.Extra.values
            in
            (case options of
                [] ->
                    CG.fun "identity"

                op :: ops ->
                    CG.chain op (List.map CG.parens ops)
            )
                |> CG.letFunction "optionsFn" []
        )
        (propertiesApi.top.getOptionalStringProperty "jsonVersion")
        (propertiesApi.top.getOptionalStringProperty "signingName")
        (propertiesApi.top.getOptionalStringProperty "targetPrefix")
        (propertiesApi.top.getOptionalStringProperty "xmlNamespace")


regionalService : PropertiesAPI pos -> L3 pos -> ResultME L3.L3Error ( Declaration, Linkage )
regionalService propertiesApi model =
    ResultME.map5
        (\endpointPrefix apiVersion protocol signer options ->
            let
                sig =
                    CG.funAnn
                        (CG.fqTyped coreServiceMod "Region" [])
                        (CG.fqTyped coreServiceMod "Service" [])

                impl =
                    CG.apply
                        [ CG.fqFun coreServiceMod "defineRegional"
                        , CG.string endpointPrefix
                        , CG.string apiVersion
                        , CG.fqVal coreServiceMod protocol
                        , CG.fqVal coreServiceMod signer
                        , CG.fun "optionsFn"
                        ]
                        |> CG.letExpr [ options ]

                doc =
                    CG.emptyDocComment
                        |> CG.markdown "Configuration for this service."
            in
            ( CG.funDecl
                (Just doc)
                (Just sig)
                "service"
                []
                impl
            , CG.emptyLinkage
                |> CG.addImport (CG.importStmt coreServiceMod Nothing Nothing)
                |> CG.addExposing (CG.funExpose "service")
            )
        )
        (propertiesApi.top.getStringProperty "endpointPrefix")
        (propertiesApi.top.getStringProperty "apiVersion")
        (propertiesApi.top.getEnumProperty protocolEnum "protocol")
        (propertiesApi.top.getEnumProperty signerEnum "signer")
        (optionsFn propertiesApi model)


globalService : PropertiesAPI pos -> L3 pos -> ResultME L3.L3Error ( Declaration, Linkage )
globalService propertiesApi model =
    ResultME.map5
        (\endpointPrefix apiVersion protocol signer options ->
            let
                sig =
                    CG.fqTyped coreServiceMod "Service" []

                impl =
                    CG.apply
                        [ CG.fqFun coreServiceMod "defineGlobal"
                        , CG.string endpointPrefix
                        , CG.string apiVersion
                        , CG.fqVal coreServiceMod protocol
                        , CG.fqVal coreServiceMod signer
                        , CG.fun "optionsFn"
                        ]
                        |> CG.letExpr [ options ]

                doc =
                    CG.emptyDocComment
                        |> CG.markdown "Configuration for this service."
            in
            ( CG.funDecl
                (Just doc)
                (Just sig)
                "service"
                []
                impl
            , CG.emptyLinkage
                |> CG.addImport (CG.importStmt coreServiceMod Nothing Nothing)
                |> CG.addExposing (CG.funExpose "service")
            )
        )
        (propertiesApi.top.getStringProperty "endpointPrefix")
        (propertiesApi.top.getStringProperty "apiVersion")
        (propertiesApi.top.getEnumProperty protocolEnum "protocol")
        (propertiesApi.top.getEnumProperty signerEnum "signer")
        (optionsFn propertiesApi model)



--== Operations


operations : PropertiesAPI pos -> L3 pos -> ResultME AWSStubsError ( List Declaration, Linkage )
operations propertiesApi model =
    filterDictByProps propertiesApi (notPropFilter isExcluded) model.declarations
        |> ResultME.map (Dict.map (operation propertiesApi model))
        |> ResultME.map Dict.values
        |> ResultME.map ResultME.combineList
        |> ResultME.mapError l3ToAwsStubsError
        |> ResultME.flatten
        |> ResultME.map combineDeclarations


operation :
    PropertiesAPI pos
    -> L3 pos
    -> String
    -> L1.Declarable pos L2.RefChecked
    -> ResultME AWSStubsError ( List Declaration, Linkage )
operation propertiesApi model name decl =
    case decl of
        DAlias pos _ (TFunction funpos props request response) ->
            requestFn
                propertiesApi
                model
                (propertiesApi.declarable decl)
                (propertiesApi.type_ (TFunction funpos props request response))
                name
                pos
                request
                response

        _ ->
            ( [], CG.emptyLinkage ) |> Ok


requestFn :
    PropertiesAPI pos
    -> L3 pos
    -> L3.PropertyGet
    -> L3.PropertyGet
    -> String
    -> pos
    -> L1.Type pos L2.RefChecked
    -> L1.Type pos L2.RefChecked
    -> ResultME AWSStubsError ( List Declaration, Linkage )
requestFn propertiesApi model declPropertyGet funPropertyGet name pos request response =
    ResultME.map3
        (requestFnFromParams propertiesApi model name request response)
        (funPropertyGet.getStringProperty "url"
            |> ResultME.mapError l3ToAwsStubsError
        )
        (funPropertyGet.getStringProperty "httpMethod"
            |> ResultME.mapError l3ToAwsStubsError
        )
        (declPropertyGet.getOptionalStringProperty "documentation"
            |> ResultME.mapError l3ToAwsStubsError
        )
        |> ResultME.flatten


requestFnFromParams :
    PropertiesAPI pos
    -> L3 pos
    -> String
    -> L1.Type pos L2.RefChecked
    -> L1.Type pos L2.RefChecked
    -> String
    -> String
    -> Maybe String
    -> ResultME AWSStubsError ( List Declaration, Linkage )
requestFnFromParams propertiesApi model name request response urlSpec httpMethod documentation =
    ResultME.map
        (\{ maybeRequestType, argPatterns, encoder, jsonBody, requestLinkage, url } ->
            let
                ( responseType, responseDecoder, responseLinkage ) =
                    requestFnResponse name response

                wrappedResponseType =
                    CG.fqTyped coreHttpMod "Request" [ responseType ]

                requestSig =
                    case maybeRequestType of
                        Just requestType ->
                            CG.funAnn requestType wrappedResponseType

                        Nothing ->
                            CG.fqTyped coreHttpMod "Request" [ responseType ]

                requestImpl =
                    CG.apply
                        [ CG.fqFun coreHttpMod "requestWithJsonDecoder"
                        , CG.string (Naming.safeCCU name)
                        , CG.fqVal coreHttpMod httpMethod
                        , CG.string url
                        , CG.val "jsonBody"
                        , CG.val "decoder"
                        ]
                        |> CG.letExpr
                            (Maybe.Extra.values
                                [ encoder
                                , jsonBody |> Just
                                , responseDecoder |> Just
                                ]
                            )

                doc =
                    documentation
                        |> Maybe.map Documentation.htmlToDocComment
                        |> Maybe.withDefault CG.emptyDocComment
            in
            ( [ CG.funDecl
                    (Just doc)
                    (Just requestSig)
                    (Naming.safeCCL name)
                    argPatterns
                    requestImpl
              ]
            , CG.combineLinkage
                [ requestLinkage
                , responseLinkage
                , CG.emptyLinkage
                    |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
                    |> CG.addExposing (CG.funExpose (Naming.safeCCL name))
                ]
            )
        )
        (requestFnRequest propertiesApi model name urlSpec request)


{-| Figures out what the request type for the endpoint will be.

If there is no request type defined for the endpoint then 'Nothing' will be returned,
and an empty JSON body expression will be given.

The output of this is the optional request type alias, a list of patterns for the
request functions arguments, the json body and any linkage that needs to be rolled up.

-- encoder for body fields
-- add header fields
-- add url params

-}
requestFnRequest :
    PropertiesAPI pos
    -> L3 pos
    -> String
    -> String
    -> L1.Type pos L2.RefChecked
    ->
        ResultME AWSStubsError
            { maybeRequestType : Maybe TypeAnnotation
            , argPatterns : List Pattern
            , encoder : Maybe LetDeclaration
            , jsonBody : LetDeclaration
            , requestLinkage : Linkage
            , url : String
            }
requestFnRequest propertiesApi model name urlSpec request =
    case request of
        (L1.TNamed _ _ requestTypeName _) as l1RequestType ->
            ResultME.map4
                (\headerFieldsTypeDecl queryStringFieldsTypeDecl uriFieldTypeDecl bodyFieldsTypeDecl ->
                    let
                        ( loweredType, loweredLinkage ) =
                            Elm.Lang.lowerType l1RequestType

                        ( encoder, encoderLinkage ) =
                            Elm.Encode.encoder requestTypeName bodyFieldsTypeDecl
                                |> FunDecl.asLetDecl { defaultOptions | name = Just "encoder" }

                        urlWithParams =
                            UrlParser.parseUrlParams urlSpec
                                |> ResultME.fromResult
                                |> ResultME.map (buildUrlFromParams uriFieldTypeDecl)
                                |> ResultME.mapError UrlDidNotParse
                                |> Result.withDefault "/default/url"

                        --
                        -- headers obj req  =
                        --   setHeader req "Blah" obj.blah
                        --   |> setHeader req "Blah" obj.blah
                        --
                        jsonBody =
                            CG.pipe (CG.val "req")
                                [ CG.apply
                                    [ CG.fqFun codecMod "encoder"
                                    , CG.val (Naming.safeCCL requestTypeName ++ "Codec")
                                    ]
                                , CG.fqVal coreHttpMod "jsonBody"
                                ]
                                |> CG.letVal "jsonBody"

                        linkage =
                            CG.combineLinkage
                                [ CG.emptyLinkage
                                    |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
                                , loweredLinkage
                                , encoderLinkage
                                ]
                    in
                    { maybeRequestType = Just loweredType
                    , argPatterns = [ CG.varPattern "req" ]
                    , encoder = Just encoder
                    , jsonBody = jsonBody
                    , requestLinkage = linkage
                    , url = urlWithParams
                    }
                )
                (L3.deref requestTypeName model
                    |> ResultME.mapError l3ToAwsStubsError
                    |> ResultME.andThen (filterProductDecl propertiesApi isInHeader)
                )
                (L3.deref requestTypeName model
                    |> ResultME.mapError l3ToAwsStubsError
                    |> ResultME.andThen (filterProductDecl propertiesApi isInQueryString)
                )
                (L3.deref requestTypeName model
                    |> ResultME.mapError l3ToAwsStubsError
                    |> ResultME.andThen (filterProductDecl propertiesApi isInUri)
                )
                (L3.deref requestTypeName model
                    |> ResultME.mapError l3ToAwsStubsError
                    |> ResultME.andThen (filterProductDecl propertiesApi isInBody)
                )

        _ ->
            let
                emptyJsonBody =
                    CG.fqVal coreHttpMod "emptyBody"
                        |> CG.letVal "jsonBody"

                linkage =
                    CG.emptyLinkage |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
            in
            { maybeRequestType = Nothing
            , argPatterns = []
            , jsonBody = emptyJsonBody
            , encoder = Nothing
            , requestLinkage = linkage
            , url = ""
            }
                |> Ok


buildUrlFromParams : L1.Declarable pos L2.RefChecked -> List UrlPart -> String
buildUrlFromParams uriFieldTypeDecl urlParts =
    -- Generated code needs to look like:
    -- url =
    --     "/2015-03-31/functions/"
    --         ++ val.functionName
    --         ++ "/aliases/"
    --         ++ val.name
    -- Error if param not set.
    List.foldl
        (\part acc ->
            case part of
                PathLiteral lit ->
                    lit :: acc

                Param name ->
                    name :: acc
        )
        []
        urlParts
        |> List.reverse
        |> String.concat


{-| Given a product declaration as a type alias, filters its fields to get just
the ones that match the specified filter.

Note that if not fields pass the filter, a `TEmptyProduct` will be returned.

-}
filterProductDecl :
    PropertiesAPI pos
    -> PropertyFilter pos ( String, Type pos L2.RefChecked, Properties )
    -> L1.Declarable pos L2.RefChecked
    -> ResultME AWSStubsError (L1.Declarable pos L2.RefChecked)
filterProductDecl propertiesApi filter decl =
    let
        fieldsToProductOrEmpty pos props filtered =
            case filtered of
                [] ->
                    TEmptyProduct pos props

                f :: fs ->
                    Nonempty f fs |> TProduct pos props
    in
    case decl of
        DAlias dpos dprops (TProduct tpos tprops fields) ->
            filterNonemptyByProps propertiesApi filter fields
                |> ResultME.map (fieldsToProductOrEmpty tpos tprops)
                |> ResultME.map (DAlias dpos dprops)
                |> ResultME.mapError l3ToAwsStubsError

        _ ->
            decl |> Ok


{-| Figures out what response type for the endpoint will be.

If there is no response type defined for the endpoint then `()` is used to indicate
that the response has completed but returned no data.

The output of this is the response type alias for the endpoint, the decoder for this
expected response and any linkage that needs to be rolled up.

When there is no response shape, the decoder will be `(AWS.Core.Decode.FixedResult ()`.

-}
requestFnResponse :
    String
    -> L1.Type pos L2.RefChecked
    -> ( TypeAnnotation, LetDeclaration, Linkage )
requestFnResponse name response =
    case response of
        (L1.TNamed _ _ responseTypeName _) as l1ResponseType ->
            let
                ( loweredType, loweredLinkage ) =
                    Elm.Lang.lowerType l1ResponseType

                responseType =
                    loweredType

                linkage =
                    CG.combineLinkage
                        [ CG.emptyLinkage
                            |> CG.addImport (CG.importStmt coreDecodeMod Nothing Nothing)
                        , loweredLinkage
                        ]

                decoder =
                    CG.apply
                        [ CG.fqFun codecMod "decoder"
                        , CG.val (Naming.safeCCL responseTypeName ++ "Codec")
                        ]
                        |> CG.parens
                        |> CG.letVal "decoder"
            in
            ( responseType, decoder, linkage )

        _ ->
            let
                linkage =
                    CG.emptyLinkage
                        |> CG.addImport (CG.importStmt coreDecodeMod Nothing Nothing)
                        |> CG.addImport decodeImport

                decoder =
                    CG.apply
                        [ CG.fqVal decodeMod "succeed"
                        , CG.unit
                        ]
                        |> CG.letVal "decoder"

                responseType =
                    CG.unitAnn
            in
            ( responseType, decoder, linkage )



--== Types and Codecs


typeDeclarations :
    PropertiesAPI pos
    -> L3 pos
    -> ResultME AWSStubsError ( List Declaration, Linkage )
typeDeclarations propertiesApi model =
    filterDictByProps propertiesApi (notPropFilter isExcluded) model.declarations
        |> ResultME.mapError l3ToAwsStubsError
        |> ResultME.map (Dict.map (typeDeclaration propertiesApi))
        |> ResultME.map Dict.values
        |> ResultME.map ResultME.combineList
        |> ResultME.flatten
        |> ResultME.map combineDeclarations


typeDeclaration :
    PropertiesAPI pos
    -> String
    -> L1.Declarable pos L2.RefChecked
    -> ResultME AWSStubsError ( List Declaration, Linkage )
typeDeclaration propertiesAPI name decl =
    case decl of
        DAlias _ _ (TFunction _ _ _ _) ->
            ( [], CG.emptyLinkage ) |> Ok

        _ ->
            let
                doc =
                    CG.emptyDocComment
                        |> CG.markdown ("The " ++ Naming.safeCCU name ++ " data model.")
            in
            Elm.Lang.typeDecl name doc decl |> Ok


jsonCodecs : PropertiesAPI pos -> L3 pos -> ResultME AWSStubsError ( List Declaration, Linkage )
jsonCodecs propertiesApi model =
    filterDictByProps propertiesApi (notPropFilter (orPropFilter isRequest isExcluded)) model.declarations
        |> ResultME.map (Dict.map jsonCodec)
        |> ResultME.map Dict.values
        |> ResultME.map combineDeclarations
        |> ResultME.mapError l3ToAwsStubsError


jsonCodec : String -> L1.Declarable pos L2.RefChecked -> ( List Declaration, Linkage )
jsonCodec name decl =
    case decl of
        DAlias _ _ (TFunction _ _ _ _) ->
            ( [], CG.emptyLinkage )

        _ ->
            Elm.Codec.codec name decl
                |> FunDecl.asTopLevel FunDecl.defaultOptions
                |> Tuple.mapFirst List.singleton



-- Property Filters


type alias PropertyFilter pos a =
    PropertiesAPI pos -> a -> ResultME L3.L3Error Bool


andPropFilter : PropertyFilter pos a -> PropertyFilter pos a -> PropertyFilter pos a
andPropFilter filterA filterB =
    \propertiesAPI val ->
        filterA propertiesAPI val
            |> ResultME.andThen
                (\bool ->
                    if bool then
                        filterB propertiesAPI val

                    else
                        Ok False
                )


orPropFilter : PropertyFilter pos a -> PropertyFilter pos a -> PropertyFilter pos a
orPropFilter filterA filterB =
    \propertiesAPI val ->
        filterA propertiesAPI val
            |> ResultME.andThen
                (\bool ->
                    if bool then
                        Ok True

                    else
                        filterB propertiesAPI val
                )


notPropFilter : PropertyFilter pos a -> PropertyFilter pos a
notPropFilter filterA =
    \propertiesAPI val ->
        filterA propertiesAPI val
            |> ResultME.andThen
                (\bool ->
                    if bool then
                        Ok False

                    else
                        Ok True
                )


filterDictByProps :
    PropertiesAPI pos
    -> PropertyFilter pos a
    -> Dict String a
    -> ResultME L3.L3Error (Dict String a)
filterDictByProps propertiesApi filter dict =
    let
        ( filtered, errors ) =
            Dict.foldl
                (\name val ( accum, errAccum ) ->
                    case filter propertiesApi val of
                        Ok False ->
                            ( accum, errAccum )

                        Ok True ->
                            ( Dict.insert name val accum, errAccum )

                        Err err ->
                            ( accum, Nonempty.toList err ++ errAccum )
                )
                ( Dict.empty, [] )
                dict
    in
    case errors of
        [] ->
            Ok filtered

        e :: es ->
            Err (Nonempty e es)


filterListByProps :
    PropertiesAPI pos
    -> PropertyFilter pos a
    -> List a
    -> ResultME L3.L3Error (List a)
filterListByProps propertiesApi filter vals =
    let
        ( filtered, errors ) =
            List.foldl
                (\val ( accum, errAccum ) ->
                    case filter propertiesApi val of
                        Ok False ->
                            ( accum, errAccum )

                        Ok True ->
                            ( val :: accum, errAccum )

                        Err err ->
                            ( accum, Nonempty.toList err ++ errAccum )
                )
                ( [], [] )
                vals
    in
    case errors of
        [] ->
            Ok filtered

        e :: es ->
            Err (Nonempty e es)


filterNonemptyByProps :
    PropertiesAPI pos
    -> PropertyFilter pos a
    -> Nonempty a
    -> ResultME L3.L3Error (List a)
filterNonemptyByProps propertiesApi filter vals =
    filterListByProps propertiesApi filter (Nonempty.toList vals)


isInHeader : PropertyFilter pos ( String, Type pos ref, Properties )
isInHeader =
    isInLocation "header"


isInQueryString : PropertyFilter pos ( String, Type pos ref, Properties )
isInQueryString =
    isInLocation "queryString"


isInUri : PropertyFilter pos ( String, Type pos ref, Properties )
isInUri =
    isInLocation "uri"


isInBody : PropertyFilter pos ( String, Type pos ref, Properties )
isInBody =
    isInLocation "body"


isInLocation : String -> PropertyFilter pos ( String, Type pos ref, Properties )
isInLocation location propertiesApi ( _, _, props ) =
    case (propertiesApi.field props).getEnumProperty locationEnum "location" of
        Ok val ->
            location == val |> Ok

        Err err ->
            Err err


isExcluded : PropertyFilter pos (L1.Declarable pos L2.RefChecked)
isExcluded propertiesAPI decl =
    (propertiesAPI.declarable decl).getBoolProperty "exclude"


isRequest : PropertyFilter pos (L1.Declarable pos L2.RefChecked)
isRequest propertiesAPI decl =
    case (propertiesAPI.declarable decl).getOptionalEnumProperty topLevelEnum "topLevel" of
        Ok (Just "request") ->
            Ok True

        Ok blah ->
            Ok False

        Err err ->
            Err err



-- Helpers


{-| Combines linkages from a list of declarations and linkages, into a list of declarations
and a single combined linkage.
-}
combineDeclarations : List ( List Declaration, Linkage ) -> ( List Declaration, Linkage )
combineDeclarations declList =
    List.foldl
        (\( decls, linkage ) ( declAccum, linkageAccum ) ->
            ( List.append declAccum decls, CG.combineLinkage [ linkageAccum, linkage ] )
        )
        ( [], CG.emptyLinkage )
        declList


{-| Combines linkages from a list of single declarations and linkages, into a
list of declarations and a single combined linkage.
-}
combineDeclaration : List ( Declaration, Linkage ) -> ( List Declaration, Linkage )
combineDeclaration declList =
    List.foldl
        (\( decl, linkage ) ( declAccum, linkageAccum ) ->
            ( decl :: declAccum, CG.combineLinkage [ linkageAccum, linkage ] )
        )
        ( [], CG.emptyLinkage )
        declList


decodeMod : List String
decodeMod =
    [ "Json", "Decode" ]


codecMod : List String
codecMod =
    [ "Codec" ]


coreHttpMod : List String
coreHttpMod =
    [ "AWS", "Core", "Http" ]


coreDecodeMod : List String
coreDecodeMod =
    [ "AWS", "Core", "Decode" ]


coreServiceMod : List String
coreServiceMod =
    [ "AWS", "Core", "Service" ]


decodeImport : Import
decodeImport =
    CG.importStmt decodeMod Nothing Nothing

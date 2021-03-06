module Templates.AWSStubs exposing
    ( processorImpl
    , generate
    , elmEnumStyleEnum, locationEnum, protocolEnum, signerEnum, topLevelEnum
    , isExcluded, isInBody, isInHeader, isInQueryString, isInStatusCode, isInUri, isRequest, isResponse
    , withLocationName
    )

{-| Code generator to output AWS service stubs.


# Code generator.

@docs processorImpl
@docs generate


# Property Enums.

@docs elmEnumStyleEnum, locationEnum, protocolEnum, signerEnum, topLevelEnum


# Filters on Properties.

@docs isExcluded, isInBody, isInHeader, isInQueryString, isInStatusCode, isInUri, isRequest, isResponse
@docs withLocationName

-}

import Dict exposing (Dict)
import Documentation
import Elm.CodeGen as CG exposing (Declaration, Expression, File, Import, LetDeclaration, Linkage, Module, Pattern, TopLevelExpose, TypeAnnotation)
import Elm.FunDecl as FunDecl exposing (defaultOptions)
import Elm.Json.Coding as Coding
import Elm.Lang
import Enum exposing (Enum)
import Errors exposing (Error, ErrorBuilder)
import L1 exposing (Declarable(..), Field, PropSpec(..), Properties, Property(..), Type(..))
import L2 exposing (L2)
import L3 exposing (DefaultProperties, L3, L3Error(..), ProcessorImpl, PropertiesAPI)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Maybe.Extra
import Naming
import Query exposing (PropertyFilter)
import ResultME exposing (ResultME)
import SourcePos exposing (SourceLines)
import Templates.KVDecode as KVDecode
import Templates.KVEncode as KVEncode
import Tuple3
import UrlParser exposing (UrlPart(..))



-- TODO:
-- 2. Wire up the L3 modules explcitly. Only use error builders of wired up modules,
-- don't expose them directly.
--
-- 3. Responses may contain header fields. Therefore, response decoding needs to be partial...
--
--=== This processor.


processorImpl : ProcessorImpl pos AWSStubsError
processorImpl =
    { name = "AWSStubs"
    , defaults = defaultProperties
    , check = check
    , buildError = errorBuilder
    }



--=== Wire up processors this depends on.
-- stringEncodeProcessor =
--     L3.builder posFn KVEncode.processorImpl
--=== Errors


type AWSStubsError
    = L3Error L3.L3Error
    | KVEncodeError KVEncode.KVEncodeError
    | KVDecodeError KVDecode.KVDecodeError
    | JsonCodingError Coding.JsonCodingError
    | UrlDidNotParse String
    | UnmatchedUrlParam String
    | UnsupportedType String
    | UnsupportedDeclaration String


{-| The error catalogue for this processor.
-}
errorCatalogue =
    Dict.fromList
        [ ( 401
          , { title = "Unable to Parse URL Spec"
            , body = "Parsing the URL specification gave this error: []{arg|key=errMsg }."
            }
          )
        , ( 402
          , { title = "Unmatched URL Parameter"
            , body = """
The parameter named []{arg|key=name } did not match a parameter in the URL specification.
"""
            }
          )
        , ( 403
          , { title = "Unsupported Type."
            , body = "Type not support []{arg|key=name }."
            }
          )
        , ( 404
          , { title = "Unsupported Declarable."
            , body = "Declarable not suported []{arg|key=name }."
            }
          )
        ]


errorBuilder : ErrorBuilder pos AWSStubsError
errorBuilder posFn err =
    case err of
        L3Error l3error ->
            L3.errorBuilder posFn l3error

        KVEncodeError stringEncodeError ->
            KVEncode.errorBuilder posFn stringEncodeError

        KVDecodeError stringEncodeError ->
            KVDecode.errorBuilder posFn stringEncodeError

        JsonCodingError jsonCodingError ->
            Coding.errorBuilder posFn jsonCodingError

        UrlDidNotParse errMsg ->
            Errors.lookupError errorCatalogue
                401
                (Dict.fromList [ ( "errMsg", errMsg ) ])
                []

        UnmatchedUrlParam name ->
            Errors.lookupError errorCatalogue
                402
                (Dict.fromList [ ( "name", name ) ])
                []

        UnsupportedType name ->
            Errors.lookupError errorCatalogue
                403
                (Dict.fromList [ ( "name", name ) ])
                []

        UnsupportedDeclaration name ->
            Errors.lookupError errorCatalogue
                404
                (Dict.fromList [ ( "name", name ) ])
                []


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
        , "querystring"
        , "statuscode"
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
            , ( "xmlNamespace", PSOptional PSString )
            , ( "targetPrefix", PSOptional PSString )
            , ( "signingName", PSOptional PSString )
            , ( "jsonVersion", PSOptional PSString )
            , ( "documentation", PSOptional PSString )
            ]
            [ ( "isRegional", PBool False )
            ]
    , alias =
        L1.defineProperties
            [ ( "documentation", PSOptional PSString )
            , ( "topLevel", PSOptional (PSEnum topLevelEnum) )
            , ( "url", PSString )
            , ( "httpMethod", PSString )
            ]
            [ ( "exclude", PBool False )
            , ( "kvEncode", PBool False )
            , ( "kvDecode", PBool False )
            , ( "hasErrors", PBool False )
            ]
    , sum =
        L1.defineProperties
            [ ( "documentation", PSOptional PSString )
            , ( "topLevel", PSOptional (PSEnum topLevelEnum) )
            ]
            [ ( "exclude", PBool False )
            ]
    , enum =
        L1.defineProperties
            [ ( "documentation", PSOptional PSString )
            , ( "topLevel", PSOptional (PSEnum topLevelEnum) )
            ]
            [ ( "exclude", PBool False )
            , ( "elmEnumStyle", PEnum elmEnumStyleEnum "customType" )
            , ( "kvEncode", PBool False )
            , ( "kvDecode", PBool False )
            ]
    , restricted =
        L1.defineProperties
            [ ( "documentation", PSOptional PSString )
            , ( "topLevel", PSOptional (PSEnum topLevelEnum) )
            ]
            [ ( "exclude", PBool False )
            , ( "kvEncode", PBool False )
            , ( "kvDecode", PBool False )
            ]
    , fields =
        L1.defineProperties
            [ ( "location", PSEnum locationEnum )
            , ( "serializedName", PSOptional PSString )
            , ( "documentation", PSOptional PSString )
            , ( "topLevel", PSOptional (PSEnum topLevelEnum) )
            ]
            []
    }


check : L3 pos -> ResultME err (L3 pos)
check l3 =
    -- Debug.todo "check"
    l3 |> Ok


generate : (pos -> SourceLines) -> PropertiesAPI pos -> L3 pos -> ResultME Error File
generate posFn propertiesApi model =
    ResultME.map7
        (\( serviceFn, serviceLinkage ) ( endpoints, operationsLinkage ) ( types, typeDeclLinkage ) ( codecs, codecsLinkage ) ( kvKVEncoders, kvKVEncodersLinkage ) ( kvKVDecoders, kvKVDecodersLinkage ) documentation ->
            let
                declarations =
                    kvKVEncoders
                        |> List.append kvKVDecoders
                        |> List.append codecs
                        |> List.append types
                        |> List.append endpoints
                        |> (::) serviceFn

                linkages =
                    [ serviceLinkage
                    , operationsLinkage
                    , typeDeclLinkage
                    , Tuple.mapSecond (always []) codecsLinkage
                    , Tuple.mapSecond (always []) kvKVEncodersLinkage
                    , Tuple.mapSecond (always []) kvKVDecodersLinkage
                    ]

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

                -- |> CG.markdown "# JSON Codecs for the data model."
                -- |> CG.docTagsFromExposings (Tuple.second codecsLinkage)
                -- |> CG.markdown "# Key-Value String encoders for the data model."
                -- |> CG.docTagsFromExposings (Tuple.second kvKVEncodersLinkage)
                -- |> CG.markdown "# Key-Value String decoders for the data model."
                -- |> CG.docTagsFromExposings (Tuple.second kvKVDecodersLinkage)
            in
            module_ propertiesApi model exposings
                |> ResultME.map (\moduleSpec -> CG.file moduleSpec imports declarations (Just doc))
        )
        (service propertiesApi model)
        (operations propertiesApi model)
        (typeDeclarations propertiesApi model)
        (jsonCodings propertiesApi model)
        (kvEncoders propertiesApi model)
        (kvDecoders propertiesApi model)
        (propertiesApi.top.getOptionalStringProperty "documentation"
            |> ResultME.mapError L3Error
        )
        |> ResultME.flatten
        |> ResultME.mapError (errorBuilder posFn)



--== Module Specification (with exposing).


module_ : PropertiesAPI pos -> L3 pos -> List TopLevelExpose -> ResultME AWSStubsError Module
module_ propertiesApi model exposings =
    propertiesApi.top.getQNameProperty "name"
        |> ResultME.map (\path -> CG.normalModule path exposings)
        |> ResultME.mapError L3Error



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
        |> ResultME.mapError L3Error


optionsFn : PropertiesAPI pos -> L3 pos -> ResultME L3.L3Error (List Expression)
optionsFn propertiesApi model =
    ResultME.map4
        (\jsonVersion signingName targetPrefix xmlNamespace ->
            let
                jsonVersionOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun awsConfigMod "withJsonVersion", CG.string name ])
                        jsonVersion

                signingNameOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun awsConfigMod "withSigningName", CG.string name ])
                        signingName

                targetPrefixOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun awsConfigMod "withTargetPrefix", CG.string name ])
                        targetPrefix

                xmlNamespaceOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun awsConfigMod "withXmlNamespace", CG.string name ])
                        xmlNamespace
            in
            [ jsonVersionOption, signingNameOption, targetPrefixOption, xmlNamespaceOption ] |> Maybe.Extra.values
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
                        (CG.fqTyped awsConfigMod "Region" [])
                        (CG.fqTyped awsServiceMod "Service" [])

                fromConfig =
                    CG.fqFun awsServiceMod "service"

                impl =
                    CG.pipe
                        (CG.apply
                            [ CG.fqFun awsConfigMod "defineRegional"
                            , CG.string endpointPrefix
                            , CG.string apiVersion
                            , CG.fqVal awsConfigMod protocol
                            , CG.fqVal awsConfigMod signer
                            , CG.val "region"
                            ]
                        )
                        (List.append options [ fromConfig ])

                doc =
                    CG.emptyDocComment
                        |> CG.markdown "Configuration for this service."
            in
            ( CG.funDecl
                (Just doc)
                (Just sig)
                "service"
                [ CG.varPattern "region" ]
                impl
            , CG.emptyLinkage
                |> CG.addImport (CG.importStmt awsConfigMod Nothing Nothing)
                |> CG.addImport (CG.importStmt awsServiceMod Nothing Nothing)
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
                    CG.fqTyped awsServiceMod "Service" []

                fromConfig =
                    CG.fqFun awsServiceMod "service"

                impl =
                    CG.pipe
                        (CG.apply
                            [ CG.fqFun awsConfigMod "defineRegional"
                            , CG.string endpointPrefix
                            , CG.string apiVersion
                            , CG.fqVal awsConfigMod protocol
                            , CG.fqVal awsConfigMod signer
                            ]
                        )
                        (List.append options [ fromConfig ])

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
                |> CG.addImport (CG.importStmt awsConfigMod Nothing Nothing)
                |> CG.addImport (CG.importStmt awsServiceMod Nothing Nothing)
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
    Query.filterDictByProps propertiesApi (Query.notPropFilter isExcluded) model.declarations
        |> ResultME.map (Dict.map (operation propertiesApi model))
        |> ResultME.map Dict.values
        |> ResultME.map ResultME.combineList
        |> ResultME.mapError L3Error
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
        DAlias pos _ (TFunction funpos request response) ->
            requestFn
                propertiesApi
                model
                (propertiesApi.declarable decl)
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
    -> String
    -> pos
    -> L1.Type pos L2.RefChecked
    -> L1.Type pos L2.RefChecked
    -> ResultME AWSStubsError ( List Declaration, Linkage )
requestFn propertiesApi model declPropertyGet name pos request response =
    ResultME.map4
        (requestFnFromParams propertiesApi model name request response)
        (declPropertyGet.getStringProperty "url"
            |> ResultME.mapError L3Error
        )
        (declPropertyGet.getStringProperty "httpMethod"
            |> ResultME.mapError L3Error
        )
        (declPropertyGet.getBoolProperty "hasErrors"
            |> ResultME.mapError L3Error
        )
        (declPropertyGet.getOptionalStringProperty "documentation"
            |> ResultME.mapError L3Error
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
    -> Bool
    -> Maybe String
    -> ResultME AWSStubsError ( List Declaration, Linkage )
requestFnFromParams propertiesApi model name request response urlSpec httpMethod hasErrors documentation =
    ResultME.map2
        (\{ maybeRequestType, argPatterns, encoder, setHeaders, setQueryParams, jsonBody, requestLinkage, url } ( responseType, responseDecoder, responseLinkage ) ->
            let
                errType =
                    if hasErrors then
                        CG.fqTyped awsHttpMod "AWSAppError" []

                    else
                        CG.typed "Never" []

                wrappedResponseType =
                    CG.fqTyped awsHttpMod "Request" [ errType, responseType ]

                requestSig =
                    case maybeRequestType of
                        Just requestType ->
                            CG.funAnn requestType wrappedResponseType

                        Nothing ->
                            CG.fqTyped awsHttpMod "Request" [ responseType ]

                maybeAddHeaders reqImpl =
                    Maybe.map
                        (\_ ->
                            CG.applyBinOp reqImpl
                                CG.piper
                                (CG.apply
                                    [ CG.fqVal awsHttpMod "addHeaders"
                                    , CG.parens
                                        (CG.pipe
                                            (CG.apply [ CG.val "headersEncoder", CG.val "req" ])
                                            [ CG.fqFun awsKVEncodeMod "encode" ]
                                        )
                                    ]
                                )
                        )
                        setHeaders
                        |> Maybe.withDefault reqImpl

                maybeAddQuery reqImpl =
                    Maybe.map
                        (\_ ->
                            CG.applyBinOp reqImpl
                                CG.piper
                                (CG.apply
                                    [ CG.fqVal awsHttpMod "addQuery"
                                    , CG.parens
                                        (CG.pipe
                                            (CG.apply [ CG.val "queryEncoder", CG.val "req" ])
                                            [ CG.fqFun awsKVEncodeMod "encode" ]
                                        )
                                    ]
                                )
                        )
                        setQueryParams
                        |> Maybe.withDefault reqImpl

                baseRequestImpl =
                    CG.apply
                        [ CG.fqFun awsHttpMod "request"
                        , CG.string (Naming.safeCCU name)
                        , CG.fqVal awsHttpMod httpMethod
                        , CG.val "url"
                        , CG.val "jsonBody"
                        , CG.val "decoder"
                        , if hasErrors then
                            CG.fqFun awsHttpMod "awsAppErrDecoder"

                          else
                            CG.fqFun awsHttpMod "neverAppErrDecoder"
                        ]

                requestImpl =
                    baseRequestImpl
                        |> maybeAddHeaders
                        |> maybeAddQuery
                        |> CG.letExpr
                            (Maybe.Extra.values
                                [ encoder
                                , setHeaders
                                , setQueryParams
                                , jsonBody |> Just
                                , url |> Just
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
                    |> CG.addImport (CG.importStmt awsHttpMod Nothing Nothing)
                    |> CG.addExposing (CG.funExpose (Naming.safeCCL name))
                ]
            )
        )
        (requestFnRequest propertiesApi model name urlSpec request)
        (requestFnResponse propertiesApi model name response)


{-| Figures out what the request type for the endpoint will be.

If there is no request type defined for the endpoint then 'Nothing' will be returned,
and an empty JSON body expression will be given.

The output of this is the optional request type alias, a list of patterns for the
request functions arguments, the json body and any linkage that needs to be rolled up.

-- add header fields

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
            , setHeaders : Maybe LetDeclaration
            , setQueryParams : Maybe LetDeclaration
            , jsonBody : LetDeclaration
            , requestLinkage : Linkage
            , url : LetDeclaration
            }
requestFnRequest propertiesApi model name urlSpec request =
    case request of
        (L1.TNamed _ requestTypeName _) as l1RequestType ->
            ResultME.map4
                (\headerFields queryStringFields uriFields bodyFields ->
                    ResultME.map3
                        (\( urlWithParams, urlLinkage ) ( headersFnImpl, headersFnLinkage ) ( queryFnImpl, queryFnLinkage ) ->
                            ResultME.map
                                (\( encoder, encoderLinkage ) ->
                                    let
                                        ( loweredType, loweredLinkage ) =
                                            Elm.Lang.lowerType l1RequestType

                                        jsonBody =
                                            case bodyFields of
                                                [] ->
                                                    CG.fqVal awsHttpMod "emptyBody"
                                                        |> CG.letVal "jsonBody"

                                                _ ->
                                                    CG.pipe (CG.val "req")
                                                        [ CG.val "encoder"
                                                        , CG.fqVal awsHttpMod "jsonBody"
                                                        ]
                                                        |> CG.letVal "jsonBody"

                                        linkage =
                                            CG.combineLinkage
                                                (Maybe.Extra.values
                                                    [ CG.emptyLinkage
                                                        |> CG.addImport (CG.importStmt awsHttpMod Nothing Nothing)
                                                        |> Just
                                                    , loweredLinkage |> Just
                                                    , encoderLinkage
                                                    , queryFnLinkage
                                                    , headersFnLinkage
                                                    , urlLinkage |> Just
                                                    ]
                                                )
                                    in
                                    { maybeRequestType = Just loweredType
                                    , argPatterns = [ CG.varPattern "req" ]
                                    , encoder = encoder
                                    , setHeaders = headersFnImpl
                                    , setQueryParams = queryFnImpl
                                    , jsonBody = jsonBody
                                    , requestLinkage = linkage
                                    , url = urlWithParams
                                    }
                                )
                                (case bodyFields of
                                    [] ->
                                        ( Nothing, Nothing )
                                            |> Ok

                                    b :: bs ->
                                        Coding.partialCoding propertiesApi model.declarations requestTypeName "Encoder" (Nonempty b bs)
                                            |> ResultME.map (FunDecl.asLetDecl { defaultOptions | name = Just "encoder" })
                                            |> ResultME.map (Tuple.mapBoth Just Just)
                                            |> ResultME.mapError JsonCodingError
                                )
                        )
                        (UrlParser.parseUrlParams urlSpec
                            |> ResultME.fromResult
                            |> ResultME.mapError UrlDidNotParse
                            |> ResultME.andThen (buildUrlFromParams propertiesApi uriFields)
                        )
                        (headersFn propertiesApi headerFields)
                        (queryFn propertiesApi queryStringFields)
                        |> ResultME.flatten
                )
                (Query.deref requestTypeName model.declarations
                    |> ResultME.andThen (filterProductDecl propertiesApi isInHeader)
                    |> ResultME.mapError L3Error
                )
                (Query.deref requestTypeName model.declarations
                    |> ResultME.andThen (filterProductDecl propertiesApi isInQueryString)
                    |> ResultME.mapError L3Error
                )
                (Query.deref requestTypeName model.declarations
                    |> ResultME.andThen (filterProductDecl propertiesApi isInUri)
                    |> ResultME.mapError L3Error
                )
                (Query.deref requestTypeName model.declarations
                    |> ResultME.andThen (filterProductDecl propertiesApi isInBody)
                    |> ResultME.mapError L3Error
                )
                |> ResultME.flatten

        _ ->
            let
                emptyJsonBody =
                    CG.fqVal awsHttpMod "emptyBody"
                        |> CG.letVal "jsonBody"

                linkage =
                    CG.emptyLinkage |> CG.addImport (CG.importStmt awsHttpMod Nothing Nothing)
            in
            { maybeRequestType = Nothing
            , argPatterns = []
            , jsonBody = emptyJsonBody
            , encoder = Nothing
            , setHeaders = Nothing
            , setQueryParams = Nothing
            , requestLinkage = linkage
            , url = CG.letVal "url" CG.unit
            }
                |> Ok


{-| Given a product declaration as a type alias of a product type, filters its
fields to get just the ones that match the specified filter.

Note that if no fields pass the filter, an empty list will be returned.

-}
filterProductDecl :
    PropertiesAPI pos
    -> PropertyFilter pos (Field pos L2.RefChecked)
    -> L1.Declarable pos L2.RefChecked
    -> ResultME L3.L3Error (List (Field pos L2.RefChecked))
filterProductDecl propertiesApi filter decl =
    Query.expectAlias decl
        |> ResultME.map Tuple3.third
        |> ResultME.andThen Query.expectProductOrEmpty
        |> ResultME.map Tuple.second
        |> ResultME.andThen (Query.filterListByProps propertiesApi filter)


headersFn :
    PropertiesAPI pos
    -> List (Field pos L2.RefChecked)
    -> ResultME AWSStubsError ( Maybe LetDeclaration, Maybe Linkage )
headersFn propertiesApi fields =
    let
        headersEncoderFn =
            KVEncode.partialKVEncoder propertiesApi "requestTypeName" fields
                |> ResultME.map (FunDecl.asLetDecl { defaultOptions | name = Just "headersEncoder" })
                |> ResultME.map (Tuple.mapBoth Just Just)
                |> ResultME.mapError KVEncodeError
    in
    case fields of
        [] ->
            Ok ( Nothing, Nothing )

        _ ->
            headersEncoderFn


queryFn :
    PropertiesAPI pos
    -> List (Field pos L2.RefChecked)
    -> ResultME AWSStubsError ( Maybe LetDeclaration, Maybe Linkage )
queryFn propertiesApi fields =
    let
        queryEncoderFn =
            KVEncode.partialKVEncoder propertiesApi "requestTypeName" fields
                |> ResultME.map (FunDecl.asLetDecl { defaultOptions | name = Just "queryEncoder" })
                |> ResultME.map (Tuple.mapBoth Just Just)
                |> ResultME.mapError KVEncodeError
    in
    case fields of
        [] ->
            Ok ( Nothing, Nothing )

        _ ->
            queryEncoderFn


buildUrlFromParams :
    PropertiesAPI pos
    -> List (Field pos L2.RefChecked)
    -> List UrlPart
    -> ResultME AWSStubsError ( LetDeclaration, Linkage )
buildUrlFromParams propertiesApi uriFields urlParts =
    let
        pathParts =
            List.map
                (\part ->
                    case part of
                        PathLiteral lit ->
                            ( CG.string lit, CG.emptyLinkage ) |> Ok

                        Param name ->
                            case Query.filterListByProps propertiesApi (withLocationName name) uriFields of
                                Ok (field :: _) ->
                                    fieldAsString field

                                Ok [] ->
                                    UnmatchedUrlParam name |> ResultME.error

                                Err err ->
                                    Err err |> ResultME.mapError L3Error
                )
                urlParts
                |> ResultME.combineList

        appendParts vals =
            case List.reverse vals of
                [] ->
                    CG.string ""

                part :: ps ->
                    CG.binOpChain part CG.append ps
    in
    pathParts
        |> ResultME.map List.unzip
        |> ResultME.map (Tuple.mapFirst (appendParts >> CG.letVal "url"))
        |> ResultME.map (Tuple.mapSecond CG.combineLinkage)


{-|

  - TODO: Generate toString encoder functions, these should be generated for the transitive closure of all
    url, header and query params.
  - TODO: Pass up the linkage.

-}
fieldAsString : Field pos L2.RefChecked -> ResultME AWSStubsError ( Expression, Linkage )
fieldAsString ( fname, l2type, _ ) =
    KVEncode.typeToString l2type
        (CG.access (CG.val "req") (Naming.safeCCL fname))
        |> Result.mapError KVEncodeError
        |> ResultME.fromResult


{-| Figures out what response type for the endpoint will be.

If there is no response type defined for the endpoint then `()` is used to indicate
that the response has completed but returned no data.

Not that some AWS service specs define a response type, but it is just an empty
structure. These are given the response type `()` also.

The output of this is the response type alias for the endpoint, the decoder for this
expected response and any linkage that needs to be rolled up.

When there is no response shape, the decoder will be `(AWS.Http.constantDecoder ()`.

-}
requestFnResponse :
    PropertiesAPI pos
    -> L3 pos
    -> String
    -> L1.Type pos L2.RefChecked
    -> ResultME AWSStubsError ( TypeAnnotation, LetDeclaration, Linkage )
requestFnResponse propertiesApi model name responseType =
    case responseType of
        L1.TNamed _ responseTypeName L2.RcTProduct ->
            ResultME.andThen
                (\responseDecl ->
                    case responseDecl of
                        L1.DAlias _ _ (L1.TProduct _ fields) ->
                            nameTypedResponseDecoder propertiesApi model responseTypeName responseType fields

                        _ ->
                            L1.declarableConsName responseDecl |> UnsupportedDeclaration |> ResultME.error
                )
                (Query.deref responseTypeName model.declarations
                    |> ResultME.mapError L3Error
                )

        L1.TNamed _ responseTypeName L2.RcTEmptyProduct ->
            fixedResponseDecoder CG.unit |> Ok

        TUnit _ ->
            fixedResponseDecoder CG.unit |> Ok

        _ ->
            L1.typeConsName responseType |> UnsupportedType |> ResultME.error


{-| Generates a response decoder that always yield `()`. For use in situations
where an endpoint does not give any response data.
-}
fixedResponseDecoder : Expression -> ( TypeAnnotation, LetDeclaration, Linkage )
fixedResponseDecoder expr =
    let
        linkage =
            CG.emptyLinkage
                |> CG.addImport (CG.importStmt awsHttpMod Nothing Nothing)
                |> CG.addImport decodeImport

        decoder =
            CG.apply
                [ CG.fqVal awsHttpMod "constantDecoder"
                , expr
                ]
                |> CG.letVal "decoder"

        responseType =
            CG.unitAnn
    in
    ( responseType, decoder, linkage )


{-| Generates a response decoder for a named type.
-}
nameTypedResponseDecoder :
    PropertiesAPI pos
    -> L3 pos
    -> String
    -> L1.Type pos L2.RefChecked
    -> Nonempty (Field pos ref)
    -> ResultME AWSStubsError ( TypeAnnotation, LetDeclaration, Linkage )
nameTypedResponseDecoder propertiesApi model responseTypeName l1ResponseType fields =
    ResultME.map3
        (\statusCodeFields headerFields bodyFields ->
            ResultME.map
                (\( bodyDecoder, bodyDecoderLinkage ) ->
                    let
                        ( loweredType, loweredLinkage ) =
                            Elm.Lang.lowerType l1ResponseType

                        ( headersDecoder, headersDecoderLinkage ) =
                            KVDecode.partialKVDecoder propertiesApi "requestTypeName" headerFields
                                |> ResultME.map (FunDecl.asExpression FunDecl.defaultOptions)
                                |> ResultME.mapError KVDecodeError
                                |> Result.withDefault ( CG.string "failedKVDecoder", CG.emptyLinkage )

                        constructorFn =
                            CG.lambda
                                (List.map (\( fname, _, _ ) -> Naming.safeCCL (fname ++ "Fld") |> CG.varPattern)
                                    (statusCodeFields ++ headerFields ++ bodyFields)
                                )
                                (CG.record
                                    (Nonempty.map
                                        (\( fname, _, _ ) ->
                                            ( Naming.safeCCL fname, CG.val (Naming.safeCCL (fname ++ "Fld")) )
                                        )
                                        fields
                                        |> Nonempty.toList
                                    )
                                )

                        maybeAppliedToStatusCode =
                            case statusCodeFields of
                                [] ->
                                    constructorFn

                                _ ->
                                    CG.apply
                                        [ constructorFn |> CG.parens
                                        , CG.apply
                                            [ CG.val "Just"
                                            , CG.access (CG.val "metadata") "statusCode"
                                            ]
                                            |> CG.parens
                                        ]

                        maybeWithHeaderDecoding =
                            case headerFields of
                                [] ->
                                    CG.pipe
                                        maybeAppliedToStatusCode
                                        [ CG.fqFun decodeMod "succeed" ]

                                _ ->
                                    CG.pipe
                                        (maybeAppliedToStatusCode |> CG.parens)
                                        [ headersDecoder
                                        , CG.fqFun awsKVDecodeMod "buildObject"
                                        , CG.pipe
                                            (CG.apply
                                                [ CG.fqFun awsKVDecodeMod "decodeKVPairs"
                                                , CG.val "obj"
                                                , CG.access (CG.val "metadata") "headers"
                                                ]
                                            )
                                            [ CG.apply [ CG.fqVal resultMod "map", CG.fqVal decodeMod "succeed" ]
                                            , CG.apply [ CG.fqVal resultMod "withDefault", CG.apply [ CG.fqVal decodeMod "fail", CG.string "fail" ] |> CG.parens ]
                                            ]
                                            |> CG.lambda [ CG.varPattern "obj" ]
                                        ]

                        maybeWithBodyDecoder =
                            case bodyFields of
                                [] ->
                                    maybeWithHeaderDecoding |> CG.parens

                                _ ->
                                    CG.pipe
                                        (maybeWithHeaderDecoding |> CG.parens)
                                        [ bodyDecoder ]

                        isJustBody =
                            List.isEmpty statusCodeFields && List.isEmpty headerFields

                        decoder =
                            if isJustBody then
                                CG.applyBinOp maybeWithBodyDecoder
                                    CG.piper
                                    (CG.fqFun awsHttpMod "jsonBodyDecoder")
                                    |> CG.letVal "decoder"

                            else
                                CG.applyBinOp
                                    (CG.lambda
                                        [ CG.varPattern "metadata" ]
                                        maybeWithBodyDecoder
                                    )
                                    CG.piper
                                    (CG.fqFun awsHttpMod "jsonFullDecoder")
                                    |> CG.letVal "decoder"

                        linkage =
                            CG.combineLinkage
                                [ CG.emptyLinkage
                                    |> CG.addImport (CG.importStmt awsHttpMod Nothing Nothing)
                                , loweredLinkage
                                , bodyDecoderLinkage
                                , headersDecoderLinkage
                                ]
                    in
                    ( loweredType, decoder, linkage )
                )
                (case bodyFields of
                    [] ->
                        ( CG.val "noBody", CG.emptyLinkage )
                            |> Ok

                    bf :: bfs ->
                        Coding.partialCoding propertiesApi model.declarations "" "Decoder" (Nonempty bf bfs)
                            |> ResultME.map (FunDecl.asExpression FunDecl.defaultOptions)
                            |> ResultME.mapError JsonCodingError
                )
        )
        (Query.deref responseTypeName model.declarations
            |> ResultME.andThen (filterProductDecl propertiesApi isInStatusCode)
            |> ResultME.mapError L3Error
        )
        (Query.deref responseTypeName model.declarations
            |> ResultME.andThen (filterProductDecl propertiesApi isInHeader)
            |> ResultME.mapError L3Error
        )
        (Query.deref responseTypeName model.declarations
            |> ResultME.andThen (filterProductDecl propertiesApi isInBody)
            |> ResultME.mapError L3Error
        )
        |> ResultME.flatten



-- What does a full JSON decoder look like?
-- fullJsonDecoder status metadata body =
--     Json.Decode.succeed
--         (KVDecode.succeed
--             ((\status nextMarkerFld aliasesFld ->
--                 { aliases = aliasesFld, nextMarker = nextMarkerFld }
--              )
--                 metadata.statusCode
--             )
--             |> KVDecode.object
--             |> KVDecode.field
--             |> KVDecode.optional
--             |> KVDecode.buildObject
--             |> (\obj -> KvDecode.decodeKVPairs obj metadata.headers)
--         )
--         |> Pipeline.optional "NextMarker" (Json.Decode.maybe (Codec.decoder stringCodec)) Nothing
--         |> Pipeline.optional "Aliases" (Json.Decode.maybe (Codec.decoder aliasListCodec)) Nothing
--== Types Declarations


typeDeclarations :
    PropertiesAPI pos
    -> L3 pos
    -> ResultME AWSStubsError ( List Declaration, Linkage )
typeDeclarations propertiesApi model =
    Query.filterDictByProps propertiesApi (Query.notPropFilter isExcluded) model.declarations
        |> ResultME.mapError L3Error
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
        DAlias _ _ (TFunction _ _ _) ->
            ( [], CG.emptyLinkage ) |> Ok

        _ ->
            let
                doc =
                    CG.emptyDocComment
                        |> CG.markdown ("The " ++ Naming.safeCCU name ++ " data model.")
            in
            Elm.Lang.typeDecl name doc decl |> Ok



--== JSON Codecs


jsonCodings : PropertiesAPI pos -> L3 pos -> ResultME AWSStubsError ( List Declaration, Linkage )
jsonCodings propertiesApi model =
    Query.filterDictByProps propertiesApi
        (Query.andPropFilter
            (Query.notPropFilter isExcluded)
            hasJsonCoding
        )
        model.declarations
        |> ResultME.mapError L3Error
        |> ResultME.andThen (Dict.map (jsonCoding propertiesApi model.declarations) >> ResultME.combineDict)
        |> ResultME.map Dict.values
        |> ResultME.map combineDeclarations


jsonCoding :
    PropertiesAPI pos
    -> L2 pos
    -> String
    -> L1.Declarable pos L2.RefChecked
    -> ResultME AWSStubsError ( List Declaration, Linkage )
jsonCoding propertiesApi model name decl =
    case decl of
        DAlias _ _ (TFunction _ _ _) ->
            ( [], CG.emptyLinkage )
                |> Ok

        _ ->
            Coding.coding propertiesApi model name decl
                |> ResultME.map (FunDecl.asTopLevel FunDecl.defaultOptions)
                |> ResultME.map (Tuple.mapFirst List.singleton)
                |> ResultME.mapError JsonCodingError



--== Key-Value String Encoders


kvEncoders : PropertiesAPI pos -> L3 pos -> ResultME AWSStubsError ( List Declaration, Linkage )
kvEncoders propertiesApi model =
    Query.filterDictByProps propertiesApi
        (Query.andPropFilter
            (Query.notPropFilter isExcluded)
            isKVEncoded
        )
        model.declarations
        |> ResultME.mapError L3Error
        |> ResultME.map (Dict.map (kvEncoder propertiesApi))
        |> ResultME.map Dict.values
        |> ResultME.map ResultME.combineList
        |> ResultME.flatten
        |> ResultME.map combineDeclarations


kvEncoder : PropertiesAPI pos -> String -> L1.Declarable pos L2.RefChecked -> ResultME AWSStubsError ( List Declaration, Linkage )
kvEncoder propertiesApi name decl =
    case decl of
        DAlias _ _ (TFunction _ _ _) ->
            ( [], CG.emptyLinkage ) |> Ok

        _ ->
            KVEncode.kvEncoder propertiesApi name decl
                |> ResultME.map (FunDecl.asTopLevel FunDecl.defaultOptions)
                |> ResultME.map (Tuple.mapFirst List.singleton)
                |> ResultME.mapError KVEncodeError



--== Key-Value String Decoders


kvDecoders : PropertiesAPI pos -> L3 pos -> ResultME AWSStubsError ( List Declaration, Linkage )
kvDecoders propertiesApi model =
    Query.filterDictByProps propertiesApi
        (Query.andPropFilter
            (Query.notPropFilter isExcluded)
            isKVDecoded
        )
        model.declarations
        |> ResultME.mapError L3Error
        |> ResultME.map (Dict.map (kvDecoder propertiesApi))
        |> ResultME.map Dict.values
        |> ResultME.map ResultME.combineList
        |> ResultME.flatten
        |> ResultME.map combineDeclarations


kvDecoder : PropertiesAPI pos -> String -> L1.Declarable pos L2.RefChecked -> ResultME AWSStubsError ( List Declaration, Linkage )
kvDecoder propertiesApi name decl =
    case decl of
        DAlias _ _ (TFunction _ _ _) ->
            ( [], CG.emptyLinkage ) |> Ok

        _ ->
            KVDecode.kvDecoder propertiesApi name decl
                |> ResultME.map (FunDecl.asTopLevel FunDecl.defaultOptions)
                |> ResultME.map (Tuple.mapFirst List.singleton)
                |> ResultME.mapError KVDecodeError



-- Property Filters


isInHeader : PropertyFilter pos ( String, Type pos ref, Properties )
isInHeader =
    isInLocation "header"


isInQueryString : PropertyFilter pos ( String, Type pos ref, Properties )
isInQueryString =
    isInLocation "querystring"


isInUri : PropertyFilter pos ( String, Type pos ref, Properties )
isInUri =
    isInLocation "uri"


isInBody : PropertyFilter pos ( String, Type pos ref, Properties )
isInBody =
    isInLocation "body"


isInStatusCode : PropertyFilter pos ( String, Type pos ref, Properties )
isInStatusCode =
    isInLocation "statuscode"


isInLocation : String -> PropertyFilter pos ( String, Type pos ref, Properties )
isInLocation location propertiesApi ( _, _, props ) =
    case (propertiesApi.field props).getEnumProperty locationEnum "location" of
        Ok val ->
            location == val |> Ok

        Err err ->
            Err err


withLocationName : String -> PropertyFilter pos (Field pos ref)
withLocationName name propertiesApi ( _, _, props ) =
    ResultME.map
        (Maybe.map ((==) name) >> Maybe.withDefault False)
        ((propertiesApi.field props).getOptionalStringProperty "serializedName")


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


isResponse : PropertyFilter pos (L1.Declarable pos L2.RefChecked)
isResponse propertiesAPI decl =
    case (propertiesAPI.declarable decl).getOptionalEnumProperty topLevelEnum "topLevel" of
        Ok (Just "response") ->
            Ok True

        Ok blah ->
            Ok False

        Err err ->
            Err err


hasJsonCoding : PropertyFilter pos (L1.Declarable pos L2.RefChecked)
hasJsonCoding propertiesAPI decl =
    (propertiesAPI.declarable decl).getOptionalEnumProperty Coding.jsonCodingEnum "jsonCoding"
        |> ResultME.map Maybe.Extra.isJust


isKVEncoded : PropertyFilter pos (L1.Declarable pos L2.RefChecked)
isKVEncoded propertiesAPI decl =
    (propertiesAPI.declarable decl).getBoolProperty "kvEncode"


isKVDecoded : PropertyFilter pos (L1.Declarable pos L2.RefChecked)
isKVDecoded propertiesAPI decl =
    (propertiesAPI.declarable decl).getBoolProperty "kvDecode"



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


awsConfigMod : List String
awsConfigMod =
    [ "AWS", "Config" ]


awsHttpMod : List String
awsHttpMod =
    [ "AWS", "Http" ]


awsKVEncodeMod : List String
awsKVEncodeMod =
    [ "AWS", "KVEncode" ]


awsKVDecodeMod : List String
awsKVDecodeMod =
    [ "AWS", "KVDecode" ]


awsServiceMod : List String
awsServiceMod =
    [ "AWS", "Service" ]


decodeMod : List String
decodeMod =
    [ "Json", "Decode" ]


codecMod : List String
codecMod =
    [ "Codec" ]


refinedMod : List String
refinedMod =
    [ "Refined" ]


resultMod : List String
resultMod =
    [ "Result" ]


stringMod : List String
stringMod =
    [ "String" ]


decodeImport : Import
decodeImport =
    CG.importStmt decodeMod Nothing Nothing

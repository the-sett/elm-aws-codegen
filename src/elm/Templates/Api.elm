module Templates.Api exposing (coreServiceMod, docs, globalService, module_, regionalService, service, serviceFile)

import AWSApiModel exposing (AWSApiModel, Endpoint)
import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Declaration, Expression, File, Linkage, Module, Pattern, TopLevelExpose, TypeAnnotation)
import Enum
import HttpMethod exposing (HttpMethod)
import L1
import Maybe.Extra
import Templates.L1
import Templates.Util as Util


serviceFile : AWSApiModel -> File
serviceFile model =
    let
        ( serviceFn, linkage ) =
            service model

        ( endpoints, linkage2 ) =
            operations model

        ( types, linkage3 ) =
            typeDeclarations model

        ( codecs, linkage4 ) =
            jsonCodecs model

        declarations =
            codecs
                |> List.append types
                |> List.append endpoints
                |> (::) serviceFn

        linkages =
            linkage4
                |> List.append linkage3
                |> List.append linkage2
                |> (::) linkage

        ( imports, exposings ) =
            CG.combineLinkage linkages

        moduleSpec =
            module_ model exposings
    in
    CG.file moduleSpec imports declarations []


coreServiceMod : List String
coreServiceMod =
    [ "AWS", "Core", "Service" ]



--== Module Specification (with exposing).


module_ : AWSApiModel -> List TopLevelExpose -> Module
module_ model exposings =
    CG.normalModule model.name exposings



--== Module Documentation
-- {-| {{= it.documentation }}
--
-- @docs service
--
-- ## Table of Contents
--
-- * [Operations](#operations){{~ it.categories :c }}
-- * [{{= c.name }}](#{{= c.name.toLowerCase() }}){{~}}
--
-- ## Operations
--
-- {{~ it.operationNames :name }}* [{{= name }}](#{{= name }})
-- {{~}}
--
-- @docs {{= it.operationNames.join(',') }}
-- {{~ it.categories :c }}
-- ## {{= c.name }}
--
-- {{~ c.types.filter(t => t.exposeAs).map(t => t.type) :t }}* [{{= t }}](#{{= t }})
-- {{~}}
--
-- @docs {{= c.types.filter(t => t.exposeAs).map(t => t.type).join(',') }}
-- {{~}}
-- -}


docs =
    ""



--== Service Definition


service : AWSApiModel -> ( Declaration, Linkage )
service model =
    if model.isRegional then
        regionalService model

    else
        globalService model


optionsFn model =
    let
        jsonVersionOption =
            Maybe.map
                (\name -> CG.apply [ CG.fqFun coreServiceMod "setJsonVersion", CG.string name ])
                model.jsonVersion

        signingNameOption =
            Maybe.map
                (\name -> CG.apply [ CG.fqFun coreServiceMod "setSigningName", CG.string name ])
                model.signingName

        targetPrefixOption =
            Maybe.map
                (\name -> CG.apply [ CG.fqFun coreServiceMod "setTargetPrefix", CG.string name ])
                model.targetPrefix

        xmlNamespaceOption =
            Maybe.map
                (\name -> CG.apply [ CG.fqFun coreServiceMod "setXmlNamespace", CG.string name ])
                model.xmlNamespace

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


regionalService : AWSApiModel -> ( Declaration, Linkage )
regionalService model =
    let
        sig =
            CG.funAnn
                (CG.fqTyped coreServiceMod "Region" [])
                (CG.fqTyped coreServiceMod "Service" [])

        impl =
            CG.apply
                [ CG.fqFun coreServiceMod "defineRegional"
                , CG.string model.endpointPrefix
                , CG.string model.apiVersion
                , CG.fqFun coreServiceMod model.protocol
                , CG.fqFun coreServiceMod model.signer
                , CG.fun "optionsFn"
                ]
                |> CG.letExpr [ optionsFn model ]
    in
    ( CG.funDecl
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport (CG.importStmt coreServiceMod Nothing Nothing)
        |> CG.addExposing (CG.funExpose "service")
    )


globalService : AWSApiModel -> ( Declaration, Linkage )
globalService model =
    let
        sig =
            CG.fqTyped coreServiceMod "Service" []

        impl =
            CG.apply
                [ CG.fqFun coreServiceMod "defineGlobal"
                , CG.string model.endpointPrefix
                , CG.string model.apiVersion
                , CG.fqFun coreServiceMod model.protocol
                , CG.fqFun coreServiceMod model.signer
                , CG.fun "optionsFn"
                ]
                |> CG.letExpr [ optionsFn model ]
    in
    ( CG.funDecl
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport (CG.importStmt coreServiceMod Nothing Nothing)
        |> CG.addExposing (CG.funExpose "service")
    )



--== Operations


operations : AWSApiModel -> ( List Declaration, List Linkage )
operations model =
    Dict.foldl
        (\name operation ( declAccum, linkageAccum ) ->
            requestFn name operation
                |> Tuple.mapFirst (\decl -> decl :: declAccum)
                |> Tuple.mapSecond (\linkage -> linkage :: linkageAccum)
        )
        ( [], [] )
        model.operations


requestFn : String -> Endpoint -> ( Declaration, Linkage )
requestFn name op =
    let
        { maybeRequestType, argPatterns, jsonBody, requestLinkage } =
            requestFnRequest name op

        ( responseType, responseDecoder, responseLinkage ) =
            requestFnResponse name op

        wrappedResponseType =
            CG.fqTyped coreHttpMod "Request" [ CG.fqTyped coreDecodeMod "ResponseWrapper" [ responseType ] ]

        wrappedDecoder =
            CG.apply
                [ CG.fqVal coreDecodeMod "responseWrapperDecoder"
                , CG.string (Util.safeCCU name)
                , responseDecoder
                    |> CG.parens
                ]

        requestSig =
            case maybeRequestType of
                Just requestType ->
                    CG.funAnn requestType wrappedResponseType

                Nothing ->
                    wrappedResponseType

        requestImpl =
            CG.apply
                [ CG.fqFun coreHttpMod "request"
                , CG.fqVal coreHttpMod (Enum.toString HttpMethod.httpMethodEnum op.httpMethod)
                , CG.string op.url
                , CG.val "jsonBody"
                , CG.val "wrappedDecoder"
                ]
                |> CG.letExpr
                    [ jsonBody |> CG.letVal "jsonBody"
                    , wrappedDecoder |> CG.letVal "wrappedDecoder"
                    ]
    in
    ( CG.funDecl
        (Just "{-| AWS Endpoint. -}")
        (Just requestSig)
        (Util.safeCCL name)
        argPatterns
        requestImpl
    , CG.combineLinkage
        [ requestLinkage
        , responseLinkage
        , CG.emptyLinkage |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
        ]
    )


{-| Figures out what the request type for the endpoint will be.

If there is no request type defined for the endpoint then 'Nothing' will be returned,
and an empty JSON body expression will be given.

The output of this is the optional request type alias, a list of patterns for the
request functions arguments, the json body and any linkage that needs to be rolled up.

-}
requestFnRequest :
    String
    -> Endpoint
    ->
        { maybeRequestType : Maybe TypeAnnotation
        , argPatterns : List Pattern
        , jsonBody : Expression
        , requestLinkage : Linkage
        }
requestFnRequest name op =
    case op.request of
        Just ( requestTypeName, l1RequestType ) ->
            let
                ( loweredType, loweredLinkage ) =
                    Templates.L1.lowerType l1RequestType

                linkage =
                    CG.combineLinkage
                        [ CG.emptyLinkage
                            |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
                        , loweredLinkage
                        ]

                jsonBody =
                    CG.pipe (CG.val "req")
                        [ CG.apply
                            [ CG.fqFun codecMod "encoder"
                            , CG.val (Util.safeCCL requestTypeName ++ "Codec")
                            ]
                        , CG.fqVal coreHttpMod "jsonBody"
                        ]
            in
            { maybeRequestType = Just loweredType
            , argPatterns = [ CG.varPattern "req" ]
            , jsonBody = jsonBody
            , requestLinkage = linkage
            }

        Nothing ->
            let
                emptyJsonBody =
                    CG.fqVal coreHttpMod "emptyBody"

                linkage =
                    CG.emptyLinkage |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
            in
            { maybeRequestType = Nothing
            , argPatterns = []
            , jsonBody = emptyJsonBody
            , requestLinkage = linkage
            }


{-| Figures out what response type for the endpoint will be.

If there is no response type defined for the endpoint then `()` is used to indicate
that the response has completed but returned no data.

The output of this is the response type alias for the endpoint, the decoder for this
expected response and any linkage that needs to be rolled up.

When there is no response shape, the decoder will be `(AWS.Core.Decode.FixedResult ()`.

-}
requestFnResponse : String -> Endpoint -> ( TypeAnnotation, Expression, Linkage )
requestFnResponse name op =
    case op.response of
        Just ( responseTypeName, l1ResponseType ) ->
            let
                ( loweredType, loweredLinkage ) =
                    Templates.L1.lowerType l1ResponseType

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
                        [ CG.fqFun coreDecodeMod "ResultDecoder"
                        , CG.string responseTypeName
                        , CG.apply
                            [ CG.fqFun codecMod "decoder"
                            , CG.val (Util.safeCCL responseTypeName ++ "Codec")
                            ]
                            |> CG.parens
                        ]
            in
            ( responseType, decoder, linkage )

        Nothing ->
            let
                linkage =
                    CG.emptyLinkage
                        |> CG.addImport (CG.importStmt coreDecodeMod Nothing Nothing)

                decoder =
                    CG.apply
                        [ CG.fqVal coreDecodeMod "FixedResult"
                        , CG.unit
                        ]

                responseType =
                    CG.unitAnn
            in
            ( responseType, decoder, linkage )



--== Types and Codecs


typeDeclarations : AWSApiModel -> ( List Declaration, List Linkage )
typeDeclarations model =
    Dict.foldl
        (\name decl ( declAccum, linkageAccum ) ->
            Templates.L1.typeDecl name decl
                |> Tuple.mapFirst (List.append declAccum)
                |> Tuple.mapSecond (List.append linkageAccum)
        )
        ( [], [] )
        model.declarations


jsonCodecs : AWSApiModel -> ( List Declaration, List Linkage )
jsonCodecs model =
    Dict.foldl
        (\name decl accum -> Templates.L1.codec name decl :: accum)
        []
        model.declarations
        |> List.unzip



-- {{? t.toStringDef }}
-- {{= t.toStringDef }}
-- {{?}}
-- {{~}}


toParams =
    ()



-- {{~ it.types.filter(t => t.category === 'request') :t }}
-- {{= t.typeDef }}
-- {{~}}


requests =
    ()



-- Helpers


codecMod : List String
codecMod =
    [ "Codec" ]


coreHttpMod : List String
coreHttpMod =
    [ "AWS", "Core", "Http" ]


coreDecodeMod : List String
coreDecodeMod =
    [ "AWS", "Core", "Decode" ]

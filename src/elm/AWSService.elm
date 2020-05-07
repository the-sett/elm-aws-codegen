module AWSService exposing
    ( AWSService
    , AWSType(..)
    , AuthorizationStrategy
    , Authorizers
    , Location(..)
    , MetaData
    , Operation
    , Shape
    , ShapeRef
    , awsServiceCodec
    )

{-| AWS Service2 Descriptor. This module provides the data model and decoders.
-}

import AWS.Core.Service exposing (Protocol(..), Signer(..), TimestampFormat(..), protocolEnum, signerEnum, timestampFormatEnum)
import Codec exposing (Codec)
import Dict exposing (Dict)
import Enum exposing (Enum)
import HttpMethod exposing (HttpMethod)
import Json.Decode as Decode
import Json.Encode as Encode


type alias AWSService =
    { metaData : MetaData
    , operations : Dict String Operation
    , shapes : Dict String Shape
    , authorizers : Maybe Authorizers
    , documentation : Maybe String
    , version : Maybe String
    }


awsServiceCodec =
    Codec.object AWSService
        |> Codec.field "metadata" .metaData metaDataCodec
        |> Codec.field "operations" .operations (Codec.dict operationCodec)
        |> Codec.field "shapes" .shapes (Codec.dict shapeCodec)
        |> Codec.optionalField "authorizers" .authorizers authorizersCodec
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.optionalField "version" .version Codec.string
        |> Codec.buildObject


type alias MetaData =
    { apiVersion : String
    , endpointPrefix : String
    , protocol : Protocol
    , serviceId : String
    , checksumFormat : Maybe String
    , globalEndpoint : Maybe String
    , jsonVersion : Maybe String
    , serviceAbbreviation : Maybe String
    , serviceFullName : Maybe String
    , signatureVersion : Maybe Signer
    , signingName : Maybe String
    , targetPrefix : Maybe String
    , uid : Maybe String
    , xmlNamespace : Maybe String
    }


protocolCodec =
    Codec.build (Enum.encoder protocolEnum) (Enum.decoder protocolEnum)


signerCodec =
    Codec.build (Enum.encoder signerEnum) (Enum.decoder signerEnum)


metaDataCodec =
    Codec.object MetaData
        |> Codec.field "apiVersion" .apiVersion Codec.string
        |> Codec.field "endpointPrefix" .endpointPrefix Codec.string
        |> Codec.field "protocol" .protocol protocolCodec
        |> Codec.field "serviceId" .serviceId Codec.string
        |> Codec.optionalField "checksumFormat" .checksumFormat Codec.string
        |> Codec.optionalField "globalEndpoint" .globalEndpoint Codec.string
        |> Codec.optionalField "jsonVersion" .jsonVersion Codec.string
        |> Codec.optionalField "serviceAbbreviation" .serviceAbbreviation Codec.string
        |> Codec.optionalField "serviceFullName" .serviceFullName Codec.string
        |> Codec.optionalField "signatureVersion" .signatureVersion signerCodec
        |> Codec.optionalField "signingName" .signingName Codec.string
        |> Codec.optionalField "targetPrefix" .targetPrefix Codec.string
        |> Codec.optionalField "uid" .uid Codec.string
        |> Codec.optionalField "xmlNamespace" .xmlNamespace Codec.string
        |> Codec.buildObject


type alias Authorizers =
    { authorization_strategy : AuthorizationStrategy
    }


authorizersCodec =
    Codec.object Authorizers
        |> Codec.field "authorization_strategy" .authorization_strategy authorizationStrategyCodec
        |> Codec.buildObject


type alias AuthorizationStrategy =
    { name : String
    , placement : Placement
    , type_ : String
    }


authorizationStrategyCodec =
    Codec.object AuthorizationStrategy
        |> Codec.field "name" .name Codec.string
        |> Codec.field "placement" .placement placementCodec
        |> Codec.field "type" .type_ Codec.string
        |> Codec.buildObject


type alias Placement =
    { location : String
    , name : String
    }


placementCodec =
    Codec.object Placement
        |> Codec.field "location" .location Codec.string
        |> Codec.field "name" .name Codec.string
        |> Codec.buildObject


type alias Operation =
    { http : Http
    , name : String
    , alias : Maybe String
    , authtype : Maybe String
    , deprecated : Maybe Bool
    , deprecatedMessage : Maybe String
    , documentation : Maybe String
    , documentationUrl : Maybe String
    , endpoint : Maybe Endpoint
    , errors : Maybe (List ShapeRef)
    , idempotent : Maybe Bool
    , input : Maybe ShapeRef
    , output : Maybe ShapeRef
    }


operationCodec =
    Codec.object Operation
        |> Codec.field "http" .http httpCodec
        |> Codec.field "name" .name Codec.string
        |> Codec.optionalField "alias" .alias Codec.string
        |> Codec.optionalField "authtype" .authtype Codec.string
        |> Codec.optionalField "deprecated" .deprecated Codec.bool
        |> Codec.optionalField "deprecatedMessage" .deprecatedMessage Codec.string
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.optionalField "documentationUrl" .documentationUrl Codec.string
        |> Codec.optionalField "endpoint" .endpoint endpointCodec
        |> Codec.optionalField "errors" .errors (Codec.list shapeRefCodec)
        |> Codec.optionalField "idempotent" .idempotent Codec.bool
        |> Codec.optionalField "input" .input shapeRefCodec
        |> Codec.optionalField "output" .output shapeRefCodec
        |> Codec.buildObject


type alias Endpoint =
    { hostPrefix : String
    }


endpointCodec =
    Codec.object Endpoint
        |> Codec.field "hostPrefix" .hostPrefix Codec.string
        |> Codec.buildObject


type alias Http =
    { method : HttpMethod
    , requestUri : Maybe String
    , requireUri : Maybe Bool
    , responseCode : Maybe Int
    }


httpCodec =
    Codec.object Http
        |> Codec.field "method" .method HttpMethod.httpMethodCodec
        |> Codec.optionalField "requestUri" .requestUri Codec.string
        |> Codec.optionalField "requireUri" .requireUri Codec.bool
        |> Codec.optionalField "responseCode" .responseCode Codec.int
        |> Codec.buildObject


type alias Error =
    { code : String
    , httpStatusCode : Int
    , senderFault : Bool
    }


errorCodec =
    Codec.object Error
        |> Codec.field "code" .code Codec.string
        |> Codec.field "httpStatusCode" .httpStatusCode Codec.int
        |> Codec.field "senderFault" .senderFault Codec.bool
        |> Codec.buildObject


type alias ShapeRef =
    { shape : String
    , box : Maybe Bool
    , deprecated : Maybe Bool
    , deprecatedMessage : Maybe String
    , documentation : Maybe String
    , eventpayload : Maybe Bool
    , flattened : Maybe Bool
    , idempotencyToken : Maybe String
    , jsonvalue : Maybe Bool
    , location : Location
    , locationName : Maybe String
    , queryName : Maybe String
    , resultWrapper : Maybe String
    , streaming : Maybe Bool
    , timestampFormat : Maybe TimestampFormat
    , xmlAttribute : Maybe String
    , xmlNamespace : Maybe String
    }


timestampFormatCodec =
    Codec.build (Enum.encoder timestampFormatEnum) (Enum.decoder timestampFormatEnum)


shapeRefCodec =
    Codec.object
        (\shape box deprecated deprecatedMessage documentation eventpayload flattened idempotencyToken jsonvalue location locationName queryName resultWrapper streaming timestampFormat xmlAttribute xmlNamespace ->
            { shape = shape
            , box = box
            , deprecated = deprecated
            , deprecatedMessage = deprecatedMessage
            , documentation = documentation
            , eventpayload = eventpayload
            , flattened = flattened
            , idempotencyToken = idempotencyToken
            , jsonvalue = jsonvalue
            , location = location |> Maybe.withDefault Body
            , locationName = locationName
            , queryName = queryName
            , resultWrapper = resultWrapper
            , streaming = streaming
            , timestampFormat = timestampFormat
            , xmlAttribute = xmlAttribute
            , xmlNamespace = xmlNamespace
            }
        )
        |> Codec.field "shape" .shape Codec.string
        |> Codec.optionalField "box" .box Codec.bool
        |> Codec.optionalField "deprecated" .deprecated Codec.bool
        |> Codec.optionalField "deprecatedMessage" .deprecatedMessage Codec.string
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.optionalField "eventpayload" .eventpayload Codec.bool
        |> Codec.optionalField "flattened" .flattened Codec.bool
        |> Codec.optionalField "idempotencyToken" .idempotencyToken Codec.string
        |> Codec.optionalField "jsonvalue" .jsonvalue Codec.bool
        |> Codec.optionalField "location" (.location >> Just) locationCodec
        |> Codec.optionalField "serializedName" .locationName Codec.string
        |> Codec.optionalField "queryName" .queryName Codec.string
        |> Codec.optionalField "resultWrapper" .resultWrapper Codec.string
        |> Codec.optionalField "streaming" .streaming Codec.bool
        |> Codec.optionalField "timestampFormat" .timestampFormat timestampFormatCodec
        |> Codec.optionalField "xmlAttribute" .xmlAttribute Codec.string
        |> Codec.optionalField "xmlNamespace" .xmlNamespace Codec.string
        |> Codec.buildObject


type alias Shape =
    { type_ : AWSType
    , box : Maybe Bool
    , deprecated : Maybe Bool
    , deprecatedMessage : Maybe String
    , documentation : Maybe String
    , enum : Maybe (List String)
    , error : Maybe Error
    , event : Maybe Bool
    , eventstream : Maybe Bool
    , exception : Maybe Bool
    , fault : Maybe Bool
    , flattened : Maybe Bool
    , key : Maybe ShapeRef
    , locationName : Maybe String
    , max : Maybe Int
    , member : Maybe ShapeRef
    , members : Maybe (Dict String ShapeRef)
    , min : Maybe Int
    , pattern : Maybe String
    , payload : Maybe String
    , required : Maybe (List String)
    , sensitive : Maybe Bool
    , streaming : Maybe Bool
    , timestampFormat : Maybe TimestampFormat
    , value : Maybe ShapeRef
    , wrapper : Maybe Bool
    , xmlNamespace : Maybe String
    , xmlOrder : Maybe (List String)
    }


shapeCodec =
    Codec.object Shape
        |> Codec.field "type" .type_ typesCodec
        |> Codec.optionalField "box" .box Codec.bool
        |> Codec.optionalField "deprecated" .deprecated Codec.bool
        |> Codec.optionalField "deprecatedMessage" .deprecatedMessage Codec.string
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.optionalField "enum" .enum (Codec.list Codec.string)
        |> Codec.optionalField "error" .error errorCodec
        |> Codec.optionalField "event" .event Codec.bool
        |> Codec.optionalField "eventstream" .eventstream Codec.bool
        |> Codec.optionalField "exception" .exception Codec.bool
        |> Codec.optionalField "fault" .fault Codec.bool
        |> Codec.optionalField "flattened" .flattened Codec.bool
        |> Codec.optionalField "key" .key shapeRefCodec
        |> Codec.optionalField "serializedName" .locationName Codec.string
        |> Codec.optionalField "max" .max Codec.int
        |> Codec.optionalField "member" .member shapeRefCodec
        |> Codec.optionalField "members" .members (Codec.dict shapeRefCodec)
        |> Codec.optionalField "min" .min Codec.int
        |> Codec.optionalField "pattern" .pattern Codec.string
        |> Codec.optionalField "payload" .payload Codec.string
        |> Codec.optionalField "required" .required (Codec.list Codec.string)
        |> Codec.optionalField "sensitive" .sensitive Codec.bool
        |> Codec.optionalField "streaming" .streaming Codec.bool
        |> Codec.optionalField "timestampFormat" .timestampFormat timestampFormatCodec
        |> Codec.optionalField "value" .value shapeRefCodec
        |> Codec.optionalField "wrapper" .wrapper Codec.bool
        |> Codec.optionalField "xmlNamespace" .xmlNamespace Codec.string
        |> Codec.optionalField "xmlOrder" .xmlOrder (Codec.list Codec.string)
        |> Codec.buildObject


type Location
    = Header
    | QueryString
    | StatusCode
    | Uri
    | Body


locationCodec : Codec Location
locationCodec =
    let
        locationEncoder loc =
            case loc of
                Header ->
                    Encode.string "header"

                QueryString ->
                    Encode.string "querystring"

                StatusCode ->
                    Encode.string "statusCode"

                Uri ->
                    Encode.string "uri"

                Body ->
                    Encode.string "body"

        locationDecoder =
            Decode.string
                |> Decode.andThen
                    (\val ->
                        case val of
                            "headers" ->
                                Decode.succeed Header

                            "header" ->
                                Decode.succeed Header

                            "querystring" ->
                                Decode.succeed QueryString

                            "statusCode" ->
                                Decode.succeed StatusCode

                            "uri" ->
                                Decode.succeed Uri

                            _ ->
                                Decode.fail <| "Could not decode value to enum: " ++ val
                    )
    in
    Codec.build locationEncoder locationDecoder



-- "headers"


type AWSType
    = AString
    | ABoolean
    | AInteger
    | ALong
    | AFloat
    | ADouble
    | ABlob
    | AStructure
    | AList
    | AMap
    | ATimestamp
    | AUnknown


awsTypeEnum : Enum AWSType
awsTypeEnum =
    Enum.define
        [ AString
        , ABoolean
        , AInteger
        , ALong
        , AFloat
        , ADouble
        , ABlob
        , AStructure
        , AList
        , AMap
        , ATimestamp
        , AUnknown
        ]
        (\types ->
            case types of
                AString ->
                    "string"

                ABoolean ->
                    "boolean"

                AInteger ->
                    "integer"

                ALong ->
                    "long"

                AFloat ->
                    "float"

                ADouble ->
                    "double"

                ABlob ->
                    "blob"

                AStructure ->
                    "structure"

                AList ->
                    "list"

                AMap ->
                    "map"

                ATimestamp ->
                    "timestamp"

                AUnknown ->
                    "unknown"
        )


typesCodec : Codec AWSType
typesCodec =
    Codec.build (Enum.encoder awsTypeEnum) (Enum.decoder awsTypeEnum)

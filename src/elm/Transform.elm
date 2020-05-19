module Transform exposing (transform)

import AWS.Config exposing (Protocol(..), Signer(..))
import AWSService exposing (AWSService, AWSType(..), Location(..), Operation, Shape, ShapeRef)
import Checker
import Dict exposing (Dict)
import Enum exposing (Enum)
import Errors exposing (Error, ErrorBuilder)
import HttpMethod exposing (HttpMethod(..))
import L1
    exposing
        ( Basic(..)
        , Container(..)
        , Declarable(..)
        , L1
        , PropSpec(..)
        , Property(..)
        , Restricted(..)
        , Type(..)
        , Unchecked(..)
        )
import L2 exposing (L2, RefChecked(..))
import L3 exposing (L3)
import List.Nonempty
import Maybe.Extra
import Naming
import Query
import ResultME exposing (ResultME)
import SourcePos exposing (SourceLines)
import String.Case as Case
import Templates.AWSStubs as AWSStubs


type TransformError pos
    = UnresolvedRef pos String
    | NoMembers pos String
    | MapKeyEmpty pos
    | MapValueEmpty pos
    | ListMemberEmpty pos
    | UnknownNotImplemented pos
    | UnsupportedProtocol String


{-| The error catalogue for this transform.
-}
errorCatalogue =
    Dict.fromList
        [ ( 801
          , { title = "Unresolved Reference"

            --hint ++ " reference did not resolve."
            , body = ""
            }
          )
        , ( 802
          , { title = "Structure has No Members"

            --     name ++ ": structure has no members"
            , body = ""
            }
          )
        , ( 803
          , { title = "Map Key Empty"

            --     "Map .key is empty."
            , body = ""
            }
          )
        , ( 804
          , { title = "Map Value Empty"

            --     "Map .value is empty."
            , body = ""
            }
          )
        , ( 805
          , { title = "List Member is Empty"

            --     "List .member is empty, but should be a shape reference."
            , body = ""
            }
          )
        , ( 806
          , { title = "Unknown Not Implemented"

            --     "Unknown not implemented."
            , body = ""
            }
          )
        , ( 807
          , { title = "Unsupported protocol."
            , body = "The []{arg|key=protocol } protocol is not supported."
            }
          )
        ]


errorBuilder : ErrorBuilder pos (TransformError pos)
errorBuilder posFn err =
    case err of
        UnresolvedRef pos name ->
            Errors.lookupError errorCatalogue
                801
                (Dict.fromList [ ( "name", name ) ])
                [ posFn pos ]

        NoMembers pos name ->
            Errors.lookupError errorCatalogue
                802
                (Dict.fromList [ ( "name", name ) ])
                [ posFn pos ]

        MapKeyEmpty pos ->
            Errors.lookupErrorNoArgs errorCatalogue 803 [ posFn pos ]

        MapValueEmpty pos ->
            Errors.lookupErrorNoArgs errorCatalogue 804 [ posFn pos ]

        ListMemberEmpty pos ->
            Errors.lookupErrorNoArgs errorCatalogue 805 [ posFn pos ]

        UnknownNotImplemented pos ->
            Errors.lookupErrorNoArgs errorCatalogue 806 [ posFn pos ]

        UnsupportedProtocol protocol ->
            Errors.lookupError errorCatalogue
                807
                (Dict.fromList [ ( "protocol", protocol ) ])
                []


transform : (() -> SourceLines) -> AWSService -> ResultME Error (L3 ())
transform posFn service =
    let
        errorMapFn =
            errorBuilder posFn

        shapesResult : ResultME Error (L1 ())
        shapesResult =
            modelShapes service.shapes
                |> ResultME.mapError errorMapFn

        operationsResult : ResultME Error (L1 ())
        operationsResult =
            modelOperations service.operations
                |> ResultME.mapError errorMapFn

        l2Checker =
            L2.builder posFn Checker.processorImpl

        checkProtocolSupported checked =
            case service.metaData.protocol of
                JSON ->
                    checked |> Ok

                REST_JSON ->
                    checked |> Ok

                _ ->
                    protocolToString service.metaData.protocol
                        |> UnsupportedProtocol
                        |> ResultME.error
                        |> ResultME.mapError errorMapFn

        l2Result =
            Result.map2 List.append shapesResult operationsResult
                |> ResultME.andThen l2Checker.check
                |> ResultME.map (markTopLevelShapes service.operations)
                |> ResultME.map markCodecs
                |> ResultME.andThen checkProtocolSupported
    in
    ResultME.map
        (\l2 ->
            { properties =
                Dict.empty
                    |> Dict.insert "name" (PQName [ "AWS", Case.toCamelCaseUpper service.metaData.serviceId ])
                    |> Dict.insert "isRegional" (PBool (Maybe.Extra.isNothing service.metaData.globalEndpoint))
                    |> Dict.insert "endpointPrefix" (PString service.metaData.endpointPrefix)
                    |> Dict.insert "apiVersion" (PString service.metaData.apiVersion)
                    |> Dict.insert "protocol"
                        (protocolToString service.metaData.protocol
                            |> PEnum AWSStubs.protocolEnum
                        )
                    |> Dict.insert "signer"
                        (service.metaData.signatureVersion
                            |> Maybe.withDefault SignV4
                            |> signerToString
                            |> PEnum AWSStubs.signerEnum
                        )
                    |> Dict.update "xmlNamespace" (always (Maybe.map PString service.metaData.xmlNamespace))
                    |> Dict.update "targetPrefix" (always (Maybe.map PString service.metaData.targetPrefix))
                    |> Dict.update "signingName" (always (Maybe.map PString service.metaData.signingName))
                    |> Dict.update "jsonVersion" (always (Maybe.map PString service.metaData.jsonVersion))
                    |> Dict.update "documentation" (always (Maybe.map PString service.documentation))
            , declarations = l2
            }
        )
        l2Result


protocolToString : Protocol -> String
protocolToString proto =
    case proto of
        EC2 ->
            "EC2"

        JSON ->
            "JSON"

        QUERY ->
            "QUERY"

        REST_JSON ->
            "REST_JSON"

        REST_XML ->
            "REST_XML"


signerToString : Signer -> String
signerToString signer =
    case signer of
        SignV4 ->
            "SignV4"

        SignS3 ->
            "SignS3"


httpMethodToString : HttpMethod -> String
httpMethodToString method =
    case method of
        DELETE ->
            "DELETE"

        GET ->
            "GET"

        HEAD ->
            "HEAD"

        OPTIONS ->
            "OPTIONS"

        POST ->
            "POST"

        PUT ->
            "PUT"


shapeRefToL1Type : ShapeRef -> Type () Unchecked
shapeRefToL1Type ref =
    TNamed () Dict.empty ref.shape Unchecked



--=== L1 Model Assembly Pass
-- A complete L1 model is generated for each shape.
-- Errors in the shape definitions are detected, but other checking of the L1
-- model is handled when it is lowered into L2.


modelShapes :
    Dict String Shape
    -> ResultME (TransformError ()) (L1 ())
modelShapes shapeDict =
    Dict.map
        (\key value -> modelShape value key)
        shapeDict
        |> ResultME.combineDict
        |> ResultME.map Dict.toList


modelShape : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelShape shape name =
    case shape.type_ of
        AString ->
            modelString shape name

        ABoolean ->
            (BBool |> TBasic () L1.emptyProperties) |> DAlias () L1.emptyProperties |> Ok

        AInteger ->
            modelInt shape name

        ALong ->
            (BInt |> TBasic () L1.emptyProperties) |> DAlias () L1.emptyProperties |> Ok

        AFloat ->
            (BReal |> TBasic () L1.emptyProperties) |> DAlias () L1.emptyProperties |> Ok

        ADouble ->
            (BReal |> TBasic () L1.emptyProperties) |> DAlias () L1.emptyProperties |> Ok

        ABlob ->
            (BString |> TBasic () L1.emptyProperties) |> DAlias () L1.emptyProperties |> Ok

        AStructure ->
            modelStructure shape name

        AList ->
            modelList shape name

        AMap ->
            modelMap shape name

        ATimestamp ->
            (BString |> TBasic () L1.emptyProperties) |> DAlias () L1.emptyProperties |> Ok

        AUnknown ->
            UnknownNotImplemented () |> ResultME.error


modelString : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelString shape name =
    case
        ( shape.enum
        , Maybe.Extra.isJust shape.max
            || Maybe.Extra.isJust shape.min
            || Maybe.Extra.isJust shape.pattern
        )
    of
        ( Just enumVals, False ) ->
            case List.Nonempty.fromList enumVals of
                Just nonemptyEnumVals ->
                    DEnum () L1.emptyProperties nonemptyEnumVals
                        |> Ok

                Nothing ->
                    NoMembers () name |> ResultME.error

        ( Nothing, True ) ->
            DRestricted
                ()
                L1.emptyProperties
                (RString { minLength = shape.min, maxLength = shape.max, regex = shape.pattern })
                |> Ok

        ( _, _ ) ->
            TBasic () L1.emptyProperties BString
                |> DAlias () L1.emptyProperties
                |> Ok


modelInt : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelInt shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            DRestricted ()
                L1.emptyProperties
                (RInt { min = shape.min, max = shape.max, width = Nothing })
                |> Ok

        _ ->
            (BInt |> TBasic () L1.emptyProperties) |> DAlias () L1.emptyProperties |> Ok


modelStructure : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelStructure shape name =
    let
        -- shape.required lists names of fields that are required.
        modelField :
            String
            -> ShapeRef
            -> List ( String, Type () Unchecked, L1.Properties )
            -> List ( String, Type () Unchecked, L1.Properties )
        modelField memberName shapeRef fieldAccum =
            let
                type_ =
                    shapeRefToL1Type shapeRef

                fieldProperties =
                    let
                        loc =
                            case shapeRef.location of
                                Header ->
                                    "header"

                                QueryString ->
                                    "querystring"

                                StatusCode ->
                                    "statuscode"

                                Uri ->
                                    "uri"

                                Body ->
                                    "body"

                        locationName =
                            shapeRef.locationName
                    in
                    Dict.empty
                        |> Dict.update "serializedName" (always (Maybe.map PString locationName))
                        |> Dict.insert "location" (PEnum AWSStubs.locationEnum loc)

                optionalField =
                    ( memberName
                    , type_ |> COptional |> TContainer () L1.emptyProperties
                    , fieldProperties
                    )

                requiredField =
                    ( memberName, type_, fieldProperties )
            in
            case shape.required of
                Nothing ->
                    optionalField :: fieldAccum

                Just requiredFields ->
                    if List.member memberName requiredFields then
                        requiredField :: fieldAccum

                    else
                        optionalField :: fieldAccum
    in
    case shape.members of
        Nothing ->
            NoMembers () name |> ResultME.error

        Just members ->
            let
                fields =
                    Dict.foldl modelField [] members
            in
            case List.Nonempty.fromList fields of
                Just nonemptyFields ->
                    nonemptyFields
                        |> Naming.sortNonemptyNamed
                        |> TProduct () L1.emptyProperties
                        |> DAlias () L1.emptyProperties
                        |> Ok

                Nothing ->
                    TEmptyProduct () L1.emptyProperties
                        |> DAlias () L1.emptyProperties
                        |> Ok


modelList : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelList shape name =
    case shape.member of
        Nothing ->
            ListMemberEmpty () |> ResultME.error

        Just ref ->
            (shapeRefToL1Type ref |> CList |> TContainer () L1.emptyProperties) |> DAlias () L1.emptyProperties |> Ok


modelMap : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelMap shape name =
    let
        keyTypeRes =
            case shape.key of
                Nothing ->
                    MapKeyEmpty () |> ResultME.error

                Just keyRef ->
                    shapeRefToL1Type keyRef |> Ok

        valTypeRes =
            case shape.value of
                Nothing ->
                    MapValueEmpty () |> ResultME.error

                Just valRef ->
                    shapeRefToL1Type valRef |> Ok
    in
    ResultME.map2
        (\keyType valType -> TContainer () L1.emptyProperties (CDict keyType valType) |> DAlias () L1.emptyProperties)
        keyTypeRes
        valTypeRes



--== Operations - First Pass


modelOperations :
    Dict String Operation
    -> ResultME (TransformError ()) (L1 ())
modelOperations operations =
    Dict.map
        (\name operation -> modelOperation name operation)
        operations
        |> ResultME.combineDict
        |> ResultME.map Dict.toList


modelOperation : String -> Operation -> ResultME (TransformError ()) (Declarable () Unchecked)
modelOperation name operation =
    let
        paramType opShapeRef =
            case opShapeRef of
                Nothing ->
                    TUnit () L1.emptyProperties |> Ok

                Just shapeRef ->
                    TNamed () L1.emptyProperties shapeRef.shape Unchecked |> Ok

        requestRes =
            paramType operation.input

        responseRes =
            paramType operation.output
    in
    ResultME.map2
        (\request response ->
            let
                declProps =
                    Dict.empty
                        |> Dict.update "documentation" (always (Maybe.map PString operation.documentation))

                funProps =
                    Dict.empty
                        |> Dict.insert "url" (operation.http.requestUri |> Maybe.withDefault "/" |> PString)
                        |> Dict.insert "httpMethod" (httpMethodToString operation.http.method |> PString)

                funType =
                    TFunction () funProps request response
            in
            DAlias () declProps funType
        )
        requestRes
        responseRes



--== Operations - Second Pass
-- The second pass over the operations is used to mark declarations that are the
-- requests and responses of the operations.


{-| Mark declarations in the model that are either requests or responses.
-}
markTopLevelShapes : Dict String Operation -> L2 () -> L2 ()
markTopLevelShapes operations model =
    Dict.foldl markTopLevelShape
        model
        operations


markTopLevelShape : String -> Operation -> L2 () -> L2 ()
markTopLevelShape _ operation l2model =
    let
        setTopLevelProp val props =
            Dict.insert "topLevel" (PEnum AWSStubs.topLevelEnum val) props

        markTopLevel tlPropVal opSide model =
            Maybe.map .shape opSide
                |> Maybe.andThen (\name -> Dict.get name model |> Maybe.map (\decl -> ( decl, name )))
                |> Maybe.map (\( decl, name ) -> ( L1.updatePropertiesOfDeclarable (setTopLevelProp tlPropVal) decl, name ))
                |> Maybe.map (\( decl, name ) -> Dict.insert name decl model)
    in
    markTopLevel "request" operation.input l2model
        |> Maybe.andThen (markTopLevel "response" operation.output)
        |> Maybe.withDefault l2model



--== Which serializers are needed - Third Pass
-- The third  pass over the data model looks at what parts of it need the
-- various encoders and decoders generated for them. This depends on whether
-- are serialized as JSON or XML, in the body, headers or query parameters and
-- so on.


{-| Find shapes that need decoder, encoder or both (codec).
Need encoder if request body has something as a dependency.
Need decoder if response body has something as a dependency.

Find shapes that need kv-encoder
Need kv-encoder if query params or request header has something as a dependency.

Find shapes that need kv-decoder.
Need kv-decoder if response header has something as a dependency.

Algorithm:

1.  Filter to relevant fields
2.  Take transitive closure
3.  Mark all in the closure.

-}
markCodecs : L2 () -> L2 ()
markCodecs l2 =
    let
        -- This makes a properties API on top of an empty set of defaults and specs.
        -- Useful function to pull out somewhere - maybe in the Query API?
        propertiesApi =
            L3.makePropertiesAPI L3.emptyDefaultProperties
                { properties = L1.emptyProperties
                , declarations = l2
                }

        closure =
            selectClosure propertiesApi l2 AWSStubs.isRequest

        -- _ =
        --     closure
        --         |> Result.withDefault Dict.empty
        --         |> Dict.keys
        --         |> Debug.log "\nClosure against the model."
        -- Use Dict.update to update matches with properties.
    in
    l2


{-| Filters out a starting set from the model and then computes its transitive closure.

This allows a set of things to be selected along with all of their dependencies.

-}
selectClosure propertiesApi model filter =
    Query.filterDictByProps propertiesApi filter model
        |> ResultME.andThen (\filtered -> Query.transitiveClosure filtered model)

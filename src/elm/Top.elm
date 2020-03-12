port module Top exposing (main)

import AWSService exposing (AWSService)
import Codec
import Dict exposing (Dict)
import Elm.CodeGen as CG
import Elm.Pretty
import Elm.Writer
import Errors exposing (Error)
import Json.Decode as Decode
import Json.Decode.Generic as Generic
import L3
import List.Nonempty
import Pretty
import Random exposing (Seed)
import ResultME exposing (ResultME)
import String.Case as Case
import Task
import Templates.AWSStubs
import Time exposing (Posix)
import Transform



-- Top level construction


main : Program () Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- Ports for data input and output


port modelInPort : (( String, String ) -> msg) -> Sub msg


port codeOutPort : ( String, String, List String ) -> Cmd msg


port errorOutPort : ( String, List String ) -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Error ->
            Sub.none

        _ ->
            Sub.batch
                [ modelInPort (\( name, value ) -> ModelData name value)
                ]



-- State Machine


type Model
    = Ready
    | Error



-- Events


type Msg
    = ModelData String String


init : a -> ( Model, Cmd Msg )
init _ =
    ( Ready, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Ready, ModelData name val ) ->
            case processServiceModel val of
                Ok ( service, outputString ) ->
                    ( Ready
                    , ( Case.toCamelCaseUpper service.metaData.serviceId ++ ".elm"
                      , outputString
                      , []
                      )
                        |> codeOutPort
                    )

                Err errors ->
                    let
                        printableErrors =
                            List.Nonempty.map Errors.asConsoleString errors
                                |> List.Nonempty.toList
                    in
                    ( Error, ( name, printableErrors ) |> errorOutPort )

        ( _, _ ) ->
            ( model, Cmd.none )


processServiceModel : String -> ResultME Error ( AWSService, String )
processServiceModel val =
    decodeServiceModel val
        |> ResultME.andThen transformToApiModel
        |> ResultME.andThen generateAWSStubs
        |> ResultME.map prettyPrint


decodeServiceModel : String -> ResultME Error AWSService
decodeServiceModel val =
    let
        decodeErrorFn err =
            { code = -1
            , title = "Decode Error"
            , body = "Could not decode the AWS service descriptor."
            , sources = []
            }
    in
    Codec.decodeString AWSService.awsServiceCodec val
        |> ResultME.fromResult
        |> ResultME.mapError decodeErrorFn


transformToApiModel : AWSService -> ResultME Error ( AWSService, L3.L3 () )
transformToApiModel service =
    Transform.transform service
        |> ResultME.map (Tuple.pair service)


generateAWSStubs : ( AWSService, L3.L3 () ) -> ResultME Error ( AWSService, CG.File )
generateAWSStubs ( service, apiModel ) =
    let
        posFn _ =
            Errors.emptySourceLines

        l3StubProcessor =
            L3.builder posFn Templates.AWSStubs.processorImpl

        propsAPI =
            L3.makePropertiesAPI l3StubProcessor.defaults apiModel
    in
    Templates.AWSStubs.generate propsAPI apiModel
        |> ResultME.map (Tuple.pair service)


prettyPrint : ( AWSService, CG.File ) -> ( AWSService, String )
prettyPrint ( service, stubFile ) =
    ( service, Elm.Pretty.pretty 120 stubFile )

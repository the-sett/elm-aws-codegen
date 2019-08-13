port module Top exposing (main)

import Codec
import Dict exposing (Dict)
import Diff
import Json.Decode as Decode
import Json.Decode.Generic as Generic
import Random exposing (Seed)
import Service exposing (Service)
import Task
import Time exposing (Posix)



-- Top level construction


main : Program () Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- Ports for data input and output


port modelInPort : (( String, String ) -> msg) -> Sub msg


port codeOutPort : String -> Cmd msg


subscriptions model =
    case model of
        Error _ ->
            Sub.none

        _ ->
            Sub.batch
                [ modelInPort (\( name, value ) -> ModelData name value)
                ]



-- State Machine


type Model
    = Initial
    | Seeded { seed : Seed }
    | LoadedModel { seed : Seed, dataModel : Service }
    | ModelProcessed
    | TemplateProcessed
    | Done
    | Error String



-- Events


type Msg
    = CreateSeed Posix
    | ModelData String String


init _ =
    ( Initial
    , Task.perform CreateSeed Time.now
    )


update msg model =
    case ( model, msg ) of
        ( Initial, CreateSeed posix ) ->
            ( Seeded { seed = Random.initialSeed <| Time.posixToMillis posix }, Cmd.none )

        ( Seeded { seed }, ModelData name val ) ->
            let
                example =
                    Codec.decodeString Service.serviceCodec val
            in
            case example of
                Ok service ->
                    let
                        _ =
                            Debug.log "Ok" name

                        original =
                            Decode.decodeString Generic.json val

                        parsed =
                            Decode.decodeString Generic.json (Codec.encodeToString 0 Service.serviceCodec service)

                        diffs =
                            case ( original, parsed ) of
                                ( Ok jsonl, Ok jsonr ) ->
                                    Diff.diff jsonl jsonr |> Diff.diffsToString |> logIfVal "Diffs"

                                ( _, _ ) ->
                                    "Failed to generic decode" |> Debug.log "Error"
                    in
                    ( model
                    , Codec.encodeToString 4 Service.serviceCodec service |> codeOutPort
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "Error" (name ++ " - " ++ Decode.errorToString err)
                    in
                    ( model
                    , Decode.errorToString err |> codeOutPort
                    )

        ( _, _ ) ->
            ( model, Cmd.none )


logIfVal : String -> String -> String
logIfVal label val =
    if val == "" then
        val

    else
        Debug.log label val

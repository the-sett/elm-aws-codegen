module IO exposing (..)

import Task


example : IO () String (List String)
example =
    task (Task.succeed [])
        |> andThen addOneOrFail
        |> onError (\errorMsg -> pure [ errorMsg ] |> Debug.log "recovery")
        |> andThen addOneOrFail
        |> andThen (\ys -> task (Task.succeed ys) |> Debug.log "later task")
        |> andThen (\zs -> List.reverse zs |> Debug.log "reverse" |> pure)


addOneOrFail =
    \xs ->
        if List.isEmpty xs then
            err "Was Empty" |> Debug.log "failed"

        else
            "next" :: xs |> Debug.log "added" |> pure


main =
    run () example



--


type alias Program s x a =
    Platform.Program () s (IO s x a)


run : s -> IO s x a -> Platform.Program () s (IO s x a)
run state io =
    Platform.worker
        { init = \_ -> update io state
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : IO s x a -> s -> ( s, Cmd (IO s x a) )
update (State io) state =
    case io state of
        ( innerS, IOTask t ) ->
            ( innerS
            , Task.attempt
                (\r ->
                    case r of
                        Ok x ->
                            x

                        Err e ->
                            err e
                )
                t
            )

        ( innerS, IOOk x ) ->
            ( innerS
            , Cmd.none
            )

        ( innerS, IOErr e ) ->
            ( innerS
            , Cmd.none
            )



--


type IO s x a
    = State (s -> ( s, T s x a ))


type T s x a
    = IOTask (Task.Task x (IO s x a))
    | IOOk a
    | IOErr x



--


pure : a -> IO s x a
pure val =
    (\s -> ( s, IOOk val ))
        |> State


err : x -> IO s x a
err e =
    (\s -> ( s, IOErr e ))
        |> State


task : Task.Task x a -> IO s x a
task t =
    (\s -> ( s, t |> Task.map pure |> IOTask ))
        |> State



--


void : IO s x a -> IO s x ()
void =
    map (always ())



--


map : (a -> b) -> IO s x a -> IO s x b
map mf (State io) =
    (\s ->
        case io s of
            ( innerS, IOTask t ) ->
                ( innerS
                , Task.andThen (\inner -> Task.succeed (map mf inner)) t |> IOTask
                )

            ( innerS, IOOk x ) ->
                ( innerS
                , mf x |> IOOk
                )

            ( innerS, IOErr e ) ->
                ( innerS
                , IOErr e
                )
    )
        |> State


map2 =
    Debug.todo ""


andThen : (a -> IO s x b) -> IO s x a -> IO s x b
andThen mf (State io) =
    (\s ->
        case io s of
            ( innerS, IOTask t ) ->
                ( innerS
                , Task.andThen (\inner -> Task.succeed (andThen mf inner)) t
                    |> IOTask
                )

            ( innerS, IOOk x ) ->
                let
                    (State stateFn) =
                        mf x
                in
                stateFn innerS

            ( innerS, IOErr e ) ->
                ( innerS
                , IOErr e
                )
    )
        |> State


andMap : IO s x a -> IO s x (a -> b) -> IO s x b
andMap ma mf =
    andThen (\f -> andThen (f >> pure) ma) mf


onError : (x -> IO s y a) -> IO s x a -> IO s y a
onError ef (State io) =
    (\s ->
        case io s of
            ( innerS, IOTask t ) ->
                ( innerS
                , Task.onError
                    (\e -> ef e |> Task.succeed)
                    (t |> Task.map (onError ef))
                    |> IOTask
                )

            ( innerS, IOOk x ) ->
                ( innerS, IOOk x )

            ( innerS, IOErr e ) ->
                let
                    (State stateFn) =
                        ef e
                in
                stateFn innerS
    )
        |> State


sequence : List (IO s x a) -> IO s x (List a)
sequence ios =
    List.foldr (map2 (::)) (pure []) ios

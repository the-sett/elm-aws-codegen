module IO exposing (..)

import Task


type alias Model =
    { messages : List String }


example : IO Model String ()
example =
    task (Task.succeed "")
        |> andThen (\_ -> err "error")
        |> andThen push
        |> onError (\_ -> pure "recovery1" |> andThen push)
        |> andThen (\_ -> pure "success")
        |> andThen push
        |> andThen (\_ -> task (Task.succeed "task"))
        |> andThen push
        |> andThen (\_ -> task (Task.fail "failed task"))
        |> andThen push
        |> onError (\_ -> pure "recovery2" |> andThen push)
        |> andThen (\_ -> get)
        |> map (\s -> Debug.log "state" s)
        |> andThen (\_ -> modify (\state -> { state | messages = List.reverse state.messages }))


push : String -> IO Model String ()
push =
    \msg -> modify (\state -> { state | messages = msg :: state.messages })


main =
    program { messages = [ "initial" ] } example



--


type alias Program s x a =
    Platform.Program () s (IO s x a)


program : s -> IO s x a -> Platform.Program () s (IO s x a)
program state io =
    Platform.worker
        { init = \_ -> evalTasks io state
        , update = evalTasks
        , subscriptions = \_ -> Sub.none
        }



--run : IO s x a -> s -> ( s, Result x a )
--run (State io) state =
--    Debug.todo ""


evalTasks : IO s x a -> s -> ( s, Cmd (IO s x a) )
evalTasks (State io) state =
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


advance : (s -> ( s, a )) -> IO s x a
advance fn =
    (\s -> fn s |> Tuple.mapSecond IOOk)
        |> State



--


get : IO s x s
get =
    State (\s -> ( s, IOOk s ))


put : s -> IO s x ()
put s =
    State (\_ -> ( s, IOOk () ))


modify : (s -> s) -> IO s x ()
modify fn =
    State (\s -> ( fn s, IOOk () ))


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

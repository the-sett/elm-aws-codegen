module IO exposing (..)

import Task


type alias Model =
    { messages : List String }


example : Procedure Model String ()
example =
    task (Task.succeed "success1")
        |> andThen push
        |> andThen (\_ -> err "error")
        |> andThen push
        |> onError recover
        |> andThen (\_ -> pure "success2")
        |> andThen push
        |> andThen (\_ -> task (Task.succeed "task"))
        |> andThen push
        |> andThen (\_ -> task (Task.fail "failed task"))
        |> andThen push
        |> andThen (\_ -> pure "skipped pure")
        |> andThen push
        |> andThen (\_ -> task (Task.succeed "skipped task"))
        |> andThen push
        |> onError recover
        |> andThen (\_ -> get)
        |> map (\s -> Debug.log "state" s)
        |> andThen (\_ -> modify (\state -> { state | messages = List.reverse state.messages }))


push : String -> Procedure Model String ()
push msg =
    modify (\state -> { state | messages = msg :: state.messages })


recover : String -> Procedure Model String ()
recover msg =
    pure ("recovered " ++ msg) |> andThen push


main =
    program { messages = [ "initial" ] } example


mostlyApplicative f aResult bResult =
    pure (\a b -> { a = a, b = b, c = f a b })
        |> andMap aResult
        |> andMap bResult



--


type alias Program s x a =
    Platform.Program () s (Procedure s x a)


program : s -> Procedure s x a -> Platform.Program () s (Procedure s x a)
program state io =
    Platform.worker
        { init = \_ -> run io state
        , update = run
        , subscriptions = \_ -> Sub.none
        }


run : Procedure s x a -> s -> ( s, Cmd (Procedure s x a) )
run proc state =
    let
        ( innerState, maybeCmd ) =
            eval proc state
    in
    case maybeCmd of
        Just cmd ->
            ( innerState, cmd )

        Nothing ->
            let
                _ =
                    Debug.log "run" "terminated"
            in
            ( innerState, Cmd.none )


eval : Procedure s x a -> s -> ( s, Maybe (Cmd (Procedure s x a)) )
eval (State io) state =
    case io state of
        ( innerS, PTask t ) ->
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
                |> Just
            )

        ( innerS, POk x ) ->
            ( innerS
            , Nothing
            )

        ( innerS, PErr e ) ->
            ( innerS
            , Nothing
            )



--


type Procedure s x a
    = State (s -> ( s, T s x a ))


type T s x a
    = PTask (Task.Task x (Procedure s x a))
    | POk a
    | PErr x



--


pure : a -> Procedure s x a
pure val =
    (\s -> ( s, POk val ))
        |> State


err : x -> Procedure s x a
err e =
    (\s -> ( s, PErr e ))
        |> State


task : Task.Task x a -> Procedure s x a
task t =
    (\s -> ( s, t |> Task.map pure |> PTask ))
        |> State


advance : (s -> ( s, a )) -> Procedure s x a
advance fn =
    (\s -> fn s |> Tuple.mapSecond POk)
        |> State



--


get : Procedure s x s
get =
    State (\s -> ( s, POk s ))


put : s -> Procedure s x ()
put s =
    State (\_ -> ( s, POk () ))


modify : (s -> s) -> Procedure s x ()
modify fn =
    State (\s -> ( fn s, POk () ))


void : Procedure s x a -> Procedure s x ()
void =
    map (always ())



--


map : (a -> b) -> Procedure s x a -> Procedure s x b
map mf (State io) =
    (\s ->
        case io s of
            ( innerS, PTask t ) ->
                ( innerS
                , Task.andThen (\inner -> Task.succeed (map mf inner)) t |> PTask
                )

            ( innerS, POk x ) ->
                ( innerS
                , mf x |> POk
                )

            ( innerS, PErr e ) ->
                ( innerS
                , PErr e
                )
    )
        |> State


map2 :
    (a -> b -> c)
    -> Procedure s x a
    -> Procedure s x b
    -> Procedure s x c
map2 f p1 p2 =
    pure f
        |> andMap p1
        |> andMap p2


map3 :
    (a -> b -> c -> d)
    -> Procedure s x a
    -> Procedure s x b
    -> Procedure s x c
    -> Procedure s x d
map3 f p1 p2 p3 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3


map4 :
    (a -> b -> c -> d -> e)
    -> Procedure s x a
    -> Procedure s x b
    -> Procedure s x c
    -> Procedure s x d
    -> Procedure s x e
map4 f p1 p2 p3 p4 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Procedure s x a
    -> Procedure s x b
    -> Procedure s x c
    -> Procedure s x d
    -> Procedure s x e
    -> Procedure s x f
map5 f p1 p2 p3 p4 p5 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4
        |> andMap p5


map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Procedure s x a
    -> Procedure s x b
    -> Procedure s x c
    -> Procedure s x d
    -> Procedure s x e
    -> Procedure s x f
    -> Procedure s x g
map6 f p1 p2 p3 p4 p5 p6 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4
        |> andMap p5
        |> andMap p6


andThen : (a -> Procedure s x b) -> Procedure s x a -> Procedure s x b
andThen mf (State io) =
    (\s ->
        case io s of
            ( innerS, PTask t ) ->
                ( innerS
                , Task.andThen (\inner -> Task.succeed (andThen mf inner)) t
                    |> PTask
                )

            ( innerS, POk x ) ->
                let
                    (State stateFn) =
                        mf x
                in
                stateFn innerS

            ( innerS, PErr e ) ->
                ( innerS
                , PErr e
                )
    )
        |> State


andMap : Procedure s x a -> Procedure s x (a -> b) -> Procedure s x b
andMap ma mf =
    andThen (\f -> map f ma) mf


onError : (x -> Procedure s y a) -> Procedure s x a -> Procedure s y a
onError ef (State io) =
    (\s ->
        case io s of
            ( innerS, PTask t ) ->
                ( innerS
                , Task.onError
                    (\e -> ef e |> Task.succeed)
                    (t |> Task.map (onError ef))
                    |> PTask
                )

            ( innerS, POk x ) ->
                ( innerS, POk x )

            ( innerS, PErr e ) ->
                let
                    (State stateFn) =
                        ef e
                in
                stateFn innerS
    )
        |> State


sequence : List (Procedure s x a) -> Procedure s x (List a)
sequence ios =
    List.foldr (map2 (::)) (pure []) ios

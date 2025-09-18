module Imp exposing
    ( Imp
    , advance
    , andMap
    , andThen
    , err
    , eval
    , get
    , map
    , map2
    , map3
    , map4
    , map5
    , map6
    , modify
    , onError
    , program
    , pure
    , put
    , result
    , sequence
    , task
    , void
    )

import Task


type alias Program flags model err res =
    Platform.Program flags model (Imp model err res)


program : flags -> (flags -> s) -> Imp s x a -> Platform.Program flags s (Imp s x a)
program flags initFn io =
    Platform.worker
        { init = \_ -> eval io (initFn flags)
        , update = eval
        , subscriptions = \_ -> Sub.none
        }


eval : Imp s x a -> s -> ( s, Cmd (Imp s x a) )
eval (State io) state =
    case io state |> Debug.log "eval" of
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
            )

        ( innerS, POk x ) ->
            ( innerS
            , Cmd.none
            )

        ( innerS, PErr e ) ->
            ( innerS
            , Cmd.none
            )



--


type Imp s x a
    = State (s -> ( s, T s x a ))


type T s x a
    = PTask (Task.Task x (Imp s x a))
    | POk a
    | PErr x



--


pure : a -> Imp s x a
pure val =
    (\s -> ( s, POk val ))
        |> State


err : x -> Imp s x a
err e =
    (\s -> ( s, PErr e ))
        |> State


task : Task.Task x a -> Imp s x a
task t =
    (\s -> ( s, t |> Task.map pure |> PTask ))
        |> State


result : Result x a -> Imp s x a
result res =
    case res of
        Ok x ->
            pure x

        Err e ->
            err e



--


get : Imp s x s
get =
    State (\s -> ( s, POk s ))


put : s -> Imp s x ()
put s =
    State (\_ -> ( s, POk () ))


advance : (s -> ( s, a )) -> Imp s x a
advance fn =
    (\s -> fn s |> Tuple.mapSecond POk)
        |> State


modify : (s -> s) -> Imp s x ()
modify fn =
    State (\s -> ( fn s, POk () ))


void : Imp s x a -> Imp s x ()
void =
    map (always ())



--


map : (a -> b) -> Imp s x a -> Imp s x b
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
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
map2 f p1 p2 =
    pure f
        |> andMap p1
        |> andMap p2


map3 :
    (a -> b -> c -> d)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
    -> Imp s x d
map3 f p1 p2 p3 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3


map4 :
    (a -> b -> c -> d -> e)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
    -> Imp s x d
    -> Imp s x e
map4 f p1 p2 p3 p4 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
    -> Imp s x d
    -> Imp s x e
    -> Imp s x f
map5 f p1 p2 p3 p4 p5 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4
        |> andMap p5


map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Imp s x a
    -> Imp s x b
    -> Imp s x c
    -> Imp s x d
    -> Imp s x e
    -> Imp s x f
    -> Imp s x g
map6 f p1 p2 p3 p4 p5 p6 =
    pure f
        |> andMap p1
        |> andMap p2
        |> andMap p3
        |> andMap p4
        |> andMap p5
        |> andMap p6


andThen : (a -> Imp s x b) -> Imp s x a -> Imp s x b
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


andMap : Imp s x a -> Imp s x (a -> b) -> Imp s x b
andMap ma mf =
    andThen (\f -> map f ma) mf


onError : (x -> Imp s y a) -> Imp s x a -> Imp s y a
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


sequence : List (Imp s x a) -> Imp s x (List a)
sequence ios =
    List.foldr (map2 (::)) (pure []) ios

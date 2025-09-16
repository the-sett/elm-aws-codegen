module IO exposing (..)

import Task



--


type alias Program =
    Platform.Program () Model Msg


run : IO Never () -> Program
run app =
    Platform.worker
        { init = update app
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    ()


type alias Msg =
    IO Never ()


update : Msg -> model -> ( model, Cmd Msg )
update msg model =
    case msg of
        IOResult r ->
            case r of
                Ok x ->
                    ( model
                    , Task.perform identity (Task.succeed (pure x))
                    )

                Err e ->
                    ( model
                    , Task.perform identity (Task.fail e)
                    )

        IOTask t ->
            ( model
            , Task.perform (Task.succeed >> IOTask) t
            )

        IOPure x ->
            ( model
            , Task.perform identity (Task.succeed (pure x))
            )



--


type IO x a
    = IOResult (Result x a)
    | IOTask (Task.Task x (IO x a))
    | IOPure a



--


ok : a -> IO x a
ok val =
    Ok val |> IOResult


err : x -> IO x a
err e =
    Err e |> IOResult


succeed : a -> IO x a
succeed t =
    Task.succeed (pure t) |> IOTask


fail : x -> IO x a
fail e =
    Task.fail e |> IOTask


pure : a -> IO x a
pure x =
    IOPure x



--


map : (a -> b) -> IO x a -> IO x b
map mf io =
    case io of
        IOResult r ->
            Result.map mf r |> IOResult

        IOTask t ->
            Task.andThen (\inner -> Task.succeed (map mf inner)) t
                |> IOTask

        IOPure x ->
            mf x |> IOPure


andThen : (a -> IO x b) -> IO x a -> IO x b
andThen mf io =
    case io of
        IOResult r ->
            case r of
                Ok x ->
                    mf x

                Err e ->
                    Err e |> IOResult

        IOTask t ->
            Task.andThen (\inner -> Task.succeed (andThen mf inner)) t
                |> IOTask

        IOPure x ->
            mf x


andMap : IO x a -> IO x (a -> b) -> IO x b
andMap ma mf =
    andThen (\f -> andThen (f >> pure) ma) mf



-- TASKS
--run : IO x a -> IO Never (Result x a)
--run task =
--    task
--        |> IO.map Ok
--        |> IO.onError (Err >> IO.succeed)
--
--
--throw : x -> IO x a
--throw =
--    IO.fail
-- IO
--io : IO Never a -> IO x a
--io work =
--    IO.mapError never work
--
--
--mio : x -> IO Never (Maybe a) -> IO x a
--mio x work =
--    work
--        |> IO.mapError never
--        |> IO.andThen
--            (\m ->
--                case m of
--                    Just a ->
--                        IO.succeed a
--
--                    Nothing ->
--                        IO.fail x
--            )
--
--
--eio : (x -> y) -> IO Never (Result x a) -> IO y a
--eio func work =
--    work
--        |> IO.mapError never
--        |> IO.andThen
--            (\m ->
--                case m of
--                    Ok a ->
--                        IO.succeed a
--
--                    Err err ->
--                        func err |> IO.fail
--            )
--
--
--
---- INSTANCES
--
--
--void : IO x a -> IO x ()
--void =
--    IO.map (always ())
--
--
--pure : a -> IO x a
--pure =
--    IO.succeed
--
--
--apply : IO x a -> IO x (a -> b) -> IO x b
--apply ma mf =
--    bind (\f -> bind (pure << f) ma) mf
--
--
--fmap : (a -> b) -> IO x a -> IO x b
--fmap =
--    IO.map
--
--
--bind : (a -> IO x b) -> IO x a -> IO x b
--bind =
--    IO.andThen
--
--
--mapM : (a -> IO x b) -> List a -> IO x (List b)
--mapM f =
--    List.map f >> IO.sequence

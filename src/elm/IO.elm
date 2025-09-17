module IO exposing (..)

import Task


example : IO String (List Int)
example =
    pure 1
        |> andThen (\x -> [ x ] |> Debug.log "step 1" |> pure)
        |> andThen
            (\xs ->
                if List.isEmpty xs then
                    err "Was Empty"

                else
                    2 :: xs |> Debug.log "step 2" |> pure
            )
        |> andThen (\ys -> task (Task.succeed ys) |> Debug.log "step 3")
        |> andThen (\zs -> List.reverse zs |> Debug.log "step 4" |> pure)
        |> andThen (\ys -> pure ys |> Debug.log "step 5")


main =
    run () example



--


type alias Program model x a =
    Platform.Program () model (IO x a)


run : model -> IO x a -> Platform.Program () model (IO x a)
run model io =
    Platform.worker
        { init = \_ -> update io model
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : IO x a -> model -> ( model, Cmd (IO x a) )
update msg model =
    case msg |> Debug.log "msg" of
        IOResult r ->
            case r of
                Ok x ->
                    ( model
                    , Cmd.none
                    )

                Err e ->
                    ( model
                    , Cmd.none
                    )

        IOTask t ->
            ( model
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

        IOPure x ->
            ( model
            , Cmd.none
            )



--


type IO x a
    = IOResult (Result x a)
    | IOTask (Task.Task x (IO x a))
    | IOPure a



--


pure : a -> IO x a
pure val =
    Ok val |> IOResult


err : x -> IO x a
err e =
    Err e |> IOResult


task : Task.Task x a -> IO x a
task t =
    t |> Task.map pure |> IOTask



--
--void : IO x a -> IO x ()
--void =
--    IO.map (always ())
--
--
--mapM : (a -> IO x b) -> List a -> IO x (List b)
--mapM f =
--    List.map f >> IO.sequence
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

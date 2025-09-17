module IO exposing (..)

import Task


example : IO String (List String)
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

        IOOk x ->
            ( model
            , Cmd.none
            )

        IOErr e ->
            ( model
            , Cmd.none
            )



--


type IO x a
    = IOTask (Task.Task x (IO x a))
    | IOOk a
    | IOErr x



--


pure : a -> IO x a
pure val =
    IOOk val


err : x -> IO x a
err e =
    IOErr e


task : Task.Task x a -> IO x a
task t =
    t |> Task.map pure |> IOTask



--


void : IO x a -> IO x ()
void =
    map (always ())



--


map : (a -> b) -> IO x a -> IO x b
map mf io =
    case io of
        IOTask t ->
            Task.andThen (\inner -> Task.succeed (map mf inner)) t
                |> IOTask

        IOOk x ->
            mf x |> IOOk

        IOErr e ->
            IOErr e


map2 =
    Debug.todo ""


andThen : (a -> IO x b) -> IO x a -> IO x b
andThen mf io =
    case io of
        IOTask t ->
            Task.andThen (\inner -> Task.succeed (andThen mf inner)) t
                |> IOTask

        IOOk x ->
            mf x

        IOErr e ->
            IOErr e


andMap : IO x a -> IO x (a -> b) -> IO x b
andMap ma mf =
    andThen (\f -> andThen (f >> pure) ma) mf


onError : (x -> IO y a) -> IO x a -> IO y a
onError ef io =
    case io of
        IOTask t ->
            Task.onError
                (\e -> ef e |> Task.succeed)
                (t |> Task.map (onError ef))
                |> IOTask

        IOOk x ->
            pure x

        IOErr e ->
            ef e


sequence : List (IO x a) -> IO x (List a)
sequence ios =
    List.foldr (map2 (::)) (pure []) ios

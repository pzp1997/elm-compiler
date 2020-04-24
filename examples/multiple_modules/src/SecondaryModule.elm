module SecondaryModule exposing (update)


update : msg -> Int -> ( Int, Cmd msg )
update _ n =
    ( n + 1, Cmd.none )

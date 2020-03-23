module Main exposing (main)

one : Int
one = 1

noSubs : flag -> Sub msg
noSubs _ = Sub.none

main : Program () Int Never
main =
    Platform.worker
        { init = \_ -> ( one, Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = noSubs
        }

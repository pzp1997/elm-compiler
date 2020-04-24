module Main exposing (main)

seven = 7

double x = x + x

main : Program () Int Never
main =
    Platform.worker
        { init = \_ -> ( double (double seven * seven), Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }

module Main exposing (main)

import SecondaryModule


main : Program () Int Never
main =
    Platform.worker
        { init = \_ -> ( 0, Cmd.none )
        , update = SecondaryModule.update
        , subscriptions = \_ -> Sub.none
        }

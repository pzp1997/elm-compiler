module Main exposing (main)

import List

one : Int
one = 1

noSubs : flag -> Sub msg
noSubs _ = Sub.none

main : Program () (List Int) Never
main =
    Platform.worker
        { init = \_ -> ( List.map identity [one], Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = noSubs
        }

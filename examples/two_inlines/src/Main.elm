module Main exposing (main)

import Html


formatAnswer : Int -> String
formatAnswer ans =
    "The answer is " ++ String.fromInt ans ++ "!"


abs : Int -> Int
abs n =
    if n < 0 then -n
    else n

main : Html.Html msg
main = Html.text (formatAnswer (abs -2))

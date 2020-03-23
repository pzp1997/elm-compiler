module Main exposing (factorial)

import Html

factorial : Int -> Int
factorial n =
    if n <= 0 then 1
    else n * factorial (n - 1)

main : Html.Html msg
main = Html.text (String.fromInt (factorial 5))

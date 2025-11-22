module Util.String exposing (toTitle)


toTitle : String -> String
toTitle str =
    case String.uncons str of
        Nothing ->
            str

        Just ( first, rest ) ->
            String.cons (Char.toUpper first) (String.toLower rest)
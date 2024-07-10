module Parse exposing (parse, parseNumber, parseNumberHelper, toTokenList)

import Expression exposing (Expression, Operation(..), Token(..))
import IsValid exposing (check)


parse : Expression -> Result String (List Token)
parse expression =
    toTokenList expression
        |> Result.andThen check


toTokenList : Expression -> Result String (List Token)
toTokenList expression =
    case expression |> String.toList |> List.head of
        Just c ->
            if Char.isDigit c then
                case parseNumber expression of
                    Just ( num, rest ) ->
                        appendIfOk [ Number num ] (toTokenList rest)

                    Nothing ->
                        Err "Unexpected end of expression"

            else
                case c of
                    ' ' ->
                        appendIfOk [] (toTokenList (String.dropLeft 1 expression))

                    '(' ->
                        appendIfOk [ Open ] (toTokenList (String.dropLeft 1 expression))

                    ')' ->
                        appendIfOk [ Close ] (toTokenList (String.dropLeft 1 expression))

                    '+' ->
                        appendIfOk [ Operation Add ] (toTokenList (String.dropLeft 1 expression))

                    '-' ->
                        appendIfOk [ Operation Sub ] (toTokenList (String.dropLeft 1 expression))

                    '*' ->
                        appendIfOk [ Operation Mul ] (toTokenList (String.dropLeft 1 expression))

                    '/' ->
                        appendIfOk [ Operation Div ] (toTokenList (String.dropLeft 1 expression))

                    _ ->
                        Err ("Unexpected character: '" ++ String.fromChar c ++ "'")

        Nothing ->
            Ok []


parseNumber : String -> Maybe ( Float, String )
parseNumber str =
    parseNumberHelper str ""


parseNumberHelper : String -> String -> Maybe ( Float, String )
parseNumberHelper str acc =
    if String.isEmpty str then
        String.toFloat acc |> Maybe.map (\num -> ( num, "" ))

    else if Char.isDigit (str |> String.toList |> List.head |> Maybe.withDefault ' ') then
        parseNumberHelper (String.dropLeft 1 str) (acc ++ String.left 1 str)

    else
        String.toFloat acc |> Maybe.map (\num -> ( num, str ))


appendIfOk : List a -> Result String (List a) -> Result String (List a)
appendIfOk list result =
    case result of
        Ok r ->
            Ok (List.append list r)

        Err e ->
            Err e

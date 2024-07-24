module Parse exposing (errorToString, parse)

import Expression exposing (Expression, Operation(..), Token(..))
import IsValid exposing (check)


parse : Expression -> Result ParseError (List Token)
parse expression =
    case toTokenList 0 expression of
        -- Ok tokens ->
        --     check tokens
        Ok tokens ->
            Ok tokens

        Err e ->
            Err e


errorToString : ParseError -> String
errorToString error =
    case error of
        UnexpectedCharacter c index ->
            "Unexpected character: '" ++ String.fromChar c ++ "' at position " ++ String.fromInt index

        UnexpectedEndOfExpression ->
            "Unexpected end of expression"


type ParseError
    = UnexpectedCharacter Char Int
    | UnexpectedEndOfExpression


toTokenList : Int -> Expression -> Result ParseError (List Token)
toTokenList index expression =
    case expression |> String.toList |> List.head of
        Just c ->
            if Char.isDigit c then
                case parseNumber expression of
                    Just ( num, rest ) ->
                        appendIfOk [ Number num ] (toTokenList (index + 1) rest)

                    Nothing ->
                        Err UnexpectedEndOfExpression

            else
                case c of
                    ' ' ->
                        appendIfOk [] (toTokenList (index + 1) (String.dropLeft 1 expression))

                    '(' ->
                        appendIfOk [ Open ] (toTokenList (index + 1) (String.dropLeft 1 expression))

                    ')' ->
                        appendIfOk [ Close ] (toTokenList (index + 1) (String.dropLeft 1 expression))

                    '+' ->
                        appendIfOk [ Operation Add ] (toTokenList (index + 1) (String.dropLeft 1 expression))

                    '-' ->
                        appendIfOk [ Operation Sub ] (toTokenList (index + 1) (String.dropLeft 1 expression))

                    '*' ->
                        appendIfOk [ Operation Mul ] (toTokenList (index + 1) (String.dropLeft 1 expression))

                    '/' ->
                        appendIfOk [ Operation Div ] (toTokenList (index + 1) (String.dropLeft 1 expression))

                    _ ->
                        Err (UnexpectedCharacter c index)

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


appendIfOk : List a -> Result b (List a) -> Result b (List a)
appendIfOk list result =
    case result of
        Ok r ->
            Ok (List.append list r)

        Err e ->
            Err e

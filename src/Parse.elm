module Parse exposing (errorToString, parse)

import Expression exposing (Expression, Operation(..), Token(..))


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

const_pi : Float
const_pi = 3.141592653
const_e : Float
const_e = 2.718281828
const_tau : Float
const_tau = 2 * const_pi

toTokenList : Int -> Expression -> Result ParseError (List Token)
toTokenList index expression =
    case expression |> String.toList  of
            ' ' :: rest ->
                appendIfOk [] (toTokenList (index + 1) (String.fromList rest))

            '(' :: rest ->
                appendIfOk [ Open ] (toTokenList (index + 1) (String.fromList rest))

            ')' ::rest ->
                appendIfOk [ Close ] (toTokenList (index + 1) (String.fromList rest))

            '+' :: rest ->
                appendIfOk [ Operation Add ] (toTokenList (index + 1) (String.fromList rest))

            '-' :: rest ->
                appendIfOk [ Operation Sub ] (toTokenList (index + 1) (String.fromList rest))

            '*' :: rest ->
                appendIfOk [ Operation Mul ] (toTokenList (index + 1) (String.fromList rest))

            '/' :: rest ->
                appendIfOk [ Operation Div ] (toTokenList (index + 1) (String.fromList rest))

            'p' :: 'i' :: rest ->
                appendIfOk [ Number 3.141592653 ] (toTokenList (index + 1) (String.fromList rest))

            'e' :: rest ->
                appendIfOk [ Number 2.718281828 ] (toTokenList (index + 1) (String.fromList rest))

            't' :: 'a' :: 'u' :: rest ->
                appendIfOk [ Number const_tau ] (toTokenList (index + 1) (String.fromList rest))

            's' :: 'i' :: 'n' :: rest ->
                appendIfOk [ Operation Sin] (toTokenList (index + 1) (String.fromList rest))

            c :: _ ->
                if Char.isDigit c then
                    case parseNumber expression of
                        Just ( num, re ) ->
                            appendIfOk [ Number num ] (toTokenList (index + 1) re)

                        Nothing ->
                            Err UnexpectedEndOfExpression
                        
                else
                    Err (UnexpectedCharacter c index)
                
            [] ->
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

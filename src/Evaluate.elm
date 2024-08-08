module Evaluate exposing (..)


type Token
    = Number Float
    | Operation Operation
    | Open
    | Close


type Operation
    = Add
    | Sub
    | Mul
    | Div
    | Sin
    | Exp


operationToString : Operation -> String
operationToString op =
    case op of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "*"

        Div ->
            "/"

        Sin ->
            "sin"

        Exp ->
            "^"


evaluate : String -> Result String Float
evaluate expression =
    parse expression |> Result.map evaluateTokens


parse : String -> Result String (List Token)
parse expression =
    expression |> String.toList |> charListToTokenList


charListToTokenList : List Char -> Result String (List Token)
charListToTokenList expression =
    let
        ( parseResult, rest ) =
            case expression of
                ' ' :: r ->
                    ( Ok [], r )

                '(' :: r ->
                    ( Ok [ Open ], r )

                ')' :: r ->
                    ( Ok [ Close ], r )

                '+' :: r ->
                    ( Ok [ Operation Add ], r )

                '-' :: r ->
                    ( Ok [ Operation Sub ], r )

                '*' :: r ->
                    ( Ok [ Operation Mul ], r )

                '/' :: r ->
                    ( Ok [ Operation Div ], r )

                '^' :: r ->
                    ( Ok [ Operation Exp ], r )

                's' :: 'i' :: 'n' :: r ->
                    ( Ok [ Operation Sin ], r )

                'p' :: 'i' :: r ->
                    ( Ok [ Number 3.141592653 ], r )

                'e' :: r ->
                    ( Ok [ Number 2.718281828 ], r )

                't' :: 'a' :: 'u' :: r ->
                    ( Ok [ Number (2 * 3.141592653) ], r )

                c :: _ ->
                    -- If the character is a digit, parse the string until the end of the number
                    if Char.isDigit c then
                        case parseNumber expression of
                            Just ( num, r ) ->
                                ( Ok [ Number num ], r )

                            Nothing ->
                                ( Err "Unexpected end of expression", expression )

                    else
                        ( Err ("Unexpected character: '" ++ String.fromChar c ++ "'"), expression )

                [] ->
                    ( Ok [], expression )
    in
    if rest == [] then
        parseResult

    else
        case parseResult of
            Ok tokens ->
                case charListToTokenList rest of
                    Ok r ->
                        Ok (List.append tokens r)

                    Err err ->
                        Err err

            Err err ->
                Err err


parseNumber : List Char -> Maybe ( Float, List Char )
parseNumber numberString =
    let
        isDigitOrDot : Char -> Bool
        isDigitOrDot ch =
            Char.isDigit ch || ch == '.'

        parseNumberHelper : List Char -> List Char -> Maybe ( Float, List Char )
        parseNumberHelper str acc =
            if isDigitOrDot (str |> List.head |> Maybe.withDefault ' ') then
                parseNumberHelper (List.drop 1 str) (acc ++ List.take 1 str)

            else
                acc |> String.fromList |> String.toFloat |> Maybe.map (\num -> ( num, str ))
    in
    parseNumberHelper numberString []


evaluateTokens : List Token -> Float
evaluateTokens tokens =
    let
        binaryOperation : Operation -> (Float -> Float -> Float) -> List Token -> Maybe (List Token)
        binaryOperation op doOperation t =
            case t of
                (Number left) :: (Operation op2) :: (Number right) :: xs ->
                    if op == op2 then
                        Just (Number (doOperation left right) :: xs)

                    else
                        Nothing

                _ ->
                    Nothing

        parenthesesTransform : List Token -> Maybe (List Token)
        parenthesesTransform t =
            case t of
                Open :: x :: Close :: xs ->
                    Just (x :: xs)

                Open :: Close :: xs ->
                    Just xs

                _ ->
                    Nothing

        sinRadiansTransform : List Token -> Maybe (List Token)
        sinRadiansTransform t =
            case t of
                (Operation Sin) :: (Number right) :: xs ->
                    Just (Number (sin right) :: xs)

                _ ->
                    Nothing

        resolve : (List Token -> Maybe (List Token)) -> List Token -> List Token
        resolve doOperation t =
            case doOperation t of
                Just newTokens ->
                    resolve doOperation newTokens

                Nothing ->
                    case t of
                        x :: xs ->
                            x :: resolve doOperation xs

                        [] ->
                            []
    in
    case tokens of
        [ Number result ] ->
            result

        [] ->
            0

        _ ->
            tokens
                |> resolve parenthesesTransform
                |> resolve sinRadiansTransform
                |> resolve (binaryOperation Exp (^))
                |> resolve (binaryOperation Mul (*))
                |> resolve (binaryOperation Div (/))
                -- Note: Subtraction needs to be done before addition because we're evaluating left to right
                |> resolve (binaryOperation Sub (-))
                |> resolve (binaryOperation Add (+))
                |> evaluateTokens

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
    | Cos
    | Tan
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

        Cos ->
            "cos"

        Tan ->
            "tan"

        Exp ->
            "^"


evaluate : String -> Result String Float
evaluate expression =
    parse expression |> Result.map evaluateTokens


parse : String -> Result String (List Token)
parse expression =
    case expression |> String.toList |> charListToTokenList 0 of
        Ok tokens ->
            Ok tokens

        Err err ->
            Err (expression ++ "\n" ++ err)


charListToTokenList : Int -> List Char -> Result String (List Token)
charListToTokenList index expression =
    let
        ( parseResult, rest, newIndex ) =
            case expression of
                ' ' :: r ->
                    ( Ok [], r, index + 1 )

                '(' :: r ->
                    ( Ok [ Open ], r, index + 1 )

                ')' :: r ->
                    ( Ok [ Close ], r, index + 1 )

                '+' :: r ->
                    ( Ok [ Operation Add ], r, index + 1 )

                '-' :: r ->
                    ( Ok [ Operation Sub ], r, index + 1 )

                '*' :: r ->
                    ( Ok [ Operation Mul ], r, index + 1 )

                '/' :: r ->
                    ( Ok [ Operation Div ], r, index + 1 )

                '^' :: r ->
                    ( Ok [ Operation Exp ], r, index + 1 )

                's' :: 'i' :: 'n' :: r ->
                    ( Ok [ Operation Sin ], r, index + 3 )

                'c' :: 'o' :: 's' :: r ->
                    ( Ok [ Operation Cos ], r, index + 3 )

                't' :: 'a' :: 'n' :: r ->
                    ( Ok [ Operation Tan ], r, index + 3 )

                'p' :: 'i' :: r ->
                    ( Ok [ Number 3.141592653 ], r, index + 2 )

                'e' :: r ->
                    ( Ok [ Number 2.718281828 ], r, index + 1 )

                't' :: 'a' :: 'u' :: r ->
                    ( Ok [ Number (2 * 3.141592653) ], r, index + 3 )

                c :: _ ->
                    -- If the character is a digit, parse the string until the end of the number
                    if Char.isDigit c then
                        case parseNumber expression of
                            Just ( num, r ) ->
                                ( Ok [ Number num ], r, index + List.length expression - List.length r )

                            Nothing ->
                                ( Err "Unexpected end of expression", expression, index )

                    else
                        ( Err (unexpectedCharacterErrorMessage index expression), expression, index )

                [] ->
                    ( Ok [], expression, index )
    in
    if rest == [] then
        parseResult

    else
        case parseResult of
            Ok tokens ->
                case charListToTokenList newIndex rest of
                    Ok r ->
                        Ok (List.append tokens r)

                    Err err ->
                        Err err

            Err err ->
                Err err


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

        unaryOperation : Operation -> (Float -> Float) -> List Token -> Maybe (List Token)
        unaryOperation op doOperation t =
            case t of
                (Operation op2) :: (Number right) :: xs ->
                    if op == op2 then
                        Just (Number (doOperation right) :: xs)

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
                |> resolve (unaryOperation Sin sin)
                |> resolve (unaryOperation Cos cos)
                |> resolve (unaryOperation Tan tan)
                |> resolve (binaryOperation Exp (^))
                |> resolve (binaryOperation Mul (*))
                |> resolve (binaryOperation Div (/))
                -- Note: Subtraction needs to be done before addition because we're evaluating left to right
                |> resolve (binaryOperation Sub (-))
                |> resolve (binaryOperation Add (+))
                |> evaluateTokens


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


unexpectedCharacterErrorMessage : Int -> List Char -> String
unexpectedCharacterErrorMessage offset expression =
    let
        indent =
            String.repeat offset " "

        indentedErrorMessage errorLength line1 line2 =
            indent
                ++ String.repeat errorLength "^"
                ++ " "
                ++ line1
                ++ "\n"
                ++ String.repeat (errorLength + 1) " "
                ++ indent
                ++ line2
    in
    case expression of
        '!' :: _ ->
            indentedErrorMessage
                1
                "Unexpected character: '!'"
                "Factorials are not supported"

        's' :: 'i' :: c :: _ ->
            let
                operation =
                    "si" ++ String.fromChar c
            in
            indentedErrorMessage
                3
                ("Undefined operation: '"
                    ++ operation
                    ++ "'"
                )
                "Did you mean 'sin'?"

        c :: _ ->
            indentedErrorMessage
                1
                ("Unexpected character: '" ++ String.fromChar c ++ "'")
                ""

        [] ->
            "Unexpected end of expression"

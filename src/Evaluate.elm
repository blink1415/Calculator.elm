module Evaluate exposing (..)


type alias Expression =
    String


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


evaluate : Expression -> Result String Float
evaluate expression =
    case parse expression of
        Ok tokens ->
            evaluateTokens tokens

        Err e ->
            Err (errorToString e)


parse : Expression -> Result ParseError (List Token)
parse expression =
    case stringToTokenList expression of
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


stringToTokenList : Expression -> Result ParseError (List Token)
stringToTokenList expression =
    expression |> String.toList |> charListToTokenList 0


charListToTokenList : Int -> List Char -> Result ParseError (List Token)
charListToTokenList index expression =
    let
        ( parseResult, rest ) =
            case expression of
                ' ' :: r ->
                    ( Ok [], r )

                '(' :: r ->
                    ( Ok [ Open ], r )

                ')' :: r ->
                    ( Ok [ Close ], r )

                -- Operations
                '+' :: r ->
                    ( Ok [ Operation Add ], r )

                '-' :: r ->
                    ( Ok [ Operation Sub ], r )

                '*' :: r ->
                    ( Ok [ Operation Mul ], r )

                '/' :: r ->
                    ( Ok [ Operation Div ], r )

                's' :: 'i' :: 'n' :: r ->
                    ( Ok [ Operation Sin ], r )

                -- Constants
                'p' :: 'i' :: r ->
                    ( Ok [ Number 3.141592653 ], r )

                'e' :: r ->
                    ( Ok [ Number 2.718281828 ], r )

                't' :: 'a' :: 'u' :: r ->
                    ( Ok [ Number (2 * 3.141592653) ], r )

                c :: _ ->
                    -- If the character is a digit, parse the string until the end of the number
                    if Char.isDigit c then
                        case parseNumber (String.fromList expression) of
                            Just ( num, r ) ->
                                ( Ok [ Number num ], String.toList r )

                            Nothing ->
                                ( Err UnexpectedEndOfExpression, expression )

                    else
                        ( Err (UnexpectedCharacter c index), expression )

                [] ->
                    ( Ok [], expression )
    in
    if rest == [] then
        parseResult

    else
        case parseResult of
            Ok tokens ->
                case charListToTokenList (index + 1) rest of
                    Ok r ->
                        Ok (List.append tokens r)

                    Err err ->
                        Err err

            Err err ->
                Err err


parseNumber : String -> Maybe ( Float, String )
parseNumber numberString =
    let
        parseNumberHelper : String -> String -> Maybe ( Float, String )
        parseNumberHelper str acc =
            if String.isEmpty str then
                String.toFloat acc |> Maybe.map (\num -> ( num, "" ))

            else if Char.isDigit (str |> String.toList |> List.head |> Maybe.withDefault ' ') then
                parseNumberHelper (String.dropLeft 1 str) (acc ++ String.left 1 str)

            else
                String.toFloat acc |> Maybe.map (\num -> ( num, str ))
    in
    parseNumberHelper numberString ""


evaluateTokens : List Token -> Result String Float
evaluateTokens tokens =
    let
        multiply : List Token -> Result String (Maybe (List Token))
        multiply t =
            case t of
                (Number left) :: (Operation Mul) :: (Number right) :: xs ->
                    Ok (Just (Number (left * right) :: xs))

                _ ->
                    Ok Nothing

        divide : List Token -> Result String (Maybe (List Token))
        divide t =
            case t of
                (Number left) :: (Operation Div) :: (Number right) :: xs ->
                    if right == 0 then
                        Err "Unexpected operation: Division by zero"

                    else
                        Ok (Just (Number (left / right) :: xs))

                _ ->
                    Ok Nothing

        add : List Token -> Result String (Maybe (List Token))
        add t =
            case t of
                (Number left) :: (Operation Add) :: (Number right) :: xs ->
                    Ok (Just (Number (left + right) :: xs))

                _ ->
                    Ok Nothing

        subtract : List Token -> Result String (Maybe (List Token))
        subtract t =
            case t of
                (Number left) :: (Operation Sub) :: (Number right) :: xs ->
                    Ok (Just (Number (left - right) :: xs))

                _ ->
                    Ok Nothing

        parentheses : List Token -> Result String (Maybe (List Token))
        parentheses t =
            case t of
                Open :: x :: Close :: xs ->
                    Ok (Just (x :: xs))

                Open :: Close :: xs ->
                    Ok (Just xs)

                _ ->
                    Ok Nothing

        sinRadians : List Token -> Result String (Maybe (List Token))
        sinRadians t =
            case t of
                (Operation Sin) :: (Number right) :: xs ->
                    Ok (Just (Number (sin right) :: xs))

                _ ->
                    Ok Nothing

        resolve : (List Token -> Result String (Maybe (List Token))) -> List Token -> Result String (List Token)
        resolve doOperation t =
            case doOperation t of
                Ok v ->
                    case v of
                        Just newTokens ->
                            resolve doOperation newTokens

                        Nothing ->
                            case t of
                                x :: xs ->
                                    Result.map (\list -> x :: list) <| resolve doOperation xs

                                [] ->
                                    Ok []

                Err e ->
                    Err e
    in
    case tokens of
        [ Number result ] ->
            Ok result

        [] ->
            Err "Expression evaluates to empty expression"

        _ ->
            Ok tokens
                |> Result.andThen (resolve parentheses)
                |> Result.andThen (resolve sinRadians)
                |> Result.andThen (resolve multiply)
                |> Result.andThen (resolve divide)
                |> Result.andThen (resolve add)
                |> Result.andThen (resolve subtract)
                |> Result.andThen evaluateTokens

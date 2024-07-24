module Evaluate exposing (..)

import Expression exposing (Expression, Operation(..), Token(..), operationToString)
import Parse exposing (errorToString, parse)



-- Examples of varying complexity:
-- (1 + 2) * 3
-- (1 + 2) * (3 + 4)
-- ((13 - 5 * 3) + 2) / 3


evaluate : Expression -> Result String Float
evaluate expression =
    case parse expression of
        Ok tokens ->
            evaluateTokens tokens

        Err e ->
            Err (errorToString e)


evaluateTokens : List Token -> Result String Float
evaluateTokens tokens =
    case tokens of
        [ Number result ] ->
            Ok result

        -- [_,  Open ] ->
        --     Err "Unexpected open parenthesis"
        --
        -- [Open, Operation _ ] ->
        --     Err "Unexpected open parenthesis"
        --
        -- [Open, Number _ ] ->
        --     Err "Unexpected open parenthesis"
        --
        -- [Operation _,  Close ] ->
        --     Err "Unexpected close parenthesis"
        --
        -- [Number _,  Close ] ->
        --     Err "Unexpected close parenthesis"
        --
        -- [Close, _] ->
        --     Err "Unexpected close parenthesis"
        --
        -- (Operation op :: _) ->
        --     Err ("Unexpected operation: " ++ operationToString op)

        [] ->
            Err "Expression evaluates to empty expression"

        _ ->
            tokens
                |> forEach3 resolveParenthesesWithNumber
                |> forEach2 resolveEmptyParentheses
                |> forEach2 (resolveUnaryOperation Sin)
                |> forEach3 (resolveBinaryOperation Mul)
                |> forEach3 (resolveBinaryOperation Div)
                |> forEach3 (resolveBinaryOperation Sub)
                |> forEach3 (resolveBinaryOperation Add)
                |> evaluateTokens


forEach3 : (Token -> Token -> Token -> Maybe Token) -> List Token -> List Token
forEach3 f tokens =
    case tokens of
        (t1 :: t2 :: t3 :: rest) ->
            case f t1 t2 t3 of
                Just newToken ->
                    forEach3 f (newToken :: rest)

                Nothing ->
                    t1 :: forEach3 f (t2 :: t3 :: rest)

        _ ->
            tokens

forEach2 : (Token -> Token -> Maybe Token) -> List Token -> List Token
forEach2 f tokens =
    case tokens of
        (t1 :: t2 :: rest) ->
            case f t1 t2 of
                Just newToken ->
                    forEach2 f (newToken :: rest)

                Nothing ->
                    t1 :: forEach2 f (t2 :: rest)

        _ ->
            tokens

resolveParenthesesWithNumber : Token -> Token -> Token -> Maybe Token
resolveParenthesesWithNumber t1 t2 t3 =
    case ( t1, t2, t3 ) of
        ( Open, Number left, Close ) ->
            Just (Number left)

        _ ->
            Nothing

resolveEmptyParentheses : Token -> Token -> Maybe Token
resolveEmptyParentheses t1 t2 =
    case ( t1, t2 ) of
        ( Open, Close ) ->
            Just (Number 0)

        _ ->
            Nothing

{-|
resolveBinaryOperation takes in a binary operation and 3 tokens. It will evaluate the tokens according to the operation.
Normal arithmetic operations are expeced in an infix notation, see examples below.
If the tokens do not match the format expected by the operation, Nothing is returned.

Examples:
Mul (Number 6) (Operation Mul) (Number 2) --> Just (Number 12)
Add (Number 6) (Operation Add) (Number 2) --> Just (Number 8)
Sub (Number 6) (Operation Sub) (Number 2) --> Just (Number 4)
Div (Number 6) (Operation Div) (Number 2) --> Just (Number 3)
Add (Number 6) (Operation Sub) (Number 2) --> Nothing
Add (Operation Add) (Number 6) (Number 2) --> Nothing
-}
resolveBinaryOperation : Operation -> Token -> Token -> Token -> Maybe Token
resolveBinaryOperation op t1 t2 t3 =
    case (op, ( t1, t2, t3 )) of
        (Div, ( Number left, Operation Div, Number right )) ->
            if right == 0 then
                Nothing
            else
                Just (Number (left / right))

        (Mul, ( Number left, Operation Mul, Number right )) ->
            Just (Number (left * right))

        (Add, ( Number left, Operation Add, Number right )) ->
            Just (Number (left + right))

        (Sub, ( Number left, Operation Sub, Number right )) ->
            Just (Number (left - right))

        _ ->
            Nothing

resolveUnaryOperation : Operation -> Token -> Token -> Maybe Token
resolveUnaryOperation op t1 t2 =
    case (op, ( t1, t2 )) of
        (Sin, ( Operation Sin, Number right )) ->
            Just (Number (sin right))
        
        _ ->
            Nothing


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

        [_,  Open ] ->
            Err "Unexpected open parenthesis"

        [Open, _ ] ->
            Err "Unexpected open parenthesis"

        [_,  Close ] ->
            Err "Unexpected close parenthesis"

        [Close, _] ->
            Err "Unexpected close parenthesis"

        (Operation op :: _) ->
            Err ("Unexpected operation: " ++ operationToString op)

        [] ->
            Err "Expression evaluates to empty expression"

        _ ->
            tokens
                |> forEach resolveParentheses
                |> forEach (resolve Mul)
                |> forEach (resolve Div)
                |> forEach (resolve Sub)
                |> forEach (resolve Add)
                |> evaluateTokens


forEach : (Token -> Token -> Token -> Maybe Token) -> List Token -> List Token
forEach f tokens =
    case tokens of
        (t1 :: t2 :: t3 :: rest) ->
            case f t1 t2 t3 of
                Just newToken ->
                    forEach f (newToken :: rest)

                Nothing ->
                    t1 :: forEach f (t2 :: t3 :: rest)

        _ ->
            tokens

resolveParentheses : Token -> Token -> Token -> Maybe Token
resolveParentheses t1 t2 t3 =
    case ( t1, t2, t3 ) of
        ( Open, Number left, Close ) ->
            Just (Number left)

        _ ->
            Nothing

resolve : Operation -> Token -> Token -> Token -> Maybe Token
resolve op t1 t2 t3 =
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

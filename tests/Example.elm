module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, pair)
import Test exposing (..)

import Evaluate exposing (evaluate)


suite : Test
suite =
    describe "can perform basic operations"
    [
        -- fuzz (pair int int) "adds two numbers" <|
        --     \(a, b) ->
        --         Expect.equal (evaluate (String.fromInt a ++ "+" ++ String.fromInt b)) (Ok (toFloat (a + b)))
        
        test "able to add 1 to 1" <|
            \() ->
                Expect.equal (evaluate "1 + 1") (Ok 2)

        , test "ignores spaces in expression" <|
            \() ->
                Expect.equal (evaluate "1 + 1") (Ok 2)
            
        , test "errors when dividing by zero" <|
            \() ->
                Expect.equal (evaluate "1 / 0") (Err "Unexpected operation: /")


        -- , fuzz (pair int int) "subtracts two numbers" <|
        --     \(a, b) ->
        --         Expect.equal (evaluate (String.fromInt a ++ "+" ++ String.fromInt b)) (Ok (toFloat (a - b)))
        -- 
        -- , fuzz (pair int int) "multiplies two numbers" <|
        --     \(a, b) ->
        --         Expect.equal (evaluate (String.fromInt a ++ "*" ++ String.fromInt b)) (Ok (toFloat (a * b)))
        -- 
        -- , fuzz (pair int int) "divides two numbers" <|
        --     \(a, b) ->
        --         Expect.equal (evaluate (String.fromInt a ++ "/" ++ String.fromInt b)) (Ok (toFloat(a) / toFloat(b)))
        ]

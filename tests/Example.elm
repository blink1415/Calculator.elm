module Example exposing (..)

import Evaluate exposing (evaluate)
import Expect
import Fuzz exposing (intRange, pair)
import Test exposing (..)


suite : Test
suite =
    describe "can perform basic operations"
        [ test "able to add 1 to 1" <|
            \() ->
                Expect.equal (evaluate "1+1") (Ok 2)
        , test "ignores spaces in expression" <|
            \() ->
                Expect.equal (evaluate "1 + 1") (Ok 2)
        , test "errors when dividing by zero" <|
            \() ->
                Expect.equal (evaluate "1 / 0") (Err "Unexpected operation: Division by zero")
        , test "respects order of operations" <|
            \() ->
                Expect.equal (evaluate "1+1*2") (Ok 3)
        , test "can use parentheses" <|
            \() ->
                Expect.equal (evaluate "(1+1)*2") (Ok 4)
        , fuzz (pair (intRange 0 1000000) (intRange 0 1000000)) "multiplies two numbers" <|
            \( a, b ) ->
                Expect.equal (evaluate (String.fromInt a ++ "*" ++ String.fromInt b)) (Ok (toFloat (a * b)))
        , fuzz (pair (intRange 0 1000000) (intRange 1 1000000)) "divides two numbers" <|
            \( a, b ) ->
                Expect.equal (evaluate (String.fromInt a ++ "/" ++ String.fromInt b)) (Ok (toFloat a / toFloat b))
        ]

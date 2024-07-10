module Evaluate exposing (..)

import Expression exposing (Expression, Operation(..), Token(..))
import Parse exposing (parse)



-- Examples of varying complexity:
-- (1 + 2) * 3
-- (1 + 2) * (3 + 4)
-- ((13 - 5 * 3) + 2) / 3


evaluate : Expression -> Result String Float
evaluate expression =
    case parse expression of
        Ok _ ->
            Ok 0

        Err e ->
            Err e

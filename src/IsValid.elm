module IsValid exposing (check)

import Expression exposing (Token)


check : List Token -> Result String (List Token)
check tokens =
    case tokens of
        [] ->
            Err "Invalid expression"

        _ ->
            Ok tokens

module Expression exposing (Expression, Operation(..), Token(..))


type alias Expression =
    String


type Operation
    = Add
    | Sub
    | Mul
    | Div


type Token
    = Number Float
    | Operation Operation
    | Open
    | Close

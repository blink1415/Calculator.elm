module Expression exposing (Expression, Operation(..), Token(..), operationToString)


type alias Expression =
    String


type Operation
    = Add
    | Sub
    | Mul
    | Div

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

type Token
    = Number Float
    | Operation Operation
    | Open
    | Close

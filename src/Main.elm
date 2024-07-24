module Main exposing (Input(..), State, btn, main, update, view)

import Browser
import Html exposing (Html, br, div, p, text)
import Html.Events exposing (onClick)
import Evaluate


type alias State =
    {
        expression : String,
        result : String
    }


type Input
    = Char Char
    | Clear
    | Evaluate


main : Program () State Input
main =
    Browser.sandbox { init = { expression = "", result = "" }, view = view, update = update }


update : Input -> State -> State
update msg state =
    case msg of
        Clear ->
            { expression = "", result = state.result }

        Char c ->
            { expression = state.expression ++ String.fromChar c, result = state.result }

        Evaluate ->
            case Evaluate.evaluate state.expression of
                Ok result ->
                    { expression = state.expression, result = String.fromFloat result }

                Err e ->
                    { expression = state.expression, result = e }


btn : Input -> Html Input
btn msg =
    case msg of
        Char c ->
            Html.button [ onClick msg ] [ text (String.fromChar c) ]

        Clear ->
            Html.button [ onClick msg ] [ text "CE" ]

        Evaluate ->
            Html.button [ onClick msg ] [ text "=" ]


view : State -> Html Input
view model =
    div []
        [ div [] [ text model.expression ]
        , div [] [ text ("Result" ++ model.result) ]
        , div []
            [ p [] [ text "Operations:" ]
            , btn (Char '1')
            , btn (Char '2')
            , btn (Char '3')
            , btn (Char '-')
            , btn (Char '(')
            , btn (Char ')')
            , br [] []
            , btn (Char '4')
            , btn (Char '5')
            , btn (Char '6')
            , btn (Char '+')
            , br [] []
            , btn (Char '7')
            , btn (Char '8')
            , btn (Char '9')
            , btn (Char '*')
            , br [] []
            , btn Clear
            , btn (Char '0')
            , btn (Char '/')
            , btn Evaluate
            ]
        ]

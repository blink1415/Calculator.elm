module Main exposing (Input(..), State, btn, main, update, view)

import Browser
import Html exposing (Html, br, div, p, text)
import Html.Events exposing (onClick)


type alias State =
    String


type Input
    = Char Char
    | Clear


main : Program () State Input
main =
    Browser.sandbox { init = "", view = view, update = update }


update : Input -> State -> State
update msg state =
    case msg of
        Clear ->
            ""

        Char c ->
            state ++ String.fromChar c


btn : Input -> Html Input
btn msg =
    case msg of
        Char c ->
            Html.button [ onClick msg ] [ text (String.fromChar c) ]

        Clear ->
            Html.button [ onClick msg ] [ text "CE" ]


view : State -> Html Input
view model =
    div []
        [ div [] [ text model ]
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
            ]
        ]

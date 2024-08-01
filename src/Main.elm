module Main exposing (Input(..), State, btn, main, update, view)

import Browser
import Css exposing (backgroundColor, color, fontFamily, hover, margin, monospace, px, rgb, textDecoration, underline, width)
import Evaluate
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, type_, value)
import Html.Styled.Events exposing (onClick, onInput)


type alias State =
    { expression : String
    , result : String
    }


type Input
    = Char Char
    | Clear
    | Evaluate
    | Input String


main : Program () State Input
main =
    Browser.sandbox { init = { expression = "", result = "" }, view = view >> toUnstyled, update = update }


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

        Input "" ->
            { expression = "", result = "" }

        Input s ->
            case Evaluate.evaluate s of
                Ok result ->
                    { expression = s, result = String.fromFloat result }

                Err e ->
                    { expression = s, result = e }


btn : Input -> Html Input
btn msg =
    let
        stylizedButton =
            styled button
                [ margin (px 3)
                , color (rgb 0 0 0)
                , width (px 35)
                , hover
                    [ backgroundColor (rgb 250 250 250)
                    , textDecoration underline
                    ]
                ]
    in
    case msg of
        Char c ->
            stylizedButton [ onClick msg ] [ text (String.fromChar c) ]

        Clear ->
            stylizedButton [ onClick msg ] [ text "CE" ]

        Evaluate ->
            stylizedButton [ onClick msg ] [ text "=" ]

        _ ->
            div [] []


view : State -> Html Input
view state =
    styled div
        [ fontFamily monospace
        ]
        []
        [ input [ type_ "text", value state.expression, onInput (\s -> Input s) ] []
        , br [] []
        , input [ type_ "text", value ("= " ++ state.result), disabled True ] []
        , div []
            [ btn (Char '1')
            , btn (Char '2')
            , btn (Char '3')
            , btn Clear
            , br [] []
            , btn (Char '4')
            , btn (Char '5')
            , btn (Char '6')
            , btn (Char '(')
            , btn (Char ')')
            , br [] []
            , btn (Char '7')
            , btn (Char '8')
            , btn (Char '9')
            , btn (Char '+')
            , btn (Char '-')
            , br [] []
            , btn (Char '.')
            , btn (Char '0')
            , btn Evaluate
            , btn (Char '/')
            , btn (Char '*')
            ]
        ]

module Main exposing (Input(..), State, btn, main, update, view)

import Browser
import Css exposing (auto, backgroundColor, border3, borderRadius, color, fitContent, fontFamily, fontSize, height, hex, hover, margin, marginLeft, marginRight, maxWidth, monospace, overflowWrap, padding, pre, px, rgb, solid, textDecoration, underline, whiteSpace, width)
import Evaluate
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (disabled, type_, value)
import Html.Styled.Events exposing (onClick, onInput)


type alias State =
    { expression : String
    , result : String
    }


type Input
    = Value String
    | Clear
    | Evaluate


main : Program () State Input
main =
    Browser.sandbox { init = { expression = "", result = "" }, view = view >> toUnstyled, update = update }


update : Input -> State -> State
update msg state =
    let
        newExpression =
            case msg of
                Clear ->
                    ""

                Evaluate ->
                    state.expression

                Value s ->
                    s
    in
    { expression = newExpression
    , result =
        case Evaluate.evaluate newExpression of
            Ok result ->
                String.fromFloat result

            Err e ->
                e
    }


btn : Input -> Html Input
btn msg =
    let
        stylizedButton =
            styled button
                [ margin (px 1)
                , color (rgb 0 0 0)
                , width (px 60)
                , fontSize (px 30)
                , backgroundColor (hex "fff")
                , border3 (px 1) solid (hex "eee")
                , borderRadius (px 5)
                , hover
                    [ textDecoration underline
                    , border3 (px 1) solid (hex "000")
                    ]
                ]
    in
    case msg of
        Clear ->
            stylizedButton [ onClick msg ] [ text "CE" ]

        Evaluate ->
            stylizedButton [ onClick msg ] [ text "=" ]

        Value s ->
            stylizedButton [ onClick msg ] [ text s ]


inputStyle : List Css.Style
inputStyle =
    [ fontSize (px 20)
    , width (px 700)
    ]


view : State -> Html Input
view state =
    styled div
        [ fontFamily monospace
        , maxWidth fitContent
        , marginLeft auto
        , marginRight auto
        , backgroundColor (hex "eee")
        , padding (px 10)
        , borderRadius (px 10)
        ]
        []
        [ styled input inputStyle [ type_ "text", value state.expression, onInput (\s -> Value s) ] []
        , br [] []
        , styled textarea
            (inputStyle
                ++ [ height (px 75)
                   , whiteSpace Css.pre
                   , overflowWrap Css.normal
                   ]
            )
            [ value state.result, disabled True ]
            []
        , styled div
            [ maxWidth fitContent
            , marginLeft auto
            , marginRight auto
            ]
            []
            [ btn (Value "1")
            , btn (Value "2")
            , btn (Value "3")
            , btn Clear
            , btn (Value "^")
            , btn (Value "sin")
            , br [] []
            , btn (Value "4")
            , btn (Value "5")
            , btn (Value "6")
            , btn (Value "(")
            , btn (Value ")")
            , br [] []
            , btn (Value "7")
            , btn (Value "8")
            , btn (Value "9")
            , btn (Value "+")
            , btn (Value "-")
            , br [] []
            , btn (Value ".")
            , btn (Value "0")
            , btn Evaluate
            , btn (Value "/")
            , btn (Value "*")
            ]
        ]

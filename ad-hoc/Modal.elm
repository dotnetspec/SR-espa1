module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as B
import Element.Events as E
import Element.Font as F
import Element.Input as I
import Html exposing (Html)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { modalState : ModalState }


type ModalState
    = Open
    | Closed


init : Model
init =
    Model Closed


type Msg
    = OpenModal
    | CloseModal


update : Msg -> Model -> Model
update msg model =
    case msg of
        OpenModal ->
            { model | modalState = Open }

        CloseModal ->
            { model | modalState = Closed }


view : Model -> Html Msg
view model =
    let
        modalString =
            case model.modalState of
                Open ->
                    "Open"

                Closed ->
                    "Closed"
    in
    layout
        [ inFront <| viewModal model ]
    <|
        el
            [ width fill
            , height fill
            , padding 20
            ]
        <|
            column
                [ width fill
                , height fill
                , spacing 50
                ]
                [ el [ F.color (rgb 0.2 0.2 0.2) ] (text modalString)
                , el [ centerX ] <| button (rgb 0.25 0.75 0.75) OpenModal "Open"
                ]


viewModal : Model -> Element Msg
viewModal { modalState } =
    let
        box =
            el
                [ width (px 300)
                , height (px 300)
                , centerX
                , centerY
                , B.color (rgb 1 1 1)
                ]
            <|
                el [ centerX, centerY ] <|
                    button (rgb 0.95 0.6 0.25) CloseModal "Close"
    in
    case modalState of
        Open ->
            el
                [ width fill
                , height fill
                , behindContent <|
                    el
                        [ width fill
                        , height fill
                        , B.color (rgba 0.5 0.5 0.5 0.7)
                        , E.onClick CloseModal
                        ]
                        none
                ]
                box

        Closed ->
            el [] none


button : Color -> Msg -> String -> Element Msg
button color msg label =
    I.button
        [ padding 20
        , B.color color
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , F.center
                , F.color (rgba 0.2 0.2 0.2 0.9)
                ]
                (text label)
        }

module Ui exposing (colors, hero, markdown)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Attr
import Markdown


type alias Colors =
    { coral : Element.Color
    , white : Element.Color
    , lightblue : Element.Color
    , blue : Element.Color
    , green : Element.Color
    , purple : Element.Color
    , black : Element.Color
    , red : Element.Color
    , darkBlue : Element.Color
    , grey : Element.Color
    }



-- some are plain rgb!


colors : Colors
colors =
    { coral = rgb255 204 75 75
    , white = rgb255 255 255 255
    , lightblue = rgb255 0 128 255
    , blue = rgb255 2 7 239
    , green = rgb255 0 153 0
    , purple = rgb255 102 0 102
    , black = rgb255 0 0 0
    , red = rgb 0.8 0 0
    , darkBlue = rgb 0 0 0.9
    , grey = rgb 0.9 0.9 0.9
    }


hero : { title : String, description : String, buttons : List ( String, String ) } -> Element msg
hero options =
    column
        [ centerX
        , paddingXY 16 128
        , spacing 24
        ]
        (column [ spacing 12 ]
            [ el [ centerX, Font.size 64, Font.semiBold ]
                (text options.title)
            , el [ centerX, Font.size 24, alpha 0.5 ]
                (text options.description)
            ]
            :: (if List.isEmpty options.buttons then
                    []

                else
                    [ row [ centerX, spacing 8 ] (List.map viewButton options.buttons) ]
               )
        )


viewButton : ( String, String ) -> Element msg
viewButton ( label, url ) =
    link
        [ Background.color colors.white
        , Border.color colors.coral
        , Font.color colors.coral
        , paddingXY 24 8
        , Border.rounded 4
        , Border.width 2
        , Font.size 14
        , mouseOver
            [ Background.color colors.coral
            , Font.color colors.white
            ]
        , transition 200 [ "color", "background-color" ]
        ]
        { url = url, label = text label }


transition : Int -> List String -> Attribute msg
transition duration properties =
    Element.htmlAttribute <|
        Attr.style
            "transition"
            (properties
                |> List.map (\prop -> prop ++ " " ++ String.fromInt duration ++ "color 200ms ease-in-out")
                |> String.join ", "
            )


markdown : String -> Element msg
markdown =
    let
        defaults =
            Markdown.defaultOptions
    in
    Markdown.toHtmlWith
        { defaults
            | sanitize = False
            , githubFlavored = Just { tables = True, breaks = False }
        }
        [ Attr.class "markdown" ]
        >> Element.html
        >> List.singleton
        >> paragraph []

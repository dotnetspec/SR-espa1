module Main exposing (main)

import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Framework
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Group as Group
import Framework.Heading as Heading
import Framework.Input as Input
import Html exposing (Html)


heading : Element msg
heading =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Username"
        , Element.column Card.fill
            [ Element.el Heading.h1 <| Element.text "Username"
            ]
        ]


group : Element msg
group =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Group"
        , Element.column (Card.fill ++ Grid.simple)
            [ Element.wrappedRow Grid.simple
                [ Element.el (Card.fill ++ Group.left) <| Element.text "Group.left"
                , Element.el (Card.fill ++ Group.center) <| Element.text "Group.center"
                , Element.el (Card.fill ++ Group.right) <| Element.text "Group.right"
                , Element.el (Card.fill ++ Group.top) <| Element.text "Group.top"
                , Element.el (Card.fill ++ Group.bottom) <| Element.text "Group.bottom"
                ]
            ]
        ]


color : Element msg
color =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Color"
        , Element.wrappedRow (Card.fill ++ Grid.simple)
            [ Element.column Grid.simple
                [ Element.el Heading.h3 <| Element.text "Elm-Ui Attributes"
                , Element.el (Card.fill ++ Color.primary) <| Element.text "Color.primary"
                , Element.el (Card.fill ++ Color.info) <| Element.text "Color.info"
                , Element.el (Card.fill ++ Color.success) <| Element.text "Color.success"
                , Element.el (Card.fill ++ Color.warning) <| Element.text "Color.warning"
                , Element.el (Card.fill ++ Color.danger) <| Element.text "Color.danger"
                , Element.el (Card.fill ++ Color.light) <| Element.text "Color.light"
                , Element.el (Card.fill ++ Color.simple) <| Element.text "Color.simple"
                , Element.el (Card.fill ++ Color.dark) <| Element.text "Color.dark"
                , Element.el (Card.fill ++ Color.disabled) <| Element.text "Color.disabled"
                ]
            ]
        ]


grid : Element msg
grid =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Grid"
        , Element.column Grid.simple <|
            [ Element.wrappedRow Grid.simple
                [ Element.column (Card.simple ++ Grid.simple) <|
                    [ Element.el Heading.h3 <| Element.text "Grid.simple"
                    , Element.row Grid.simple <|
                        [ Element.el Card.simple <| Element.text "item"
                        , Element.el Card.simple <| Element.text "item"
                        , Element.el Card.simple <| Element.text "item"
                        ]
                    ]
                ]
            ]
        ]


button : Element msg
button =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Button"
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button Button.simple <|
                    { onPress = Nothing
                    , label = Element.text "Button.simple"
                    }
                , Input.button Button.fill <|
                    { onPress = Nothing
                    , label = Element.text "Button.fill"
                    }
                ]
            ]
        , Element.column Grid.simple <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text "Button attributes can be combined with other attributes."
            ]
        ]


input : Element ()
input =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Input"
        , Element.wrappedRow (Card.fill ++ Grid.simple)
            [ Element.column Grid.simple
                [ Input.text Input.simple
                    { onChange = always ()
                    , text = "Input.simple"
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Input.label"
                    }
                , Input.multiline Input.simple
                    { onChange = always ()
                    , text = "Input.simple"
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Input.label"
                    , spellcheck = False
                    }
                ]
            , Element.column Grid.simple
                [ Input.currentPassword Input.simple
                    { onChange = always ()
                    , text = "Input.simple"
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Input.label"
                    , show = False
                    }
                ]
            ]
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Input attributes can be combined with other attributes."
        , Element.paragraph (Card.fill ++ Color.warning) <|
            [ Element.el [ Font.bold ] <| Element.text "Warning: "
            , Element.paragraph [] <|
                List.singleton <|
                    Element.text "color changing attributes need to come before the Input attribute."
            ]
        ]


view : Element ()
view =
    Element.column
        Framework.container
        [ Element.el Heading.h1 <| Element.text "SportRank"
        , heading
        , group
        , color
        , grid
        , button
        , input
        ]


main : Html ()
main =
    Framework.responsiveLayout [] <|
        view

module Main exposing (main)

--import Browser

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
import SR.Types



-- document :
--     { init : flags -> ( model, Cmd msg )
--     , view : model -> Document msg
--     , update : msg -> model -> ( model, Cmd msg )
--     , subscriptions : model -> Sub msg
--     }
--     -> Program flags model msg
-- document =
-- type alias Document msg =
--     { title : String
--     , body : List (Html msg)
--     }


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


rankingbuttons : Element msg
rankingbuttons =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Global Rankings"
        , Element.column (Card.simple ++ Grid.simple) <|
            insertRankingList rankingInfoList
        , Element.column Grid.simple <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text "Button attributes can be combined with other attributes."
            ]
        ]


rankingInfoList =
    [ { id = "5e787508b325b3162e3cd426"
      , active = True
      , rankingname = "mmmmmm"
      , rankingdesc = "mmmmmmm"
      , rankingowneraddr = "0x847700b781667abdd98e1393420754e503dca5b7"
      }
    , { id = "5e7301aad3ffb01648aa73be"
      , active = True
      , rankingname = "pppppppp"
      , rankingdesc = "pppppppp"
      , rankingowneraddr = "0x847700B781667abdD98E1393420754E503dca5b7"
      }
    , { id = "5e7301aad3ffb01648aa73be"
      , active = True
      , rankingname = "oooooooo"
      , rankingdesc = "pppppppp"
      , rankingowneraddr = "0x847700B781667abdD98E1393420754E503dca5b7"
      }
    , { id = "5e7301aad3ffb01648aa73be"
      , active = True
      , rankingname = "tttttttt"
      , rankingdesc = "pppppppp"
      , rankingowneraddr = "0x847700B781667abdD98E1393420754E503dca5b7"
      }
    ]


addRankingInfoToAnyElText rankingobj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.info) <|
            { onPress = Nothing --rankingobj.id
            , label = Element.text rankingobj.rankingname
            }
        ]


insertRankingList : List SR.Types.RankingInfo -> List (Element msg)
insertRankingList rnkgInfoList =
    let
        mapOutRankingList =
            List.map
                addRankingInfoToAnyElText
                rnkgInfoList
    in
    mapOutRankingList


newrankingbuttons : Element msg
newrankingbuttons =
    Element.column Grid.section <|
        [ Element.el Heading.h4 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Nothing
                    , label = Element.text "Button.simple"
                    }
                , Input.button (Button.fill ++ Color.success) <|
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

        --, color
        , grid
        , rankingbuttons
        , input
        , newrankingbuttons
        ]


main : Html ()
main =
    Framework.responsiveLayout [] <|
        view

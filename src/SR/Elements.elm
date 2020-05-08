module SR.Elements exposing (ethereumWalletWarning, footer, justParasimpleUserInfoText, placeholder, selectedRankingHeaderEl, simpleUserInfoText)

--import RemoteData

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Types
import Eth.Utils
import Framework
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Group as Group
import Framework.Heading as Heading
import Framework.Input as Input
import Internal.Types
import SR.Defaults
import SR.Types
import Utils.MyUtils


globalHeading : SR.Types.User -> Element msg
globalHeading user =
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Global Rankings"
        , Element.column Card.fill
            [ Element.el Heading.h4 <| Element.text user.username
            ]
        ]


selectedRankingHeaderEl : SR.Types.RankingInfo -> Element msg
selectedRankingHeaderEl rnkInfo =
    Element.el Heading.h5 <| Element.text <| "Selected Ranking " ++ "-" ++ rnkInfo.rankingname


simpleUserInfoText : Element msg
simpleUserInfoText =
    Element.column Grid.simple <|
        [ justParasimpleUserInfoText
        ]


justParasimpleUserInfoText : Element msg
justParasimpleUserInfoText =
    Element.paragraph [] <|
        List.singleton <|
            Element.text "Use of this application is without any liablity whatsoever"


selectedHeading : SR.Types.User -> SR.Types.RankingInfo -> Element msg
selectedHeading user rnkInfo =
    let
        _ =
            Debug.log "rank id " rnkInfo.id
    in
    Element.column Grid.section <|
        [ Element.el Heading.h5 <|
            Element.text (user.username ++ " you selected ranking")
        , Element.column Card.fill
            [ Element.el Heading.h4 <|
                Element.text rnkInfo.rankingname
            , Element.text rnkInfo.rankingdesc
            ]
        ]


ethereumWalletWarning : Element msg
ethereumWalletWarning =
    Element.paragraph (Card.fill ++ Color.warning) <|
        [ Element.el [ Font.bold ] <| Element.text "Please note: "
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "This confirmation will interact with your Ethereum wallet"
        ]


footer : Element msg
footer =
    Element.paragraph [] <|
        List.singleton <|
            Element.text "SportRank - all rights reserved"


wireframeTheme =
    { bg = Color.grey
    , frame = Color.black
    , text = Color.black
    }


placeholder : List (Element.Attribute msg) -> String -> Element msg
placeholder attr name =
    Element.text name
        |> Element.el
            [ Border.rounded 5
            , Border.dotted
            , Border.color wireframeTheme.frame
            , Border.width 2
            , Element.height Element.fill
            , Element.width Element.fill
            , Element.padding 20
            , Background.color wireframeTheme.bg
            , Font.center
            , Font.color wireframeTheme.text
            ]
        |> Element.el attr

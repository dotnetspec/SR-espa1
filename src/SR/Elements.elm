module SR.Elements exposing (
    
    ethereumWalletWarning, footer, 
    justParasimpleUserInfoText, legalUserInfoText, placeholder, selectedRankingHeaderEl, 
    simpleUserInfoText, warningParagraph, warningText, ethereumNotEnabledPara, permanentlyDeleteWarnPara, missingDataPara)

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
import Data.Rankings
import Data.Users
import Data.Selected


warningText : String -> Element msg
warningText warningTxt =
    Element.el
        [ Font.color (Element.rgb 1 0 0)
        , Font.size 18
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        , Font.center
        ]
        (Element.text  warningTxt)

warningParagraph : Element msg
warningParagraph =
    Element.paragraph (Card.fill ++ Color.warning) <|
        [ Element.el [ Font.bold ] <| Element.text "Please note: "
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Clicking 'Confirm' interacts with your Ethereum wallet."
        ]


missingDataPara : Element msg
missingDataPara =
    Element.paragraph (Card.fill ++ Color.warning) <|
        [ Element.el [ Font.bold ] <| Element.text "Please note: "
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Essential data is missing!"
        ]

ethereumNotEnabledPara : Element msg
ethereumNotEnabledPara =
    Element.paragraph (Card.fill ++ Color.warning) <|
        [ Element.el [ Font.bold ] <| Element.text "Please note: "
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Ethereum is not current enabled for this dApp. Please click 'Enable Ethereum' (at top) to continue."
        ]

permanentlyDeleteWarnPara : Element msg
permanentlyDeleteWarnPara =
    Element.paragraph (Card.fill ++ Color.warning) <|
        [ Element.el [ Font.bold ] <| Element.text "Please note: "
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Clicking 'Confirm' will permanently delete this ranking. Participants will be notified in due course."
        ]


globalHeading : Data.Users.User -> Element msg
globalHeading user =
    case user of
        Data.Users.Spectator _ _ ->
            Element.column Grid.section <|
            [ Element.el Heading.h5 <| Element.text "Global Rankings"
            , Element.column Card.fill
                [ Element.el Heading.h4 <| Element.text "Spectator"
                ]
            ]
        
        (Data.Users.Registered userInfo _) -> 
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text "Global Rankings"
                , Element.column Card.fill
                    [ Element.el Heading.h4 <| Element.text userInfo.username
                    ]
                ]


selectedRankingHeaderEl : Data.Selected.Selected -> Element msg
selectedRankingHeaderEl s =
    Element.el Heading.h5 <| Element.text <| "SportRank" ++ " - " ++ (Data.Selected.gotRanking s).rankingname
    ++  " - " ++ Data.Selected.playerStatusAsStr s


simpleUserInfoText : Element msg
simpleUserInfoText =
    Element.column Grid.simple <|
        [ justParasimpleUserInfoText
        ]


justParasimpleUserInfoText : Element msg
justParasimpleUserInfoText =
    Element.paragraph [] <|
        List.singleton <|
            Element.text "Can your challengers reach you if you don't submit contact details?"


legalUserInfoText : Element msg
legalUserInfoText =
    Element.paragraph (Card.fill ++ Color.warning) <|
        [ Element.el [ Font.bold ] <| Element.text "Please note: "
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Terms and Conditions Apply"
        ]


-- selectedHeading : Data.Users.User -> Data.Rankings.Ranking -> Element msg
-- selectedHeading user rnkInfo =
--     Element.column Grid.section <|
--         [ Element.el Heading.h5 <|
--             Element.text (user.username ++ " you selected ranking")
--         , Element.column Card.fill
--             [ Element.el Heading.h4 <|
--                 Element.text rnkInfo.rankingname
--             , Element.text rnkInfo.rankingdesc
--             ]
--         ]


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
            Element.text """SportRank - all rights reserved. \nUse of this application is without \nany liablity whatsoever."""


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

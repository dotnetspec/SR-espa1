module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Tx
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Units
import Eth.Utils
import Framework
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Group as Group
import Framework.Heading as Heading
import Framework.Input as Input
import Html exposing (Html)
import Http
import Internal.Types as Internal
import Ports
import RemoteData
import SR.Decode
import SR.Types
import Ui


init : () -> ( Model, Cmd Msg )
init _ =
    ( GlobalRankings RemoteData.Loading "" "" SR.Types.RenderAllRankings "", Cmd.none )



--   , Http.get
--       { url = "https://elm-lang.org/assets/public-opinion.txt"
--       , expect = Http.expectString GotText
--       }
--Cmd.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--The model represents the state of the application
-- Model is what is going to change via Update (which is changed from places like View, Subs etc.)
-- it will go from 1 state to another
-- functions like view will just reflect
-- current state of model
--nb: each variant added to model has to be handled e.g. do you need 'failure' if it's anyway handled by RemoteData?
-- AllRankingsJson is just the current list of all rankings
-- AddingNewRankingToGlobalList holds a new ranking id, data for a new ranking and the existing global list to add the new data to


type Model
    = GlobalRankings (RemoteData.WebData (List SR.Types.RankingInfo)) String String SR.Types.UIState String
      --| SelectedRanking DynaModel
    | SelectedRanking (List SR.Types.Player) Internal.RankingId


type alias DynaModel =
    { --browserEnv : BrowserEnv
      --, settings : Maybe SettingsData
      --,
      isopponenthigherrank : Maybe SR.Types.OpponentRelativeRank

    --, players : RemoteData.WebData (List SR.Types.Player)
    , players : List SR.Types.Player
    , fetchedContentNotPlayerList : String
    , error : String
    , rankingid : String
    , modalState : SR.Types.ModalState
    , playerid : Int
    , player : SR.Types.Player
    , opponent : SR.Types.Player
    , selectedRadio : SR.Types.ResultRadioOptions
    , tempMsg : String
    , txSentry : Eth.Sentry.Tx.TxSentry Msg
    , account : Maybe Eth.Types.Address
    , node : Ports.EthNode
    , blockNumber : Maybe Int
    , txHash : Maybe Eth.Types.TxHash
    , tx : Maybe Eth.Types.Tx
    , txReceipt : Maybe Eth.Types.TxReceipt
    , blockDepth : Maybe Eth.Sentry.Tx.TxTracker
    , errors : List String
    , playerRank : Int
    , opponentRank : Int
    , playerStatus : SR.Types.PlayerAvailability
    , opponentStatus : SR.Types.PlayerAvailability
    }



-- Msg is a description of the transition that already happened
-- Messages that delivered the response (orign doc says 'will deliver')


type Msg
    = --GotGlobalRankingsJson (RemoteData.WebData (List SR.Types.RankingInfo))
      --|
      GotRankingId Internal.RankingId
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))


update : Msg -> Model -> ( Model, Cmd Msg )
update msgOfTransitonThatAlreadyHappened previousmodel =
    case msgOfTransitonThatAlreadyHappened of
        GotRankingId rnkidstr ->
            let
                _ =
                    Debug.log "rank id " rnkidstr
            in
            ( SelectedRanking [] rnkidstr, fetchRanking rnkidstr )

        -- case previousmodel of
        --     GlobalRankings _ _ _ _ _ ->
        --         --( SelectedRanking (), fetchRanking rnkidstr )
        --         ( GlobalRankings RemoteData.Loading "" "" SR.Types.RenderAllRankings "", Cmd.none )
        -- SelectedRanking dynamodel ->
        --     ( SelectedRanking { dynamodel | rankingid = "5e787508b325b3162e3cd426" }, Cmd.none )
        PlayersReceived players ->
            --remove the first record (created on ranking creation with different format)
            let
                playerAsJustList =
                    extractPlayersFromWebData players

                _ =
                    Debug.log "player list " playerAsJustList
            in
            ( SelectedRanking playerAsJustList (Internal.RankingId ""), Cmd.none )



-- case previousmodel of
--     SelectedRanking dynaModel ->
--         --( { dynaModel | players = players }, Cmd.none )
--         ( SelectedRanking { dynaModel | players = playerAsJustList }, Cmd.none )
--     GlobalRankings _ _ _ _ _ ->
--         ( GlobalRankings RemoteData.Loading "" "" SR.Types.RenderAllRankings "", Cmd.none )
-- GotGlobalRankingsJson rmtdata ->
--     case rmtdata of
--         RemoteData.Success a ->
--             case previousmodel of
--                 GlobalRankings globalList _ _ _ rnkowner ->
--                     ( GlobalRankings (RemoteData.Success a) "" "" SR.Types.RenderAllRankings rnkowner, Cmd.none )
-- _ ->
--     ( GlobalRankings RemoteData.Loading "" "" SR.Types.RenderAllRankings "", Cmd.none )


heading : Element Msg
heading =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Global Rankings"
        , Element.column Card.fill
            [ Element.el Heading.h1 <| Element.text "Username"
            ]
        ]


selectedHeading : Element Msg
selectedHeading =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Selected Ranking"
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


rankingbuttons : Element Msg
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
    [ { id = "5e7301aad3ffb01648aa73be"
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


addRankingInfoToAnyElText : SR.Types.RankingInfo -> Element Msg
addRankingInfoToAnyElText rankingobj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.info) <|
            { onPress = Just (GotRankingId (Internal.RankingId rankingobj.id))
            , label = Element.text rankingobj.rankingname
            }
        ]


insertRankingList : List SR.Types.RankingInfo -> List (Element Msg)
insertRankingList rnkgInfoList =
    let
        mapOutRankingList =
            List.map
                addRankingInfoToAnyElText
                rnkgInfoList
    in
    mapOutRankingList


playerbuttons : List SR.Types.Player -> Element Msg
playerbuttons playerInfoList =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Selected Ranking"
        , Element.column (Card.simple ++ Grid.simple) <|
            insertPlayerList playerInfoList
        , Element.column Grid.simple <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text "Button attributes can be combined with other attributes."
            ]
        ]


addPlayerInfoToAnyElText : SR.Types.Player -> Element Msg
addPlayerInfoToAnyElText playerObj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.info) <|
            { --onPress = Just (GotRankingId (Internal.RankingId playerObj.id))
              onPress = Nothing
            , label = Element.text playerObj.name
            }
        ]


insertPlayerList : List SR.Types.Player -> List (Element Msg)
insertPlayerList playerInfoList =
    let
        mapOutPlayerList =
            List.map
                addPlayerInfoToAnyElText
                playerInfoList
    in
    mapOutPlayerList


newrankingbuttons : Element Msg
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



-- () means there is a value there, but you don't care what it is


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



--view : Element ()
-- globalResponsiveview : Model -> Html Msg
-- globalResponsiveview model =


globalResponsiveview : Html Msg
globalResponsiveview =
    -- globalResponsiveview : DynaModel -> Html Msg
    -- globalResponsiveview dynamodel =
    -- let
    --     globalRankingList =
    --     case model of
    --         GlobalRankings rnkList ____ ->
    -- in
    --Html <|
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h1 <| Element.text "SportRank"
            , heading

            --, group
            --, color
            --, grid
            , rankingbuttons --dynamodel

            --, input
            , newrankingbuttons
            ]


selectedResponsiveview : List SR.Types.Player -> Html Msg
selectedResponsiveview playerList =
    --Html <|
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h1 <| Element.text "SportRank"
            , selectedHeading

            --, group
            --, color
            --, grid
            , playerbuttons playerList

            --, input
            , newrankingbuttons
            ]


view : Model -> Html Msg
view model =
    case model of
        GlobalRankings _ _ _ _ _ ->
            globalResponsiveview

        SelectedRanking playerList rnkid ->
            selectedResponsiveview playerList


extractPlayersFromWebData : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.Player
extractPlayersFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success players ->
            players

        RemoteData.Failure httpError ->
            []



-- extractPlayersFromWebData : Model -> List SR.Types.Player
-- extractPlayersFromWebData model =
--     case model of
--         SelectedRanking dynamodel ->
--             case dynamodel.players of
--                 RemoteData.NotAsked ->
--                     []
--                 RemoteData.Loading ->
--                     []
--                 RemoteData.Success players ->
--                     players
--                 RemoteData.Failure httpError ->
--                     []
--         GlobalRankings _ _ _ _ _ ->
--             []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


fetchRanking : Internal.RankingId -> Cmd Msg
fetchRanking (Internal.RankingId rankingId) =
    let
        _ =
            Debug.log "rankingid in fetchRanking" rankingId

        headerKey =
            Http.header
                "secret-key"
                "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"
    in
    --PlayersReceived is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    Http.request
        { body = Http.emptyBody
        , expect =
            SR.Decode.ladderOfPlayersDecoder
                |> Http.expectJson (RemoteData.fromResult >> PlayersReceived)
        , headers = [ headerKey ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = "https://api.jsonbin.io/b/" ++ rankingId ++ "/latest"
        }

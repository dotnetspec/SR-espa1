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
import Json.Decode.Pipeline
import Json.Encode
import Ports
import Process
import RemoteData
import SR.Constants
import SR.Decode
import SR.Defaults
import SR.Types
import Task
import Ui
import Utils.MyUtils


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
    = Greeting (List SR.Types.User) SR.Types.UserState SR.Types.WalletState
    | GlobalRankings (List SR.Types.RankingInfo) String String SR.Types.UIState Eth.Types.Address
    | SelectedRanking (List SR.Types.RankingInfo) (List SR.Types.Player) Internal.RankingId



--init : Commands msg -> Flags -> ( Model, Cmd Msg )


init : () -> ( Model, Cmd Msg )
init _ =
    --( GlobalRankings [] "" "" SR.Types.RenderAllRankings "", getRankingList )
    ( Greeting [] SR.Types.NewUser SR.Types.Missing
      --, Cmd.none )
    , Ports.log "Sending out msg from init "
    )


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
-- The messages use RemoteData. The model does not (strip out)


type Msg
    = WalletStatus Eth.Sentry.Wallet.WalletSentry
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | SentUserInfoAndDecodedResponseToNewUser (RemoteData.WebData (List SR.Types.User))
      --| PollBlock (Result Http.Error Int)
      --| TxSentryMsg Eth.Sentry.Tx.Msg
    | GotGlobalRankingsJson (RemoteData.WebData (List SR.Types.RankingInfo))
    | GotRankingId Internal.RankingId
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | UsersReceived (RemoteData.WebData (List SR.Types.User))
    | MissingWalletInstructions
    | OpenWalletInstructions
    | NewUser
    | ResetToShowGlobal (List SR.Types.RankingInfo) Eth.Types.Address
    | ExistingUser Eth.Types.Address
    | Fail String


update : Msg -> Model -> ( Model, Cmd Msg )
update msgOfTransitonThatAlreadyHappened currentmodel =
    case currentmodel of
        Greeting uList userState walletState ->
            case msgOfTransitonThatAlreadyHappened of
                WalletStatus walletSentry_ ->
                    case walletSentry_.networkId of
                        Mainnet ->
                            case walletSentry_.account of
                                Nothing ->
                                    handleMsg OpenWalletInstructions

                                Just a ->
                                    handleMsg (ExistingUser a)

                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    handleMsg OpenWalletInstructions

                                Just a ->
                                    handleMsg (ExistingUser a)

                        _ ->
                            handleMsg MissingWalletInstructions

                -- PollBlock (Ok blockNumber) ->
                --     ( { model | blockNumber = Just blockNumber }
                --     , Task.attempt PollBlock <|
                --         Task.andThen (\_ -> Eth.getBlockNumber model.node.http) (Process.sleep 1000)
                --     )
                -- PollBlock (Err error) ->
                --     ( model, Cmd.none )
                MissingWalletInstructions ->
                    ( Greeting [] SR.Types.NewUser SR.Types.Missing, Cmd.none )

                OpenWalletInstructions ->
                    ( Greeting [] SR.Types.NewUser SR.Types.Locked, Cmd.none )

                NewUser ->
                    ( Greeting [] SR.Types.NewUser SR.Types.Opened, Cmd.none )

                ExistingUser uname ->
                    let
                        _ =
                            Debug.log "ExistingUser " "str"
                    in
                    --( Greeting (SR.Types.ExistingUser uname) SR.Types.Opened, Cmd.none )
                    --( GlobalRankings [] "" "" SR.Types.RenderAllRankings "", getRankingList )
                    ( GlobalRankings [] "" "" SR.Types.RenderAllRankings (Internal.Address ""), getRankingList )

                UsersReceived userList ->
                    let
                        usersAsJustList =
                            extractUsersFromWebData userList
                    in
                    ( Greeting usersAsJustList SR.Types.NewUser SR.Types.Missing, Cmd.none )

                Fail str ->
                    let
                        _ =
                            Debug.log "GlobalRankings fail " str
                    in
                    ( Greeting [] SR.Types.NewUser SR.Types.Missing, Cmd.none )

                _ ->
                    ( Greeting [] SR.Types.NewUser SR.Types.Missing, Cmd.none )

        GlobalRankings lrankingInfo nameStr descStr uIState rnkOwnerAddr ->
            case msgOfTransitonThatAlreadyHappened of
                GotGlobalRankingsJson rmtrnkingdata ->
                    let
                        rankingsAsJustList =
                            extractRankingsFromWebData rmtrnkingdata
                    in
                    ( GlobalRankings rankingsAsJustList "" "" SR.Types.RenderAllRankings rnkOwnerAddr, Cmd.none )

                GotRankingId rnkidstr ->
                    ( SelectedRanking lrankingInfo [] rnkidstr, fetchRanking rnkidstr )

                -- this is the response from createNewPlayerListWithCurrentUser Cmd
                -- it had the Http.expectStringResponse in it
                -- it's already created the new ranking with current player as the first entry
                -- the result now is the ranking id only at this point which was pulled out by the decoder
                -- the globalList is preserved
                SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder ->
                    case currentmodel of
                        GlobalRankings globalList newrankingName newRankingDesc _ rnkowneraddr ->
                            --todo: this is just holding code - needs re-factor
                            ( GlobalRankings globalList newrankingName newRankingDesc SR.Types.RenderAllRankings rnkowneraddr, Cmd.none )

                        _ ->
                            ( GlobalRankings lrankingInfo "" "" SR.Types.RenderAllRankings (Internal.Address ""), Cmd.none )

                SentUserInfoAndDecodedResponseToNewUser serverResponse ->
                    case currentmodel of
                        GlobalRankings globalList newrankingName newRankingDesc _ rnkowneraddr ->
                            --todo: this is just holding code - needs re-factor
                            ( GlobalRankings globalList newrankingName newRankingDesc SR.Types.RenderAllRankings rnkowneraddr, Cmd.none )

                        _ ->
                            ( GlobalRankings lrankingInfo "" "" SR.Types.RenderAllRankings (Internal.Address ""), Cmd.none )

                ResetToShowGlobal globalList rnkowneraddr ->
                    ( GlobalRankings globalList "" "" SR.Types.RenderAllRankings rnkowneraddr, Cmd.none )

                Fail str ->
                    let
                        _ =
                            Debug.log "GlobalRankings fail " str
                    in
                    ( GlobalRankings lrankingInfo "" "" SR.Types.RenderAllRankings (Internal.Address ""), Cmd.none )

                _ ->
                    ( GlobalRankings lrankingInfo "" "" SR.Types.RenderAllRankings (Internal.Address ""), Cmd.none )

        SelectedRanking globalList lPlayer intrankingId ->
            case msgOfTransitonThatAlreadyHappened of
                PlayersReceived players ->
                    let
                        playerAsJustList =
                            extractPlayersFromWebData players
                    in
                    ( SelectedRanking globalList playerAsJustList (Internal.RankingId ""), Cmd.none )

                ResetToShowGlobal _ rnkowneraddr ->
                    ( GlobalRankings globalList "" "" SR.Types.RenderAllRankings rnkowneraddr, Cmd.none )

                Fail str ->
                    let
                        _ =
                            Debug.log "SelectedRanking fail " str
                    in
                    ( SelectedRanking globalList lPlayer (Internal.RankingId ""), Cmd.none )

                _ ->
                    ( SelectedRanking globalList lPlayer (Internal.RankingId ""), Cmd.none )


handleMsg : Msg -> ( Model, Cmd Msg )
handleMsg msg =
    case msg of
        MissingWalletInstructions ->
            ( Greeting [] SR.Types.NewUser SR.Types.Missing, Cmd.none )

        OpenWalletInstructions ->
            ( Greeting [] SR.Types.NewUser SR.Types.Locked, Cmd.none )

        NewUser ->
            ( Greeting [] SR.Types.NewUser SR.Types.Opened, Cmd.none )

        ExistingUser uaddr ->
            --( Greeting (SR.Types.ExistingUser uaddr) SR.Types.Opened, Cmd.none )
            ( GlobalRankings [] "" "" SR.Types.RenderAllRankings uaddr, getRankingList )

        _ ->
            ( Greeting [] SR.Types.NewUser SR.Types.Missing, Cmd.none )


greetingHeading : String -> Element Msg
greetingHeading greetingStr =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Greeting Username"
        , Element.column Card.fill
            [ Element.el Heading.h4 <| Element.text greetingStr
            ]
        ]


globalHeading : Eth.Types.Address -> Element Msg
globalHeading uaddr =
    let
        uaddrStr =
            Eth.Utils.addressToString uaddr
    in
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Global Rankings"
        , Element.column Card.fill
            [ --Element.el Heading.h4 <| Element.text (Just uaddr)
              Element.el Heading.h4 <| Element.text uaddrStr
            ]
        ]



--Just (GotRankingId (Internal.RankingId <| String.fromInt playerObj.id))


selectedHeading : Element Msg
selectedHeading =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Selected Ranking"
        , Element.column Card.fill
            [ Element.el Heading.h4 <| Element.text "Username"
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


rankingbuttons : List SR.Types.RankingInfo -> Element Msg
rankingbuttons rankingList =
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Global Rankings"
        , Element.column (Card.simple ++ Grid.simple) <|
            insertRankingList rankingList
        , Element.column Grid.simple <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text "Button attributes can be combined with other attributes."
            ]
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
            { onPress = Just (GotRankingId (Internal.RankingId <| String.fromInt playerObj.id))

            --onPress = Nothing
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
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Nothing
                    , label = Element.text "Create New"
                    }
                , Input.button (Button.simple ++ Color.success) <|
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


homebutton : List SR.Types.RankingInfo -> Eth.Types.Address -> Element Msg
homebutton rankingList uaddr =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal rankingList uaddr
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.success) <|
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


globalResponsiveview : List SR.Types.RankingInfo -> Eth.Types.Address -> Html Msg
globalResponsiveview rankingList uaddr =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , globalHeading uaddr
            , homebutton rankingList uaddr

            --, group
            --, color
            --, grid
            , rankingbuttons rankingList

            --, input
            ]


selectedResponsiveview : List SR.Types.RankingInfo -> List SR.Types.Player -> Html Msg
selectedResponsiveview globalList playerList =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , selectedHeading
            , homebutton globalList (Internal.Address "")

            --, group
            --, color
            --, grid
            , playerbuttons playerList

            --, input
            ]


greetingView : String -> Html Msg
greetingView greetingMsg =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , greetingHeading greetingMsg
            ]


view : Model -> Html Msg
view model =
    case model of
        GlobalRankings globalList _ _ _ uaddr ->
            globalResponsiveview globalList uaddr

        SelectedRanking globalList playerList rnkid ->
            selectedResponsiveview globalList playerList

        -- Greeting _ _ ->
        --     greetingView "Greeting"
        Greeting userList userState walletState ->
            -- let
            --     -- usrlist =
            --     --     gotUserListFromRemData (SR.Types.Success userList)
            --     _ =
            --         Debug.log "user list : " userList
            -- in
            case walletState of
                SR.Types.Locked ->
                    greetingView "OpenWalletInstructions"

                SR.Types.Missing ->
                    greetingView "MissingWalletInstructions"

                SR.Types.Opened ->
                    case userState of
                        SR.Types.NewUser ->
                            greetingView "NewUserInstructions"

                        SR.Types.ExistingUser a ->
                            greetingView "Welcome back "

                SR.Types.Active ->
                    greetingView "Active "

                SR.Types.Inactive ->
                    greetingView "Inactive "


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


extractRankingsFromWebData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
extractRankingsFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success rankings ->
            rankings

        RemoteData.Failure httpError ->
            []


extractUsersFromWebData : RemoteData.WebData (List SR.Types.User) -> List SR.Types.User
extractUsersFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success users ->
            users

        RemoteData.Failure httpError ->
            []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Greeting _ _ _ ->
            Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)

        GlobalRankings _ _ _ _ _ ->
            Sub.none

        SelectedRanking _ _ _ ->
            Sub.none



--  Sub.batch
--     [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
--     , Eth.Sentry.Tx.listen model.txSentry
--     ]


fetchRanking : Internal.RankingId -> Cmd Msg
fetchRanking (Internal.RankingId rankingId) =
    --PlayersReceived is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    Http.request
        { body = Http.emptyBody
        , expect =
            SR.Decode.ladderOfPlayersDecoder
                |> Http.expectJson (RemoteData.fromResult >> PlayersReceived)
        , headers = [ SR.Defaults.secretKey ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = "https://api.jsonbin.io/b/" ++ rankingId ++ "/latest"
        }


getRankingList : Cmd Msg
getRankingList =
    let
        binName =
            Http.header
                "name"
                "Global"

        containerId =
            Http.header
                "collection-id"
                "5d7deab3371673119fab12a6"
    in
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> GotGlobalRankingsJson) SR.Decode.rankingsDecoder
        , headers = [ SR.Defaults.secretKey, binName, containerId ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingReadLink
        }


createNewPlayerListWithCurrentUser : Cmd Msg
createNewPlayerListWithCurrentUser =
    let
        binName =
            Http.header
                "name"
                "Selected"

        containerId =
            Http.header
                "collection-id"
                "5d7deb68371673119fab12d7"

        idJsonObj : Json.Encode.Value
        idJsonObj =
            Json.Encode.list
                Json.Encode.object
                [ [ ( "datestamp", Json.Encode.int 123456 )
                  , ( "active", Json.Encode.bool True )
                  , ( "currentchallengername", Json.Encode.string "" )
                  , ( "currentchallengerid", Json.Encode.int 0 )
                  , ( "address", Json.Encode.string "" )
                  , ( "rank", Json.Encode.int 1 )
                  , ( "name", Json.Encode.string "" )
                  , ( "playerid", Json.Encode.int 1 )
                  , ( "currentchallengeraddress", Json.Encode.string "" )
                  ]
                ]
    in
    --SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- decoder relates to what comes back from server. Nothing to do with above.
    Http.request
        { body =
            Http.jsonBody <| idJsonObj
        , expect = Http.expectJson (RemoteData.fromResult >> SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId) SR.Decode.newRankingIdDecoder

        -- at this point we don't have the ranking id, it's in the ranking object
        --, expect = Http.expectJson (RemoteData.fromResult >> SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId RemoteData.NotAsked) SR.Decode.newRankingDecoder
        , headers = [ SR.Defaults.secretKey, binName, containerId ]
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlForCreateNewEntryAndRespond
        }


createNewUser : Cmd Msg
createNewUser =
    let
        binName =
            Http.header
                "name"
                "Users"

        containerId =
            Http.header
                "collection-id"
                "5e4cf4ba4d073155b0dca8b8"

        idJsonObj : Json.Encode.Value
        idJsonObj =
            Json.Encode.list
                Json.Encode.object
                [ [ ( "datestamp", Json.Encode.int 1569839363942 )
                  , ( "active", Json.Encode.bool True )
                  , ( "username", Json.Encode.string "" )
                  , ( "ethaddress", Json.Encode.string "" )
                  , ( "description", Json.Encode.string "" )
                  , ( "email", Json.Encode.string "" )
                  , ( "mobile", Json.Encode.string "" )
                  ]
                ]
    in
    --SentUserInfoAndDecodedResponseToNewUser is the Msg handled by update whenever a request is made by button click
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- decoder relates to what comes back from server. Nothing to do with above.
    Http.request
        { body =
            Http.jsonBody <| idJsonObj
        , expect = Http.expectJson (RemoteData.fromResult >> SentUserInfoAndDecodedResponseToNewUser) SR.Decode.decodeNewUserListServerResponse
        , headers = [ SR.Defaults.secretKey, binName, containerId ]
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlForCreateNewEntryAndRespond
        }

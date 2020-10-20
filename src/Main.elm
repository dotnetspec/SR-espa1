module  Main exposing (Model(..), Msg(..), emptyTxRecord, init, main, update, view)

import Browser
import Element exposing (Element)
--import ElementAttributes exposing (ElementAttributes)
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
import Framework.Heading as Heading
import Framework.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Internal.Types
import Json.Encode
import Ports
import RemoteData
import SR.Constants
--import SR.Decode
import SR.Defaults
import SR.Elements
--import SR.Encode
import SR.Types
import Task
import Time exposing (Posix)
import Utils.MyUtils
import Utils.Validation.Validate
import Validate
import Data.Selected
import Data.AppState
import EverySet exposing (EverySet)
import Data.Users
import Data.Global
import Data.Rankings
import Data.Players
import Widget exposing (..)
import SR.Types
import SR.Types
import SR.Types
import Bridge
import Graphql.Http as GQLHttp

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


--The model represents the state of the application
-- Model is what is going to change via Update and assoc functions
-- update called whenever there is a Msg or Cmd Msg (e.g. from a sub)
-- it will go from 1 state to another
-- functions like view will just reflect
-- existing state of model


type Model
    = AppOps SR.Types.WalletState DataState SR.Types.AppInfo SR.Types.UIState SR.Types.SubState SR.Types.AccountState TxRecord
    | Failure String



type DataState
  = AllEmpty
  | StateFetched Data.Users.Users Data.Rankings.Rankings DataKind
  | StateUpdated Data.Users.Users Data.Rankings.Rankings DataKind

type DataKind
  =
  Global Data.Global.Global 
  | Selected Data.Selected.Selected



init : () -> ( Model, Cmd Msg )
init _ =
    ( AppOps SR.Types.WalletStateLocked AllEmpty SR.Defaults.emptyAppInfo SR.Types.UILoading  SR.Types.Subscribe SR.Types.Guest emptyTxRecord
    , Cmd.batch
        [ 
        allUsers
        ,
        allRankings
        , Ports.log
            "Sending out msg from init "
        ]
    )


emptyTxRecord : TxRecord
emptyTxRecord =
    let
        node =
            --Net.toNetworkId networkId
            Net.toNetworkId 4
                |> Ports.ethNode
    in
    { txSentry = Eth.Sentry.Tx.init ( Ports.txOut, Ports.txIn ) TxSentryMsg node.http
    , account = Nothing
    , node = node
    , blockNumber = Nothing
    , txHash = Nothing
    , tx = Nothing
    , txReceipt = Nothing
    , blockDepth = Nothing
    , errors = []
    , incomingData = ""
    }


type alias TxRecord =
    { txSentry : Eth.Sentry.Tx.TxSentry Msg
    , account : Maybe Eth.Types.Address
    , node : Ports.EthNode
    , blockNumber : Maybe Int
    , txHash : Maybe Eth.Types.TxHash
    , tx : Maybe Eth.Types.Tx
    , txReceipt : Maybe Eth.Types.TxReceipt
    , blockDepth : Maybe Eth.Sentry.Tx.TxTracker
    , errors : List String
    , incomingData : String
    }


getTime : Cmd Msg
getTime =
    Time.now
        |> Task.perform TimeUpdated



-- Msg is a description of the transition that already happened
-- Messages that delivered the response (orign doc says 'will deliver')
-- The messages use RemoteData. The model does not (strip out)


type Msg
    = -- User Ops
    ClickedSelectedOwnedRanking Internal.Types.RankingId String String
    | ClickedSelectedMemberRanking Internal.Types.RankingId String String
    | ClickedSelectedNeitherOwnerNorMember Internal.Types.RankingId String String
    | ClickedRegister
    | ClickedConfirmedRegisterNewUser
    | ClickedUpdateExistingUser
    | ClickedConfirmedUpdateExistingUser
    | ClickedCreateNewLadder
    | ClickedConfirmCreateNewLadder
    | ClickedNewChallengeConfirm
    | ClickedChallengeOpponent SR.Types.UserPlayer
    | ClickedJoinSelected
    | ClickedChangedUIStateToEnterResult SR.Types.UserPlayer
    | ClickedDeleteRanking (Maybe Eth.Types.Address)
    | ClickedDeleteRankingConfirmed
    | ClickedRemoveFromUserMemberRankings
    | ClickedEnableEthereum
    | Cancel
    | ResetToShowSelected
    | ResetRejectedNewUserToShowGlobal
    | LadderNameInputChg String
    | LadderDescInputChg String
    | NewUserNameInputChg String
    | NewUserPasswordInputChg String
    | NewUserDescInputChg String
    | NewUserEmailInputChg String
    | NewUserMobileInputChg String
    | ExistingUserNameInputChg String
    | ExistingUserPasswordInputChg String
    | ExistingUserDescInputChg String
    | ExistingUserEmailInputChg String
    | ExistingUserMobileInputChg String
    | ClickedLogInUser
    | LoggedInUser (Result (GQLHttp.Error SR.Types.Token) SR.Types.Token)
    | ReceivedUserNames (Result (GQLHttp.Error (List String)) (List String))
    | ReceivedUsers (Result (GQLHttp.Error (Maybe (List (Maybe SR.Types.FUser)))) (Maybe (List (Maybe SR.Types.FUser))))
    | ReceivedRankings (Result (GQLHttp.Error (Maybe (List (Maybe SR.Types.FRanking)))) (Maybe (List (Maybe SR.Types.FRanking))))
    | ReceivedPlayersByRankingId (Result (GQLHttp.Error (Maybe (List (Maybe SR.Types.FPlayer)))) (Maybe (List (Maybe SR.Types.FPlayer)))) String
    | CreatedGlobal
      -- App Only Ops
    | MissingWalletInstructions
    | OpenWalletInstructions
    | NoOp
    | SentResultToJsonbin (Result Http.Error ())
    | SentUserInfoAndDecodedResponseToNewUser (RemoteData.WebData (List SR.Types.User))
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | ReturnFromPlayerListUpdate (RemoteData.WebData (List SR.Types.Player))
    | ReturnFromUserListUpdate (RemoteData.WebData (List SR.Types.User))
    | ReturnedFromDeletedSelectedRankingFromJsonBin (RemoteData.WebData ( SR.Types.DeleteBinResponse))
    --| ReturnedFromDeletedRankingFromGlobalList (RemoteData.WebData (List SR.Types.Ranking))
    --| ReturnedFromDeletedRankingFromGlobalList (Result Http.Error SR.Types.)
    | ReturnedFromDeletedRankingFromGlobalList (RemoteData.WebData (SR.Types.UpdateGlobalBinResponse))
    | SentResultToWallet SR.Types.ResultOfMatch
    | AddedNewRankingToGlobalList (RemoteData.WebData (List SR.Types.Ranking))
    | TimeUpdated Posix
    | ProcessResult SR.Types.ResultOfMatch
      --Wallet Ops
    | WatchTxHash (Result String Eth.Types.TxHash)
    | WatchTx (Result String Eth.Types.Tx)
    | WatchTxReceipt (Result String Eth.Types.TxReceipt)
    | TrackTx Eth.Sentry.Tx.TxTracker
    | TxSentryMsg Eth.Sentry.Tx.Msg
    | WalletStatus Eth.Sentry.Wallet.WalletSentry
    | Fail String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WalletStatus walletSentry_, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            -- walletState might be unnecessary here, because WalletStatus is only relevant at time of unlocking i.e. one off
            case walletState of
                SR.Types.WalletStateLocked ->
                    case walletSentry_.networkId of
                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    ( AppOps SR.Types.WalletStateLocked dataState SR.Defaults.emptyAppInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription accountState emptyTxRecord
                                    , Cmd.none
                                    )

                                Just uaddr ->
                                    let
                                        newModel = AppOps SR.Types.WalletOpened dataState (gotWalletAddrApplyToUser appInfo uaddr) SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.EthEnabled emptyTxRecord
                                    in
                                    (newModel, Cmd.none)
                        _ ->
                            (model, Cmd.none)

                SR.Types.WalletStopSub ->
                    let 
                        _ = Debug.log "in walletstopsub" "here5"
                    in
                    (AppOps SR.Types.WalletStateLocked dataState appInfo uiState SR.Types.StopSubscription SR.Types.Registered  txRec, Cmd.none)

                SR.Types.WalletOpened ->
                    (model, Cmd.none)


                SR.Types.WalletWaitingForTransactionReceipt ->
                    
                    handleWalletWaitingForUserInput msg walletState dataState appInfo txRec

                _ ->
                    let 
                        _ = Debug.log "fell thru at: " "update - walletState"
                    in
                    
                            ( AppOps SR.Types.WalletStopSub AllEmpty SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions SR.Types.StopSubscription SR.Types.Registered emptyTxRecord
                            , Cmd.none
                            )
        ( WalletStatus _, Failure _ ) ->
            (model, Cmd.none)

        (ClickedEnableEthereum, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            case accountState of
                SR.Types.Guest ->
                    (AppOps walletState dataState appInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription accountState txRec, Cmd.none)
                SR.Types.Registered ->
                    (AppOps walletState dataState appInfo SR.Types.UIEnableEthereum SR.Types.StopSubscription accountState txRec, Ports.log "eth_requestAccounts")
                SR.Types.EthEnabled ->
                    (AppOps walletState dataState appInfo SR.Types.UIEthAlreadyEnabled SR.Types.StopSubscription accountState txRec, Cmd.none)
                SR.Types.EthEnabledAndRegistered ->
                    (AppOps walletState dataState appInfo SR.Types.UIEthAlreadyEnabled SR.Types.StopSubscription accountState txRec, Cmd.none)
            

        (ClickedRemoveFromUserMemberRankings, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            case dataState of
                StateFetched sUsers sRankings dKind ->
                    case dKind of
                        Selected sSelected ->
                            case appInfo.m_user of
                                Nothing ->
                                    (Failure "Err", Cmd.none)

                                Just user ->
                                    let 
                                        _ = Debug.log "User: " appInfo.m_user
                                        --newUser = Data.Rankings.removedDeletedRankingsFromUserJoined user rankings
                                        --todo: replace with a real set of rankings
                                        newUser = Data.Rankings.removedDeletedRankingsFromUserJoined user Data.Rankings.empty
                                        _ = Debug.log "newUser: " newUser
                                        updatedsUsers = Data.Users.updatedUserInSet sUsers newUser
                                    in
                                     (AppOps walletState dataState appInfo uiState SR.Types.StopSubscription SR.Types.Registered txRec, httpUpdateUsers updatedsUsers)
                        
                        _ ->
                            (model, Cmd.none)

                _ ->
                    (model, Cmd.none)


        (ClickedConfirmedRegisterNewUser, AppOps walletState dataState appInfo uiState subState accountState txRec ) ->
            
            ( AppOps walletState dataState appInfo uiState SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )
                

        (ClickedRegister, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let 
                newAppInfo = {appInfo | appState = SR.Types.AppStateCreateNewUser}
            in
                --( AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec, Cmd.none )
            case walletState of
                SR.Types.WalletStateLocked ->
                    ( AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec, Cmd.none )
                SR.Types.WalletOpenedNoUserAccount ->
                    ( AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec, Cmd.none )
                SR.Types.WalletOperational ->
                    ( AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec, Cmd.none )
                SR.Types.WalletStopSub ->
                    ( AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec, Cmd.none )
                SR.Types.WalletOpened ->
                    case newAppInfo.m_user of
                        Nothing ->
                            ( AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec, Cmd.none )
                        Just user ->
                            case user.m_ethaddress of
                                Nothing ->
                                    (model, Cmd.none)
                                Just addr ->
                                    ( AppOps SR.Types.WalletOperational dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec, Cmd.none )
                _ ->
                    (model, Cmd.none)


        (PlayersReceived response, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            (model, Cmd.none)
            -- let 
            --     --_ = Debug.log "players received " (Tuple.second (Data.Players.handleFetchedPlayers response))
            --     --_ = Debug.log "players received dataState " dataState
            --     httpReponse = Tuple.second (Data.Players.handleFetchedPlayers response)
            --     lplayer = Data.Players.asList (Tuple.first (Data.Players.handleFetchedPlayers response))
            --     --_ = Debug.log "players received lplayer " lplayer
            -- in
            --     case (Data.Players.handleFetchedPlayers response) of
            --         (sPlayers, "Success") ->
            --             case dataState of
            --                 StateFetched sUsers sRankings dKind -> 
            --                     case dKind of 
            --                             Selected sSelected ->
            --                                     case user.m_ethaddress of 
            --                                         Nothing ->
            --                                             (model, Cmd.none)
            --                                         Just addr ->
            --                                             let 
            --                                                 newSSelected = Data.Selected.createdSelected (Data.Players.asList sPlayers) sUsers rnkId
                                                        
            --                                                 m_newAppPlayer = { appInfo | player = Data.Selected.gotUserAsPlayer newSSelected addr }
            --                                             in
            --                                                 case m_newAppPlayer of
            --                                                     Nothing ->
            --                                                         (model, Cmd.none)
            --                                                     Just newAppPlayer ->
            --                                                         let
            --                                                             m_userPlayer = Data.Selected.gotUserAsPlayer newSSelected (Eth.Utils.unsafeToAddress newAppPlayer.player.player.challengeraddress)
            --                                                         in
            --                                                         case m_userPlayer of
            --                                                             Nothing ->
            --                                                                 (model, Cmd.none)
            --                                                             Just userPlayer ->
            --                                                                 let
            --                                                                     newAppChallengerAndPlayer = { newAppPlayer | challenger = userPlayer }
            --                                                                     newDataKind = Selected newSSelected rnkId user status rankings
            --                                                                     newDataState = StateFetched sUsers sRankings newDataKind
            --                                                                 in
            --                                                                     case status of 
            --                                                                         SR.Types.UserIsOwner ->     
            --                                                                             (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsOwner SR.Types.StopSubscription accountState emptyTxRecord, Cmd.none)
            --                                                                         SR.Types.UserIsMember  ->
            --                                                                             (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsPlayer SR.Types.StopSubscription accountState emptyTxRecord, Cmd.none)
            --                                                                         SR.Types.UserIsNeitherOwnerNorMember ->
            --                                                                             (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer SR.Types.StopSubscription accountState emptyTxRecord, Cmd.none)
                    
            --                             _ ->
            --                                 (model, Cmd.none)

                            -- StateUpdated sUsers sRankings dKind -> 
                            --     case dKind of 
                            --             Selected sSelected ->
                            --                 case user.m_ethaddress of 
                            --                     Nothing -> 
                            --                         (model, Cmd.none)
                            --                     Just addr ->
                            --                         let 
                            --                             newSSelected = Data.Selected.createdSelected (Data.Players.asList sPlayers) sUsers rnkId
                                            
                            --                             newAppPlayer = { appInfo | player = Data.Selected.gotUserAsPlayer newSSelected addr }

                            --                             m_userPlayer = Data.Selected.gotUserAsPlayer newSSelected (Eth.Utils.unsafeToAddress newAppPlayer.player.player.challengeraddress)
     
                            --                         in
                            --                             case m_userPlayer of 
                            --                                 Nothing ->
                            --                                     (model, Cmd.none)
                            --                                 Just userPlayer ->
                            --                                     let 
                            --                                         newAppChallengerAndPlayer = { newAppPlayer | challenger = userPlayer }
                            --                                         newDataKind = Selected newSSelected rnkId user status rankings
                            --                                         newDataState = StateFetched sUsers sRankings newDataKind
                            --                                     in
                            --                                         case status of 
                            --                                             SR.Types.UserIsOwner ->     
                            --                                                 (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsOwner SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none)
                            --                                             SR.Types.UserIsMember  ->
                            --                                                 (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsPlayer SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none)
                            --                                             SR.Types.UserIsNeitherOwnerNorMember ->
                            --                                                 (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer SR.Types.StopSubscription accountState emptyTxRecord, Cmd.none)
                                                                            
                            --             _ ->
                            --                     (model, Cmd.none)
                                

                            -- AllEmpty ->
                            --     (model, Cmd.none)
                    
                    -- (sPlayers, "404") ->
                    --     let 
                    --         _ = Debug.log " 404" "here"
                    --     in 
                    --     case dataState of
                    --         StateFetched sUsers sRankings dKind -> 
                    --             case dKind of 
                    --                     Selected sSelected ->
                    --                         (AppOps walletState dataState appInfo SR.Types.UIOwnerDeletedRanking SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none)
                    --                     _ ->
                    --                         (model, Cmd.none)
                    --         _ ->
                    --             (model, Cmd.none)

                    -- (sPlayers, "422") ->
                    --     let 
                    --         _ = Debug.log " 422" "here"
                    --     in 
                    --     case dataState of
                    --         StateFetched sUsers sRankings dKind -> 
                    --             case dKind of 
                    --                     Selected sSelected ->
                    --                         (AppOps walletState dataState appInfo SR.Types.UIOwnerDeletedRanking SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none)
                    --                     _ ->
                    --                         (model, Cmd.none)
                    --         _ ->
                    --             (model, Cmd.none)

                    -- (_, _) ->
                    --     (model, Cmd.none)

        ( PlayersReceived _, Failure _ ) ->
            (model, Cmd.none)

        (ClickedSelectedOwnedRanking rnkidstr rnkownerstr rnknamestr, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            let
                _ = Debug.log "selected ranking is : " rnkidstr
            in
            case dataState of 
                StateFetched sUsers sRankings dKind ->
                        case dKind of 
                            Global sGlobal  ->
                                let                                                     
                                    newAppInfo =
                                        updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr

                                    newDataKind = Selected (Data.Selected.created [] sUsers rnkidstr)
                                    newDataState = StateFetched sUsers sRankings newDataKind
                            
                                in
                                    ( AppOps SR.Types.WalletOpened newDataState newAppInfo SR.Types.UILoading SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, 
                                    fetchedSingleRanking rnkidstr )

                            _ ->
                                (model, Cmd.none)

                -- you may have just done an update, we can re-set to StateFetched here         
                StateUpdated sUsers sRankings dKind ->
                        case dKind of 
                            Global sGlobal  ->
                                        let
                                            newAppInfo =
                                                updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr

                                        -- re-factor from appInfo to AppState over time
                                            initAppState = 
                                                Data.AppState.updateAppState appInfo.m_user appInfo.player 
                                                appInfo.challenger (rnkidstr)

                                            --todo: replace with real players
                                            newDataKind = Selected (Data.Selected.created [] sUsers rnkidstr)
                                        
                                            newDataState = StateFetched sUsers sRankings newDataKind
                                    
                                        in
                                            ( AppOps SR.Types.WalletOpened newDataState newAppInfo SR.Types.UILoading SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, 
                                            fetchedSingleRanking rnkidstr )

                            _ ->
                                (model, Cmd.none)
                AllEmpty ->
                    (model, Cmd.none)


        (ClickedSelectedMemberRanking rnkidstr rnkownerstr rnknamestr, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            case dataState of 
                StateFetched sUsers sRankings dKind ->
                    case dKind of 
                        Global sGlobal  ->
                                    let
                                        newAppInfo = updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr

                                        -- re-factor from appInfo to AppState over time
                                        initAppState = 
                                            Data.AppState.updateAppState appInfo.m_user appInfo.player 
                                            appInfo.challenger ( rnkidstr)


                                        --newDataKind = Selected Data.Selected.empty (Internal.Types.RankingId "") appInfo.m_user SR.Types.UserIsMember (Data.Players.empty)
                                        --todo: we need to get the list of players from fauna
                                        newDataKind = Selected (Data.Selected.created [] sUsers rnkidstr)
                                        --todo: replace with real players
                                        newDataState = StateFetched sUsers sRankings newDataKind
                                    in
                                        ( AppOps walletState newDataState newAppInfo SR.Types.UISelectedRankingUserIsPlayer SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, 
                                        fetchedSingleRanking rnkidstr )
                        _ -> 
                            (model, Cmd.none)
                _ -> 
                                (model, Cmd.none)

        --todo: below will be handled differently. clicking will fetch a set of players from fauna first, then replace newsSelected with real set of players
        (ClickedSelectedNeitherOwnerNorMember rnkidstr rnkownerstr rnknamestr, AppOps walletState dataState appInfo uiState subState accountState  txRec)  ->
            let 
                _ = Debug.log "rnkid1" rnkidstr
            in
            case dataState of
                (StateFetched sUsers sRankings (Global sGlobal )) ->
                    let
                        newAppInfo =
                            updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr

                        -- re-factor from appInfo to AppState over time
                        initAppState = 
                            Data.AppState.updateAppState appInfo.m_user appInfo.player 
                            appInfo.challenger ( rnkidstr)
                        
                        --newDataKind = Selected Data.Selected.empty rnkidstr appInfo.m_user SR.Types.UserIsNeitherOwnerNorMember (Data.Global.asRankings sGlobal)
                        
                        newsSelected = Data.Selected.created [] sUsers rnkidstr
                        newDataKind = Selected newsSelected
                        newDataState = StateFetched sUsers sRankings newDataKind
                    in
                        ( AppOps walletState newDataState newAppInfo SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer SR.Types.StopSubscription accountState emptyTxRecord, 
                        fetchedSingleRanking rnkidstr )
                
                (StateFetched _ _ _) ->
                    (Failure "err", Cmd.none)

                ( AllEmpty)->
                    (Failure "err", Cmd.none)

                ( StateUpdated _ _ _ )->
                    (Failure "err", Cmd.none)
        
        


        (ClickedChangedUIStateToEnterResult player, AppOps walletState dataState appInfo uiState subState accountState  txRec)  ->
            ( AppOps walletState dataState appInfo SR.Types.UIEnterResult SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )


        (SentResultToWallet result, AppOps walletState dataState appInfo uiState subState accountState  txRec)  ->
            let
                _ =
                    Debug.log "SentResultToWallet" result


                txParams =
                    { to = txRec.account
                    , from = txRec.account
                    , gas = Nothing
                    , gasPrice = Just <| Eth.Units.gwei 4
                    , value = Just <| Eth.Units.gwei 1
                    , data = Nothing
                    , nonce = Nothing
                    }


                ( newSentry, sentryCmd ) =
                    Eth.Sentry.Tx.customSend
                        txRec.txSentry
                        { onSign = Just WatchTxHash
                        , onBroadcast = Just WatchTx
                        , onMined = Just ( WatchTxReceipt, Just { confirmations = 3, toMsg = TrackTx } )
                        }
                        txParams


                _ =
                    Debug.log "about to switch to " "SR.Types.WalletWaitingForTransactionReceipt"
                


            in


            case result of
                SR.Types.Won ->
                    let 
                        newAppInfo = {appInfo | appState = SR.Types.AppStateEnterWon }
                    in
                        ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState newAppInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txSentry = newSentry }
                        , sentryCmd
                        )
                        
                SR.Types.Lost ->                                     
                    let
                            newAppInfo = {appInfo | appState = SR.Types.AppStateEnterLost }
                    in
                        ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState newAppInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txSentry = newSentry }
                        , sentryCmd
                        )
                SR.Types.Undecided -> 
                    let
                            newAppInfo = {appInfo | appState = SR.Types.AppStateEnterUndecided }
                    in
                        ( AppOps walletState dataState newAppInfo SR.Types.UIEnterResultTxProblem SR.Types.StopSubscription SR.Types.Registered emptyTxRecord
                            , sentryCmd
                            )


        (ProcessResult result, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->               
            let
                _ =
                    Debug.log "process result" result
            in
            case result of
                SR.Types.Won ->
                    case dataState of
                        StateUpdated sUsers sRankings dKind -> 
                            case dKind of 
                                Selected sSelected ->
                                    let 
                                        handleWonTuple = Data.Selected.handleWon sSelected appInfo sUsers
                                        newDataKind = Selected (Tuple.first handleWonTuple)
                                        newDataState = StateUpdated sUsers sRankings newDataKind
                                        newModel = 
                                                AppOps walletState newDataState (Tuple.second handleWonTuple)  
                                                (Data.Selected.resultView (Data.Selected.gotStatus sSelected)) SR.Types.StopSubscription SR.Types.Registered txRec
                                    in
                                            (newModel, httpPlayerList newDataState) 
                                
                                _ -> 
                                    let 
                                        _ = Debug.log "2.1 - dataState" dataState
                                    in
                                        (model, Cmd.none)
                        _ -> 
                            let 
                                _ = Debug.log "2.1 - dataState" dataState
                            in
                                (model, Cmd.none)


                SR.Types.Lost ->
                    case dataState of
                        StateUpdated sUsers sRankings dKind -> 
                            case dKind of 
                                Selected sSelected ->
                                    let 
                                        handleLostTuple = Data.Selected.handleLost sSelected appInfo
                                        newDataKind = Selected (Tuple.first handleLostTuple)
                                        newDataState = StateUpdated sUsers sRankings newDataKind
                                        newModel = 
                                                AppOps walletState newDataState (Tuple.second handleLostTuple)  
                                                (Data.Selected.resultView (Data.Selected.gotStatus sSelected)) SR.Types.StopSubscription SR.Types.Registered txRec
                                    in
                                            (newModel, httpPlayerList newDataState) 
                                _ -> 
                                    let 
                                        _ = Debug.log "3 - dataState" dataState
                                    in
                                        (model, Cmd.none)
                        _ -> 
                            let 
                                _ = Debug.log "3 - dataState" dataState
                            in
                                (model, Cmd.none)


                SR.Types.Undecided ->
                    case dataState of
                        StateUpdated sUsers sRankings dKind -> 
                            case dKind of 
                                Selected sSelected ->
                                    let 
                                        handleUndecidedTuple = Data.Selected.handleUndecided sSelected appInfo
                                        newDataKind = Selected (Tuple.first handleUndecidedTuple)
                                        newDataState = StateUpdated sUsers sRankings newDataKind
                                        newModel = 
                                                AppOps walletState newDataState (Tuple.second handleUndecidedTuple)  
                                                (Data.Selected.resultView (Data.Selected.gotStatus sSelected)) SR.Types.StopSubscription SR.Types.Registered txRec
                                    in
                                            (newModel, httpPlayerList newDataState) 


                                _ -> 
                                    let 
                                        _ = Debug.log "4 - dataState in Undecided" dataState
                                    in
                                        (model, Cmd.none)
                        _ -> 
                            let 
                                _ = Debug.log "5 - dataState in undecided" dataState
                            in
                                (model, Cmd.none)


        (ClickedCreateNewLadder, AppOps walletState dataState appInfo uiState subState accountState  txRec) ->
            let
                newSelectedRanking =
                    appInfo.selectedRanking


                clearedNameFieldInSelectedRanking =
                    { newSelectedRanking | rankingname = "" }


                clearedNameFieldAppInfo =
                    { appInfo | selectedRanking = clearedNameFieldInSelectedRanking }

                _ = Debug.log "ClickedCreateNewLadder" dataState
            in
            ( AppOps walletState dataState clearedNameFieldAppInfo SR.Types.UICreateNewLadder SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )


        (Cancel, AppOps walletState (StateFetched sUsers sRankings dKind) appInfo uiState subState accountState  txRec ) ->
            let
                newAppInfo = {appInfo | appState = SR.Types.AppStateGeneral}
            in
                ( AppOps walletState (StateFetched sUsers sRankings dKind) newAppInfo uiState SR.Types.StopSubscription accountState emptyTxRecord, Cmd.none )
        
                                
        (Cancel, AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState  txRec ) ->
            case appInfo.m_user of
                Nothing ->
                    (model, Cmd.none)
                Just userVal ->
                    let
                        newDataKind = Global Data.Global.empty
                        newDataState = StateFetched sUsers sRankings newDataKind

                        _ = Debug.log "toGlobal now" "stateupdated"
                    in
                    ( AppOps walletState newDataState appInfo SR.Types.UILoading SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )

        (Cancel, AppOps walletState AllEmpty appInfo uiState subState accountState  txRec ) ->
            (Failure "Network error ...", Cmd.none)

        (ResetToShowSelected, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            case dataState of 
                StateFetched sUsers sRankings dKind ->
                    case dKind of
                        Selected sSelected ->
                            case (Data.Selected.gotStatus sSelected) of 
                                SR.Types.UserIsOwner ->
                                    (AppOps walletState dataState appInfo SR.Types.UISelectedRankingUserIsOwner SR.Types.StopSubscription accountState emptyTxRecord, Cmd.none )
                                SR.Types.UserIsMember ->
                                    (AppOps walletState dataState appInfo SR.Types.UISelectedRankingUserIsPlayer SR.Types.StopSubscription accountState emptyTxRecord, Cmd.none )
                                SR.Types.UserIsNeitherOwnerNorMember ->
                                    (AppOps walletState dataState appInfo SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer SR.Types.StopSubscription accountState emptyTxRecord, Cmd.none )
                        _ -> 
                            (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)


        (ClickedUpdateExistingUser, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let 
                _ = Debug.log "ClickedUpdateExistingUser " walletState
                --newDataState = StateFetched sUsers sRankings dKind
                --
            in
            -- if user already did an update, need to ensure we start with StateFetched again
            case dataState of
                StateUpdated sUsers sRankings user ->
                    let 
                        newDataState = StateFetched sUsers sRankings user
                    in
                        ( AppOps walletState newDataState appInfo SR.Types.UIUpdateExistingUser SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )
                _ ->
                    ( AppOps walletState dataState appInfo SR.Types.UIUpdateExistingUser SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )


        (LadderNameInputChg namefield, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                newSelectedRanking =
                    appInfo.selectedRanking


                updatedSelectedRanking =
                    { newSelectedRanking | rankingname = namefield }


                newAppInfo =
                    { appInfo | selectedRanking = updatedSelectedRanking }
            in
            ( AppOps walletState dataState newAppInfo SR.Types.UICreateNewLadder SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )


        (LadderDescInputChg descfield, AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                newSelectedRanking =
                    appInfo.selectedRanking


                updatedSelectedRanking =
                    { newSelectedRanking | rankingdesc = (Just descfield) }


                newAppInfo =
                    { appInfo | selectedRanking = updatedSelectedRanking }
            in
            ( AppOps walletState dataState newAppInfo SR.Types.UICreateNewLadder SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )


        
        (NewUserNameInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserNameInputChg updateField), Cmd.none )

        (NewUserPasswordInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserPasswordInputChg updateField), Cmd.none )


        (NewUserDescInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserDescInputChg updateField), Cmd.none )


        (NewUserEmailInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserEmailInputChg updateField), Cmd.none )


        (NewUserMobileInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserMobileInputChg updateField), Cmd.none )


        (ExistingUserNameInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserNameInputChg updateField), Cmd.none )

        (ExistingUserPasswordInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserPasswordInputChg updateField), Cmd.none )


        (ExistingUserDescInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserDescInputChg updateField), Cmd.none )


        (ExistingUserEmailInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserEmailInputChg updateField), Cmd.none )


        (ExistingUserMobileInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserMobileInputChg updateField), Cmd.none )


        (ClickedConfirmedUpdateExistingUser, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            case dataState of
                StateFetched sUsers sRankings user ->
                    let 
                                _ = Debug.log "14.1" dataState
                                newDataState = StateUpdated sUsers sRankings user
                    in
                    case appInfo.m_user of
                        Nothing ->
                            (model, Cmd.none)
                        Just userVal ->
                            --( AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered txRec, updateExistingUser (Data.Users.asList sUsers) userVal )
                            ( AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered txRec, 
                            --updateExistingUser (Data.Users.asList sUsers) userVal
                            updateExistingUser <| Data.Users.updatedUserInSet sUsers userVal
                            )
                    
                _ -> 
                            let 
                                _ = Debug.log "14.3 - dataState" dataState
                            in
                                (model, Cmd.none)


        (SentUserInfoAndDecodedResponseToNewUser serverResponse, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            (AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )


        (ClickedChallengeOpponent opponentAsPlayer, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            case dataState of
                StateFetched sUsers sRankings dKind ->                   
                    case dKind of 
                        Selected sSelected ->
                            case appInfo.m_user of
                                Nothing ->
                                    (model, Cmd.none)
                                Just userVal ->
                                    ( updatedForChallenge model (Data.Selected.asList sSelected) opponentAsPlayer userVal, Cmd.none )
                        _ ->
                            (model, Cmd.none)
                _ ->
                    (model, Cmd.none)


        (ClickedDeleteRanking uaddr, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            case dataState of 
                StateFetched sUsers sRankings dKind ->
                    case dKind of 
                        Selected sSelected ->
                            case appInfo.m_user of 
                                Nothing ->
                                    (Failure "Err", Cmd.none)
                                Just user ->
                                    let 
                                        newsUsers = Data.Users.updatedUserInSet sUsers 
                                            (Data.Users.removedRankindIdFromUser (Utils.MyUtils.stringFromRankingId (Data.Selected.gotRankingId sSelected)) user)
                                        --removedRanking = Data.Rankings.removedById rnkId sRanking
                                        --todo: replace with a real set of rankings
                                        removedRanking = Data.Rankings.removedById (Data.Selected.gotRankingId sSelected) Data.Rankings.empty
                                        newDataKind = Global (Data.Global.created removedRanking sUsers)
                                        
                                        newDataState = StateUpdated newsUsers sRankings newDataKind
                                        _ = Debug.log "ranking should have been removed from rankings" removedRanking
                                    in
                                        ( AppOps walletState
                                            newDataState
                                            appInfo
                                            SR.Types.UIDeleteRankingConfirm
                                            SR.Types.StopSubscription SR.Types.Registered
                                            txRec
                                        , Cmd.none
                                        )
                        _ -> 
                            let 
                                _ = Debug.log "8 - dataState should be updated" dataState
                            in
                                (model, Cmd.none)
                _ -> 
                            let 
                                _ = Debug.log "9 - dataState" dataState
                            in
                                (model, Cmd.none)
        
        (ClickedDeleteRankingConfirmed, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            case dataState of 
                StateUpdated sUsers sRankings dKind ->
                    case dKind of 
                        Selected sSelected ->
                             ( AppOps walletState
                                    dataState
                                    appInfo
                                    uiState
                                    SR.Types.StopSubscription SR.Types.Registered
                                    txRec
                                , 
                                    httpDeleteSelectedRankingFromJsonBin (Utils.MyUtils.stringFromRankingId (Data.Selected.gotRankingId sSelected))
                            )
                        
                        Global sGlobal  ->
                           (Failure "Selected?", Cmd.none)
                _ ->
                    ( model, Cmd.none )


        (ReturnedFromDeletedSelectedRankingFromJsonBin result, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            -- nb. you haven't used the result
            let 
                _= Debug.log "result"  result
            in
        
            case dataState of
                StateUpdated sUsers sRankings dKind ->
                    case dKind of 
                        Global sGlobal  ->
                            let
                                -- disabled due to refacroring
                                --newGlobal = Data.Global.removedUserRankingByRankingId sGlobal rnkId
                                newDataKind = Global Data.Global.empty
                                newDataState = StateUpdated sUsers sRankings newDataKind           
                            in 
                                ( AppOps walletState
                                    newDataState
                                    appInfo
                                    uiState
                                    SR.Types.StopSubscription SR.Types.Registered
                                    txRec
                                --, httpDeleteSelectedRankingFromGlobalList newGlobal
                                , Cmd.none
                                )
                        _ ->
                            ( model, Cmd.none )
                _ ->
                    ( model, Cmd.none )


        (ReturnedFromDeletedRankingFromGlobalList response, AppOps walletState dataState appInfo uiState subState accountState  txRec )  ->
            let 
                _ = Debug.log "Result response " response
            in
            case (Data.Rankings.handleServerDeletedRanking response) of
                (sRanking, "Success") ->
                    case dataState of 
                        StateUpdated sUsers sRankings dKind ->
                            case dKind of
                                Global sGlobal  ->
                                            let
                                                newDataKind = Global sGlobal 
                                                newDataState = StateFetched sUsers sRankings newDataKind
                                                
                                                _ = Debug.log "Ranking removed on return from list updated? " Data.Global.asList sGlobal
                                                
                                            in
                                                ( AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )
                                        
                                _ -> 
                                            let 
                                                _ = Debug.log "11 - dataState should be Global" dataState
                                            in
                                                (model, Cmd.none)
                        _ -> 
                            (model, Cmd.none)

                (sRanking, "404") ->
                        case dataState of
                            StateUpdated sUsers sRankings dKind -> 
                                case dKind of 
                                        Global sGlobal  ->
                                            (AppOps walletState dataState appInfo SR.Types.UIUnableToFindGlobalRankings SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none)
                                        _ ->
                                            (model, Cmd.none)
                            _ ->
                                (model, Cmd.none)
                
                -- todo: add more error conditions
                (_, _) ->
                    (model, Cmd.none)    
                

        (WatchTxHash (Ok txHash), AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
                    let
                        _ =
                            Debug.log "WatchTxHash in wallet operational " "Ok - hash watched and all ok"
                    in
                    case walletState of 
                        SR.Types.WalletOperational -> 
                            ( AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered { txRec | txHash = Just txHash }, Cmd.none )
                        _ ->
                            (model, Cmd.none)

        (WatchTxHash (Err err),  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ =
                    Debug.log "WatchTxHash" "Err"
            in
                case walletState of 
                    SR.Types.WalletOperational -> 
                        ( AppOps SR.Types.WalletStateMissing dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )
                    _ -> 
                        (model, Cmd.none)
        
        
        (WatchTx (Ok tx),  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ =
                    Debug.log "WatchTx" "tx Ok"
            in
            case walletState of 
                SR.Types.WalletOperational -> 
                    AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered { txRec | tx = Just tx } |> update (ProcessResult SR.Types.Won)
                _ ->
                    (model, Cmd.none)

        (WatchTx (Err err),  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ =
                    Debug.log "WatchTx tx err" err
            in
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps SR.Types.WalletStateLocked dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )
                _ -> 
                    (model, Cmd.none)
        
        
        (WatchTxReceipt (Ok txReceipt),  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ =
                    Debug.log "handleWalletStateOpenedAndOperational Receipt" txReceipt
            in
            case walletState of 
                SR.Types.WalletOperational ->
                    AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered emptyTxRecord
                        |> update (ProcessResult SR.Types.Won)
                _ -> 
                    (model, Cmd.none)

        (WatchTxReceipt (Err err),  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ =
                    Debug.log "tx err" err
            in
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )
                _ ->
                    (model, Cmd.none)
        
        (TrackTx blockDepth,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ =
                    Debug.log "TrackTx" "TrackTx"
            in
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | blockDepth = Just blockDepth }, Cmd.none )
                _ -> 
                    (model, Cmd.none)

        -- this is the response from addedUserAsFirstPlayerInNewList Cmd
        -- it had the Http.expectStringResponse in it
        -- it's already created the new ranking with current player as the first entry
        -- the result now is the ranking id only at this point which was pulled out by the decoder
        (SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    case dataState of 
                            StateFetched sUsers sRankings dKind ->
                                case dKind of 
                                    Global sGlobal  ->
                                        case appInfo.m_user of
                                            Nothing ->
                                                (model, Cmd.none)
                                            Just userVal ->
                                                let
                                                    extractedRankingId = Data.Global.gotNewRankingIdFromWebData idValueFromDecoder
                                                    newSGlobal = Data.Global.addUserRanking sGlobal extractedRankingId appInfo.selectedRanking userVal
                                                    newGlobalAsList = Data.Global.rankingsAsList newSGlobal
                                                    newGlobalUpdated = Global newSGlobal
                                                    newDataState = StateUpdated sUsers sRankings newGlobalUpdated
                                                in
                                                    ( AppOps SR.Types.WalletOperational newDataState appInfo SR.Types.UICreateNewLadder SR.Types.StopSubscription SR.Types.Registered emptyTxRecord
                                                    ,
                                                    httpPutRequestForAddGlobal (Data.Global.newJsonEncodedList (newGlobalAsList)) newGlobalAsList
                                                    )
                                    _ -> 
                                        let 
                                            _ = Debug.log "dataState should be Global" dataState
                                        in
                                            (model, Cmd.none)

                            StateUpdated sUsers sRankings dKind ->
                                case dKind of 
                                    Global sGlobal  -> 
                                        let 
                                            _ = Debug.log "6 - dataState SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId" dataState
                                        in
                                            (AppOps SR.Types.WalletOpened dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none)
                                    _ -> 
                                        (model, Cmd.none)

                            AllEmpty ->
                                (model, Cmd.none)
                
                _ -> 
                    (model, Cmd.none)

        (ResetRejectedNewUserToShowGlobal,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let 
                newAppInfo = {appInfo | m_user = Just (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)}
            in 
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps SR.Types.WalletOperational dataState newAppInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, allRankings )
                _ -> 
                    (model, Cmd.none)


        (ClickedConfirmCreateNewLadder,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            case walletState of 
                SR.Types.WalletOpened ->
                    case dataState of 
                        StateFetched sUsers sRankings dKind ->
                                case dKind of 
                                    Global sGlobal  ->
                                        --if Data.Users.isRegistered (Data.Users.asList sUsers) appInfo.m_user then
                                        case appInfo.m_user of
                                            Nothing ->
                                                (model, Cmd.none)
                                            
                                            Just user ->
                                                case user.m_ethaddress of 
                                                    Nothing ->
                                                        ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )
                                                    Just addr ->

                                                --if user.m_ethaddress /= "" then
                                                        let
                                                            txParams =
                                                                { to = txRec.account
                                                                , from = txRec.account
                                                                , gas = Nothing
                                                                , gasPrice = Just <| Eth.Units.gwei 4
                                                                , value = Just <| Eth.Units.gwei 1
                                                                , data = Nothing
                                                                , nonce = Nothing
                                                                }

                                                            ( newSentry, sentryCmd ) =
                                                                Eth.Sentry.Tx.customSend
                                                                    txRec.txSentry
                                                                    { onSign = Just WatchTxHash
                                                                    , onBroadcast = Just WatchTx
                                                                    , onMined = Just ( WatchTxReceipt, Just { confirmations = 3, toMsg = TrackTx } )
                                                                    }
                                                                    txParams

                                                            newAppInfo =
                                                                    { appInfo | appState = SR.Types.AppStateCreateNewLadder }
                                                            
                                                            _ = Debug.log "global with useradd" user.m_ethaddress
                                                            
                                                        in
                                                        ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState newAppInfo SR.Types.UIRenderAllRankings SR.Types.Subscribe SR.Types.Registered { txRec | txSentry = newSentry }
                                                        --Cmd.batch [ sentryCmd, addedUserAsFirstPlayerInNewList appInfo.m_user ] )
                                                        ,sentryCmd)

                                        -- else
                                        --     ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )

                                    _ -> 
                                        let 
                                            _ = Debug.log "6 - dataState should be Global" dataState
                                        in
                                            (model, Cmd.none)
                        _ -> 
                                    let 
                                        _ = Debug.log "6 - dataState" dataState
                                    in
                                        (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)

        (AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            -- I think the global set has already been updated
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered emptyTxRecord, Cmd.none )
                _ ->
                    (model, Cmd.none)
            
        (ClickedNewChallengeConfirm,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    case dataState of
                        StateFetched sUsers sRankings dKind -> 
                            case dKind of 
                                Selected sSelected ->
                                    let
                                        newDataKind = Selected (Data.Selected.assignChallengerAddrsForBOTHPlayers sSelected appInfo)
                                        newDataState = StateUpdated sUsers sRankings newDataKind
                                        updatedModel = AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered txRec
                                    in 
                                        ( updatedModel, httpPlayerList (newDataState))

                                _ -> 
                                    let 
                                        _ = Debug.log "7.1 - dataState shuld be Selected" dataState
                                    in
                                        (model, Cmd.none)
                        _ -> 
                                    let 
                                        _ = Debug.log "7.1 - dataState" dataState
                                    in
                                        (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)


        (ClickedJoinSelected,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            (model, Cmd.none)
        -- todo: re-implement when fauna ready
            -- let 
            --     --_ = Debug.log "ClickedJoinSelected" "here"
            --     _ = Debug.log "walletstatein ClickedJoinSelected" walletState
            -- in
            -- case walletState of 
            --     SR.Types.WalletOpened ->
            --         case dataState of
            --             StateFetched sUsers sRankings dKind -> 
            --                 case dKind of 
            --                     Selected sSelected ->
            --                         case accountState of
            --                             SR.Types.Guest -> 
            --                                 ( AppOps walletState dataState appInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription accountState txRec, Cmd.none )
            --                             SR.Types.Registered ->
            --                                 ( AppOps walletState dataState appInfo SR.Types.UIEthAlreadyEnabled SR.Types.StopSubscription accountState txRec, Cmd.none )

            --                             SR.Types.EthEnabled ->
            --                                 ( AppOps walletState dataState appInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription accountState txRec, Cmd.none )

            --                             SR.Types.EthEnabledAndRegistered ->
            --                                 case appInfo.m_user of
            --                                     Nothing ->
            --                                         (model, Cmd.none)
            --                                     Just userVal ->
            --                                         let
            --                                             newLUPlayer = Data.Selected.userAdded sUsers appInfo.selectedRanking.id_ (Data.Selected.asList sSelected) userVal
            --                                             newSelected = Data.Selected.asSelected (EverySet.fromList newLUPlayer)
                                                        
            --                                             newDataKind = Selected newSelected rnkId user SR.Types.UserIsMember Data.Players.empty
            --                                             newDataState = StateUpdated sUsers sRankings newDataKind
            --                                             updatedModel = AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription accountState txRec
            --                                         in
            --                                             ( updatedModel, httpPlayerList (newDataState))

            --                     _ -> 
            --                         let 
            --                             _ = Debug.log "12 - dataState should be Selected" dataState
            --                         in
            --                             (model, Cmd.none)

            --             _ -> 
            --                         let 
            --                             _ = Debug.log "12 - dataState" dataState
            --                         in
            --                             (model, Cmd.none)

                -- SR.Types.WalletStopSub ->
                --     ( AppOps walletState dataState appInfo SR.Types.UIEnableEthereum SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )

                -- SR.Types.WalletStateLocked ->
                --     ( AppOps walletState dataState appInfo SR.Types.UIEnableEthereum SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )

                -- SR.Types.WalletOpenedNoUserAccount ->
                --     ( AppOps walletState dataState appInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription accountState txRec, Cmd.none )

                -- _ -> 
                --     let 
                --         _ = Debug.log "walletState in ClickedJoinSelected : " walletState
                --     in
                --     (model, Cmd.none)

        (ReturnFromPlayerListUpdate response,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ = Debug.log "ReturnFromPlayerListUpdate" walletState
            in
            case walletState of 
                SR.Types.WalletOpened ->
                    case dataState of 
                        StateUpdated sUsers sRankings dKind ->
                            case dKind of 
                                Selected sSelected ->  
                                    let
                                        lplayer =
                                            Data.Players.extractPlayersFromWebData response

                                        --addedNewJoinedRankingId : String -> SR.Types.User -> List SR.Types.User -> List SR.Types.User
                                        --newUserList = Data.Users.addedNewJoinedRankingId (Utils.MyUtils.stringFromRankingId rnkId) user (Data.Users.asList sUsers)
                                        newUserList = Data.Users.asList (Data.Selected.asUsers sSelected)

                                        convertedToUserPlayers =
                                            Data.Selected.convertPlayersToUserPlayers
                                                lplayer
                                                --(Data.Users.asList sUsers)
                                                newUserList

                                        _ = Debug.log "ReturnFromPlayerListUpdate fetched selected" convertedToUserPlayers

                                        --httpUpdateUsersJoinRankings is the http cmd that we need to do here
                                        
                                    in
                                        case appInfo.m_user of
                                            Nothing ->
                                                (model, Cmd.none)
                                            Just user ->
                                                ( updateSelectedRankingPlayerList model convertedToUserPlayers
                                                --, httpUpdateUsersJoinRankings (Utils.MyUtils.stringFromRankingId rnkId) user newUserList )
                                                , httpUpdateUsersJoinRankings (Utils.MyUtils.stringFromRankingId (Data.Selected.gotRankingId sSelected)) user newUserList )
                                
                                _ -> 
                                    let 
                                        _ = Debug.log "13 - dataState should be Selected" dataState
                                    in
                                        (model, Cmd.none)
                        _ -> 
                            let 
                                _ = Debug.log "13 - dataState" dataState
                            in
                                (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)

            

        (ReturnFromUserListUpdate response,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let 
                _ =
                    Debug.log "ReturnFromUserListUpdate" walletState
            in
            case walletState of 
                SR.Types.WalletOpened ->
                    case dataState of 
                        StateFetched sUsers sRankings dKind ->
                            case dKind of 
                                Global sGlobal  ->
                                    let 
                                        lusers = Data.Users.extractUsersFromWebData response
                                        newGlobal = Data.Global.created (Data.Global.asRankings sGlobal) (Data.Users.asUsers (EverySet.fromList lusers))
                                        newDataKind = Global newGlobal
                                        newDataState = StateFetched (Data.Users.asUsers (EverySet.fromList lusers)) sRankings newDataKind
                                    in
                                    (AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none)
                                _ -> 
                                    (model, Cmd.none)
                        
                        StateUpdated sUsers sRankings dKind ->
                            case dKind of 
                                    Global sGlobal  ->
                                        let 
                                            lusers = Data.Users.extractUsersFromWebData response
                                            newGlobal = Data.Global.created (Data.Global.asRankings sGlobal) (Data.Users.asUsers (EverySet.fromList lusers))
                                            newDataKind = Global newGlobal
                                            newDataState = StateUpdated (Data.Users.asUsers (EverySet.fromList lusers)) sRankings newDataKind
                                        in
                                        (AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none)
                                    _ -> 
                                        (model, Cmd.none)
                        AllEmpty ->
                        --_ ->
                            (model, Cmd.none)

                _ ->
                    (model, Cmd.none)

        (TimeUpdated posixTime,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ =
                    Debug.log "posixtime" posixTime
            in
            ( model, Cmd.none )
        

        -- TxSentryMsg updates when user clicks 'Confirm' in the wallet
        (TxSentryMsg subMsg,  AppOps walletState dataState appInfo uiState subState accountState  txRec ) ->
            let
                _ =
                    Debug.log "handleTxSubMsg subMsg" <| handleTxSubMsg subMsg
            
            
                ( subModel, subCmd ) =
                    Eth.Sentry.Tx.update subMsg txRec.txSentry
            in
            if handleTxSubMsg subMsg then
                case appInfo.appState of
                    SR.Types.AppStateCreateNewLadder ->
                        case appInfo.m_user of 
                            Nothing ->
                                let
                                    _ =
                                            Debug.log "No User" "16"
                                in
                                (model, Cmd.none)

                            Just userVal ->
                                ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txSentry = subModel }
                                , Cmd.batch [subCmd, addedUserAsFirstPlayerInNewList userVal] )
                    
                    SR.Types.AppStateCreateNewUser ->
                        case appInfo.m_user of 
                            Nothing ->
                                (model, Cmd.none)

                            Just userVal ->
                                let
                                    userSet = case dataState of 
                                                StateFetched users rankings dKind -> 
                                                    users 
                                                StateUpdated users rankings dKind ->
                                                    users 
                                                _ -> 
                                                    Data.Users.empty
                                in
                                ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txSentry = subModel }
                                , Cmd.batch [subCmd,  createNewUser userSet userVal])

                    SR.Types.AppStateEnterWon -> 
                        let 
                            _ =
                                Debug.log "in AppStateEnterWon" "yes"
                        in
                        case dataState of 
                            StateFetched sUsers sRankings dKind -> 
                                let 
                                    newDataState = StateUpdated sUsers sRankings dKind
                                    _ =
                                        Debug.log "handleTxSubMsg subMsg  dataState" newDataState
                                in 
                        
                                    (AppOps walletState newDataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Won) )
                            
                            _ ->
                                ( AppOps walletState dataState appInfo SR.Types.UIEnterResultTxProblem SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )
                          

                    SR.Types.AppStateEnterLost ->
                        case dataState of
                            StateFetched sUsers sRankings dKind -> 
                                let
                                    _ =
                                        Debug.log "in AppStateEnterLost" "yes"

                                    newDataState = StateUpdated sUsers sRankings dKind
                                in
                                    (AppOps SR.Types.WalletOperational newDataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Lost))
                            _ ->
                                ( AppOps walletState dataState appInfo SR.Types.UIEnterResultTxProblem SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )
                    
                    SR.Types.AppStateEnterUndecided -> 
                        case dataState of
                            StateFetched sUsers sRankings dKind -> 
                                let
                                    _ =
                                       Debug.log "in AppStateEnterUndecided" "yes"

                                    newDataState = StateUpdated sUsers sRankings dKind
                                in
                                    ( AppOps SR.Types.WalletOperational newDataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Undecided))
                            _ ->
                                ( AppOps walletState dataState appInfo SR.Types.UIEnterResultTxProblem SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )

                    _ -> 
                       ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txSentry = subModel }, subCmd ) 

            else
                ( AppOps walletState dataState appInfo SR.Types.UIEnterResultTxProblem SR.Types.StopSubscription SR.Types.Registered txRec, Cmd.none )

                
        (ClickedLogInUser, model_) ->
            case model of 
                AppOps walletState dataState appInfo uiState subState accountState txRec ->
                    case appInfo.m_user of 
                        Nothing ->
                            (model, Cmd.none)
                        Just userVal ->
                            --(model, createAndOrLoginUser userVal.username userVal.password userVal.m_ethaddress)
                            (model, loginUser userVal.username userVal.password)
                            
                    
                Failure _ ->
                    (model, Cmd.none)
           

        (LoggedInUser response, modelReDef) ->
            ( updateFromLoggedInUser modelReDef response
               , commandFromLoggedInUser response
            )

        (ReceivedUsers response, modelReDef) ->
            ( updateWithReceivedUsers modelReDef response
            , Cmd.none
            )

        (ReceivedRankings response, modelReDef) ->
            ( updateWithReceivedRankings modelReDef response
            , Cmd.none
            )

        (ReceivedPlayersByRankingId response rankingid, modelReDef) ->
            ( updateWithReceivedPlayersByRankingId modelReDef response rankingid
            , Cmd.none
            )

        (CreatedGlobal, _) ->
            (updateGlobal model, Cmd.none)

        (ReceivedUserNames response, modelReDef) ->
            ( updateModelFromReceivedUserNames modelReDef response
            , Cmd.none
            )
        
        (NoOp, _) ->
            let
                _ =
                    Debug.log "handledWalletStateOpened no op" msg
            in
            ( model, Cmd.none )


        (_, _) ->
            let
                _ =
                    Debug.log "handledWalletStateOpened1" msg
            in
            ( Failure "handledWalletStateOpened2"
            , Cmd.none
            )



-- GQL commands

commandFromLoggedInUser : Result (GQLHttp.Error SR.Types.Token) SR.Types.Token -> Cmd Msg
commandFromLoggedInUser response =
    case response of
        Ok token ->
            allUserNames token

        Err _ ->
            Cmd.none

createAndOrLoginUser : String -> String -> String -> Cmd Msg
createAndOrLoginUser user_name password m_ethaddress =
    GQLHttp.send LoggedInUser (Bridge.requestCreateAndOrLoginUser Bridge.handleCreateAndOrLoginUserOptionalArguments user_name password m_ethaddress)

loginUser : String -> String -> Cmd Msg
loginUser user_name password =
    GQLHttp.send LoggedInUser (Bridge.requestLoginUser user_name password)


allUserNames : SR.Types.Token -> Cmd Msg
allUserNames token =
    GQLHttp.send ReceivedUserNames (Bridge.requestAllUserNames token)

allUsers : Cmd Msg
allUsers  =
    GQLHttp.send ReceivedUsers (Bridge.requestAllUsers)

allRankings : Cmd Msg
allRankings  =
    GQLHttp.send ReceivedRankings (Bridge.requestAllRankings)


-- gotRankingById : Cmd Msg 
-- gotRankingById = 
--     GQLHttp.send ReceivedRankingById (Bridge.findRankingById)

       
-- model handlers

updateGlobal : Model -> Model 
updateGlobal model = 
    case model of 
        AppOps walletState dataState appInfo uiState subState accountState txRec ->
            case dataState of 
                AllEmpty ->
                    model
                StateFetched sUsers sRankings dKind ->
                    case dKind of 
                        Global _ ->
                            let
                                newDataKind = Global (Data.Global.created sRankings sUsers)
                                newDataState = StateUpdated sUsers sRankings newDataKind
                            in
                                AppOps walletState newDataState appInfo uiState subState accountState txRec

                        Selected _ ->
                            Failure "updateGlobal"

                StateUpdated _ _ _ ->
                    model

        Failure str ->
            Failure "updateGlobal"

-- todo: change the name here as it's not actually updating the model - just testing faunadb
updateModelFromReceivedUserNames : Model -> Result (GQLHttp.Error (List String)) (List String) -> Model
updateModelFromReceivedUserNames model response =
    case response of
        Ok lusernames ->
                model

        Err _ ->
            model

--GQLHttp.Error (Maybe (List (Maybe SR.Types.FUser)))) (Maybe (List (Maybe SR.Types.FUser))
updateWithReceivedUsers : Model -> Result (GQLHttp.Error (Maybe (List (Maybe SR.Types.FUser)))) (Maybe (List (Maybe SR.Types.FUser))) -> Model
updateWithReceivedUsers model response =
    case (model, response) of -- AllEmpty, so fill the User set
        (AppOps walletState AllEmpty appInfo uiState subState accountState txRec, Ok lusers)  ->
                    let
                        filteredFUserList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lusers)
                        -- need to convert from FRanking to Ranking (id_ needs to be a String)
                        lFromFToUser = List.map SR.Types.newUser filteredFUserList
                        --_ = Debug.log "lFromFToUser : " lFromFToUser
                        sUsers = Data.Users.asUsers (EverySet.fromList lFromFToUser)
                        newDataState = StateFetched sUsers Data.Rankings.empty (Global Data.Global.empty)
                        
                    in
                        AppOps walletState newDataState appInfo uiState subState accountState txRec
        
        (AppOps walletState (StateFetched sUsers sRankings  (Global _)) appInfo uiState subState accountState txRec, Ok lusers) ->
                if Data.Rankings.isEmpty sRankings then -- just fill the User set
                    let
                        filteredFUserList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lusers)
                        lFromFToUser = List.map SR.Types.newUser filteredFUserList
                        newsUsers = Data.Users.asUsers (EverySet.fromList lFromFToUser)
                        newDataState = StateFetched sUsers sRankings (Global Data.Global.empty)
                    in
                        AppOps walletState newDataState appInfo uiState subState accountState txRec

                else --if sRankings isn't empty we can populate Global now
                    let
                        filteredFUserList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lusers)
                        lFromFToUser = List.map SR.Types.newUser filteredFUserList
                        newsUsers = Data.Users.asUsers (EverySet.fromList lFromFToUser)
                        newDataKind = Global (Data.Global.created sRankings newsUsers)
                        newDataState = StateFetched newsUsers sRankings newDataKind
                    in
                        AppOps walletState newDataState appInfo uiState subState accountState txRec


        (AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState txRec, Ok lusers) ->
            model

        ( AppOps _ (StateFetched _ _ (Selected _)) _ _ _ _ _, Ok _ ) ->
            (Failure "updateWithReceivedUsers8")

        (AppOps walletState AllEmpty appInfo uiState subState accountState txRec, Err _ )  ->
            (Failure "Unable to obtain User data. \nPlease check your network connection ...")

        (AppOps walletState (StateFetched sUsers sRankings dKind) appInfo uiState subState accountState txRec, Err _)  ->
            (Failure "updateWithReceivedUsers10")

        (AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState txRec, Err _ ) ->
            (Failure "updateWithReceivedUsers11")

        (Failure _, Ok lusers) ->
            (Failure "updateWithReceivedUsers20")

        (Failure _, Err str) ->
            (Failure "Unable to obtain User data. \nPlease check your network connection ...")



updateWithReceivedPlayersByRankingId : Model -> Result (GQLHttp.Error (Maybe (List (Maybe SR.Types.FPlayer)))) (Maybe (List (Maybe SR.Types.FPlayer))) -> String -> Model
updateWithReceivedPlayersByRankingId model response rankingid =
    case (model, response) of -- AllEmpty, so fill the player set
        (AppOps walletState AllEmpty appInfo uiState subState accountState txRec, Ok lplayers)  ->
            (Failure "updateWithReceivedPlayersByRankingId10")

        (AppOps walletState (StateFetched sUsers  sRankings  (Global _ )) appInfo uiState subState accountState txRec, Ok lplayers) ->
                --if Data.Users.isEmpty sUsers then -- just fill the player set
                    let
                        filteredFPlayerList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lplayers)
                        lFromFToPlayer = List.map SR.Types.newPlayer filteredFPlayerList
                        newsplayers = Data.Players.asPlayers (EverySet.fromList lFromFToPlayer)
                        -- todo: change createdSelected to accept a Set instead of a list
                        newsSelected = Data.Selected.created lFromFToPlayer sUsers (Internal.Types.RankingId rankingid)
                        newDataKind = Selected newsSelected
                        newDataState = StateFetched sUsers sRankings newDataKind
                    in
                        AppOps walletState newDataState appInfo uiState subState accountState txRec

        (AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState txRec, Ok lplayers) ->
            model

        ( AppOps _ (StateFetched _ _ (Selected _)) _ _ _ _ _, Ok _ ) ->
            (Failure "updateWithReceivedPlayersByRankingId13")

        (AppOps walletState AllEmpty appInfo uiState subState accountState txRec, Err _ )  ->
            (Failure "Unable to obtain Rankings data. Please check your network connection ...")

        (AppOps walletState (StateFetched sUsers sRankings dKind) appInfo uiState subState accountState txRec, Err _)  ->
            (Failure "updateWithReceivedPlayersByRankingId15")

        (AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState txRec, Err _ ) ->
            (Failure "updateWithReceivedPlayersByRankingId16")

        (Failure _, Ok lusers) ->
            (Failure "updateWithReceivedPlayersByRankingId17")

        (Failure _, Err _) ->
            (Failure "updateWithReceivedPlayersByRankingId18")


updateWithReceivedRankings : Model -> Result (GQLHttp.Error (Maybe (List (Maybe SR.Types.FRanking)))) (Maybe (List (Maybe SR.Types.FRanking))) -> Model
updateWithReceivedRankings model response =
     case (model, response) of -- AllEmpty, so fill the Ranking set
        (AppOps walletState AllEmpty appInfo uiState subState accountState txRec, Ok lrankings)  ->
                    let
                        filteredFRankingList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lrankings)
                        -- need to convert from FRanking to Ranking (id_ needs to be a String)
                        lFromFToRanking = List.map SR.Types.newRanking filteredFRankingList
                        --_ = Debug.log "lFromFToRanking : " lFromFToRanking
                        sRankings = Data.Rankings.asRankings (EverySet.fromList lFromFToRanking)
                        newDataState = StateFetched Data.Users.empty sRankings (Global Data.Global.empty)
                        
                    in
                        AppOps walletState newDataState appInfo uiState subState accountState txRec

        (AppOps walletState (StateFetched sUsers sRankings  (Global _)) appInfo uiState subState accountState txRec, Ok lrankings) ->
                if Data.Users.isEmpty sUsers then -- just fill the Ranking set
                    let
                        filteredFRankingList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lrankings)
                        lFromFToRanking = List.map SR.Types.newRanking filteredFRankingList
                        newsRankings = Data.Rankings.asRankings (EverySet.fromList lFromFToRanking)
                        newDataState = StateFetched sUsers  sRankings (Global Data.Global.empty)
                    in
                        AppOps walletState newDataState appInfo uiState subState accountState txRec

                else --if sUsers isn't empty we can populate Global now
                    let
                        filteredFRankingList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lrankings)
                        lFromFToRanking = List.map SR.Types.newRanking filteredFRankingList
                        newsRankings = Data.Rankings.asRankings (EverySet.fromList lFromFToRanking)
                        newDataKind = Global (Data.Global.created newsRankings sUsers)
                        newDataState = StateFetched sUsers newsRankings newDataKind
                    in
                        AppOps walletState newDataState appInfo uiState subState accountState txRec

        (AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState txRec, Ok lrankings) ->
            model

        ( AppOps _ (StateFetched _ _ (Selected _)) _ _ _ _ _, Ok _ ) ->
            (Failure "updateWithReceivedRankings13")

        (AppOps walletState AllEmpty appInfo uiState subState accountState txRec, Err _ )  ->
            (Failure "Unable to obtain Rankings data. Please check your network connection ...")

        (AppOps walletState (StateFetched sUsers sRankings dKind) appInfo uiState subState accountState txRec, Err _)  ->
            (Failure "updateWithReceivedRankings15")

        (AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState txRec, Err _ ) ->
            (Failure "updateWithReceivedRankings16")

        (Failure _, Ok lusers) ->
            (Failure "updateWithReceivedRankings17")

        (Failure _, Err _) ->
            (Failure "updateWithReceivedRankings18")

 
updateWithReceivedRankingById : Model -> Result (GQLHttp.Error (Maybe SR.Types.FRanking)) (Maybe SR.Types.FRanking) -> Model
updateWithReceivedRankingById model response =
     case (model, response) of -- AllEmpty, so fill the Ranking set
        (AppOps walletState AllEmpty appInfo uiState subState accountState txRec, Ok _)  ->
            Failure "Err"

        (AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState txRec, Ok lrankings) ->
            model

        (AppOps walletState (StateFetched sUsers sRankings (Global sGlobal)) appInfo uiState subState accountState txRec, Ok franking) ->
            let
                --filteredFRanking = Maybe.withDefault (SR.Types.Ranking 0 True "" Nothing "") franking
                -- need to convert from FRanking to Ranking (id_ needs to be a String)
                --user = Maybe.withDefault (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing) appInfo.m_user

                --ethaddr = Maybe.withDefault "" (Just (Eth.Utils.addressToString user.m_ethaddress))
                --fromFToRanking = SR.Types.newRanking filteredFRanking
                -- below just getting to compile
                fromFToRanking = (SR.Types.Ranking "" True "" Nothing "")
                -- --_ = Debug.log "lFromFToRanking : " lFromFToRanking
                newAppInfo = {appInfo | selectedRanking = fromFToRanking}
                
                --change dataKind to Selected (Rankings below is just to get it to compile)
                --newDataKind = Selected ((Data.Selected.gotUserAsPlayer Data.Selected.empty ethaddr) sUsers fromFToRanking.id_)
                
                --todo: think we're going to be adding a player set here
                newDataState = StateFetched sUsers sRankings (Selected Data.Selected.empty)
            in
                AppOps walletState newDataState newAppInfo uiState subState accountState txRec

        ( AppOps _ (StateFetched _ _ (Selected _)) _ _ _ _ _, Ok _ ) ->
            (Failure "updateWithReceivedUsers1")

        (AppOps walletState AllEmpty appInfo uiState subState accountState txRec, Err _ )  ->
            (Failure "updateWithReceivedUsers2")

        (AppOps walletState (StateFetched sUsers sRankings dKind) appInfo uiState subState accountState txRec, Err _)  ->
            (Failure "updateWithReceivedUsers3")

        (AppOps walletState (StateUpdated sUsers sRankings dKind) appInfo uiState subState accountState txRec, Err _ ) ->
            (Failure "updateWithReceivedUsers4")

        (Failure _, Ok lusers) ->
            (Failure "updateWithReceivedUsers5")

        (Failure _, Err _) ->
            (Failure "updateWithReceivedUsers6")

updateFromLoggedInUser: Model -> Result (GQLHttp.Error SR.Types.Token) SR.Types.Token -> Model
updateFromLoggedInUser model response =
    case response of
        Ok token ->
            case model of
                AppOps walletState dataState appInfo uiState subState accountState  txRec ->
                    case appInfo.m_user of
                        Just user ->
                            let
                                updated_user =
                                    { user | m_token = Just token }

                                newAppInfo = { appInfo | m_user = Just updated_user }
                            in
                            AppOps walletState dataState newAppInfo uiState subState accountState txRec

                        Nothing ->
                            model
                
                Failure _ ->
                    model
        Err _ ->
            model

updateAppInfoOnRankingSelected : SR.Types.AppInfo -> Internal.Types.RankingId -> String -> String -> SR.Types.AppInfo
updateAppInfoOnRankingSelected appInfo rnkid rnkownerstr rnknamestr =
    let
        newSelectedRanking =
            appInfo.selectedRanking

        newRnkInfo =
            { newSelectedRanking | id_ = Utils.MyUtils.stringFromRankingId rnkid, rankingowneraddr = rnkownerstr, rankingname = rnknamestr }

        newAppInfo =
            { appInfo | selectedRanking = newRnkInfo }
    in
    newAppInfo



handleWalletWaitingForUserInput : Msg -> SR.Types.WalletState -> DataState -> SR.Types.AppInfo -> TxRecord -> ( Model, Cmd Msg )
handleWalletWaitingForUserInput msg walletState dataState appInfo txRec =
    let
        _ =
            Debug.log "in handleWalletWaitingForUserInput" msg
    in
    case msg of
        WalletStatus walletSentry_ ->
            let
                _ =
                    Debug.log "ws in WaitingForTransactionReceipt" walletSentry_
            in
            ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.Subscribe SR.Types.Registered txRec
            , Cmd.none
            )

        WatchTxHash (Ok txHash) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput" "watch tx hash"
            in
            ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | txHash = Just txHash }, Cmd.none )

        WatchTxHash (Err err) ->
            ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTx (Ok tx) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput" "tx ok"
            in
      
            (AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered { txRec | tx = Just tx }, Cmd.none )

        WatchTx (Err err) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput tx err" err
            in
            ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTxReceipt (Ok txReceipt) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput tx ok" txReceipt
            in
            AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings SR.Types.StopSubscription SR.Types.Registered { txRec | txReceipt = Just txReceipt } |> update (ProcessResult SR.Types.Won)

        WatchTxReceipt (Err err) ->
            let
                _ =
                    Debug.log "tx err" err
            in
            ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt SR.Types.StopSubscription SR.Types.Registered { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

        _ ->
            let
                _ =
                    Debug.log "wallet state " walletState
            in
            ( Failure "failure"
            , Cmd.none
            )


handleTxSubMsg : Eth.Sentry.Tx.Msg -> Bool
handleTxSubMsg subMsg =
    let
        _ =
            Debug.log "TxSentryMsg" subMsg
    in
    case subMsg of
        Eth.Sentry.Tx.NoOp ->
            False

        Eth.Sentry.Tx.TxSigned int result ->
            case result of
                Err err ->
                    False

                Ok value ->
                    True

        --Eth.Sentry.Tx.TxSent int (Result Http.Error tx) ->
        Eth.Sentry.Tx.TxSent int result ->
            case result of
                Err err ->
                    False

                Ok value ->
                    True

        --Eth.Sentry.Tx.TxMined int (Result Http.Error txReceipt) ->
        Eth.Sentry.Tx.TxMined int result ->
            case result of
                Err err ->
                    False

                Ok value ->
                    True

        --Eth.Sentry.Tx.TrackTx int txTracker (Result Http.Error int) ->
        Eth.Sentry.Tx.TrackTx int txTracker result ->
            case result of
                Err err ->
                    False

                Ok value ->
                    True

        Eth.Sentry.Tx.ErrorDecoding str ->
            False


gotWalletAddrApplyToUser : SR.Types.AppInfo -> Eth.Types.Address ->  SR.Types.AppInfo
gotWalletAddrApplyToUser appInfo uaddr =
    case appInfo.m_user of
        Nothing ->
            appInfo
        Just userVal ->
            let
                newUserWithAddr =
                    { userVal | m_ethaddress =  Just uaddr }
                newAppInfo =
                    { appInfo | m_user = Just newUserWithAddr }
            in
            newAppInfo


handleNewUserInputs : Model -> Msg -> Model
handleNewUserInputs model msg =
    case (model, msg) of
        --(AppOps walletState dataState appInfo uiState subState accountState txRec, NewUserNameInputChg namefield) ->
        (AppOps walletState dataState appInfo uiState subState accountState txRec, NewUserNameInputChg namefield) ->
            let 
                newUser = Maybe.withDefault (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing) appInfo.m_user
            in
            -- case m_user of
            --     Nothing ->
                    let
                        -- create a new empty user
                        --newUser = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
                        newUserWithUpdatedNameField = 
                            { newUser | username = namefield }
                        newAppInfo =
                            { appInfo | m_user = Just newUserWithUpdatedNameField}
                    in
                        AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec

                -- Just userVal ->
                --     let
                --         updatedNewUser =
                --             { userVal | username = namefield }

                --         newAppInfo =
                --             { appInfo | m_user = Just updatedNewUser }
                --     in
                --     AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec

            
            
        (AppOps walletState dataState appInfo uiState subState accountState txRec, NewUserPasswordInputChg passwordfield) ->
            case appInfo.m_user of
                Nothing ->
                    model
                Just userVal ->
                    let
                        newUser = userVal

                        updatedNewUser =
                            { newUser | password = passwordfield }

                        newAppInfo =
                            { appInfo | m_user = Just updatedNewUser }
                    in
                        AppOps walletState dataState newAppInfo uiState SR.Types.StopSubscription accountState txRec
            

        (AppOps walletState dataState appInfo uiState subState accountState txRec,  NewUserDescInputChg descfield) ->
            case appInfo.m_user of
                Nothing ->
                    model
                Just userVal ->
                    let
                        newUser = userVal

                        updatedNewUser =
                            { newUser | description = descfield }

                        newAppInfo =
                            { appInfo | m_user = Just updatedNewUser }
                    in
                        AppOps walletState dataState newAppInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription accountState txRec

        (AppOps walletState dataState appInfo uiState subState accountState txRec, NewUserEmailInputChg emailfield) ->
            case appInfo.m_user of
                Nothing ->
                    model
                Just userVal ->
                    let
                        updatedNewUser =
                            { userVal | email = emailfield }

                        newAppInfo =
                            { appInfo | m_user = Just updatedNewUser }
                    in
                        AppOps walletState dataState newAppInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription accountState txRec
            

        (AppOps walletState dataState appInfo uiState subState accountState txRec, NewUserMobileInputChg mobilefield) ->
            case appInfo.m_user of
                Nothing ->
                    model
                Just userVal ->
                    let
                        newUser = userVal

                        updatedNewUser =
                            { newUser | mobile = mobilefield }

                        newAppInfo =
                            { appInfo | m_user = Just updatedNewUser }
                    in
                        AppOps walletState dataState newAppInfo SR.Types.UIRegisterNewUser SR.Types.StopSubscription accountState txRec            

        (_,_) ->
            Failure "NewUserNameInputChg"


handleExistingUserInputs : Model -> Msg -> Model
handleExistingUserInputs model msg =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            case msg of
                ExistingUserNameInputChg namefield ->
                    model

                -- todo: determine how to handle this:
                ExistingUserPasswordInputChg passwordfield ->
                    model

                ExistingUserDescInputChg descfield ->
                    case appInfo.m_user of
                        Nothing ->
                            model
                        Just userVal ->
                            let
                                newUser = userVal

                                updatedNewUser =
                                    { newUser | description = descfield }

                                newAppInfo =
                                    { appInfo | m_user = Just updatedNewUser }
                            in
                            AppOps walletState dataState newAppInfo SR.Types.UIUpdateExistingUser SR.Types.StopSubscription SR.Types.Registered txRec

                ExistingUserEmailInputChg emailfield ->
                    case appInfo.m_user of
                        Nothing ->
                            model
                        Just userVal ->
                            let
                                newUser = userVal

                                updatedNewUser =
                                    { newUser | email = emailfield }

                                newAppInfo =
                                    { appInfo | m_user = Just updatedNewUser }
                            in
                            AppOps walletState dataState newAppInfo SR.Types.UIUpdateExistingUser SR.Types.StopSubscription SR.Types.Registered txRec

                ExistingUserMobileInputChg mobilefield ->
                    case appInfo.m_user of
                        Nothing ->
                            model
                        Just userVal ->
                            let
                                newUser = userVal

                                updatedNewUser =
                                    { newUser | mobile = mobilefield }

                                newAppInfo =
                                    { appInfo | m_user = Just updatedNewUser }
                            in
                            AppOps walletState dataState newAppInfo SR.Types.UIUpdateExistingUser SR.Types.StopSubscription SR.Types.Registered txRec

                _ ->
                    Failure "ExistingUserNameInputChg"

        _ ->
            Failure "ExistingUserNameInputChg"


updatedForChallenge : Model -> List SR.Types.UserPlayer -> SR.Types.UserPlayer -> SR.Types.User -> Model
updatedForChallenge model luplayer opponentAsPlayer userMaybeCanDelete =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState txRec ->
            let 
                _ = Debug.log "updatedForChallenge 1 - dataState" dataState

            in
            case dataState of
                StateFetched sUsers sRankings dKind -> 
                    case dKind of 
                            Selected sSelected ->
                                case appInfo.m_user of 
                                    Nothing ->
                                        Failure "updateChallenge" 
                                    Just user ->
                                        let
                                            m_uplayer = Data.Selected.gotCurrentUserAsPlayerFromPlayerList luplayer user
                                        in
                                            case m_uplayer of
                                                Nothing -> 
                                                    model
                                                Just uplayer ->
                                                    let
                                                        newAppInfoWithPlayer = { appInfo | player = uplayer }
                                
                                                        newAppInfoWithChallengerAndPlayer = { newAppInfoWithPlayer | challenger = opponentAsPlayer }
                                                    
                                                        newDataKind = Selected (Data.Selected.updateSelectedRankingOnChallenge sSelected newAppInfoWithChallengerAndPlayer)
                                                        newDataState = StateFetched sUsers sRankings newDataKind
                                                    in
                                                        AppOps walletState newDataState newAppInfoWithChallengerAndPlayer SR.Types.UIChallenge SR.Types.StopSubscription SR.Types.Registered txRec
                                    
                            _ -> 
                                let 
                                    _ = Debug.log "updatedForChallenge - dataState" dataState
                                in
                                    model

                _ -> 
                            let 
                                _ = Debug.log "updatedForChallenge - dataState" dataState
                            in
                                model
        _ ->
            Failure <| "updatedForChallenge : "



updateSelectedRankingPlayerList : Model -> List SR.Types.UserPlayer -> Model
updateSelectedRankingPlayerList model luplayers =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            case dataState of
                StateUpdated sUsers sRankings dKind -> 
                    case dKind of 
                        Selected sSelected ->
                            let 
                            --todo: I think this means we lose the update - need to do differently ...
                                newDataKind = Selected (Data.Selected.created (Data.Selected.convertUserPlayersToPlayers luplayers) sUsers
                                    (Data.Selected.gotRankingId sSelected))
                                newDataState = StateUpdated sUsers sRankings newDataKind 
                            in
                                AppOps walletState newDataState appInfo uiState SR.Types.StopSubscription SR.Types.Registered txRec

                        _ -> 
                            let
                                _ = Debug.log "updateSelectedRankingPlayerList - dataState should be Selected (updated)" dataState
                            in
                                model
                _ -> 
                    let 
                        _ = Debug.log "updateSelectedRankingPlayerList - dataState" dataState
                    in
                        model

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


-- populatedSelected : Model -> List SR.Types.UserPlayer -> Model
-- populatedSelected model luplayer =
--     case model of
--         AppOps walletState dataState appInfo uiState subState accountState  txRec ->
--             case dataState of 
--                 Selected sSelected sUsers _ ->
--                     let
--                         newSSelected = Data.Selected.asSelected (EverySet.fromList luplayer ) sUsers (Internal.Types.RankingId appInfo.selectedRanking.id_)

--                         stateToSelected = Selected newSSelected sUsers (Internal.Types.RankingId appInfo.selectedRanking.id_)
                        
--                         newAppPlayer = { appInfo | player = Data.Selected.gotUserPlayerFromPlayerListStrAddress luplayer appInfo.m_user.m_ethaddress }

--                         newAppChallengerAndPlayer = { newAppPlayer | challenger = Data.Selected.gotUserPlayerFromPlayerListStrAddress luplayer newAppPlayer.player.player.challengeraddress }

--                         --_ = Debug.log "in populatedSelected" <| stateToSelected
                    
--                     in
--                         AppOps walletState stateToSelected newAppChallengerAndPlayer uiState SR.Types.StopSubscription SR.Types.Registered emptyTxRecord
--                 _ ->
--                     Failure <| "populatedSelected : "
--         _ ->
--             Failure <| "populatedSelected : "



-- view

view : Model -> Html Msg
view model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState txRec ->
            case (dataState, appInfo.m_user, appInfo.appState) of 
                (AllEmpty, _, _) ->
                    Html.text ("Loading ...")

                (StateFetched sUsers sRankings (Global sGlobal ), Nothing, SR.Types.AppStateGeneral) ->
                    generalLoginView (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing) sUsers sGlobal
                        
                (StateFetched sUsers sRankings (Global sGlobal ), Nothing, SR.Types.AppStateCreateNewUser) ->
                    registerNewUserView (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing) sUsers

                (StateFetched sUsers sRankings dKind, Just userVal, SR.Types.AppStateCreateNewUser) ->
                    registerNewUserView userVal sUsers

                (StateFetched sUsers sRankings (Global sGlobal ), Just userVal, SR.Types.AppStateGeneral) ->
                    gotUserView userVal sUsers sGlobal
                            
                (StateFetched sUsers sRankings (Selected _ ), Just userVal, SR.Types.AppStateGeneral) ->
                     greetingView <| "ToDo: Select w/o a token should be possible"

                (StateFetched sUsers _ _ , Nothing, SR.Types.AppStateGeneral) ->
                    Html.text ("dKind not specified")
                
                (StateFetched sUsers sRankings dKind, Just userVal, SR.Types.AppStateUpdateProfile) ->
                    case userVal.m_token of 
                        Nothing ->
                                    handleGlobalNoTokenView dataState userVal
                                
                        Just tokenVal ->
                                    inputUserDetailsView dataState appInfo
                (StateUpdated _ _ _, _, _) ->
                    Html.text ("No User - No Update")
                
                (_, _, _) ->
                            Html.text ("View fell thru")

        Failure str ->
           failureView str

-- view helpers

generalLoginView : SR.Types.User -> Data.Users.Users -> Data.Global.Global -> Html Msg 
generalLoginView userVal sUsers sGlobal =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el (Heading.h5) <|
                Element.text ("SportRank - Welcome")
            , displayEnableEthereumBtn
            , Element.text ("\n")
            --, Element.el [ Font.color SR.Types.colors.red, Font.alignLeft ] <| Element.text ("\n Please Register Below:")
            , Element.column Grid.section <|
                [ Element.el [] <| Element.text ""
                --Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                , Element.wrappedRow (Card.fill ++ Grid.simple)
                    [ Element.column
                        Grid.simple
                        [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                            { onChange = NewUserNameInputChg
                            , text = userVal.username
                            --, placeholder = Input.placeholder <| [Element.Attribute "Username"]
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username")
                            }
                        --, nameValidView appInfo sUsers
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                            { onChange = NewUserPasswordInputChg
                            , text = userVal.password
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password")
                            }
                        ]
                    ]
                ]
            , infoBtn "Log In" ClickedLogInUser
            , Element.text ("\n")
            , displayRegisterBtnIfNewUser
                (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing).username
                ClickedRegister
            , otherrankingbuttons (Data.Global.asList (Data.Global.gotOthers sGlobal (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing))) (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
            ]

registerNewUserView : SR.Types.User -> Data.Users.Users -> Html Msg 
registerNewUserView userVal sUsers = 
    Framework.responsiveLayout [] <|
        Element.column Grid.section <|
            [ Element.el Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
            , Element.wrappedRow (Card.fill ++ Grid.simple)
                [ Element.column
                    Grid.simple
                    [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                        { onChange = NewUserNameInputChg
                        , text = userVal.username
                        , placeholder = Nothing
                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username*")
                        }
                    , nameValidView userVal sUsers
                    , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                        { onChange = NewUserPasswordInputChg
                        , text = userVal.password
                        , placeholder = Nothing
                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password")
                        }
                    , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userDescription") ])
                        { onChange = NewUserDescInputChg
                        , text = userVal.description
                        , placeholder = Nothing
                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Description")
                        }
                    , userDescValidationErr userVal
                    , Input.email (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userEmail") ])
                        { onChange = NewUserEmailInputChg
                        , text = userVal.email
                        , placeholder = Nothing
                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Email")
                        }
                    , emailValidationErr userVal
                    , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userMobile") ])
                        { onChange = NewUserMobileInputChg
                        , text = Utils.Validation.Validate.validatedMaxTextLength userVal.mobile 25
                        , placeholder = Nothing
                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Mobile")
                        }
                    , mobileValidationErr userVal
                    ]
                ]
            , Element.text "* required and CANNOT be changed \nunder current ETH account"
            , SR.Elements.justParasimpleUserInfoText
            , newuserConfirmPanel (Just userVal) (Data.Users.asList sUsers)
            ]

gotUserView : SR.Types.User -> Data.Users.Users -> Data.Global.Global -> Html Msg 
gotUserView userVal sUsers sGlobal =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container 
                [ Element.el (Heading.h5) <|
                    Element.text ("SportRank - Welcome " ++ userVal.username)
                    , displayEnableEthereumBtn
                    , displayForToken userVal sGlobal
                    -- if the UI following is an issue needing branching
                    -- do it in a separate function like dispalyForToken
                    , infoBtn "Log In" ClickedLogInUser
                    , Element.text ("\n")
                            , displayRegisterBtnIfNewUser
                                (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing).username
                                ClickedRegister
                    , Element.text ("\n")
                    , otherrankingbuttons (Data.Global.asList (Data.Global.gotOthers sGlobal (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing))) (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
                ]

failureView : String -> Html Msg 
failureView str = 
     Framework.responsiveLayout [] <|
        Element.column
            Framework.container 
                [ Element.el (Heading.h5) <|
                    Element.text ("SportRank - Welcome ")
                    , displayEnableEthereumBtn
                    , Element.text ("\n")
                    , Element.el Color.danger <| Element.text str
                    , Element.text ("\n")
                    --, displayForToken userVal sGlobal
                    -- if the UI following is an issue needing branching
                    -- do it in a separate function like dispalyForToken
                    , infoBtn "Log In" ClickedLogInUser
                    , Element.text ("\n")
                            , displayRegisterBtnIfNewUser
                                (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing).username
                                ClickedRegister   
                ]


displayForToken : SR.Types.User -> Data.Global.Global -> Element Msg 
displayForToken userVal sGlobal = 
    case userVal.m_token of 
        Nothing ->
            Element.column Grid.section <|
                [ Element.el [] <| Element.text ""
                --Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                , Element.wrappedRow (Card.fill ++ Grid.simple)
                    [ Element.column
                        Grid.simple
                        [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                            { onChange = NewUserNameInputChg
                            , text = userVal.username
                            --, placeholder = Input.placeholder <| [Element.Attribute "Username"]
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username")
                            }
                        --, nameValidView appInfo sUsers
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                            { onChange = NewUserPasswordInputChg
                            , text = userVal.password
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password")
                            }
                        ]
                    ]
                ]
                                    

        Just tokenVal ->
            Element.column Grid.section <|
                [Element.text ("\n")
                , ownedrankingbuttons (Data.Global.asList (Data.Global.gotOwned sGlobal userVal)) userVal
                , memberrankingbuttons (Data.Global.gotMember sGlobal userVal) userVal
                ]

greetingHeading : String -> Element Msg
greetingHeading greetingStr =
    Element.column Grid.section <|
        [ Element.el (List.append Heading.h5 [ Element.htmlAttribute (Html.Attributes.id "greetingInitStr") ]) <| Element.text "Initializing ..."
        , Element.column Card.fill
            [ Element.el (List.append Heading.h5 [ Element.htmlAttribute (Html.Attributes.id "greetingHeadingStr") ]) <|
                Element.text greetingStr
            ]
        ]


ownedrankingbuttons : List SR.Types.UserRanking -> SR.Types.User -> Element Msg
ownedrankingbuttons urankingList user =
    let
        newRankingList =
            Data.Rankings.extractRankingList urankingList
    in
    if user.username == "" then
        Element.text ""

    else
        Element.column Grid.section <|
            [ if List.isEmpty urankingList then
                -- the button will be in the "Your Created Rankings" section instead
                Element.el [] <| Element.text ""

              else
                Element.el Heading.h5 <| Element.text "Your Created Rankings:"
            , Element.column (Card.simple ++ Grid.simple) <|
                determineOwnedRankingButtonsDisplay newRankingList user
            ]


determineOwnedRankingButtonsDisplay : List SR.Types.Ranking -> SR.Types.User -> List (Element Msg)
determineOwnedRankingButtonsDisplay lranking user =
    if List.isEmpty lranking then
                    [
                        Input.button
                            ([ Element.htmlAttribute (Html.Attributes.id "createnewrankingbtn") ]
                                ++ Button.fill
                                ++ Button.simple
                                ++ Color.info
                            )
                        <|
                            { onPress = Just <| ClickedCreateNewLadder
                            , label = Element.text "Create New Ladder"
                            }
                    ]
    else
        insertOwnedRankingList lranking user


memberrankingbuttons : List SR.Types.UserRanking -> SR.Types.User -> Element Msg
memberrankingbuttons urankingList user =
    let
        newRankingList =
            Data.Rankings.extractRankingList urankingList
    in
    if user.username == "" then
        Element.text ""

    else
        Element.column Grid.section <|
            [ Element.el Heading.h5 <| Element.text "Your Member Rankings: "
            , Element.column (Card.simple ++ Grid.simple) <|
                insertMemberRankingList newRankingList
            ]


otherrankingbuttons : List SR.Types.UserRanking -> SR.Types.User -> Element Msg
otherrankingbuttons urankingList user =
    let
        newRankingList =
            Data.Rankings.extractRankingList urankingList
    in
    if user.username == "" then
        Element.column Grid.section <|
            [ Element.el Heading.h5 <| Element.text "View Rankings: "
            , Element.column (Card.simple ++ Grid.simple) <|
                insertNeitherOwnerNorMemberRankingList newRankingList
            ]

    else
        Element.column Grid.section <|
            [ Element.el Heading.h5 <| Element.text "Other Rankings: "
            , Element.column (Card.simple ++ Grid.simple) <|
                insertNeitherOwnerNorMemberRankingList newRankingList
            ]


insertOwnedRankingList : List SR.Types.Ranking -> SR.Types.User -> List (Element Msg)
insertOwnedRankingList lrankinginfo user =
    let
        mapOutRankingList =
            List.map
                ownedRankingInfoBtn
                lrankinginfo
    in
    if user.username == "" then
                [ Input.button ([ Element.htmlAttribute (Html.Attributes.id "createnewrankingbtn") ]
                    ++ Button.fill
                    ++ Button.simple 
                    ++ Color.info) <|
                    { onPress = Just <| ClickedCreateNewLadder
                    , label = Element.text "Create New Ladder"
                    }
                ]
    else
        mapOutRankingList


ownedRankingInfoBtn : SR.Types.Ranking -> Element Msg
ownedRankingInfoBtn rankingobj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.primary) <|
            { onPress = Just (ClickedSelectedOwnedRanking (Internal.Types.RankingId rankingobj.id_) rankingobj.rankingowneraddr rankingobj.rankingname)
            , label = Element.text rankingobj.rankingname
            }
        ]


insertMemberRankingList : List SR.Types.Ranking -> List (Element Msg)
insertMemberRankingList lrankinginfo =
    let
        mapOutRankingList =
            List.map
                memberRankingInfoBtn
                lrankinginfo
    in
    if List.isEmpty lrankinginfo then
        [ Element.text "Please Click On A \nRanking Below To View or Join:" ]

    else
        mapOutRankingList


memberRankingInfoBtn : SR.Types.Ranking -> Element Msg
memberRankingInfoBtn ranking =
    if ranking.rankingname /= "" then
        Element.column Grid.simple <|
            [ Input.button (Button.fill ++ Color.primary) <|
                { onPress = Just (ClickedSelectedMemberRanking (Internal.Types.RankingId ranking.id_) ranking.rankingowneraddr ranking.rankingname)
                , label = Element.text ranking.rankingname
                }
            ]
    else 
        Element.column Grid.simple <|
            [ Input.button (Button.fill ++ Color.primary) <|
                { onPress = Just (ClickedSelectedMemberRanking (Internal.Types.RankingId ranking.id_) ranking.rankingowneraddr ranking.rankingname)
                , label = Element.el
                            [ Font.color (Element.rgb 1 0 0)
                            , Font.size 18
                            , Font.family
                                [ Font.typeface "Open Sans"
                                , Font.sansSerif
                                ]
                            , Font.center
                            ]
                            (Element.text  "              Deleted")
                }
            ]


insertNeitherOwnerNorMemberRankingList : List SR.Types.Ranking -> List (Element Msg)
insertNeitherOwnerNorMemberRankingList rnkgInfoList =
    let
        mapOutRankingList =
            List.map
                neitherOwnerNorMemberRankingInfoBtn
                rnkgInfoList
    in
    mapOutRankingList


neitherOwnerNorMemberRankingInfoBtn : SR.Types.Ranking -> Element Msg
neitherOwnerNorMemberRankingInfoBtn rankingobj =
    Element.column Grid.simple <|
        [ Input.button ([ Element.htmlAttribute (Html.Attributes.id "otherrankingbtn") ] ++ Button.fill ++ Color.primary) <|
            { onPress = Just (ClickedSelectedNeitherOwnerNorMember (Internal.Types.RankingId rankingobj.id_) rankingobj.rankingowneraddr rankingobj.rankingname)
            , label = Element.text rankingobj.rankingname
            }
        ]



playerbuttons : DataState -> SR.Types.AppInfo -> Element Msg
playerbuttons dataState appInfo =
     case dataState of
        StateFetched sUsers sRankings dKind -> 
            case dKind of 
                    Selected sSelected ->
                        Element.column Grid.section <|
                            [ SR.Elements.selectedRankingHeaderEl appInfo.selectedRanking
                            , Element.column (Card.simple ++ Grid.simple) <|
                                insertPlayerList dataState appInfo
                            ]

                    _ ->
                        Element.text "Error1"
        _ ->
            Element.text "Error1"




configureThenAddPlayerRankingBtns : Data.Selected.Selected -> Data.Users.Users -> SR.Types.AppInfo -> SR.Types.UserPlayer -> Element Msg
configureThenAddPlayerRankingBtns sSelected sUsers appInfo uplayer =
   -- nb. 'uplayer' is the player that's being mapped cf. appInfo.player which is current user as player (single instance)
    let
        _ = Debug.log "configureThenAddPlayerRankingBtns" uplayer
        printChallengerNameOrAvailable = Data.Selected.printChallengerNameOrAvailable sSelected sUsers uplayer
    in
        case appInfo.m_user of
            Nothing ->
                Element.text "No User2"
            Just userVal ->
                if Data.Selected.isUserPlayerMemberOfSelectedRanking (Data.Selected.asList sSelected) userVal then
                    
                    if Data.Selected.isPlayerCurrentUser userVal uplayer then
                        --if isCurrentUserInAChallenge then
                        if Data.Selected.isChallenged sSelected sUsers uplayer then
                            Element.column Grid.simple <|
                                [ Input.button (Button.fill ++ Color.success) <|
                                    { onPress = Just <| ClickedChangedUIStateToEnterResult appInfo.player
                                    , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                                    }
                                ]
                        else
                        -- player is current user, but not in a challenge:
                        let 
                            _ = Debug.log "player is current user, but not in a challenge" "here"
                        in
                            Element.column Grid.simple <|
                                [ Input.button (Button.fill ++ Color.info) <|
                                    { onPress = Nothing
                                    , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                                    }
                                ]
                        -- else if - this uplayer isn't the current user but the current user is in a challenge so disable any other players

                    --else if isCurrentUserInAChallenge then
                    else if Data.Selected.isChallenged sSelected sUsers uplayer then
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.disabled) <|
                                { onPress = Nothing
                                , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                                }
                            ]
                        -- else if - this uplayer isn't the current user but is being challenged

                    else if Data.Selected.isChallenged sSelected sUsers uplayer then
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.disabled) <|
                                { onPress = Nothing
                                , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                                }
                            ]
                    else
                    -- this uplayer isn't the current user and isn't challenged by anyone
                        if not (Data.Selected.isChallenged sSelected sUsers uplayer) then
                            Element.column Grid.simple <|
                                [ Input.button (Button.fill ++ Color.light) <|
                                    { onPress = Just <| ClickedChallengeOpponent uplayer
                                    , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                                    }
                                ]
                        else 
                                Element.column Grid.simple <|
                                [ Input.button (Button.fill ++ Color.disabled) <|
                                    { onPress = Nothing
                                    , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                                    }
                                ]
                else
                    -- the user isn't a member of this ranking so disable everything
                    Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.disabled) <|
                            { onPress = Nothing
                            , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                            }
                        ]
    -- _ ->
    --     Element.text "Config Btns Failed"

insertPlayerList : DataState -> SR.Types.AppInfo -> List (Element Msg)
insertPlayerList dataState appInfo =
    case dataState of
                    StateFetched sUsers sRankings dKind -> 
                        case dKind of 
                                Selected sSelected ->
                                    let
                                       
                                        mapOutPlayerList =
                                            List.map
                                                (configureThenAddPlayerRankingBtns sSelected sUsers appInfo)
                                                (Data.Selected.asList sSelected)

                                    in
                                    mapOutPlayerList

                                _ ->
                                    [ Element.text "error2" ]
                    _ ->
                        [ Element.text "error2" ]


selecteduserIsOwnerhomebutton : SR.Types.User -> Element Msg
selecteduserIsOwnerhomebutton user =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| Cancel
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.danger) <|
                    { onPress = Just <| ClickedDeleteRanking user.m_ethaddress
                    , label = Element.text "Delete"
                    }
                ]
            ]

        --, SR.Elements.simpleUserInfoText
        ]


selecteduserIsPlayerHomebutton : SR.Types.User -> Element Msg
selecteduserIsPlayerHomebutton user =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| Cancel
                    , label = Element.text "Home"
                    }
                ]
            ]
        ]


selecteduserIsNeitherPlayerNorOwnerHomebutton : SR.Types.User -> SR.Types.AccountState -> Element Msg
selecteduserIsNeitherPlayerNorOwnerHomebutton user accountState =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| Cancel
                    , label = Element.text "Home"
                    }
                , displayJoinBtnNewOrExistingUser user accountState
                ]
            ]
        ]


displayJoinBtnNewOrExistingUser : SR.Types.User -> SR.Types.AccountState -> Element Msg
displayJoinBtnNewOrExistingUser user accountState =
    case accountState of 
        SR.Types.Guest -> 
            Input.button ([ Element.htmlAttribute (Html.Attributes.id "existingUserJoinbtn") ] ++ Button.simple ++ Color.info) <|
                { onPress = Just ClickedRegister
                , label = Element.text "Join"
                }

        SR.Types.Registered ->
            Input.button ([ Element.htmlAttribute (Html.Attributes.id "newUserJoinbtn") ] ++ Button.simple ++ Color.disabled) <|
                { onPress = Nothing
                , label = Element.text "Join"
                }

        SR.Types.EthEnabled ->
            Input.button ([ Element.htmlAttribute (Html.Attributes.id "newUserJoinbtn") ] ++ Button.simple ++ Color.info) <|
                { onPress = Just ClickedRegister
                , label = Element.text "Join"
                }

        SR.Types.EthEnabledAndRegistered -> 
            Input.button ([ Element.htmlAttribute (Html.Attributes.id "newUserJoinbtn") ] ++ Button.simple ++ Color.info) <|
                { onPress = Just ClickedJoinSelected
                , label = Element.text "Join"
                }

    
       


newrankingconfirmbutton : SR.Types.AppInfo -> DataState -> Element Msg
newrankingconfirmbutton appInfo dataState =
    case dataState of 
            StateFetched sUsers sRankings dKind ->
                 case dKind of 
                    Global sGlobal  ->
                        Element.column Grid.section <|
                            [ Element.el Heading.h6 <| Element.text "Click to continue ..."
                            , Element.column (Card.simple ++ Grid.simple) <|
                                [ Element.wrappedRow Grid.simple <|
                                    [ Input.button (Button.simple ++ Color.simple) <|
                                        { onPress = Just <| Cancel
                                        , label = Element.text "Cancel"
                                        }
                                    , Input.button (Button.simple ++ enableButton (isValidatedForAllLadderDetailsInput appInfo.selectedRanking (Data.Global.asList sGlobal))) <|
                                        
                                        { onPress = Just <| ClickedConfirmCreateNewLadder
                                        , label = Element.text "Confirm"
                                        }
                                    ]
                                ]
                            , SR.Elements.warningParagraph
                            ]
                    _ -> 
                        let 
                            _ = Debug.log "newrankingconfirmbutton - dataState should be global" dataState
                        in
                            Element.text ""
                    
            _ -> 
                let 
                    _ = Debug.log "newrankingconfirmbutton - dataState" dataState
                in
                    Element.text ""


confirmDelRankingBtn : SR.Types.AppInfo -> DataState -> Element Msg
confirmDelRankingBtn appInfo dataState =
    
    case dataState of 
            StateUpdated sUsers sRankings dKind ->
                 case dKind of 
                    Global sGlobal  ->
                        let 
                            m_userRanking = Data.Global.gotUserRankingByRankingId sGlobal appInfo.selectedRanking.id_
                        in
                            case m_userRanking of
                                Nothing ->
                                    Element.text "Cannot find the ranking"
                                Just userRanking ->
                                    Element.column Grid.section <|
                                        [ 
                                        Element.el Heading.h5 <| Element.text userRanking.rankingInfo.rankingname
                                        , Element.el Heading.h6 <| Element.text "Click to continue ..."
                                        , Element.column (Card.simple ++ Grid.simple) <|
                                            [ Element.wrappedRow Grid.simple <|
                                                [ Input.button (Button.simple ++ Color.simple) <|
                                                    { onPress = Just <| Cancel
                                                    , label = Element.text "Cancel"
                                                    }
                                                    , Input.button Button.simple <|
                                                    { onPress = Just <| ClickedDeleteRankingConfirmed
                                                    , label = Element.text "Confirm"
                                                    }
                                                ]
                                            ]
                                        , SR.Elements.permanentlyDeleteWarnPara
                                        ]
                    _ -> 
                        let 
                            _ = Debug.log "newrankingconfirmbutton - dataState should be global" dataState
                        in
                            Element.text ""
                    
            _ -> 
                let 
                    _ = Debug.log "newrankingconfirmbutton - dataState" dataState
                in
                    Element.text ""

-- continueAfterDelRankingBtn : SR.Types.AppInfo -> DataState -> Element Msg
-- continueAfterDelRankingBtn appInfo dataState =
--     case dataState of 
--             StateUpdated sUsers sRankings dKind ->
--                  case dKind of 
--                     Global sGlobal  ->
--                         Element.column Grid.section <|
--                             [ Element.el Heading.h6 <| Element.text "Click to continue ..."
--                             , Element.column (Card.simple ++ Grid.simple) <|
--                                 [ Element.wrappedRow Grid.simple <|
--                                     [ Input.button (Button.simple ++ Color.simple) <|
--                                         { onPress = Just <| Cancel
--                                         , label = Element.text "Continue"
--                                         }
--                                     -- , Input.button (Button.simple ++ enableButton (isValidatedForAllLadderDetailsInput appInfo.selectedRanking (Data.Global.asList sGlobal))) <|
                                        
--                                     --     { onPress = Just <| ClickedConfirmCreateNewLadder
--                                     --     , label = Element.text "Confirm"
--                                     --     }
--                                     ]
--                                 ]
--                             , SR.Elements.warningParagraph
--                             ]
--                     _ -> 
--                         let 
--                             _ = Debug.log "newrankingconfirmbutton - dataState should be global" dataState
--                         in
--                             Element.text ""
                    
--             _ -> 
--                 let 
--                     _ = Debug.log "newrankingconfirmbutton - dataState" dataState
--                 in
--                     Element.text ""


confirmChallengebutton : Model -> Element Msg
confirmChallengebutton model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            case appInfo.m_user of
                Nothing ->
                    Element.text <| " No User3"
                Just userVal ->
                    Element.column Grid.section <|
                        [ Element.el Heading.h6 <| Element.text <| " Your opponent's details: "
                        , Element.paragraph (Card.fill ++ Color.info) <|
                            [ Element.el [] <| Element.text <| userVal.username ++ " you are challenging " ++ appInfo.challenger.user.username
                            ]
                        , Element.el [] <| Element.text <| "Email: "
                        , Element.paragraph (Card.fill ++ Color.info) <|
                            [ Element.el [] <| Element.text <| appInfo.challenger.user.email
                            ]
                        , Element.el [] <| Element.text <| "Mobile: "
                        , Element.paragraph (Card.fill ++ Color.info) <|
                            [ Element.el [] <| Element.text <| appInfo.challenger.user.mobile
                            ]
                        , Element.column (Card.simple ++ Grid.simple) <|
                            [ Element.wrappedRow Grid.simple <|
                                [ Input.button (Button.simple ++ Color.simple) <|
                                    { onPress = Just <| ResetToShowSelected
                                    , label = Element.text "Cancel"
                                    }
                                , Input.button (Button.simple ++ Color.info) <|
                                    { onPress = Just <| ClickedNewChallengeConfirm
                                    , label = Element.text "Confirm"
                                    }
                                ]
                            ]
                        ]

        _ ->
            Element.text "Fail confirmChallengebutton"


confirmResultbutton : Model -> Element Msg
confirmResultbutton model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            case dataState of
                StateFetched sUsers sRankings dKind -> 
                    case dKind of 
                        Selected sSelected ->
                            let
                                m_playerAsUser = Data.Users.gotUser sUsers appInfo.player.player.address
                            in
                            case m_playerAsUser of
                                Nothing ->
                                    Element.text "No player"
                                Just playerAsUser ->
                                    let
                                        m_challengerAsUser = Data.Users.gotUser sUsers appInfo.challenger.player.address
                                    in
                                    case m_challengerAsUser of
                                        Nothing ->
                                            Element.text "No challenger"
                                        Just challengerAsUser ->
                                            Element.column Grid.section <|
                                                [ Element.column (Card.simple ++ Grid.simple) <|
                                                    [ Element.wrappedRow Grid.simple <|
                                                        [ Input.button (Button.simple ++ Color.simple) <|
                                                            { onPress = Just <| ResetToShowSelected
                                                            , label = Element.text "Cancel"
                                                            }
                                                        ]
                                                    ]
                                                , Element.paragraph (Card.fill ++ Color.info) <|
                                                    [ Element.el [] <| Element.text <| playerAsUser.username ++ " you had a challenge match vs " ++ challengerAsUser.username
                                                    ]
                                                , Element.el Heading.h6 <| Element.text <| "Please confirm your result: "
                                                , Element.column (Card.simple ++ Grid.simple) <|
                                                    [ Element.column Grid.simple <|
                                                        [ Input.button (Button.simple  ++ Button.fill ++ Color.primary) <|
                                                            { 
                                                            onPress = Just <| SentResultToWallet SR.Types.Won
                                                            , label = Element.text "Won"
                                                            }
                                                        , Input.button (Button.simple  ++ Button.fill ++ Color.primary) <|
                                                            { onPress = Just <| SentResultToWallet SR.Types.Lost
                                                            , label = Element.text "Lost"
                                                            }
                                                        , Input.button (Button.simple  ++ Button.fill ++ Color.primary) <|
                                                            { onPress = Just <| ProcessResult SR.Types.Undecided
                                                            , label = Element.text "Undecided"
                                                            }
                                                        ]
                                                    ]
                                                , SR.Elements.ethereumWalletWarning
                                                , SR.Elements.footer
                                                ]
                        _ ->
                            Element.text "Fail confirmResultbutton"
                _ ->
                    Element.text "Fail confirmResultbutton"
        _ ->
            Element.text "Fail confirmResultbutton"


acknoweldgeTxErrorbtn : Model -> Element Msg
acknoweldgeTxErrorbtn model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            Element.column Grid.section <|
                [ 
                Element.paragraph (Card.fill ++ Color.info) <|
                    [ Element.el [] <| Element.text """ There was an error 
                                                        processing your transaction. 
                                                        It is unlikely to be 
                                                        an issue with this 
                                                        application 
                                                        but rather your 
                                                        wallet setup. Your results are unaffected and
                                                        there will have been no charge against your wallet """
                    ]
                , Element.el Heading.h6 <| Element.text <| "Please click below to continue ... "
                , Element.column (Card.simple ++ Grid.simple) <|
                    [ Element.column Grid.simple <|
                        [ Input.button (Button.simple ++ Color.primary) <|
                            { onPress = Just <| ResetToShowSelected
                            , label = Element.text "Continue ..."
                            }
                        ]
                    ]
                , SR.Elements.footer
                ]

        _ ->
            Element.text "Fail acknoweldgeTxErrorbtn"


userDescValidationErr : SR.Types.User -> Element Msg
userDescValidationErr user =
    if isUserDescValidated user then
        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "descValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "")

    else
        Element.el
            (List.append [ Element.htmlAttribute (Html.Attributes.id "descValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text "20 characters max")


ladderDescValidationErr : SR.Types.Ranking -> Element Msg
ladderDescValidationErr rankingInfo =
    if isLadderDescValidated rankingInfo then
        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "ladderdescValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "")

    else
        Element.el
            (List.append [ Element.htmlAttribute (Html.Attributes.id "ladderdescValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text "20 characters max")


isUserDescValidated : SR.Types.User -> Bool
isUserDescValidated user =
    if String.length user.description <= 20 then
        True

    else
        False


isLadderDescValidated : SR.Types.Ranking -> Bool
isLadderDescValidated rankingInfo =
    case rankingInfo.rankingdesc of
        Nothing ->
            True 
        Just rnkDesc ->
            if String.length rnkDesc <= 20 then
                True

            else
                False


isEmailValidated : SR.Types.User -> Bool
isEmailValidated user =
    if String.length user.email == 0 then
        True

    else if Validate.isValidEmail user.email then
        True

    else
        False


emailValidationErr : SR.Types.User -> Element Msg
emailValidationErr user =
    if isEmailValidated user then
        Element.el
            (List.append
                [ Font.color SR.Types.colors.green, Font.alignLeft ]
                [ Element.moveLeft 1.0 ]
            )
            (Element.text "Email OK!")

    else if String.length user.email > 0 then
        Element.el (List.append [ Font.color SR.Types.colors.red, Font.alignLeft ] [ Element.moveLeft 7.0 ])
            (Element.text """ Email, if
 entered, must be valid""")

    else
        Element.el [] <| Element.text ""


mobileValidationErr : SR.Types.User -> Element Msg
mobileValidationErr user =
    if isMobileValidated user then
        Element.el (List.append [ Font.color SR.Types.colors.green, Font.alignLeft ] [ Element.htmlAttribute (Html.Attributes.id "userMobileValid") ]) (Element.text "Mobile OK!")

    else if String.length user.mobile > 0 then
        Element.el (List.append [ Font.color SR.Types.colors.red, Font.alignLeft ] [ Element.htmlAttribute (Html.Attributes.id "userMobileInvalid") ] ++ [ Element.moveLeft 5.0 ])
            (Element.text """ Mobile number, if
 entered, must be valid""")

    else
        Element.el [] <| Element.text ""


newuserConfirmPanel : Maybe SR.Types.User -> List SR.Types.User -> Element Msg
newuserConfirmPanel  m_user luser =
        case m_user of
            Nothing ->
                if List.isEmpty luser then
                    Element.column Grid.section <|
                    [ SR.Elements.missingDataPara
                    , Element.el Heading.h6 <| Element.text "Click to continue ..."
                    , Element.column (Card.simple ++ Grid.simple) <|
                        [ Element.wrappedRow Grid.simple <|
                            [ Input.button (Button.simple ++ Color.info) <|
                                { onPress = Just <| Cancel
                                , label = Element.text "Cancel"
                                }
                            ]
                        ]
                    ]
                else
                    Element.column Grid.section <|
                        [ 
                        Element.el Heading.h6 <| Element.text "Click to continue ..."
                        , Element.column (Card.simple ++ Grid.simple) <|
                            [ Element.wrappedRow Grid.simple <|
                                [ Input.button (Button.simple ++ Color.info) <|
                                    { onPress = Just <| Cancel
                                    , label = Element.text "Cancel"
                                    }
                                ]
                            ]
                        ]

            Just userVal ->
                if List.isEmpty luser then
                    Element.column Grid.section <|
                        [ SR.Elements.missingDataPara
                        , Element.el Heading.h6 <| Element.text "Click to continue ..."
                        , Element.column (Card.simple ++ Grid.simple) <|
                            [ Element.wrappedRow Grid.simple <|
                                [ Input.button (Button.simple ++ Color.info) <|
                                    { onPress = Just <| Cancel
                                    , label = Element.text "Cancel"
                                    }
                                ]
                            ]
                        ]
                else
                    Element.column Grid.section <|
                    [ SR.Elements.warningParagraph
                    , Element.el Heading.h6 <| Element.text "Click to continue ..."
                    , Element.column (Card.simple ++ Grid.simple) <|
                        [ Element.wrappedRow Grid.simple <|
                            [ Input.button (Button.simple ++ Color.info) <|
                                { onPress = Just <| Cancel
                                , label = Element.text "Cancel"
                                }
                            , Input.button (Button.simple ++ enableButton (isValidatedForAllUserDetailsInput userVal luser False)) <|
                                { onPress = Just <| ClickedConfirmedRegisterNewUser
                                , label = Element.text "Register"
                                }
                            ]
                        ]
                    ]




existingUserConfirmPanel : SR.Types.User -> List SR.Types.User -> Element Msg
existingUserConfirmPanel user luser =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.info) <|
                    { onPress = Just <| Cancel
                    , label = Element.text "Cancel"
                    }
                , Input.button (Button.simple ++ enableButton (isValidatedForAllUserDetailsInput user luser True)) <|
                    { onPress = Just <| ClickedConfirmedUpdateExistingUser
                    , label = Element.text "Confirm"
                    }
                ]
            ]
        ]


isValidatedForAllUserDetailsInput : SR.Types.User -> List SR.Types.User -> Bool -> Bool
isValidatedForAllUserDetailsInput user luser isExistingUser =
    if
        isExistingUser
            && isUserDescValidated user
            && isEmailValidated user
            && isMobileValidated user
    then
        True

    else if
        Data.Users.isUserNameValidated user luser
            && isUserDescValidated user
            && isEmailValidated user
            && isMobileValidated user
    then
        True

    else
        False



isValidatedForAllLadderDetailsInput : SR.Types.Ranking -> List SR.Types.UserRanking -> Bool
isValidatedForAllLadderDetailsInput rnkInfo luranking =
    if
        Data.Rankings.isRankingNameValidated rnkInfo luranking
            && isLadderDescValidated rnkInfo
    then
        True

    else
        False


enableButton : Bool -> List (Element.Attribute msg)
enableButton enable =
    if enable then
        Color.info
    else
        Color.disabled


inputUpdateExistingUser : Model -> Element Msg
inputUpdateExistingUser model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            case appInfo.m_user of
                Nothing ->
                    Element.text "No User9"
                Just userVal ->
                    Element.column Grid.section <|
                        [ Element.el Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                        , Element.wrappedRow (Card.fill ++ Grid.simple)
                            [ Element.column
                                Grid.simple
                                [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ Color.disabled)
                                    { onChange = ExistingUserNameInputChg
                                    , text = userVal.username
                                    , placeholder = Just <| Input.placeholder [] <| Element.text "yah placeholder"
                                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username")
                                    }
                                , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userPassword") ])
                                    { onChange = ExistingUserPasswordInputChg
                                    , text = userVal.password
                                    , placeholder = Nothing
                                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password")
                                    }
                                , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userDescription") ])
                                    { onChange = ExistingUserDescInputChg
                                    , text = userVal.description
                                    , placeholder = Nothing
                                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Description")
                                    }
                                , userDescValidationErr userVal
                                , Input.email (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userEmail") ])
                                    { onChange = ExistingUserEmailInputChg
                                    , text = userVal.email
                                    , placeholder = Nothing
                                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Email")
                                    }
                                , emailValidationErr userVal
                                , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userMobile") ])
                                    { onChange = ExistingUserMobileInputChg
                                    , text = Utils.Validation.Validate.validatedMaxTextLength userVal.mobile 25
                                    , placeholder = Nothing
                                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Mobile")
                                    }
                                , mobileValidationErr userVal
                                ]
                            ]

                        --, Element.text "* required"
                        ]

        _ ->
            Element.text "Fail on inputNewUser"


nameValidView : SR.Types.User -> Data.Users.Users -> Element Msg
nameValidView userVal sUsers =

        if userVal.username == "" then
            Element.el
                    (List.append [ Element.htmlAttribute (Html.Attributes.id "usernameValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                        ++ [ Element.moveLeft 0.0 ]
                    )
                    (Element.text """Must be unique (4-8 continuous chars)""")
        else
            if Data.Users.isNameValid userVal.username sUsers then 
                Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "usernameValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "Username OK!")

            else
                Element.el
                    (List.append [ Element.htmlAttribute (Html.Attributes.id "usernameValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                        ++ [ Element.moveLeft 0.0 ]
                    )
                    (Element.text """Must be unique (4-8 continuous chars)""")


ladderNameValidationErr : SR.Types.AppInfo -> DataState -> Element Msg
ladderNameValidationErr appInfo dataState =
    case dataState of 
        StateFetched sUsers sRankings dKind ->
            case dKind of 
                Global sGlobal  ->
                    if Data.Rankings.isRankingNameValidated appInfo.selectedRanking (Data.Global.asList sGlobal) then
                        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "laddernameValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "Ladder name OK!")

                    else
                        Element.el
                            (List.append [ Element.htmlAttribute (Html.Attributes.id "laddernameValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                                ++ [ Element.moveLeft 0.0 ]
                            )
                            (Element.text """Must be unique (4-8 continuous chars)""")

                _ -> 
                    let 
                        _ = Debug.log "dataState - should be Global" dataState
                    in
                        (Element.text "")

        _ -> 
                let 
                    _ = Debug.log "dataState - ladderNameValidationErr" dataState
                in
                    (Element.text "") 



isMobileValidated : SR.Types.User -> Bool
isMobileValidated user =
    let
        mobileNumberInt =
            Maybe.withDefault 0 (String.toInt user.mobile)
    in
    if String.length user.mobile == 0 then
        True

    else if
        (String.length user.mobile > 4 && String.length user.mobile < 25)
            && (Just mobileNumberInt /= Just 0)
    then
        True

    else
        False
        


inputNewLadder : SR.Types.AppInfo -> DataState -> Element Msg
inputNewLadder appInfo dataState =
            Element.column Grid.section <|
                [ Element.el Heading.h6 <| Element.text "New Ladder Details"
                , Element.wrappedRow (Card.fill ++ Grid.simple)
                    [ Element.column Grid.simple
                        [ Input.text Input.simple
                            { onChange = LadderNameInputChg
                            , text = appInfo.selectedRanking.rankingname
                            , placeholder = Nothing
                            , label = Input.labelLeft Input.label <| Element.text "Name*:"
                            }
                        , ladderNameValidationErr appInfo dataState
                            , Input.multiline Input.simple 
                                {onChange = LadderDescInputChg
                                , text =  Utils.Validation.Validate.validatedMaxTextLength (Maybe.withDefault "" appInfo.selectedRanking.rankingdesc) 20
                                , placeholder = Nothing
                                , label = Input.labelLeft Input.label <| Element.text "Desc:"
                                , spellcheck = False
                                }
                        , Element.text "* Required"
                        , ladderDescValidationErr appInfo.selectedRanking
                        ]
                    ]
                ]


handleNoUserView : DataState -> Html Msg
handleNoUserView dataState =   
    let 
        userVal = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
    in
        case dataState of
            AllEmpty -> 
                Html.text ("Loading ...")
            StateUpdated _ _ _ ->
                Html.text ("No User - No Update")
            StateFetched sUsers sRankings dKind ->
                case dKind of
                    Selected sSelected ->
                        Html.text ("Nothing should have been selected yet")
                    Global sGlobal  ->
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ Element.el (Heading.h5) <|
                                    Element.text ("SportRank - Welcome")
                                , displayEnableEthereumBtn
                                , Element.text ("\n")
                                --, Element.el [ Font.color SR.Types.colors.red, Font.alignLeft ] <| Element.text ("\n Please Register Below:")
                                , Element.column Grid.section <|
                                    [ Element.el [] <| Element.text ""
                                    --Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                                    , Element.wrappedRow (Card.fill ++ Grid.simple)
                                        [ Element.column
                                            Grid.simple
                                            [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                                                { onChange = NewUserNameInputChg
                                                , text = userVal.username
                                                --, placeholder = Input.placeholder <| [Element.Attribute "Username"]
                                                , placeholder = Nothing
                                                , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username")
                                                }
                                            --, nameValidView appInfo sUsers
                                            , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                                                { onChange = NewUserPasswordInputChg
                                                , text = userVal.password
                                                , placeholder = Nothing
                                                , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password")
                                                }
                                            ]
                                        ]
                                    ]
                                , infoBtn "Log In" ClickedLogInUser
                                , Element.text ("\n")
                                , displayRegisterBtnIfNewUser
                                    ""
                                    ClickedRegister
                                , otherrankingbuttons (Data.Global.asList (Data.Global.gotOthers sGlobal (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing))) (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
                                ]


handleGlobalNoTokenView : DataState -> SR.Types.User -> Html Msg
handleGlobalNoTokenView dataState userVal =
    case dataState of
        AllEmpty -> 
            Html.text ("No Data")
        StateUpdated _ _ _ ->
            Html.text ("No User - No Update")
        StateFetched sUsers sRankings dKind ->
            case dKind of
                Selected _ ->
                    Html.text ("Nothing should have been selected yet")
                Global sGlobal -> 
                    Framework.responsiveLayout
                    []
                    <|
                    Element.column
                        Framework.container
                        [ Element.el (Heading.h5) <|
                            Element.text ("SportRank - Welcome")
                        , displayEnableEthereumBtn
                        , Element.column Grid.section <|
                            [ Element.el [] <| Element.text ""
                            --Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                            , Element.wrappedRow (Card.fill ++ Grid.simple)
                                [ Element.column
                                    Grid.simple
                                    [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                                        { onChange = NewUserNameInputChg
                                        , text = userVal.username
                                        --, placeholder = Input.placeholder <| [Element.Attribute "Username"]
                                        , placeholder = Nothing
                                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username")
                                        }
                                    --, nameValidView appInfo sUsers
                                    , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                                        { onChange = NewUserPasswordInputChg
                                        , text = userVal.password
                                        , placeholder = Nothing
                                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password")
                                        }
                                    ]
                                ]
                            ]
                        , infoBtn "Log In" ClickedLogInUser
                        , Element.text ("\n")
                                , displayRegisterBtnIfNewUser
                                    (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing).username
                                    ClickedRegister
                        , Element.text ("\n")
                        , otherrankingbuttons (Data.Global.asList (Data.Global.gotOthers sGlobal (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing))) (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
                        ]
                


displayUpdateProfileBtnIfExistingUser : String -> Element Msg
displayUpdateProfileBtnIfExistingUser uname  =
    
    if uname == "" then
        Element.text ""

    else
     Element.column (Card.simple ++ Grid.simple) <|
     [
        Input.button
            ([ Element.htmlAttribute (Html.Attributes.id "updateProfilebtn") ]
                ++ Button.fill
                ++ Button.simple
                ++ Color.info
            )
        <|
            { 
                --onPress = Just <| msg
            onPress = Just <| ClickedUpdateExistingUser
            , label = Element.text "Update Profile"
            }
     ]


displayCreateNewLadderBtnIfExistingUser : String -> List SR.Types.UserRanking -> Msg -> Element Msg
displayCreateNewLadderBtnIfExistingUser uname luserRanking msg =
    if uname == "" || List.isEmpty luserRanking then
        Element.text ""
    else
        Element.column (Card.simple ++ Grid.simple) <|
        [
            Input.button
                ([ Element.htmlAttribute (Html.Attributes.id "createnewrankingbtn") ]
                    ++ Button.fill
                    ++ Button.simple
                    ++ Color.info
                )
            <|
                { onPress = Just <| msg
                , label = Element.text "Create New Ladder"
                }
        ]


infoBtn : String -> Msg -> Element Msg
infoBtn label msg =
        Input.button
            (Button.simple ++ Button.fill ++ Color.info)
        <|
            { onPress = Just <| msg
            , label = Element.text label
            }

displayRegisterBtnIfNewUser : String -> Msg -> Element Msg
displayRegisterBtnIfNewUser uname msg =
    if uname /= "" then
        Element.text ""

    else
        Input.button
            (Button.simple ++ Button.fill ++ Color.info ++ [ Element.htmlAttribute (Html.Attributes.id "registerbtn") ])
        <|
            { onPress = Just <| msg
            , label = Element.text "Register"
            }


displayEnableEthereumBtn : Element Msg
displayEnableEthereumBtn = 
    Input.button
            (Button.simple ++ Button.fill ++ Color.warning ++ [ Element.htmlAttribute (Html.Attributes.id "enableEthereumButton") ] ++ [ Element.htmlAttribute (Html.Attributes.class "enableEthereumButton") ])
        <|
            { onPress = Just ClickedEnableEthereum
            , label = Element.text "Enable Ethereum"
            }

selectedUserIsOwnerView : DataState -> SR.Types.AppInfo -> Html Msg
selectedUserIsOwnerView dataState appInfo =
    case dataState of
        StateFetched sUsers sRankings dKind -> 
            case dKind of 
                Selected sSelected ->
                    case appInfo.m_user of
                        Nothing ->
                            Framework.responsiveLayout [] <|
                                Element.column
                                    Framework.container
                                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Owner  - No User11"
                                    , selecteduserIsOwnerhomebutton (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
                                    , playerbuttons dataState appInfo
                                    ]
                        Just userVal ->
                            Framework.responsiveLayout [] <|
                                Element.column
                                    Framework.container
                                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Owner  - " ++ userVal.username
                                    , selecteduserIsOwnerhomebutton userVal
                                    , playerbuttons dataState appInfo
                                    ]

                _ ->
                    Html.text "Fail selectedUserIsOwnerView"
        _ ->
            Html.text "Fail selectedUserIsOwnerView"

selectedUserIsPlayerView : DataState -> SR.Types.AppInfo -> Html Msg
selectedUserIsPlayerView dataState appInfo =
    case dataState of
        StateFetched sUsers sRankings dKind -> 
            case dKind of 
                Selected sSelected ->
                    case appInfo.m_user of
                        Nothing ->
                            Framework.responsiveLayout [] <|
                                Element.column
                                    Framework.container
                                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - No User12"
                                    
                                    ]
                        Just userVal ->
                            Framework.responsiveLayout [] <|
                                Element.column
                                    Framework.container
                                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ userVal.username
                                    , selecteduserIsPlayerHomebutton userVal
                                    , playerbuttons dataState appInfo
                                    ]

                _ ->
                    Html.text "Error3"

        StateUpdated sUsers sRankings dKind -> 
            case dKind of 
                Selected sSelected ->
                    case appInfo.m_user of
                        Nothing ->
                             Framework.responsiveLayout [] <|
                                Element.column
                                    Framework.container
                                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - No User13" 
                                    , Element.text <| "No User17"
                                    ]
                        Just userVal ->
                            Framework.responsiveLayout [] <|
                                Element.column
                                    Framework.container
                                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ userVal.username
                                    , Element.text <| "Challenge is On! Good luck!"
                                    , selecteduserIsPlayerHomebutton userVal
                                    , playerbuttons dataState appInfo
                                    ]
                _ ->
                    Html.text "Error4"

        AllEmpty ->
                    Html.text "Please refresh your browser"


selectedUserIsNeitherOwnerNorPlayerView : DataState -> SR.Types.AppInfo -> SR.Types.AccountState -> Html Msg
selectedUserIsNeitherOwnerNorPlayerView  dataState appInfo accountState =
    case dataState of
        StateFetched sUsers sRankings (Selected sSelected) ->
            case appInfo.m_user of
                Nothing ->
                    Framework.responsiveLayout [] <|
                        Element.column
                            Framework.container
                            [ newOrExistingUserNameDisplay (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing) accountState
                            , selecteduserIsNeitherPlayerNorOwnerHomebutton (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing) accountState
                            , playerbuttons  dataState appInfo
                            ]
                Just userVal ->
                    Framework.responsiveLayout [] <|
                        Element.column
                            Framework.container
                            [ newOrExistingUserNameDisplay userVal accountState
                            , selecteduserIsNeitherPlayerNorOwnerHomebutton userVal accountState
                            , playerbuttons  dataState appInfo
                            ]
        _ ->
            Html.text "Error4"


newOrExistingUserNameDisplay : SR.Types.User -> SR.Types.AccountState -> Element msg
newOrExistingUserNameDisplay user accountState =
    case accountState of 
        SR.Types.Guest ->
            Element.el Heading.h4 <| Element.text <| "New User - Please Register and Enable Ethereum to join"
        SR.Types.Registered ->
            Element.el Heading.h4 <| Element.text <| user.username ++ " - Please Enable Ethereum To Join"
        SR.Types.EthEnabled ->
            Element.el Heading.h4 <| Element.text <| "Please Register and Enable Ethereum to join"
        SR.Types.EthEnabledAndRegistered ->
            Element.el Heading.h4 <| Element.text <| user.username ++ " - Join?"


inputUserDetailsView : DataState -> SR.Types.AppInfo -> Html Msg
inputUserDetailsView dataState appInfo =
    case appInfo.m_user of
        Nothing ->
            case dataState of
                StateFetched sUsers sRankings dKind ->
                    let 
                        userVal = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
                    in
                    if Data.Users.isEmpty sUsers then
                        Framework.responsiveLayout [] <|
                        Element.column
                            Framework.container
                            [
                            Element.el Heading.h4 <| Element.text "No Users"
                            , newuserConfirmPanel appInfo.m_user (Data.Users.asList sUsers)
                            ]
                    else
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ displayEnableEthereumBtn
                                , Element.text "\n"
                                , Element.el Heading.h4 <| Element.text "Create New User"
                                , displayRegisterNewUser userVal sUsers
                                , newuserConfirmPanel appInfo.m_user (Data.Users.asList sUsers)
                                ]
                _ ->
                    Html.text "tbc"

        Just userVal ->
            case dataState of
                StateFetched sUsers sRankings dKind ->
                    if Data.Users.isEmpty sUsers then
                        Framework.responsiveLayout [] <|
                        Element.column
                            Framework.container
                            [
                            Element.el Heading.h4 <| Element.text "No Users"
                            , newuserConfirmPanel appInfo.m_user (Data.Users.asList sUsers)
                            ]
                    else
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ displayEnableEthereumBtn
                                , Element.text "\n"
                                , Element.el Heading.h4 <| Element.text "Create New User"
                                , displayRegisterNewUser userVal sUsers
                                , newuserConfirmPanel appInfo.m_user (Data.Users.asList sUsers)
                                ]
                _ ->
                    Html.text "tbc"

                
displayRegisterNewUser :  SR.Types.User -> Data.Users.Users -> Element Msg 
displayRegisterNewUser userVal sUsers =
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
        , Element.wrappedRow (Card.fill ++ Grid.simple)
            [ Element.column
                Grid.simple
                [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                    { onChange = NewUserNameInputChg
                    , text = userVal.username
                    , placeholder = Nothing
                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username*")
                    }
                , nameValidView userVal sUsers
                , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                    { onChange = NewUserPasswordInputChg
                    , text = userVal.password
                    , placeholder = Nothing
                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password")
                    }
                , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userDescription") ])
                    { onChange = NewUserDescInputChg
                    , text = userVal.description
                    , placeholder = Nothing
                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Description")
                    }
                , userDescValidationErr userVal
                , Input.email (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userEmail") ])
                    { onChange = NewUserEmailInputChg
                    , text = userVal.email
                    , placeholder = Nothing
                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Email")
                    }
                , emailValidationErr userVal
                , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userMobile") ])
                    { onChange = NewUserMobileInputChg
                    , text = Utils.Validation.Validate.validatedMaxTextLength userVal.mobile 25
                    , placeholder = Nothing
                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Mobile")
                    }
                , mobileValidationErr userVal
                ]
            ]
        , Element.text "* required and CANNOT be changed \nunder current ETH account"
        , SR.Elements.justParasimpleUserInfoText
        , newuserConfirmPanel (Just userVal) (Data.Users.asList sUsers)
        ]
                

    -- case dataState of 
    --     StateFetched sUsers sRankings dKind ->
    --         case walletState of 
    --             SR.Types.WalletOpened ->
    --                 -- case appInfo.m_user of
    --                 --     Nothing ->
    --                 --         Html.text "No User14"
    --                 --     Just userVal ->
    --                         Framework.responsiveLayout [] <|
    --                             Element.column
    --                                 Framework.container
    --                                 [ Element.el Heading.h4 <| Element.text "Create New User"
    --                                 , inputNewUser walletState dataState appInfo
    --                                 , newuserConfirmPanel walletState appInfo.m_user (Data.Users.asList sUsers)
    --                                 ]
    --             SR.Types.WalletStateLocked ->
    --                 -- case appInfo.m_user of
    --                 --     Nothing ->
    --                 --         Html.text "No User15"
    --                 --     Just userVal ->
    --                         Framework.responsiveLayout [] <|
    --                             Element.column
    --                                 Framework.container
    --                                 [ displayEnableEthereumBtn
    --                                 , Element.text "\n"
    --                                 , Element.el Heading.h4 <| Element.text "Create New User"
    --                                 , inputNewUser walletState dataState appInfo
    --                                 , newuserConfirmPanel walletState appInfo.m_user (Data.Users.asList sUsers)
    --                                 ]
                
    --             SR.Types.WalletOperational ->
    --                 -- case appInfo.m_user of
    --                 --     Nothing ->
    --                 --         Html.text "No User16"
    --                 --     Just userVal ->
    --                         Framework.responsiveLayout [] <|
    --                             Element.column
    --                                 Framework.container
    --                                 [ displayEnableEthereumBtn
    --                                 , Element.text "\n"
    --                                 , Element.el Heading.h4 <| Element.text "Create New User"
    --                                 , inputNewUser walletState dataState appInfo
    --                                 , newuserConfirmPanel walletState appInfo.m_user (Data.Users.asList sUsers)
    --                                 ]

    --             SR.Types.WalletOpenedNoUserAccount ->
    --                 -- case appInfo.m_user of
    --                 --     Nothing ->
    --                 --         Html.text "No User17"
    --                 --     Just userVal ->
    --                         Framework.responsiveLayout [] <|
    --                             Element.column
    --                                 Framework.container
    --                                 [ displayEnableEthereumBtn
    --                                 , Element.text "\n"
    --                                 , Element.el Heading.h4 <| Element.text "Create New User"
    --                                 , inputNewUser walletState dataState appInfo
    --                                 , newuserConfirmPanel walletState appInfo.m_user (Data.Users.asList sUsers)
    --                                 ]
    --             _ ->
    --                 Html.text "fell thru in inputUserDetailsView"
                    
        -- _ ->
        --     Html.text "Fail inputUserDetailsView"




updateExistingUserView : Model -> Html Msg
updateExistingUserView model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            case dataState of 
                StateFetched sUsers sRankings dKind -> 
                    case appInfo.m_user of
                        Nothing ->
                            Html.text "No User18"
                        Just userVal ->
                            Framework.responsiveLayout [] <|
                                Element.column
                                    Framework.container
                                    [ Element.el Heading.h4 <| Element.text "Update User Profile"
                                    , inputUpdateExistingUser model
                                    , existingUserConfirmPanel userVal (Data.Users.asList sUsers)
                                    ]
                _ ->
                    Html.text "Fail updateExistingUserView"
        _ ->
            Html.text "Fail updateExistingUserView"


inputNewLadderview : Model -> Html Msg
inputNewLadderview model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text "Create New Ladder Ranking"
                    , inputNewLadder appInfo dataState
                    , newrankingconfirmbutton appInfo dataState
                    , SR.Elements.footer
                    ]

        _ ->
            Html.text "Fail"

deleteRankingview : Model -> Html Msg
deleteRankingview model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text "Delete Ranking"
                    --, inputNewLadder appInfo dataState
                    , confirmDelRankingBtn appInfo dataState
                    --, newrankingconfirmbutton appInfo dataState
                    , SR.Elements.footer
                    ]

        _ ->
            Html.text "Fail"


displayChallengeBeforeConfirmView : Model -> Html Msg
displayChallengeBeforeConfirmView model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            case appInfo.m_user of
                Nothing ->
                    Html.text "No User19"
                Just userVal ->
                    Framework.responsiveLayout [] <|
                        Element.column
                            Framework.container
                            [ Element.el Heading.h4 <| Element.text <| userVal.username ++ " - Confirm Challenge"
                            , confirmChallengebutton model
                            ]

        _ ->
            Html.text "Error5"


displayResultBeforeConfirmView : Model -> Html Msg
displayResultBeforeConfirmView model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState txRec ->
            case dataState of
                StateFetched sUsers sRankings (Selected sSelected) -> 
                    let
                        m_playerAsUser =
                            Data.Users.gotUser sUsers appInfo.player.player.address
                    in
                        case m_playerAsUser of 
                            Nothing ->
                                Html.text "No Player"

                            Just playerAsUser ->
                                Framework.responsiveLayout [] <|
                                    Element.column
                                        Framework.container
                                        [ Element.el Heading.h4 <| Element.text <| playerAsUser.username ++ " - Result"
                                        , confirmResultbutton model
                                        ]
                _ ->
                    Html.text "Error6.2"
        _ ->
            Html.text "Error6.2"


txErrorView : Model -> Html Msg
txErrorView model =
    case model of
        AppOps walletState dataState appInfo uiState subState accountState  txRec ->
            let
                m_playerAsUser =
                    --SR.ListOps.gotUserFromUserList (EverySet.fromList dataState) appInfo.player.player.address
                    case dataState of 
                        StateFetched users rankings dKind ->
                            Data.Users.gotUser users appInfo.player.player.address
                        _ ->
                            Data.Users.gotUser Data.Users.empty appInfo.player.player.address

            in
                case m_playerAsUser of 
                    Nothing ->
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ Element.el Heading.h4 <| Element.text " Transaction Error"
                                , acknoweldgeTxErrorbtn model
                                ]

                    Just playerAsUser ->
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ Element.el Heading.h4 <| Element.text <| playerAsUser.username ++ " Transaction Error"
                                , acknoweldgeTxErrorbtn model
                                ]

        _ ->
            Html.text "Error7"


greetingView : String -> Html Msg
greetingView greetingMsg =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , greetingHeading greetingMsg
            ]

continueView : String -> Html Msg
continueView continueStr =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , greetingHeading continueStr
            , Element.column (Card.simple ++ Grid.simple) <|
                    [ Element.column Grid.simple <|
                        [ Input.button (Button.simple ++ Color.primary) <|
                            { onPress = Just <| Cancel
                            , label = Element.text "Continue ..."
                            }
                        ]
                    ]
            ]

continueWithRemoveDeletedRankingView : String -> Html Msg
continueWithRemoveDeletedRankingView continueStr =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , greetingHeading continueStr
            ,  Element.column (Card.simple ++ Grid.simple) <|
                    [ Element.column Grid.simple <|
                        [ Input.button (Button.simple ++ Color.primary) <|
                            { onPress = Just <| ClickedRemoveFromUserMemberRankings
                            , label = Element.text "Remove Listing(s)"
                            }
                        ]
                    ]
            , Element.text ("\n")
            , Element.column (Card.simple ++ Grid.simple) <|
                    [ Element.column Grid.simple <|
                        [ Input.button (Button.simple ++ Color.primary) <|
                            { onPress = Just <| Cancel
                            , label = Element.text "Cancel"
                            }
                        ]
                    ]
            ]

           


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        AppOps walletState dataState appInfo uiState  subState accountState txRec ->
            -- let 
            --     _ = Debug.log "walletState in subs" walletState

            --     _ = Debug.log "subState in subs" subState
            -- in

            case subState of 
                SR.Types.Subscribe ->
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                SR.Types.StopSubscription ->
                    Sub.none

            -- the orig code uses Ports.walletSentry ... same as here:
            -- case walletState of
            --     -- SR.Types.WalletStateUnknown ->
            --     --     Sub.batch
            --     --         [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
            --     --         , Eth.Sentry.Tx.listen txRec.txSentry
            --     --         ]


            --     SR.Types.WalletStateMissing ->
            --         Sub.batch
            --             -- decodeToMsg uses partial application to return Value -> Msg which is what walletSentry expects as an arg
            --             [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
            --             , Eth.Sentry.Tx.listen txRec.txSentry
            --             ]

            --     -- SR.Types.WalletEthEnabled ->
            --     --     Sub.batch
            --     --         -- decodeToMsg uses partial application to return Value -> Msg which is what walletSentry expects as an arg
            --     --         [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
            --     --         , Eth.Sentry.Tx.listen txRec.txSentry
            --     --         ]

            --     SR.Types.WalletStateLocked ->
            --             Sub.batch
            --                 [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
            --                 , Eth.Sentry.Tx.listen txRec.txSentry
            --                 ]

            --     SR.Types.WalletStopSub ->
            --             Sub.none
                    

            --     SR.Types.WalletStateAwaitOpening ->
            --         Sub.batch
            --             [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
            --             , Eth.Sentry.Tx.listen txRec.txSentry
            --             ]

            --     SR.Types.WalletOpened ->
            --         Sub.none

            --     SR.Types.WalletOperational ->
            --         Sub.batch
            --             [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
            --             , Eth.Sentry.Tx.listen txRec.txSentry
            --             ]

            --     -- SR.Types.WalletOperational ->
            --     --         Sub.batch   
            --     --             [
            --     --             if model.someInt < 9 then
            --     --                  Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
            --     --                 , Eth.Sentry.Tx.listen txRec.txSentry
                                
            --     --             else 
            --     --                 Sub.none
            --     --             ]
  

            --     SR.Types.WalletWaitingForTransactionReceipt ->
            --         let
            --             _ =
            --                 Debug.log "SR.Types is now WalletWaitingForTransactionReceipt :" walletState
            --         in
            --         Sub.batch
            --             [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
            --             , Eth.Sentry.Tx.listen txRec.txSentry
            --             ]

                -- _ ->
                --     let
                --         _ =
                --             Debug.log "walletState fell thru:" walletState
                --     in
                --     Sub.none

        Failure _ ->
            Sub.none





--Helper functions



-- Http ops


fetchedSingleRanking : Internal.Types.RankingId -> Cmd Msg
fetchedSingleRanking (Internal.Types.RankingId rankingId) =
    Cmd.none
    --PlayersReceived is the Msg handled by update whenever a request is made
    -- Http.request
    --     { body = Http.emptyBody
    --     , expect =
    --         SR.Decode.ladderOfPlayersDecoder
    --             |> Http.expectJson (RemoteData.fromResult >> PlayersReceived)
    --     , headers = [ SR.Defaults.secretKey ]
    --     , method = "GET"
    --     , timeout = Nothing
    --     , tracker = Nothing
    --     , url = SR.Constants.baseBinUrl ++ rankingId ++ "/latest"
    --     }


addedUserAsFirstPlayerInNewList : SR.Types.User -> Cmd Msg
addedUserAsFirstPlayerInNewList user =
    -- todo: fix
    Cmd.none
    -- let
    --     playerEncoder : Json.Encode.Value
    --     playerEncoder =
    --         Json.Encode.list
    --             Json.Encode.object
    --             [ [ ( "address", Json.Encode.string (String.toLower user.m_ethaddress) )
    --               , ( "rank", Json.Encode.int 1 )
    --               , ( "challengeraddress", Json.Encode.string "" )
    --               ]
    --             ]
    -- in
    -- --SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId is the Msg handled by update whenever a request is made
    -- -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- -- decoder relates to what comes back from server. Nothing to do with above.
    -- Http.request
    --     { body =
    --         Http.jsonBody <| playerEncoder
    --     , expect = Http.expectJson (RemoteData.fromResult >> SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId) SR.Decode.newRankingIdDecoder
    --     , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
    --     , method = "POST"
    --     , timeout = Nothing
    --     , tracker = Nothing
    --     , url = SR.Constants.jsonbinUrlForCreateNewBinAndRespond
    --     }


createNewUser : Data.Users.Users -> SR.Types.User -> Cmd Msg
createNewUser sUsers newuserinfo =
    Cmd.none
    -- let
    --     newUserToAdd : SR.Types.User
    --     newUserToAdd =
    --         Data.Users.newUser  
    --             newuserinfo.username 
    --             newuserinfo.password 
    --             newuserinfo.m_ethaddress 
    --             newuserinfo.description
    --             newuserinfo.email
    --             newuserinfo.mobile

    --     listForHttpRequest = 
    --             Data.Users.addUser newUserToAdd sUsers
    --             |> Data.Users.asList
            
    -- in
    -- --SentUserInfoAndDecodedResponseToNewUser is the Msg handled by update whenever a request is made by buttuser clicked
    -- --RemoteData is used throughout the module, including update
    -- -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- -- decoder relates to what comes back from server. Nothing to do with above.
    -- -- we mustn't submit a new user if the original list is empty for some reason ...
    -- if Data.Users.isEmpty sUsers then
    --     Http.request
    --         { body =
    --             Http.jsonBody <| jsonEncodeNewUsersList listForHttpRequest
    --         , expect = Http.expectJson (RemoteData.fromResult >> SentUserInfoAndDecodedResponseToNewUser) SR.Decode.decodeNewUserListServerResponse
    --         , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
    --         , method = "PUT"
    --         , timeout = Nothing
    --         , tracker = Nothing

    --         -- this will fail the create new user:
    --         , url = ""
    --         }

    -- else
    --     Http.request
    --         { body =
    --             Http.jsonBody <| jsonEncodeNewUsersList listForHttpRequest
    --         , expect = Http.expectJson (RemoteData.fromResult >> SentUserInfoAndDecodedResponseToNewUser) SR.Decode.decodeNewUserListServerResponse
    --         , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
    --         , method = "PUT"
    --         , timeout = Nothing
    --         , tracker = Nothing
    --         , url = SR.Constants.jsonbinUrlUpdateUserListAndRespond
    --         }

updateExistingUser : Data.Users.Users -> Cmd Msg
updateExistingUser  updatedUserInfo =
        -- todo: re-implement with fauna
        Cmd.none

httpUpdateUsers : Data.Users.Users -> Cmd Msg
httpUpdateUsers  updatedUsers =
    Cmd.none


jsonEncodeNewUsersList : List SR.Types.User -> Json.Encode.Value
jsonEncodeNewUsersList luserInfo =
    let
        encodeNewUserObj : SR.Types.User -> Json.Encode.Value
        encodeNewUserObj userInfo =
            case userInfo.m_ethaddress of
                Nothing ->
                    Json.Encode.object
                        [ ( "datestamp", Json.Encode.int 1569839363942 )
                        , ( "active", Json.Encode.bool True )
                        , ( "username", Json.Encode.string userInfo.username )
                        , ( "m_ethaddress", Json.Encode.string "")
                        , ( "description", Json.Encode.string userInfo.description )
                        , ( "email", Json.Encode.string userInfo.email )
                        , ( "mobile", Json.Encode.string userInfo.mobile )
                        , ( "userjoinrankings", Json.Encode.list encodeRankingIdList userInfo.userjoinrankings )
                        ]
                Just addr ->
                    Json.Encode.object
                        [ ( "datestamp", Json.Encode.int 1569839363942 )
                        , ( "active", Json.Encode.bool True )
                        , ( "username", Json.Encode.string userInfo.username )
                        , ( "m_ethaddress", Json.Encode.string (String.toLower (Eth.Utils.addressToString addr)) )
                        , ( "description", Json.Encode.string userInfo.description )
                        , ( "email", Json.Encode.string userInfo.email )
                        , ( "mobile", Json.Encode.string userInfo.mobile )
                        , ( "userjoinrankings", Json.Encode.list encodeRankingIdList userInfo.userjoinrankings )
                        ]

        encodeRankingIdList : String -> Json.Encode.Value
        encodeRankingIdList rankingIdstr =
            Json.Encode.string
                rankingIdstr

        encodedList =
            Json.Encode.list encodeNewUserObj luserInfo
    in
    encodedList


httpAddCurrentUserToPlayerList : DataState -> SR.Types.User -> Cmd Msg
httpAddCurrentUserToPlayerList dataState userRec =
    Cmd.none
    -- case dataState of
    --     StateUpdated sUsers sRankings dKind -> 
    --         case dKind of 
    --                 Selected sSelected -> 
    --                     --ReturnFromPlayerListUpdate is the Msg handled by update whenever a request is made
    --                     --RemoteData is used throughout the module, including update
    --                     -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    --                     -- the Decoder decodes what comes back in the response
    --                     Http.request
    --                         { body =
    --                             Http.jsonBody <| Data.Selected.jsonEncodeNewSelectedRankingPlayerList (Data.Selected.userAdded sUsers (Utils.MyUtils.stringFromRankingId rnkId) (Data.Selected.asList sSelected) userRec)
    --                         , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromPlayerListUpdate) SR.Decode.decodeNewPlayerListServerResponse
    --                         , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
    --                         , method = "PUT"
    --                         , timeout = Nothing
    --                         , tracker = Nothing
    --                         , url = SR.Constants.jsonbinUrlStubForUpdateExistingBinAndRespond ++ (Utils.MyUtils.stringFromRankingId rnkId)
                            
    --                         }
    --                 _ -> 
    --                     let 
    --                         _ = Debug.log "httpAddCurrentUserToPlayerList - dataState" dataState
    --                     in
    --                         Cmd.none
    --     _ -> 
    --         let 
    --             _ = Debug.log "httpAddCurrentUserToPlayerList - dataState" dataState
    --         in
    --             Cmd.none



httpPutRequestForAddGlobal : Json.Encode.Value -> List SR.Types.Ranking -> Cmd Msg
httpPutRequestForAddGlobal newJsonEncodedList globalListWithJsonObjAdded =
    --AddedNewRankingToGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    -- Http.request
    --     { body =
    --         Http.jsonBody <| Data.Global.newJsonEncodedList globalListWithJsonObjAdded
    --     , expect = Http.expectJson (RemoteData.fromResult >> AddedNewRankingToGlobalList) SR.Decode.decodeNewRankingListServerResponse
    --     , headers = [ SR.Defaults.secretKey, SR.Defaults.globalBinName, SR.Defaults.globalContainerId ]
    --     , method = "PUT"
    --     , timeout = Nothing
    --     , tracker = Nothing
    --     , url = SR.Constants.globalJsonbinRankingUpdateLink
    --     }
    Cmd.none

httpDeleteSelectedRankingFromGlobalList : Data.Global.Global -> Cmd Msg
httpDeleteSelectedRankingFromGlobalList sGlobalWithRankingDeleted =
    Cmd.none
    -- Http.request
    --         { body =
    --             Http.jsonBody <| Data.Global.newJsonEncodedList (Data.Global.rankingsAsList sGlobalWithRankingDeleted)
    --         , expect = Http.expectJson (RemoteData.fromResult >> ReturnedFromDeletedRankingFromGlobalList) SR.Decode.decodeUpdateGlobalBinResponse
    --         , headers = [ SR.Defaults.secretKey, SR.Defaults.globalBinName, SR.Defaults.globalContainerId ]
    --         , method = "PUT"
    --         , timeout = Nothing
    --         , tracker = Nothing
    --         -- nb. updating the 'global' list on the server actually means updating the Rankings set
    --         -- the collection is called 'Global' on the server, but it isn't 'Global' in the app
    --         -- until it's been turned into (EverySet UserRankings)
    --         , url = SR.Constants.globalJsonbinRankingUpdateLink
    --         }


httpDeleteSelectedRankingFromJsonBin : String -> Cmd Msg
httpDeleteSelectedRankingFromJsonBin rankingId =
    Cmd.none
    -- the Decoder decodes what comes back in the response
    -- let 
    --     _ = Debug.log "bin id" rankingId
    -- in
    -- Http.request
    --     { body =
    --         Http.emptyBody
    --     , expect = Http.expectJson (RemoteData.fromResult >> ReturnedFromDeletedSelectedRankingFromJsonBin) SR.Decode.decodeDeleteBinResponse
    --     , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
    --     , method = "DELETE"
    --     , timeout = Nothing
    --     , tracker = Nothing
    --     , url = "https://api.jsonbin.io/b/" ++ rankingId
        
    --     }


postResultToJsonbin : Internal.Types.RankingId -> Cmd Msg
postResultToJsonbin (Internal.Types.RankingId rankingId) =
    let
        _ =
            Debug.log "rankingid in postResultToJsonbin" rankingId

        headerKey =
            Http.header
                "secret-key"
                "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"
    in
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectWhatever SentResultToJsonbin
        , headers = [ headerKey ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = "https://api.jsonbin.io/b/" ++ rankingId
        }


httpPlayerList : DataState -> Cmd Msg
httpPlayerList dataState =
    Cmd.none
--   case dataState of
--     StateUpdated sUsers sRankings dKind -> 
--         case dKind of 
--                 Selected sSelected ->
--                     Http.request
--                         { body =
--                         Http.jsonBody <| Data.Selected.jsonEncodeNewSelectedRankingPlayerList (Data.Selected.asList sSelected)
--                         , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromPlayerListUpdate) SR.Decode.decodeNewPlayerListServerResponse
--                         , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
--                         , method = "PUT"
--                         , timeout = Nothing
--                         , tracker = Nothing
--                         , url = SR.Constants.jsonbinUrlStubForUpdateExistingBinAndRespond ++ (Utils.MyUtils.stringFromRankingId rnkId)
--                         }
--                 _ -> 
--                     let 
--                         _ = Debug.log "dataState - httpPlayerList" dataState
--                     in
--                         Cmd.none
--     _ -> 
--         let 
--             _ = Debug.log "dataState - httpPlayerList" dataState
--         in
--             Cmd.none
    


httpUpdateUsersJoinRankings : String -> SR.Types.User -> List SR.Types.User -> Cmd Msg
httpUpdateUsersJoinRankings rankingId user lUser =
    Cmd.none
    -- let 
    --     newUserList =  Data.Users.addedNewJoinedRankingId rankingId user lUser
    --     _ = Debug.log "newuserlist " newUserList
    -- in
    -- Http.request
    --     { body =
    --         Http.jsonBody <| SR.Encode.encodeUserList <| Data.Users.addedNewJoinedRankingId rankingId user lUser
    --     , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromUserListUpdate) SR.Decode.decodeNewUserListServerResponse
    --     , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
    --     , method = "PUT"
    --     , timeout = Nothing
    --     , tracker = Nothing
    --     --, url = SR.Constants.jsonbinUrlUpdateUserListAndRespond
    --     , url = ""
    --     }


module  Main exposing (Model(..), Msg(..), httpDeleteSelectedRankingFromGlobalList, emptyTxRecord, init, main, update, view)

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
import SR.Decode
import SR.Defaults
import SR.Elements
import SR.Encode
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
    = AppOps SR.Types.WalletState DataState SR.Types.AppInfo SR.Types.UIState TxRecord
    | Failure String



type DataState
  = AllEmpty
  | StateFetched Data.Users.Users DataKind
  | StateUpdated Data.Users.Users DataKind

type DataKind
  = Users SR.Types.User
  | Global Data.Global.Global Internal.Types.RankingId SR.Types.User
  | Selected Data.Selected.Selected Internal.Types.RankingId SR.Types.User SR.Types.SelectedStatus
    

emptyAllLists =
    { userRankings = Data.Global.empty
    , users = Data.Users.emptyUsers
    , userPlayers = []
    }



init : () -> ( Model, Cmd Msg )
init _ =
    ( AppOps SR.Types.WalletStateLocked AllEmpty SR.Defaults.emptyAppInfo SR.Types.UILoading emptyTxRecord
    , Cmd.batch
        [ gotUserList
        , gotGlobal
        , Ports.log
            "Sending out msg from init "
        -- , Ports.log
        --     "eth_requestAccounts"
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
    | ClickedDeleteRanking String
    | ClickedDeleteRankingConfirmed
    | ClickedEnableEthereum
    | ResetToShowGlobal
    | ResetToShowSelected
    | ResetRejectedNewUserToShowGlobal
    | LadderNameInputChg String
    | LadderDescInputChg String
    | NewUserNameInputChg String
    | NewUserDescInputChg String
    | NewUserEmailInputChg String
    | NewUserMobileInputChg String
    | ExistingUserNameInputChg String
    | ExistingUserDescInputChg String
    | ExistingUserEmailInputChg String
    | ExistingUserMobileInputChg String
    | ClickedCreateNewUserToWallet SR.Types.User
      -- App Only Ops
    | MissingWalletInstructions
    | OpenWalletInstructions
    | NoOp
    | SentResultToJsonbin (Result Http.Error ())
    | SentUserInfoAndDecodedResponseToNewUser (RemoteData.WebData (List SR.Types.User))
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | GlobalReceived (RemoteData.WebData (List SR.Types.Ranking))
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | UsersReceived (RemoteData.WebData (List SR.Types.User))
    | ReturnFromPlayerListUpdate (RemoteData.WebData (List SR.Types.Player))
    | ReturnFromUserListUpdate (RemoteData.WebData (List SR.Types.User))
    | ReturnedFromDeletedRankingFromGlobalList (RemoteData.WebData (List SR.Types.Ranking))
    | ReturnedFromDeletedSelectedRankingFromJsonBin (RemoteData.WebData (List SR.Types.Ranking))
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
    --| GotAccountAddress Eth.Types.Address


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        _ = Debug.log "msg in update" msg
    in
    case ( msg, model ) of
        ( WalletStatus walletSentry_, AppOps walletState dataState appInfo uiState txRec ) ->
            let 
                 _ = Debug.log "walletState in WalletStatus" walletState
            in
            -- walletState might be unnecessary here, because WalletStatus is only relevant at time of unlocking i.e. one off
            case walletState of
                SR.Types.WalletStateLocked ->
                    case walletSentry_.networkId of
                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    ( AppOps SR.Types.WalletStateLocked dataState SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord
                                    , Cmd.none
                                    )

                                Just uaddr ->
                                    let 
                                        _ = Debug.log "got uaddre" uaddr
                                        newModel = AppOps SR.Types.WalletOpened dataState (gotWalletAddrApplyToUser appInfo uaddr) SR.Types.UIRenderAllRankings emptyTxRecord
                                    in
                                    (newModel, Cmd.none)
                        _ ->
                            (model, Cmd.none)

                SR.Types.WalletStopSub ->
                    let 
                        _ = Debug.log "in walletstopsub" "here"
                    in
                    (model, Cmd.none)

                SR.Types.WalletOpened ->
                    (model, Cmd.none)


                SR.Types.WalletWaitingForTransactionReceipt ->
                    
                    handleWalletWaitingForUserInput msg walletState dataState appInfo txRec

                _ ->
                    let 
                        _ = Debug.log "fell thru at: " "update - walletState"
                    in
                    ( AppOps SR.Types.WalletStopSub AllEmpty SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord
                    , Cmd.none
                    )
        ( WalletStatus _, Failure _ ) ->
            (model, Cmd.none)

        (ClickedEnableEthereum, AppOps walletState dataState appInfo uiState txRec ) ->
            (AppOps SR.Types.WalletStopSub dataState appInfo uiState txRec, Ports.log "eth_requestAccounts")


        (ClickedConfirmedRegisterNewUser, AppOps walletState dataState appInfo uiState txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState appInfo SR.Types.UIWaitingForTxReceipt txRec, Cmd.none )

                SR.Types.WalletStateLocked ->
                    ( AppOps walletState dataState appInfo SR.Types.UIDisplayWalletLockedInstructions txRec, Cmd.none )
                _ ->
                    let 
                        _ = Debug.log "ClickedConfirmedRegisterNewUser walletState : " walletState
                    in
                    ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt txRec, Cmd.none )
                

        (ClickedRegister, AppOps walletState dataState appInfo uiState txRec ) ->
            let
                    _ = Debug.log "walletState in clickreg " walletState
            in
            case walletState of
                SR.Types.WalletStateLocked ->
                    ( AppOps walletState dataState appInfo SR.Types.UIRegisterNewUser txRec, Cmd.none )
                SR.Types.WalletOperational ->
                    ( AppOps walletState dataState appInfo SR.Types.UIRegisterNewUser txRec, Cmd.none )
                SR.Types.WalletStopSub ->
                    ( AppOps walletState dataState appInfo SR.Types.UIRegisterNewUser txRec, Cmd.none )
                _ ->
                    (model, Cmd.none)


        ( UsersReceived userList, AppOps walletState dataState appInfo uiState txRec ) ->
            let 
                -- _ =
                --                     Debug.log "UsersReceived" userList
                extractedList = Data.Users.validatedUserList <| Data.Users.extractUsersFromWebData userList
            in
                --todo: copy createGlobal to handle this:
                if List.isEmpty extractedList then 
                    (model, Cmd.none)
                else 
                            let
                                users = Data.Users.asUsers (EverySet.fromList (extractedList))
                                newUser = Data.Users.gotUser users appInfo.user.ethaddress
                                userInAppInfo = { appInfo | user = newUser }
                                newDataKind = Users newUser
                                newDataState = StateFetched users newDataKind
                            in
                                ( AppOps walletState newDataState userInAppInfo SR.Types.UILoading emptyTxRecord, gotGlobal )
        
        ( UsersReceived _, Failure _ ) ->
            (model, Cmd.none)


        (GlobalReceived rmtrnkingdata, AppOps walletState dataState appInfo uiState txRec ) ->
            let
                _ = Debug.log "glob rec 1" dataState
            in
                    case dataState of
                        StateFetched sUsers dKind -> 
                            case dKind of
                                Users user ->
                                    let
                                        newDataKind = Global (Data.Global.createdGlobal rmtrnkingdata sUsers) (Internal.Types.RankingId "") user
                                        newDataSet = StateFetched sUsers newDataKind
                                    in 
                                        (AppOps walletState newDataSet appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none)
                                
                                Global sGlobal rnkId user ->
                                    let
                                        newDataKind = Global (Data.Global.createdGlobal rmtrnkingdata sUsers) (Internal.Types.RankingId "") user
                                        newDataSet = StateFetched sUsers newDataKind

                                        _ = Debug.log "glob rec, global datastate" walletState
                                    in
                                       -- WalletStopSub works here to stop looping but there may be a better solution
                                        ( AppOps SR.Types.WalletStopSub newDataSet appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none)
                                           
                                _ ->
                                        (model, Cmd.none)
                        _ ->
                            (model, Cmd.none)

        ( GlobalReceived _, Failure _ ) ->
            (model, Cmd.none)

        (PlayersReceived lplayer, AppOps walletState dataState appInfo uiState txRec )  ->
                    case dataState of
                        StateFetched sUsers dKind -> 
                            case dKind of 
                                    Selected sSelected rnkId user status ->
                                        let 
                                            newSSelected = Data.Selected.createdSelected lplayer sUsers rnkId

                                        
                                            newAppPlayer = { appInfo | player = Data.Selected.gotUserAsPlayer newSSelected user.ethaddress }

                                            newAppChallengerAndPlayer = { newAppPlayer | challenger = Data.Selected.gotUserAsPlayer newSSelected newAppPlayer.player.player.challengeraddress }

                                            newDataKind = Selected newSSelected rnkId user status
                                            newDataState = StateFetched sUsers newDataKind
                                        in
                                            case status of 
                                                SR.Types.UserIsOwner ->     
                                                    (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsOwner emptyTxRecord, Cmd.none)
                                                SR.Types.UserIsMember  ->
                                                    (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord, Cmd.none)
                                                SR.Types.UserIsNeitherOwnerNorMember ->
                                                    (AppOps walletState newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, Cmd.none)
                                                SR.Types.UserIsUnRegistered -> 
                                                    (AppOps walletState newDataState appInfo SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, Cmd.none)
                                    _ ->
                                        (model, Cmd.none)
                        _ ->
                            (model, Cmd.none)
        ( PlayersReceived _, Failure _ ) ->
            (model, Cmd.none)

        (ClickedSelectedMemberRanking rnkidstr rnkownerstr rnknamestr, AppOps walletState dataState appInfo uiState txRec ) ->
                            case dataState of 
                                    StateFetched sUsers dKind ->
                                            case dKind of 
                                                Global sGlobal rnkId user ->
                                                    let
                                                        _ =
                                                            Debug.log "user clicked member" rnkidstr


                                                        newAppInfo =
                                                            updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr


                                                        -- re-factor from appInfo to AppState over time
                                                        initAppState = 
                                                            Data.AppState.updateAppState appInfo.user appInfo.player 
                                                            appInfo.challenger ( rnkidstr)


                                                        newDataKind = Selected Data.Selected.emptySelected (Internal.Types.RankingId "") user SR.Types.UserIsMember
                                                        newDataState = StateFetched sUsers newDataKind
                                                    in
                                                        ( AppOps walletState newDataState newAppInfo SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord, 
                                                        fetchedSingleRanking rnkidstr )
                                                _ -> 
                                                    (model, Cmd.none)
                                    _ -> 
                                                    (model, Cmd.none)


        (ClickedSelectedNeitherOwnerNorMember rnkidstr rnkownerstr rnknamestr, AppOps walletState dataState appInfo uiState txRec)  ->
            let 
                _ = Debug.log "rnkid" rnkidstr
            in
            case dataState of 
                        StateFetched sUsers dKind ->
                            case dKind of 
                                Global sGlobal rnkId user ->
                                    let
                                        newAppInfo =
                                            updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr

                                        -- re-factor from appInfo to AppState over time
                                        initAppState = 
                                            Data.AppState.updateAppState appInfo.user appInfo.player 
                                            appInfo.challenger ( rnkidstr)
                                        
                                        newDataKind = Selected Data.Selected.emptySelected rnkidstr user SR.Types.UserIsNeitherOwnerNorMember
                                        newDataState = StateFetched sUsers newDataKind
                                    in
                                        ( AppOps walletState newDataState newAppInfo SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, 
                                        fetchedSingleRanking rnkidstr )
                                _ -> 
                                    (model, Cmd.none)
                        _ -> 
                            (model, Cmd.none)
        
        


        (ClickedChangedUIStateToEnterResult player, AppOps walletState dataState appInfo uiState txRec)  ->
            ( AppOps walletState dataState appInfo SR.Types.UIEnterResult emptyTxRecord, Cmd.none )


        (SentResultToWallet result, AppOps walletState dataState appInfo uiState txRec)  ->
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
                        ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState newAppInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                        , sentryCmd
                        )
                        
                SR.Types.Lost ->                                     
                    let
                            newAppInfo = {appInfo | appState = SR.Types.AppStateEnterLost }
                    in
                        ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState newAppInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                        , sentryCmd
                        )
                SR.Types.Undecided -> 
                    let
                            newAppInfo = {appInfo | appState = SR.Types.AppStateEnterUndecided }
                    in
                        ( AppOps walletState dataState newAppInfo SR.Types.UIEnterResultTxProblem emptyTxRecord
                            , sentryCmd
                            )


        (ProcessResult result, AppOps walletState dataState appInfo uiState txRec )  ->               
            let
                _ =
                    Debug.log "process result" result
            in
            case result of
                SR.Types.Won ->
                    case dataState of
                        StateUpdated sUsers dKind -> 
                            case dKind of 
                                Selected sSelected rnkId user status ->
                                    let 
                                        handleWonTuple = Data.Selected.handleWon sSelected appInfo
                                        newDataKind = Selected (Tuple.first handleWonTuple) rnkId user status
                                        newDataState = StateUpdated sUsers newDataKind
                                        newModel = 
                                                AppOps walletState newDataState (Tuple.second handleWonTuple)  
                                                (Data.Selected.resultView status) txRec
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
                        StateUpdated sUsers dKind -> 
                            case dKind of 
                                Selected sSelected rnkId user status ->
                                    let 
                                        handleLostTuple = Data.Selected.handleLost sSelected appInfo
                                        newDataKind = Selected (Tuple.first handleLostTuple) rnkId user status
                                        newDataState = StateUpdated sUsers newDataKind
                                        newModel = 
                                                AppOps walletState newDataState (Tuple.second handleLostTuple)  
                                                (Data.Selected.resultView status) txRec
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
                        StateUpdated sUsers dKind -> 
                            case dKind of 
                                Selected sSelected rnkId user status ->
                                    let 
                                        handleUndecidedTuple = Data.Selected.handleUndecided sSelected appInfo
                                        newDataKind = Selected (Tuple.first handleUndecidedTuple) rnkId user status
                                        newDataState = StateUpdated sUsers newDataKind
                                        newModel = 
                                                AppOps walletState newDataState (Tuple.second handleUndecidedTuple)  
                                                (Data.Selected.resultView status) txRec
                                    in
                                            (newModel, httpPlayerList newDataState) 


                                _ -> 
                                    let 
                                        _ = Debug.log "4 - dataState" dataState
                                    in
                                        (model, Cmd.none)
                        _ -> 
                            let 
                                _ = Debug.log "4 - dataState" dataState
                            in
                                (model, Cmd.none)


        (ClickedCreateNewLadder, AppOps walletState dataState appInfo uiState txRec) ->
            let
                newSelectedRanking =
                    appInfo.selectedRanking


                clearedNameFieldInSelectedRanking =
                    { newSelectedRanking | rankingname = "" }


                clearedNameFieldAppInfo =
                    { appInfo | selectedRanking = clearedNameFieldInSelectedRanking }
            in
            ( AppOps walletState dataState clearedNameFieldAppInfo SR.Types.UICreateNewLadder emptyTxRecord, Cmd.none )




        (ResetToShowGlobal, AppOps walletState dataState appInfo uiState txRec ) ->
            let 
                _ = Debug.log "reset to global wallet state" walletState

                _ = Debug.log "datastate" dataState
            in
            -- case walletState of 
            --     SR.Types.WalletStateLocked ->
                    case dataState of 
                        StateFetched sUsers dKind ->
                            let
                                newDataKind = Global Data.Global.empty (Internal.Types.RankingId "") appInfo.user
                                newDataState = StateFetched sUsers newDataKind

                                
                                _ = Debug.log "toGlobal now" "now"
            
                            in
                            ( AppOps SR.Types.WalletStateLocked newDataState appInfo SR.Types.UILoading emptyTxRecord, gotGlobal )
                        _ -> 
                            (model, Cmd.none)
                --SR.Types.WalletOperational ->




        (ResetToShowSelected, AppOps walletState dataState appInfo uiState txRec ) ->
            case dataState of 
                StateFetched sUsers dKind ->
                    case dKind of
                        Selected sSelected rnkId user uState ->
                            case uState of 
                                SR.Types.UserIsOwner ->
                                    (AppOps walletState dataState appInfo SR.Types.UISelectedRankingUserIsOwner emptyTxRecord, Cmd.none )
                                SR.Types.UserIsMember ->
                                    (AppOps walletState dataState appInfo SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord, Cmd.none )
                                SR.Types.UserIsNeitherOwnerNorMember ->
                                    (AppOps walletState dataState appInfo SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, Cmd.none )
                                SR.Types.UserIsUnRegistered ->
                                    (AppOps walletState dataState appInfo SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, Cmd.none )
                        _ -> 
                            (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)


        (ClickedUpdateExistingUser, AppOps walletState dataState appInfo uiState txRec ) ->
            let 
                _ = Debug.log "ClickedUpdateExistingUser " walletState
                --newDataState = StateFetched sUsers dKind
                --
            in
            -- if user already did an update, need to ensure we start with StateFetched again
            case dataState of
                StateUpdated sUsers user ->
                    let 
                        newDataState = StateFetched sUsers user
                    in
                        ( AppOps walletState newDataState appInfo SR.Types.UIUpdateExistingUser txRec, Cmd.none )
                _ ->
                    ( AppOps walletState dataState appInfo SR.Types.UIUpdateExistingUser txRec, Cmd.none )


        (LadderNameInputChg namefield, AppOps walletState dataState appInfo uiState txRec ) ->
            let
                newSelectedRanking =
                    appInfo.selectedRanking


                updatedSelectedRanking =
                    { newSelectedRanking | rankingname = namefield }


                newAppInfo =
                    { appInfo | selectedRanking = updatedSelectedRanking }
            in
            ( AppOps walletState dataState newAppInfo SR.Types.UICreateNewLadder emptyTxRecord, Cmd.none )


        (LadderDescInputChg descfield, AppOps walletState dataState appInfo uiState txRec ) ->
            let
                newSelectedRanking =
                    appInfo.selectedRanking


                updatedSelectedRanking =
                    { newSelectedRanking | rankingdesc = descfield }


                newAppInfo =
                    { appInfo | selectedRanking = updatedSelectedRanking }
            in
            ( AppOps walletState dataState newAppInfo SR.Types.UICreateNewLadder emptyTxRecord, Cmd.none )


        
        (NewUserNameInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserNameInputChg updateField), Cmd.none )


        (NewUserDescInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserDescInputChg updateField), Cmd.none )


        (NewUserEmailInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserEmailInputChg updateField), Cmd.none )


        (NewUserMobileInputChg updateField, _) ->
            ( handleNewUserInputs model (NewUserMobileInputChg updateField), Cmd.none )


        (ExistingUserNameInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserNameInputChg updateField), Cmd.none )


        (ExistingUserDescInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserDescInputChg updateField), Cmd.none )


        (ExistingUserEmailInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserEmailInputChg updateField), Cmd.none )


        (ExistingUserMobileInputChg updateField, _) ->
            ( handleExistingUserInputs model (ExistingUserMobileInputChg updateField), Cmd.none )


        (ClickedConfirmedUpdateExistingUser, AppOps walletState dataState appInfo uiState txRec )  ->
            case dataState of
                StateFetched sUsers user ->
                    let 
                                _ = Debug.log "14.1" dataState
                                newDataState = StateUpdated sUsers user
                    in
                    ( AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings txRec, updateExistingUser (Data.Users.asList sUsers) appInfo.user )
                _ -> 
                            let 
                                _ = Debug.log "14.3 - dataState" dataState
                            in
                                (model, Cmd.none)


        (SentUserInfoAndDecodedResponseToNewUser serverResponse, AppOps walletState dataState appInfo uiState txRec )  ->
            (AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )


        (ClickedChallengeOpponent opponentAsPlayer, AppOps walletState dataState appInfo uiState txRec )  ->
            case dataState of
                StateFetched sUsers dKind -> 
                    
                    case dKind of 
                            Selected sSelected rnkId user status ->
                                ( updatedForChallenge model (Data.Selected.asList sSelected) opponentAsPlayer appInfo.user, Cmd.none )
                
                            _ ->
                                (model, Cmd.none)
                _ ->
                    (model, Cmd.none)


        (ClickedDeleteRanking uaddr, AppOps walletState dataState appInfo uiState txRec )  ->
            case dataState of 
                StateFetched sUsers dKind ->
                    case dKind of 
                        Selected sSelected rnkId user status ->
                            let 
                                newDataKind = Global Data.Global.empty rnkId user
                                newDataState = StateUpdated sUsers newDataKind
                            in
                                ( AppOps walletState
                                    newDataState
                                    appInfo
                                    SR.Types.UIDeleteRankingConfirm
                                    txRec
                                , Cmd.none
                                )
                        _ -> 
                            let 
                                _ = Debug.log "9 - dataState should be updated" dataState
                            in
                                (model, Cmd.none)
                _ -> 
                            let 
                                _ = Debug.log "9 - dataState" dataState
                            in
                                (model, Cmd.none)
        
        (ClickedDeleteRankingConfirmed, AppOps walletState dataState appInfo uiState txRec )  ->
            case dataState of 
                StateUpdated sUsers dKind ->
                    case dKind of 
                        Selected sSelected rnkId user status ->
                            ( AppOps walletState
                                    dataState
                                    appInfo
                                    uiState
                                    txRec
                                , httpDeleteSelectedRankingFromJsonBin (Utils.MyUtils.stringFromRankingId rnkId)
                                )
                        _ ->
                            ( model, Cmd.none )
                _ ->
                    ( model, Cmd.none )




        (ReturnedFromDeletedSelectedRankingFromJsonBin result, AppOps walletState dataState appInfo uiState txRec )  ->
            -- nb. you haven't used the result!
            case dataState of
                StateUpdated sUsers dKind ->
                    case dKind of 
                        Global sGlobal rnkId user ->
                            let
                                userRankingToDelete = Data.Global.gotUserRanking sGlobal user.ethaddress
                                newGlobal = Data.Global.removeUserRanking sGlobal userRankingToDelete
                                newDataKind = Global newGlobal rnkId user
                                newDataState = StateUpdated sUsers newDataKind
                                
                                _ = Debug.log "Ranking removed on return from id deleted? " Data.Global.asList newGlobal
                                
                            in 
                                ( AppOps walletState
                                    newDataState
                                    appInfo
                                    uiState
                                    txRec
                                , httpDeleteSelectedRankingFromGlobalList newGlobal
                                )
                        _ ->
                            ( model, Cmd.none )
                _ ->
                    ( model, Cmd.none )


        (ReturnedFromDeletedRankingFromGlobalList updatedListAfterRankingDeletedFromGlobalList, AppOps walletState dataState appInfo uiState txRec )  ->
            case dataState of 
                StateUpdated sUsers dKind ->
                    case dKind of 
                        Selected sSelected rnkId user status ->       
                            let
                                newGlobal = Data.Global.createdGlobal (updatedListAfterRankingDeletedFromGlobalList) sUsers
                                newDataKind = Global newGlobal rnkId user
                                newDataState = StateUpdated sUsers newDataKind
                                
                                _ = Debug.log "Ranking removed on return from list updated? " Data.Global.asList newGlobal
                                
                            in
                                ( AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )


                        _ -> 
                                    let 
                                        _ = Debug.log "11 - dataState should be Selected" dataState
                                    in
                                        (model, Cmd.none)
                _ -> 
                    
                        (model, Cmd.none)

        (WatchTxHash (Ok txHash), AppOps walletState dataState appInfo uiState txRec ) ->
                    let
                        _ =
                            Debug.log "WatchTxHash in wallet operational " "Ok - hash watched and all ok"
                    in
                    case walletState of 
                        SR.Types.WalletOperational -> 
                            ( AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings { txRec | txHash = Just txHash }, Cmd.none )
                        _ ->
                            (model, Cmd.none)

        (WatchTxHash (Err err),  AppOps walletState dataState appInfo uiState txRec ) ->
            let
                _ =
                    Debug.log "WatchTxHash" "Err"
            in
                case walletState of 
                    SR.Types.WalletOperational -> 
                        ( AppOps SR.Types.WalletStateMissing dataState appInfo SR.Types.UIRenderAllRankings { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )
                    _ -> 
                        (model, Cmd.none)
        
        
        (WatchTx (Ok tx),  AppOps walletState dataState appInfo uiState txRec ) ->
            let
                _ =
                    Debug.log "WatchTx" "tx Ok"
            in
            case walletState of 
                SR.Types.WalletOperational -> 
                    AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings { txRec | tx = Just tx } |> update (ProcessResult SR.Types.Won)
                _ ->
                    (model, Cmd.none)

        (WatchTx (Err err),  AppOps walletState dataState appInfo uiState txRec ) ->
            let
                _ =
                    Debug.log "WatchTx tx err" err
            in
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps SR.Types.WalletStateLocked dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )
                _ -> 
                    (model, Cmd.none)
        
        
        (WatchTxReceipt (Ok txReceipt),  AppOps walletState dataState appInfo uiState txRec ) ->
            let
                _ =
                    Debug.log "handleWalletStateOpenedAndOperational Receipt" txReceipt
            in
            case walletState of 
                SR.Types.WalletOperational ->
                    AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings emptyTxRecord
                        |> update (ProcessResult SR.Types.Won)
                _ -> 
                    (model, Cmd.none)

        (WatchTxReceipt (Err err),  AppOps walletState dataState appInfo uiState txRec ) ->
            let
                _ =
                    Debug.log "tx err" err
            in
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )
                _ ->
                    (model, Cmd.none)
        
        (TrackTx blockDepth,  AppOps walletState dataState appInfo uiState txRec ) ->
            let
                _ =
                    Debug.log "TrackTx" "TrackTx"
            in
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | blockDepth = Just blockDepth }, Cmd.none )
                _ -> 
                    (model, Cmd.none)

        -- this is the response from addedUserAsFirstPlayerInNewList Cmd
        -- it had the Http.expectStringResponse in it
        -- it's already created the new ranking with current player as the first entry
        -- the result now is the ranking id only at this point which was pulled out by the decoder
        (SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder,  AppOps walletState dataState appInfo uiState txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    case dataState of 
                            StateFetched sUsers dKind ->
                                case dKind of 
                                    Global sGlobal rnkId user ->
                                        let
                                            extractedRankingId = Data.Global.gotNewRankingIdFromWebData idValueFromDecoder
                                            newSGlobal = Data.Global.addUserRanking sGlobal extractedRankingId appInfo.selectedRanking appInfo.user
                                            newGlobalAsList = Data.Global.rankingsAsList newSGlobal
                                            newGlobalUpdated = Global newSGlobal rnkId user
                                            newDataState = StateUpdated sUsers newGlobalUpdated
                                        in
                                            ( AppOps SR.Types.WalletOperational newDataState appInfo SR.Types.UICreateNewLadder emptyTxRecord
                                            ,
                                            httpPutRequestForAddGlobal (Data.Global.newJsonEncodedList (newGlobalAsList)) newGlobalAsList
                                            )
                                    _ -> 
                                        let 
                                            _ = Debug.log "dataState should be Global" dataState
                                        in
                                            (model, Cmd.none)
                            _ -> 
                                let 
                                    _ = Debug.log "4 - dataState" dataState
                                in
                                    (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)

        -- SentUserInfoAndDecodedResponseToNewUser serverResponse ->
        --     ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

        

        (ResetRejectedNewUserToShowGlobal,  AppOps walletState dataState appInfo uiState txRec ) ->
            let 
                newAppInfo = {appInfo | user = SR.Defaults.emptyUser}
            in 
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps SR.Types.WalletOperational dataState newAppInfo SR.Types.UIRenderAllRankings emptyTxRecord, gotGlobal )
                _ -> 
                    (model, Cmd.none)


        (ClickedConfirmCreateNewLadder,  AppOps walletState dataState appInfo uiState txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    case dataState of 
                        StateFetched sUsers dKind ->
                                case dKind of 
                                    Global sGlobal rnkId user ->
                                        --if Data.Users.isRegistered (Data.Users.asList sUsers) appInfo.user then
                                        if user.ethaddress /= "" then
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
                                                
                                                
                                            in
                                            ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState newAppInfo SR.Types.UIRenderAllRankings { txRec | txSentry = newSentry }
                                            --Cmd.batch [ sentryCmd, addedUserAsFirstPlayerInNewList appInfo.user ] )
                                            ,sentryCmd)

                                        else
                                            ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIRegisterNewUser emptyTxRecord, Cmd.none )

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

        (AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList,  AppOps walletState dataState appInfo uiState txRec ) ->
            -- I think the global set has already been updated
            case walletState of 
                SR.Types.WalletOperational ->
                    ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )
                _ ->
                    (model, Cmd.none)
            
        (ClickedNewChallengeConfirm,  AppOps walletState dataState appInfo uiState txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    case dataState of
                        StateFetched sUsers dKind -> 
                            case dKind of 
                                Selected sSelected rnkId user status ->
                                    let
                                        newDataKind = Selected (Data.Selected.assignChallengerAddrsForBOTHPlayers sSelected appInfo) rnkId user status
                                        newDataState = StateUpdated sUsers newDataKind
                                        updatedModel = AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings txRec
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


        (ClickedJoinSelected,  AppOps walletState dataState appInfo uiState txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    case dataState of
                        StateUpdated sUsers dKind -> 
                            case dKind of 
                                    Selected sSelected rnkId user status ->
                                        if Data.Users.isRegistered (Data.Users.asList sUsers) appInfo.user then
                                            let
                                                newLUPlayer = Data.Selected.userAdded sUsers appInfo.selectedRanking.id (Data.Selected.asList sSelected) appInfo.user
                                                newSelected = Data.Selected.asSelected (EverySet.fromList newLUPlayer) sUsers rnkId
                                                
                                                newDataKind = Selected newSelected  rnkId user SR.Types.UserIsMember
                                                newDataState = StateFetched sUsers newDataKind
                                                updatedModel = AppOps walletState newDataState appInfo SR.Types.UIRenderAllRankings txRec
                                            in
                                            ( updatedModel, httpPlayerList (newDataState))
                                        else
                                            ( AppOps walletState dataState appInfo SR.Types.UIRegisterNewUser txRec, Cmd.none )
                                    _ -> 
                                        let 
                                            _ = Debug.log "12 - dataState should be Selected" dataState
                                        in
                                            (model, Cmd.none)

                        _ -> 
                                    let 
                                        _ = Debug.log "12 - dataState" dataState
                                    in
                                        (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)

        (ReturnFromPlayerListUpdate response,  AppOps walletState dataState appInfo uiState txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    case dataState of 
                        StateFetched sUsers dKind ->
                            case dKind of 
                                Selected sSelected rnkId user status ->  
                                    let
                                        lplayer =
                                            Data.Players.extractPlayersFromWebData response

                                        convertedToUserPlayers =
                                            Data.Selected.convertPlayersToUserPlayers
                                                lplayer
                                                (Data.Users.asList sUsers)

                                        
                                    in
                                    ( updateSelectedRankingPlayerList model convertedToUserPlayers, Cmd.none )
                                
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

            

        (ReturnFromUserListUpdate response,  AppOps walletState dataState appInfo uiState txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
                    let 
                        lusers = Data.Users.extractUsersFromWebData response
                        newDataKind = Users appInfo.user
                        resetUserList = StateUpdated (Data.Users.asUsers (EverySet.fromList lusers)) newDataKind
                    in
                        (AppOps walletState resetUserList appInfo uiState txRec, Cmd.none)
                _ ->
                    (model, Cmd.none)

        (TimeUpdated posixTime,  AppOps walletState dataState appInfo uiState txRec ) ->
            let
                _ =
                    Debug.log "posixtime" posixTime
            in
            ( model, Cmd.none )
        
        

        (ClickedCreateNewUserToWallet userInfo,  AppOps walletState dataState appInfo uiState txRec ) ->
            case walletState of 
                SR.Types.WalletOperational ->
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

                        -- we need to send a user obj to createNewUser, not just the addr
                        -- because it will update the other input details on the obj
                        userWithUpdatedAddr =
                            { userInfo | ethaddress = userInfo.ethaddress }

                        newAppInfo =
                            { appInfo | user = userWithUpdatedAddr, appState = SR.Types.AppStateCreateNewUser }
                    in
                    ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState newAppInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                    , sentryCmd)
                _ -> 
                    (model, Cmd.none)

                
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
        
       


updateAppInfoOnRankingSelected : SR.Types.AppInfo -> Internal.Types.RankingId -> String -> String -> SR.Types.AppInfo
updateAppInfoOnRankingSelected appInfo rnkid rnkownerstr rnknamestr =
    let
        newSelectedRanking =
            appInfo.selectedRanking

        newRnkInfo =
            { newSelectedRanking | id = Utils.MyUtils.stringFromRankingId rnkid, rankingowneraddr = rnkownerstr, rankingname = rnknamestr }

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
            ( AppOps SR.Types.WalletWaitingForTransactionReceipt dataState appInfo SR.Types.UIWaitingForTxReceipt txRec
            , Cmd.none
            )

        TxSentryMsg subMsg ->
            let
                _ =
                    Debug.log "handleTxSubMsg subMsg" <| handleTxSubMsg subMsg
            
            
                ( subModel, subCmd ) =
                    Eth.Sentry.Tx.update subMsg txRec.txSentry
            in
            if handleTxSubMsg subMsg then
                case appInfo.appState of
                    SR.Types.AppStateCreateNewLadder -> 
                        let 
                            _ =
                                Debug.log "in CreateNewLadder" "yes"
                        in
                        ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }
                        , Cmd.batch [subCmd, addedUserAsFirstPlayerInNewList appInfo.user] )
                    
                    SR.Types.AppStateCreateNewUser -> 
                        let 
                            _ =
                                Debug.log "in CreateNewUser" "yes"

                            userSet = case dataState of 
                                        StateFetched users dKind -> 
                                            users 
                                        StateUpdated users user ->
                                            users 
                                        _ -> 
                                            Data.Users.emptyUsers
                        in
                        ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }
                        , Cmd.batch [subCmd,  createNewUser ( Data.Users.asList userSet) appInfo.user] )

                    SR.Types.AppStateEnterWon -> 
                        let 
                            _ =
                                Debug.log "in AppStateEnterWon" "yes"
                        in
                        case dataState of 
                            StateFetched sUsers dKind -> 
                                let 
                                    newDataState = StateUpdated sUsers dKind
                                    _ =
                                        Debug.log "handleTxSubMsg subMsg  dataState" newDataState
                                in 
                        
                                    (AppOps walletState newDataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Won) )
                            
                            _ ->
                                ( AppOps walletState dataState appInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )
                          

                    SR.Types.AppStateEnterLost ->
                        case dataState of
                            StateFetched sUsers dKind -> 
                                let
                                    _ =
                                        Debug.log "in AppStateEnterLost" "yes"

                                    newDataState = StateUpdated sUsers dKind
                                in
                                    (AppOps SR.Types.WalletOperational newDataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Lost))
                            _ ->
                                ( AppOps walletState dataState appInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )
                    
                    SR.Types.AppStateEnterUndecided -> 
                        case dataState of
                            StateFetched sUsers dKind -> 
                                let
                                    _ =
                                       Debug.log "in AppStateEnterUndecided" "yes"

                                    newDataState = StateUpdated sUsers dKind
                                in
                                    ( AppOps SR.Types.WalletOperational newDataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Undecided))
                            _ ->
                                ( AppOps walletState dataState appInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )

                    _ -> 
                       ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }, subCmd ) 

            else
                ( AppOps walletState dataState appInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )

        WatchTxHash (Ok txHash) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput" "watch tx hash"
            in
            ( AppOps walletState dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txHash = Just txHash }, Cmd.none )

        WatchTxHash (Err err) ->
            ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTx (Ok tx) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput" "tx ok"
            in
      
            (AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings { txRec | tx = Just tx }, Cmd.none )

        WatchTx (Err err) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput tx err" err
            in
            ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTxReceipt (Ok txReceipt) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput tx ok" txReceipt
            in
            AppOps walletState dataState appInfo SR.Types.UIRenderAllRankings { txRec | txReceipt = Just txReceipt } |> update (ProcessResult SR.Types.Won)

        WatchTxReceipt (Err err) ->
            let
                _ =
                    Debug.log "tx err" err
            in
            ( AppOps SR.Types.WalletOperational dataState appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

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
            let
                newAppInfoUser =
                    appInfo.user

                newUserWithAddr =
                    { newAppInfoUser | ethaddress = Eth.Utils.addressToString uaddr }

                newAppInfo =
                    { appInfo | user = newUserWithAddr }
            in
            newAppInfo


-- assignChallengerAddrsForBOTHPlayers : Model -> ( Model, Cmd Msg )
-- assignChallengerAddrsForBOTHPlayers model =
--     case model of
--         AppOps walletState dataState appInfo uiState txRec ->
--             --let
--                 case dataState of 
--                     Selected sselected sUsers rnkId ->
--                     -- List SR.Types.UserPlayer -> SR.Types.UserPlayer -> String -> List SR.Types.UserPlayer
--                         --lassignedChallengerAddress = Data.Selected.assignChallengerAddr dataState.userPlayers appInfo.player appInfo.challenger.player.address
--                         let
--                             sUserUpdated = Data.Selected.assignChallengerAddr sselected appInfo.player appInfo.challenger.player.address
--                             sChallengerUpdated = StateUpdated (Data.Selected.assignChallengerAddr sUserUpdated appInfo.challenger appInfo.player.player.address) sUsers rnkId
--                             updatedModel = AppOps walletState sChallengerUpdated appInfo SR.Types.UIRenderAllRankings txRec
--                         in 
--                             ( updatedModel, httpPlayerList (sChallengerUpdated))
                



handleNewUserInputs : Model -> Msg -> Model
handleNewUserInputs model msg =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            case msg of
                NewUserNameInputChg namefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | username = namefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState dataState newAppInfo SR.Types.UIRegisterNewUser txRec

                NewUserDescInputChg descfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | description = descfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState dataState newAppInfo SR.Types.UIRegisterNewUser txRec

                NewUserEmailInputChg emailfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | email = emailfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState dataState newAppInfo SR.Types.UIRegisterNewUser txRec

                NewUserMobileInputChg mobilefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | mobile = mobilefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState dataState newAppInfo SR.Types.UIRegisterNewUser txRec

                _ ->
                    Failure "NewUserNameInputChg"

        _ ->
            Failure "NewUserNameInputChg"


handleExistingUserInputs : Model -> Msg -> Model
handleExistingUserInputs model msg =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            case msg of
                ExistingUserNameInputChg namefield ->
                    model

                ExistingUserDescInputChg descfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | description = descfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState dataState newAppInfo SR.Types.UIUpdateExistingUser txRec

                ExistingUserEmailInputChg emailfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | email = emailfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState dataState newAppInfo SR.Types.UIUpdateExistingUser txRec

                ExistingUserMobileInputChg mobilefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | mobile = mobilefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState dataState newAppInfo SR.Types.UIUpdateExistingUser txRec

                _ ->
                    Failure "ExistingUserNameInputChg"

        _ ->
            Failure "ExistingUserNameInputChg"


updatedForChallenge : Model -> List SR.Types.UserPlayer -> SR.Types.UserPlayer -> SR.Types.User -> Model
updatedForChallenge model luplayer opponentAsPlayer userMaybeCanDelete =
    case model of
        AppOps walletState dataState appInfo _ txRec ->
            let 
                _ = Debug.log "updatedForChallenge 1 - dataState" dataState

            in
            case dataState of
                StateFetched sUsers dKind -> 
                    case dKind of 
                            Selected sSelected rnkId user status -> 
                                let 

                                    _ = Debug.log "updatedForChallenge - dataState" dataState

                                    newAppInfoWithPlayer = { appInfo | player = Data.Selected.gotCurrentUserAsPlayerFromPlayerList luplayer user }
                        
                                    newAppInfoWithChallengerAndPlayer = { newAppInfoWithPlayer | challenger = opponentAsPlayer }
                                  
                                    newDataKind =  Selected (Data.Selected.updateSelectedRankingOnChallenge sSelected newAppInfoWithChallengerAndPlayer) rnkId user status
                                    newDataState = StateFetched sUsers newDataKind
                                in
                                    AppOps walletState newDataState newAppInfoWithChallengerAndPlayer SR.Types.UIChallenge txRec
                            
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
        AppOps walletState dataState appInfo uiState txRec ->
                case dataState of
                    StateUpdated sUsers dKind -> 
                        case dKind of 
                                Selected sSelected rnkId user status ->
                                        let 
                                            --todo: another way around this here? Problem is ensuredCorrectSelectedUI needs the global set:
                                            --newUiState = ensuredCorrectSelectedUI appInfo dataState 
                                            newDataKind = Selected (Data.Selected.asSelected (EverySet.fromList luplayers) sUsers rnkId) rnkId user status
                                            newDataState = StateUpdated sUsers newDataKind 
                                        in
                                            AppOps walletState newDataState appInfo uiState txRec

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
--         AppOps walletState dataState appInfo uiState txRec ->
--             case dataState of 
--                 Selected sSelected sUsers _ ->
--                     let
--                         newSSelected = Data.Selected.asSelected (EverySet.fromList luplayer ) sUsers (Internal.Types.RankingId appInfo.selectedRanking.id)

--                         stateToSelected = Selected newSSelected sUsers (Internal.Types.RankingId appInfo.selectedRanking.id)
                        
--                         newAppPlayer = { appInfo | player = Data.Selected.gotUserPlayerFromPlayerListStrAddress luplayer appInfo.user.ethaddress }

--                         newAppChallengerAndPlayer = { newAppPlayer | challenger = Data.Selected.gotUserPlayerFromPlayerListStrAddress luplayer newAppPlayer.player.player.challengeraddress }

--                         --_ = Debug.log "in populatedSelected" <| stateToSelected
                    
--                     in
--                         AppOps walletState stateToSelected newAppChallengerAndPlayer uiState emptyTxRecord
--                 _ ->
--                     Failure <| "populatedSelected : "
--         _ ->
--             Failure <| "populatedSelected : "



-- view


view : Model -> Html Msg
view model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            case uiState of
                SR.Types.UICreateNewLadder ->
                    inputNewLadderview model

                SR.Types.UISelectedRankingUserIsOwner ->
                    case dataState of
                        StateFetched sUsers dKind -> 
                            case dKind of 
                                    Selected sSelected rnkId user status ->
                                        let 
                                            _ = Debug.log "in Selected ready for view1" (Data.Selected.asList sSelected)
                                        in
                                            selectedUserIsOwnerView dataState appInfo
                                    _ -> 
                                        greetingView <| "Should be Selected"
                        _ -> 
                            greetingView <| "Owner View error"
                    

                SR.Types.UISelectedRankingUserIsPlayer ->
                    selectedUserIsPlayerView dataState appInfo
                    -- case dataState of 
                    --     StateFetched sUsers dKind ->
                    --         case dKind of 
                    --             Selected sSelected _ user status -> 
                    --                 selectedUserIsPlayerView dataState appInfo
                                
                    --             _ -> 
                    --                 greetingView <| "View error - should be Selected"

                    --     StateUpdated sUsers dKind ->
                    --         case dKind of 
                    --             Selected sSelected _ user status -> 
                    --                 selectedUserIsPlayerView dataState appInfo
                                
                    --             _ -> 
                    --                 greetingView <| "View error - should be Selected"
                        
                        -- _ -> 
                        --     greetingView <| "View error"

                SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer ->
                    let 
                        _ = Debug.log "UISelectedRankingUserIsNeitherOwnerNorPlayer" "here"
                    in
                    case dataState of
                        StateFetched sUsers dKind -> 
                            case dKind of 
                                    Selected sSelected rnkId user status ->
                                        
                                        selectedUserIsNeitherOwnerNorPlayerView dataState appInfo
                                    _ -> 
                                        greetingView <| "shuld be Selected"
                        _ -> 
                            greetingView <| "View error"

                SR.Types.UIRenderAllRankings ->
                    case dataState of 
                        StateFetched sUsers dKind ->
                             case dKind of 
                                Global sGlobal rnkId user ->
                                    globalResponsiveview sGlobal appInfo.user ""
                                _ ->
                                    greetingView <| "Should be Global 1"
                        
                        StateUpdated sUsers dKind ->
                             case dKind of
                                Global sGlobal rnkId user  -> 
                                    globalResponsiveview sGlobal appInfo.user "Your Settings Have Been Updated"

                                _ ->
                                    greetingView <| "Should be updated Global"
                        _ ->
                            greetingView <| "No rankings to display ..."


                SR.Types.UIEnterResult ->
                    displayResultBeforeConfirmView model

                SR.Types.UIEnterResultTxProblem ->
                    txErrorView model

                SR.Types.UIChallenge ->
                    displayChallengeBeforeConfirmView model

                SR.Types.UILoading ->
                    greetingView <| "Loading ..."

                SR.Types.UIWalletMissingInstructions ->
                
                    greetingView <|
                        """Your Ethereum  
wallet browser
extension is MISSING. Please 
install Metamask (or similar)     
in Chrome extensions 
before continuing and
refresh the browser"""

                SR.Types.UIDisplayWalletLockedInstructions ->
                    continueView <|
                        """Your Ethereum  
wallet browser
extension is LOCKED. Please 
use your wallet      
password to open it 
before continuing and
refresh the browser"""

                SR.Types.UIDeleteRankingConfirm ->
                    deleteRankingview model

                -- SR.Types.UIDeleteRankingInform ->
                --     deletedRankingInformView model


                SR.Types.UIRegisterNewUser ->
                    let 
                        _ = Debug.log "UIregister new " walletState
                    in 
                            inputNewUserview dataState appInfo
                       
                    

                SR.Types.UIUpdateExistingUser ->
                     let 
                        _ = Debug.log "UIUpdateExistingUser new " walletState
                    in 
                    updateExistingUserView model

                _ ->
                    greetingView <| "Loading ... "

        Failure str ->
            greetingView <| "Model failure in view: " ++ str


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
            { onPress = Just (ClickedSelectedOwnedRanking (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr rankingobj.rankingname)
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
memberRankingInfoBtn rankingobj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.primary) <|
            { onPress = Just (ClickedSelectedMemberRanking (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr rankingobj.rankingname)
            , label = Element.text rankingobj.rankingname
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
            { onPress = Just (ClickedSelectedNeitherOwnerNorMember (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr rankingobj.rankingname)
            , label = Element.text rankingobj.rankingname
            }
        ]



playerbuttons : DataState -> SR.Types.AppInfo -> Element Msg
playerbuttons dataState appInfo =
     case dataState of
        StateFetched sUsers dKind -> 
            case dKind of 
                    Selected sSelected rnkId user status ->
                        Element.column Grid.section <|
                            [ SR.Elements.selectedRankingHeaderEl appInfo.selectedRanking
                            , Element.column (Card.simple ++ Grid.simple) <|
                                insertPlayerList dataState appInfo
                            ]

                    _ ->
                        Element.text "Error1"
        _ ->
            Element.text "Error1"




configureThenAddPlayerRankingBtns : Data.Selected.Selected -> SR.Types.AppInfo -> SR.Types.UserPlayer -> Element Msg
configureThenAddPlayerRankingBtns sSelected appInfo uplayer =
   -- nb. 'uplayer' is the player that's being mapped cf. appInfo.player which is current user as player (single instance)
                let
                    printChallengerNameOrAvailable = Data.Selected.printChallengerNameOrAvailable sSelected uplayer
                in
                if Data.Selected.isUserPlayerMemberOfSelectedRanking (Data.Selected.asList sSelected) appInfo.user then
                    
                    if Data.Selected.isPlayerCurrentUser appInfo.user uplayer then
                        --if isCurrentUserInAChallenge then
                        if Data.Selected.isChallenged sSelected uplayer then
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
                    else if Data.Selected.isChallenged sSelected uplayer then
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.disabled) <|
                                { onPress = Nothing
                                , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                                }
                            ]
                        -- else if - this uplayer isn't the current user but is being challenged

                    else if Data.Selected.isChallenged sSelected uplayer then
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.disabled) <|
                                { onPress = Nothing
                                , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ uplayer.user.username ++ " vs " ++ printChallengerNameOrAvailable
                                }
                            ]
                    else
                    -- this uplayer isn't the current user and isn't challenged by anyone
                        if not (Data.Selected.isChallenged sSelected uplayer) then
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
                    StateFetched sUsers dKind -> 
                        case dKind of 
                                Selected sSelected rnkId user status ->
                                    let
                                       
                                        mapOutPlayerList =
                                            List.map
                                                (configureThenAddPlayerRankingBtns sSelected appInfo)
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
                    { onPress = Just <| ResetToShowGlobal
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.danger) <|
                    { onPress = Just <| ClickedDeleteRanking user.ethaddress
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
                    { onPress = Just <| ResetToShowGlobal
                    , label = Element.text "Home"
                    }
                ]
            ]
        ]


selecteduserIsNeitherPlayerNorOwnerHomebutton : SR.Types.User -> Element Msg
selecteduserIsNeitherPlayerNorOwnerHomebutton user =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal
                    , label = Element.text "Home"
                    }
                , displayJoinBtnNewOrExistingUser user

                -- Input.button ([ Element.htmlAttribute (Html.Attributes.id "newUserJoinbtn") ] ++ Button.simple ++ Color.info) <|
                --     { onPress = Just ClickedJoinSelected
                --     , label = Element.text "Join"
                --     }
                ]
            ]
        ]


displayJoinBtnNewOrExistingUser : SR.Types.User -> Element Msg
displayJoinBtnNewOrExistingUser user =
    if user.username == "" then
        Input.button ([ Element.htmlAttribute (Html.Attributes.id "newUserJoinbtn") ] ++ Button.simple ++ Color.info) <|
            { onPress = Just ClickedConfirmedRegisterNewUser
            , label = Element.text "Join"
            }

    else
        Input.button ([ Element.htmlAttribute (Html.Attributes.id "existingUserJoinbtn") ] ++ Button.simple ++ Color.info) <|
            { onPress = Just ClickedJoinSelected
            , label = Element.text "Join"
            }


newrankingconfirmbutton : SR.Types.AppInfo -> DataState -> Element Msg
newrankingconfirmbutton appInfo dataState =
    case dataState of 
            StateFetched sUsers dKind ->
                 case dKind of 
                    Global sGlobal rnkId user ->
                        Element.column Grid.section <|
                            [ Element.el Heading.h6 <| Element.text "Click to continue ..."
                            , Element.column (Card.simple ++ Grid.simple) <|
                                [ Element.wrappedRow Grid.simple <|
                                    [ Input.button (Button.simple ++ Color.simple) <|
                                        { onPress = Just <| ResetToShowGlobal
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
            StateUpdated sUsers dKind ->
                 case dKind of 
                    Global sGlobal rnkId user ->
                        Element.column Grid.section <|
                            [ Element.el Heading.h6 <| Element.text "Click to continue ..."
                            , Element.column (Card.simple ++ Grid.simple) <|
                                [ Element.wrappedRow Grid.simple <|
                                    [ Input.button (Button.simple ++ Color.simple) <|
                                        { onPress = Just <| ResetToShowGlobal
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

-- continueAfterDelRankingBtn : SR.Types.AppInfo -> DataState -> Element Msg
-- continueAfterDelRankingBtn appInfo dataState =
--     case dataState of 
--             StateUpdated sUsers dKind ->
--                  case dKind of 
--                     Global sGlobal rnkId user ->
--                         Element.column Grid.section <|
--                             [ Element.el Heading.h6 <| Element.text "Click to continue ..."
--                             , Element.column (Card.simple ++ Grid.simple) <|
--                                 [ Element.wrappedRow Grid.simple <|
--                                     [ Input.button (Button.simple ++ Color.simple) <|
--                                         { onPress = Just <| ResetToShowGlobal
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
        AppOps walletState dataState appInfo uiState txRec ->
            
            Element.column Grid.section <|
                [ Element.el Heading.h6 <| Element.text <| " Your opponent's details: "
                , Element.paragraph (Card.fill ++ Color.info) <|
                    [ Element.el [] <| Element.text <| appInfo.user.username ++ " you are challenging " ++ appInfo.challenger.user.username
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
        AppOps walletState dataState appInfo uiState txRec ->
            case dataState of
                StateFetched sUsers dKind -> 
                    case dKind of 
                        Selected sSelected rnkId user status ->
                            let
                                playerAsUser =
                                    
                                    Data.Users.gotUser sUsers appInfo.player.player.address

                                challengerAsUser =
                                
                                    Data.Users.gotUser sUsers appInfo.challenger.player.address
                            in
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
        AppOps walletState dataState appInfo uiState txRec ->
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
    if String.length rankingInfo.rankingdesc <= 20 then
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


newuserConfirmPanel : SR.Types.User -> List SR.Types.User -> Element Msg
newuserConfirmPanel user luser =
    Element.column Grid.section <|
        [ SR.Elements.warningParagraph
        , Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.info) <|
                    { onPress = Just <| ResetToShowGlobal
                    , label = Element.text "Cancel"
                    }
                , Input.button (Button.simple ++ enableButton (isValidatedForAllUserDetailsInput user luser False)) <|
                    { onPress = Just <| ClickedEnableEthereum
                    , label = Element.text "Enable Ethereum"
                    }
                , Input.button (Button.simple ++ enableButton (isValidatedForAllUserDetailsInput user luser False)) <|
                    { onPress = Just <| ClickedCreateNewUserToWallet user
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
                    { onPress = Just <| ResetToShowGlobal
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


inputNewUser : DataState -> SR.Types.AppInfo -> Element Msg
inputNewUser dataState appInfo =
    case dataState of
            StateUpdated sUsers dKind -> 
                case dKind of 
                    Selected sSelected rnkId user status -> 
                        Element.column Grid.section <|
                            [ Element.el Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                            , Element.wrappedRow (Card.fill ++ Grid.simple)
                                [ Element.column
                                    Grid.simple
                                    [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                                        { onChange = NewUserNameInputChg
                                        , text = appInfo.user.username
                                        , placeholder = Nothing
                                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username*")
                                        }
                                    , nameValidationErr appInfo sUsers
                                    , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userDescription") ])
                                        { onChange = NewUserDescInputChg
                                        , text = appInfo.user.description
                                        , placeholder = Nothing
                                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Description")
                                        }
                                    , userDescValidationErr appInfo.user
                                    , Input.email (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userEmail") ])
                                        { onChange = NewUserEmailInputChg
                                        , text = appInfo.user.email
                                        , placeholder = Nothing
                                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Email")
                                        }
                                    , emailValidationErr appInfo.user
                                    , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userMobile") ])
                                        { onChange = NewUserMobileInputChg
                                        , text = Utils.Validation.Validate.validatedMaxTextLength appInfo.user.mobile 25
                                        , placeholder = Nothing
                                        , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Mobile")
                                        }
                                    , mobileValidationErr appInfo.user
                                    ]
                                ]
                            , Element.text "* required and CANNOT be changed \nunder current ETH account"
                            , SR.Elements.justParasimpleUserInfoText
                            ]

                    _ ->
                        Element.text "Fail on inputNewUser"
            _ ->
                        Element.text "Fail on inputNewUser"


inputUpdateExistingUser : Model -> Element Msg
inputUpdateExistingUser model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                , Element.wrappedRow (Card.fill ++ Grid.simple)
                    [ Element.column
                        Grid.simple
                        [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ Color.disabled)
                            { onChange = ExistingUserNameInputChg
                            , text = appInfo.user.username
                            , placeholder = Just <| Input.placeholder [] <| Element.text "yah placeholder"
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username")
                            }
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userDescription") ])
                            { onChange = ExistingUserDescInputChg
                            , text = appInfo.user.description
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Description")
                            }
                        , userDescValidationErr appInfo.user
                        , Input.email (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userEmail") ])
                            { onChange = ExistingUserEmailInputChg
                            , text = appInfo.user.email
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Email")
                            }
                        , emailValidationErr appInfo.user
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userMobile") ])
                            { onChange = ExistingUserMobileInputChg
                            , text = Utils.Validation.Validate.validatedMaxTextLength appInfo.user.mobile 25
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Mobile")
                            }
                        , mobileValidationErr appInfo.user
                        ]
                    ]

                --, Element.text "* required"
                ]

        _ ->
            Element.text "Fail on inputNewUser"


nameValidationErr : SR.Types.AppInfo -> Data.Users.Users -> Element Msg
nameValidationErr appInfo sUsers =
    if Data.Users.isNameValidationErr appInfo.user.username sUsers then 
        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "usernameValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "Username OK!")

    else
        Element.el
            (List.append [ Element.htmlAttribute (Html.Attributes.id "usernameValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text """Username must be unique
and between 4-8 characters""")


ladderNameValidationErr : SR.Types.AppInfo -> DataState -> Element Msg
ladderNameValidationErr appInfo dataState =
    case dataState of 
        StateFetched sUsers dKind ->
            case dKind of 
                Global sGlobal rnkId user ->
                    if Data.Rankings.isRankingNameValidated appInfo.selectedRanking (Data.Global.asList sGlobal) then
                        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "laddernameValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "Ladder name OK!")

                    else
                        Element.el
                            (List.append [ Element.htmlAttribute (Html.Attributes.id "laddernameValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                                ++ [ Element.moveLeft 0.0 ]
                            )
                            (Element.text """Ladder name must be unique
                and between 4-8 characters""")

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
                -- Widget imported but not currently used
                -- , Widget.textInput
                --     { chips = [](Button msg)
                --     , 
                        {onChange = LadderDescInputChg
                        , text =  Utils.Validation.Validate.validatedMaxTextLength appInfo.selectedRanking.rankingdesc 20
                        , placeholder = Nothing
                        , label = Input.labelLeft Input.label <| Element.text "Desc:"
                        , spellcheck = False
                        }
                , Element.text "* Required"
                , ladderDescValidationErr appInfo.selectedRanking
                ]
            ]
        ]



globalResponsiveview : Data.Global.Global -> SR.Types.User -> String -> Html Msg
globalResponsiveview sGlobal user updatedStr =

    let
        userName =
            if user.username /= "" then
                user.username

            else
                "New User"
    in
    Framework.responsiveLayout
        []
    <|
        Element.column
            Framework.container
            [ Element.el (Heading.h5 ++ [ Element.htmlAttribute (Html.Attributes.id "globalHeader") ]) <|
                Element.text ("SportRank - " ++ userName)
            , displayUpdateProfileBtnIfExistingUser user.username
            , Element.text ("\n" ++ updatedStr)
            , displayCreateNewLadderBtnIfExistingUser user.username (Data.Global.asList (Data.Global.gotOwned sGlobal user)) ClickedCreateNewLadder
            , displayEnableEthereumBtn
            , Element.text ("\n" ++ updatedStr)
            , displayRegisterBtnIfNewUser
                user.username
                ClickedRegister
            , ownedrankingbuttons (Data.Global.asList (Data.Global.gotOwned sGlobal user)) user
            , memberrankingbuttons (Data.Global.gotMember sGlobal user) user
            , otherrankingbuttons (Data.Global.asList (Data.Global.gotOthers sGlobal user)) user
            ]


-- displayUpdateProfileBtnIfExistingUser : String -> Msg -> Element Msg
-- displayUpdateProfileBtnIfExistingUser uname msg =
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
            { onPress = Just <| ClickedEnableEthereum
            , label = Element.text "Enable Ethereum"
            }

selectedUserIsOwnerView : DataState -> SR.Types.AppInfo -> Html Msg
selectedUserIsOwnerView dataState appInfo =
    case dataState of
        StateFetched sUsers dKind -> 
            case dKind of 
                    Selected sSelected rnkId user status ->
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ Element.el Heading.h4 <| Element.text <| "SportRank - Owner  - " ++ appInfo.user.username
                                , selecteduserIsOwnerhomebutton appInfo.user
                                , playerbuttons dataState appInfo
                                ]

                    _ ->
                        Html.text "Fail selectedUserIsOwnerView"
        _ ->
            Html.text "Fail selectedUserIsOwnerView"

selectedUserIsPlayerView : DataState -> SR.Types.AppInfo -> Html Msg
selectedUserIsPlayerView dataState appInfo =
    case dataState of
        StateFetched sUsers dKind -> 
            case dKind of 
                    Selected sSelected rnkId user status ->
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ appInfo.user.username
                                , selecteduserIsPlayerHomebutton appInfo.user
                                , playerbuttons dataState appInfo
                                ]

                    _ ->
                        Html.text "Error3"

        StateUpdated sUsers dKind -> 
            case dKind of 
                    Selected sSelected rnkId user status ->
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ appInfo.user.username
                                , Element.text <| "Challenge is On! Good luck!"
                                , selecteduserIsPlayerHomebutton appInfo.user
                                , playerbuttons dataState appInfo
                                ]
                    _ ->
                        Html.text "Error4"

        AllEmpty ->
                    Html.text "Please refresh your browser"


selectedUserIsNeitherOwnerNorPlayerView : DataState -> SR.Types.AppInfo -> Html Msg
selectedUserIsNeitherOwnerNorPlayerView dataState appInfo =
    case dataState of
        StateFetched sUsers dKind -> 
            case dKind of 
                Selected sSelected rnkId user status ->
                        Framework.responsiveLayout [] <|
                            Element.column
                                Framework.container
                                [ newOrExistingUserNameDisplay appInfo.user
                                , selecteduserIsNeitherPlayerNorOwnerHomebutton appInfo.user
                                , playerbuttons  dataState appInfo
                                ]

                _ ->
                    Html.text "Error4"
        _ ->
            Html.text "Error4"


newOrExistingUserNameDisplay : SR.Types.User -> Element msg
newOrExistingUserNameDisplay user =
    if user.username == "" then
        Element.el Heading.h4 <| Element.text <| "SportRank - New User - Join?"

    else
        Element.el Heading.h4 <| Element.text <| "SportRank - " ++ user.username ++ " - Join?"


inputNewUserview : DataState -> SR.Types.AppInfo -> Html Msg
inputNewUserview dataState appInfo =
            case dataState of 
                StateFetched sUsers dKind -> 
                    Framework.responsiveLayout [] <|
                        Element.column
                            Framework.container
                            [ Element.el Heading.h4 <| Element.text "Create New User"
                            , inputNewUser dataState appInfo
                            , newuserConfirmPanel appInfo.user (Data.Users.asList sUsers)
                            ]
                _ ->
                    Html.text "Fail inputNewUserview"


updateExistingUserView : Model -> Html Msg
updateExistingUserView model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            case dataState of 
                StateFetched sUsers dKind -> 
                    Framework.responsiveLayout [] <|
                        Element.column
                            Framework.container
                            [ Element.el Heading.h4 <| Element.text "Update User Profile"
                            , inputUpdateExistingUser model
                            , existingUserConfirmPanel appInfo.user (Data.Users.asList sUsers)
                            ]
                _ ->
                    Html.text "Fail updateExistingUserView"
        _ ->
            Html.text "Fail updateExistingUserView"


inputNewLadderview : Model -> Html Msg
inputNewLadderview model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text "4Create New Ladder Ranking"
                    , inputNewLadder appInfo dataState
                    , newrankingconfirmbutton appInfo dataState
                    , SR.Elements.footer
                    ]

        _ ->
            Html.text "Fail"

deleteRankingview : Model -> Html Msg
deleteRankingview model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
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

-- deletedRankingInformView : Model -> Html Msg
-- deletedRankingInformView model =
--     case model of
--         AppOps walletState dataState appInfo uiState txRec ->
--             Framework.responsiveLayout [] <|
--                 Element.column
--                     Framework.container
--                     [ Element.el Heading.h4 <| Element.text "Deleted Ranking"
--                     --, inputNewLadder appInfo dataState
--                     , continueAfterDelRankingBtn appInfo dataState
--                     --, newrankingconfirmbutton appInfo dataState
--                     , SR.Elements.footer
--                     ]

--         _ ->
--             Html.text "Fail"


displayChallengeBeforeConfirmView : Model -> Html Msg
displayChallengeBeforeConfirmView model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| appInfo.user.username ++ " - Confirm Challenge"
                    , confirmChallengebutton model
                    ]

        _ ->
            Html.text "Error5"


displayResultBeforeConfirmView : Model -> Html Msg
displayResultBeforeConfirmView model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
                case dataState of
                    StateFetched sUsers dKind -> 
                        case dKind of 
                                Selected sSelected rnkId user status ->
                                    let
                                        playerAsUser =
                                            Data.Users.gotUser sUsers appInfo.player.player.address
                                    in
                                        Framework.responsiveLayout [] <|
                                            Element.column
                                                Framework.container
                                                [ Element.el Heading.h4 <| Element.text <| playerAsUser.username ++ " - Result"
                                                , confirmResultbutton model
                                                ]
                                _ ->
                                    Html.text "Error6"
                    _ ->
                        Html.text "Error6.1"
        _ ->
            Html.text "Error6.2"


txErrorView : Model -> Html Msg
txErrorView model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            let
                playerAsUser =
                    --SR.ListOps.gotUserFromUserList (EverySet.fromList dataState) appInfo.player.player.address
                    case dataState of 
                        StateFetched users dKind ->
                            Data.Users.gotUser users appInfo.player.player.address
                        _ ->
                            Data.Users.gotUser Data.Users.emptyUsers appInfo.player.player.address

            in
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
                            { onPress = Just <| ResetToShowGlobal
                            , label = Element.text "Continue ..."
                            }
                        ]
                    ]
            ]

           


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        AppOps walletState dataState appInfo uiState txRec ->
            let 
                _ = Debug.log "walletState in subs" walletState
            in
            -- the orig code uses Ports.walletSentry ... same as here:
            case walletState of
                -- SR.Types.WalletStateUnknown ->
                --     Sub.batch
                --         [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                --         , Eth.Sentry.Tx.listen txRec.txSentry
                --         ]


                SR.Types.WalletStateMissing ->
                    Sub.batch
                        -- decodeToMsg uses partial application to return Value -> Msg which is what walletSentry expects as an arg
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                -- SR.Types.WalletEthEnabled ->
                --     Sub.batch
                --         -- decodeToMsg uses partial application to return Value -> Msg which is what walletSentry expects as an arg
                --         [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                --         , Eth.Sentry.Tx.listen txRec.txSentry
                --         ]

                SR.Types.WalletStateLocked ->
                        Sub.batch
                            [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                            , Eth.Sentry.Tx.listen txRec.txSentry
                            ]

                SR.Types.WalletStopSub ->
                        Sub.none
                    

                SR.Types.WalletStateAwaitOpening ->
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                SR.Types.WalletOpened ->
                    Sub.none

                SR.Types.WalletOperational ->
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                -- SR.Types.WalletOperational ->
                --         Sub.batch   
                --             [
                --             if model.someInt < 9 then
                --                  Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                --                 , Eth.Sentry.Tx.listen txRec.txSentry
                                
                --             else 
                --                 Sub.none
                --             ]
  

                SR.Types.WalletWaitingForTransactionReceipt ->
                    let
                        _ =
                            Debug.log "SR.Types is now WalletWaitingForTransactionReceipt :" walletState
                    in
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                _ ->
                    let
                        _ =
                            Debug.log "walletState fell thru:" walletState
                    in
                    Sub.none

        Failure _ ->
            Sub.none





--Helper functions



-- Http ops


gotUserList : Cmd Msg
gotUserList =
    let 
        _ =
            Debug.log "here in userList :" "here"
            
    in
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> UsersReceived) SR.Decode.listOfUsersDecoder
        , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUsersReadBinLink
        }


fetchedSingleRanking : Internal.Types.RankingId -> Cmd Msg
fetchedSingleRanking (Internal.Types.RankingId rankingId) =
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
        , url = SR.Constants.jsonbinUrlForCreateNewBinAndRespond ++ rankingId ++ "/latest"
        }


gotGlobal : Cmd Msg
gotGlobal =
    let 
        _ = Debug.log "got Global " "here"
    in
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> GlobalReceived) SR.Decode.rankingsDecoder
        , headers = [ SR.Defaults.secretKey, SR.Defaults.globalContainerId, SR.Defaults.globalContainerId ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingReadLink
        }


addedUserAsFirstPlayerInNewList : SR.Types.User -> Cmd Msg
addedUserAsFirstPlayerInNewList user =
    let
        playerEncoder : Json.Encode.Value
        playerEncoder =
            Json.Encode.list
                Json.Encode.object
                [ [ ( "address", Json.Encode.string (String.toLower user.ethaddress) )
                  , ( "rank", Json.Encode.int 1 )
                  , ( "challengeraddress", Json.Encode.string "" )
                  ]
                ]
    in
    --SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId is the Msg handled by update whenever a request is made
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- decoder relates to what comes back from server. Nothing to do with above.
    Http.request
        { body =
            Http.jsonBody <| playerEncoder
        , expect = Http.expectJson (RemoteData.fromResult >> SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId) SR.Decode.newRankingIdDecoder
        , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlForCreateNewBinAndRespond
        }


createNewUser : List SR.Types.User -> SR.Types.User -> Cmd Msg
createNewUser originaluserlist newuserinfo =
    let
        newUser =
            { datestamp = 123456789
            , active = True
            , username = newuserinfo.username
            , ethaddress = newuserinfo.ethaddress
            , description = newuserinfo.description
            , email = newuserinfo.email
            , mobile = newuserinfo.mobile
            , userjoinrankings = []
            }

        newUserAddedToList =
            newUser :: originaluserlist

        ensuredListHasNoDuplicates =
            Data.Users.removedDuplicateUserFromUserList newUserAddedToList

        _ =
            Debug.log "originaluserlist " originaluserlist
    in
    --SentUserInfoAndDecodedResponseToNewUser is the Msg handled by update whenever a request is made by buttuser clicked
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- decoder relates to what comes back from server. Nothing to do with above.
    -- we mustn't submit a new user if the original list is empty for some reason ...
    if List.isEmpty originaluserlist then
        Http.request
            { body =
                Http.jsonBody <| jsonEncodeNewUsersList ensuredListHasNoDuplicates
            , expect = Http.expectJson (RemoteData.fromResult >> SentUserInfoAndDecodedResponseToNewUser) SR.Decode.decodeNewUserListServerResponse
            , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
            , method = "PUT"
            , timeout = Nothing
            , tracker = Nothing

            -- this will fail the create new user:
            , url = ""
            }

    else
        Http.request
            { body =
                Http.jsonBody <| jsonEncodeNewUsersList ensuredListHasNoDuplicates
            , expect = Http.expectJson (RemoteData.fromResult >> SentUserInfoAndDecodedResponseToNewUser) SR.Decode.decodeNewUserListServerResponse
            , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
            , method = "PUT"
            , timeout = Nothing
            , tracker = Nothing
            , url = SR.Constants.jsonbinUrlUpdateUserListAndRespond
            }


updateExistingUser : List SR.Types.User -> SR.Types.User -> Cmd Msg
updateExistingUser originaluserlist updatedUserInfo =
    let
        updatedUser =
            { datestamp = 123456789
            , active = True
            , username = updatedUserInfo.username
            , ethaddress = updatedUserInfo.ethaddress
            , description = updatedUserInfo.description
            , email = updatedUserInfo.email
            , mobile = updatedUserInfo.mobile
            , userjoinrankings = updatedUserInfo.userjoinrankings
            }

        newListWithCurrentUserRemoved =
            Data.Users.removeCurrentUserEntryFromUserList originaluserlist updatedUserInfo.ethaddress

        updatedUserList =
            updatedUser :: newListWithCurrentUserRemoved

        ensuredListHasNoDuplicates =
            Data.Users.removedDuplicateUserFromUserList updatedUserList
    in
    --SentUserInfoAndDecodedResponseToNewUser is the Msg handled by update whenever a request is made by buttuser clicked
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- decoder relates to what comes back from server. Nothing to do with above.
    -- we mustn't submit a new user if the original list is empty for some reason ...
    if List.isEmpty originaluserlist then
        Http.request
            { body =
                Http.jsonBody <| jsonEncodeNewUsersList ensuredListHasNoDuplicates
            , expect = Http.expectJson (RemoteData.fromResult >> SentUserInfoAndDecodedResponseToNewUser) SR.Decode.decodeNewUserListServerResponse
            , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
            , method = "PUT"
            , timeout = Nothing
            , tracker = Nothing

            -- this will fail the create new user, but won't upload an empty list at least
            , url = ""
            }

    else
        Http.request
            { body =
                Http.jsonBody <| jsonEncodeNewUsersList ensuredListHasNoDuplicates
            , expect = Http.expectJson (RemoteData.fromResult >> SentUserInfoAndDecodedResponseToNewUser) SR.Decode.decodeNewUserListServerResponse
            , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
            , method = "PUT"
            , timeout = Nothing
            , tracker = Nothing
            , url = SR.Constants.jsonbinUrlUpdateUserListAndRespond
            }


jsonEncodeNewUsersList : List SR.Types.User -> Json.Encode.Value
jsonEncodeNewUsersList luserInfo =
    let
        encodeNewUserObj : SR.Types.User -> Json.Encode.Value
        encodeNewUserObj userInfo =
            Json.Encode.object
                [ ( "datestamp", Json.Encode.int 1569839363942 )
                , ( "active", Json.Encode.bool True )
                , ( "username", Json.Encode.string userInfo.username )
                , ( "ethaddress", Json.Encode.string (String.toLower userInfo.ethaddress) )
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
    case dataState of
        StateUpdated sUsers dKind -> 
            case dKind of 
                    Selected sSelected rnkId user status -> 
                        --ReturnFromPlayerListUpdate is the Msg handled by update whenever a request is made
                        --RemoteData is used throughout the module, including update
                        -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
                        -- the Decoder decodes what comes back in the response
                        Http.request
                            { body =
                                Http.jsonBody <| Data.Selected.jsonEncodeNewSelectedRankingPlayerList (Data.Selected.userAdded sUsers (Utils.MyUtils.stringFromRankingId rnkId) (Data.Selected.asList sSelected) userRec)
                            , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromPlayerListUpdate) SR.Decode.decodeNewPlayerListServerResponse
                            , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
                            , method = "PUT"
                            , timeout = Nothing
                            , tracker = Nothing
                            , url = SR.Constants.jsonbinUrlStubForUpdateExistingBinAndRespond ++ (Utils.MyUtils.stringFromRankingId rnkId)
                            
                            }
                    _ -> 
                        let 
                            _ = Debug.log "httpAddCurrentUserToPlayerList - dataState" dataState
                        in
                            Cmd.none
        _ -> 
            let 
                _ = Debug.log "httpAddCurrentUserToPlayerList - dataState" dataState
            in
                Cmd.none



httpPutRequestForAddGlobal : Json.Encode.Value -> List SR.Types.Ranking -> Cmd Msg
httpPutRequestForAddGlobal newJsonEncodedList globalListWithJsonObjAdded =
    --AddedNewRankingToGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| Data.Global.newJsonEncodedList globalListWithJsonObjAdded
        , expect = Http.expectJson (RemoteData.fromResult >> AddedNewRankingToGlobalList) SR.Decode.decodeNewRankingListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.globalBinName, SR.Defaults.globalContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingUpdateLink

        --, url = ""
        }

httpDeleteSelectedRankingFromGlobalList : Data.Global.Global -> Cmd Msg
httpDeleteSelectedRankingFromGlobalList sGlobalWithRankingDeleted =
    let
        globalListWithRankingDeleted = Data.Global.rankingsAsList sGlobalWithRankingDeleted
    in
    Http.request
            { body =
                Http.jsonBody <| Data.Global.newJsonEncodedList globalListWithRankingDeleted
            , expect = Http.expectJson (RemoteData.fromResult >> ReturnedFromDeletedRankingFromGlobalList) SR.Decode.decodeNewRankingListServerResponse
            , headers = [ SR.Defaults.secretKey, SR.Defaults.globalBinName, SR.Defaults.globalContainerId ]
            , method = "PUT"
            , timeout = Nothing
            , tracker = Nothing
            , url = SR.Constants.globalJsonbinRankingUpdateLink
            }


httpDeleteSelectedRankingFromJsonBin : String -> Cmd Msg
httpDeleteSelectedRankingFromJsonBin rankingId =
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnedFromDeletedSelectedRankingFromJsonBin) SR.Decode.decodeNewRankingListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
        , method = "DELETE"
        , timeout = Nothing
        , tracker = Nothing
        --, url = "https://api.jsonbin.io/b/" ++ rankingId
        , url = ""
        }


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
  case dataState of
    StateUpdated sUsers dKind -> 
        case dKind of 
                Selected sSelected rnkId user status ->
                    Http.request
                        { body =
                        Http.jsonBody <| Data.Selected.jsonEncodeNewSelectedRankingPlayerList (Data.Selected.asList sSelected)
                        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromPlayerListUpdate) SR.Decode.decodeNewPlayerListServerResponse
                        , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
                        , method = "PUT"
                        , timeout = Nothing
                        , tracker = Nothing
                        , url = SR.Constants.jsonbinUrlStubForUpdateExistingBinAndRespond ++ (Utils.MyUtils.stringFromRankingId rnkId)
                        }
                _ -> 
                    let 
                        _ = Debug.log "dataState - httpPlayerList" dataState
                    in
                        Cmd.none
    _ -> 
        let 
            _ = Debug.log "dataState - httpPlayerList" dataState
        in
            Cmd.none
    


updateUsersJoinRankings : String -> SR.Types.User -> List SR.Types.User -> Cmd Msg
updateUsersJoinRankings rankingId user lUser =
    Http.request
        { body =
            Http.jsonBody <| SR.Encode.encodeUserList <| Data.Users.addedNewJoinedRankingId rankingId user lUser
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromUserListUpdate) SR.Decode.decodeNewUserListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlUpdateUserListAndRespond
        }


module Main exposing (Model(..), Msg(..), emptyTxRecord, init, main, update, view)

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
    --= AppOps SR.Types.WalletState AllLists SR.Types.AppInfo SR.Types.UIState TxRecord
    = AppOps SR.Types.WalletState SetState SR.Types.AppInfo SR.Types.UIState TxRecord
    | Failure String

type alias AllLists =
    { users : Data.Users.Users
    , rankings : Data.Rankings.Rankings
    , userRankings : Data.Global.Global
    -- following will eventually be defined by Global and not exist here:
    -- , lownedUserRanking : List SR.Types.UserRanking
    -- , lmemberUserRanking : List SR.Types.UserRanking
    -- , lotherUserRanking : List SR.Types.UserRanking
    , userPlayers : List SR.Types.UserPlayer
    }

type SetState =
    AllEmpty
    | UsersFetched Data.Users.Users SR.Types.User
    | UsersUpdated Data.Users.Users SR.Types.User
    | GlobalFetched Data.Global.Global Data.Users.Users SR.Types.User
    | GlobalUpdated Data.Global.Global Data.Users.Users SR.Types.User
    | Selected Data.Selected.Selected Data.Users.Users Internal.Types.RankingId
    | SelectedUpdated Data.Selected.Selected Data.Users.Users Internal.Types.RankingId
    

emptyAllLists =
    { userRankings = Data.Global.emptyGlobal
    
    , users = Data.Users.emptyUsers
    , userPlayers = []
    -- , lownedUserRanking = []
    -- , lmemberUserRanking = []
    -- , lotherUserRanking = []
    }



init : () -> ( Model, Cmd Msg )
init _ =
    ( AppOps SR.Types.WalletStateUnknown AllEmpty SR.Defaults.emptyAppInfo SR.Types.UILoading emptyTxRecord
    , Cmd.batch
        [ gotUserList
        , gotRankingList
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
      ClickedSelectedRanking Internal.Types.RankingId String String
    | ClickedSelectedOwnedRanking Internal.Types.RankingId String String
    | ClickedSelectedMemberRanking Internal.Types.RankingId String String
    | ClickedSelectedNeitherOwnerNorMember Internal.Types.RankingId String String
    | ClickedRegister
    | ClickedConfirmedRegisterNewUser
    | ClickedUpdateExistingUser
    | ClickedConfirmedUpdateExistingUser
    | ClickedCreateNewLadder
    | ClickedConfirmCreateNewLadder
    | ClickedNewChallengeConfirm
    | ResetToShowGlobal
    | ResetToShowSelected
    | ResetRejectedNewUserToShowGlobal
    | DeletedRanking String
    | ChallengeOpponentClicked SR.Types.UserPlayer
    | ClickedJoinSelected
    | LadderNameInputChg String
    | LadderDescInputChg String
    | ChangedUIStateToEnterResult SR.Types.UserPlayer
    | NewUserNameInputChg String
    | NewUserDescInputChg String
    | NewUserEmailInputChg String
    | NewUserMobileInputChg String
    | ExistingUserNameInputChg String
    | ExistingUserDescInputChg String
    | ExistingUserEmailInputChg String
    | ExistingUserMobileInputChg String
    | CreateNewUserRequested SR.Types.User
      -- App Only Ops
    | MissingWalletInstructions
    | OpenWalletInstructions
    | Fail String
    | NoOp
    | SentResultToJsonbin (Result Http.Error ())
    | SentUserInfoAndDecodedResponseToNewUser (RemoteData.WebData (List SR.Types.User))
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | GlobalRankingsReceived (RemoteData.WebData (List SR.Types.Ranking))
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | UsersReceived (RemoteData.WebData (List SR.Types.User))
    | ReturnFromPlayerListUpdate (RemoteData.WebData (List SR.Types.Player))
    | ReturnFromUserListUpdate (RemoteData.WebData (List SR.Types.User))
    | DeletedRankingFromGlobalList (RemoteData.WebData (List SR.Types.Ranking))
    | DeletedSingleRankingFromJsonBin (RemoteData.WebData (List SR.Types.Ranking))
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msgOfTransitonThatAlreadyHappened currentmodel =
    case currentmodel of
        AppOps walletState allSets appInfo uiState txRec ->
            case walletState of
                SR.Types.WalletStateUnknown ->
                    handleWalletStateUnknown msgOfTransitonThatAlreadyHappened currentmodel

                SR.Types.WalletStateLocked ->
                    handleWalletStateLocked msgOfTransitonThatAlreadyHappened currentmodel

                SR.Types.WalletStateAwaitOpening ->
                    handleWalletStateAwaitOpening msgOfTransitonThatAlreadyHappened currentmodel

                SR.Types.WalletOpened ->
                    handledWalletStateOpened msgOfTransitonThatAlreadyHappened currentmodel

                SR.Types.WalletOperational ->
                    
                    handleWalletStateOperational msgOfTransitonThatAlreadyHappened currentmodel

                SR.Types.WalletWaitingForTransactionReceipt ->
                    
                    handleWalletWaitingForUserInput msgOfTransitonThatAlreadyHappened walletState allSets appInfo txRec

                _ ->
                    ( Failure "WalletState failure", Cmd.none )

        Failure str ->
            ( Failure <| "Model failure in AppOps: " ++ str, Cmd.none )



handleWalletStateUnknown : Msg -> Model -> ( Model, Cmd Msg )
handleWalletStateUnknown msg model =
    case msg of
        WalletStatus walletSentry_ ->
            case walletSentry_.networkId of
                Rinkeby ->
                    case walletSentry_.account of
                        Nothing ->
                            ( AppOps SR.Types.WalletStateLocked AllEmpty SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord
                            , Cmd.none
                            )

                        Just uaddr ->
                            ( gotWalletAddrApplyToUser model uaddr, Cmd.none )

                _ ->
                    ( Failure "Please install and open an Etherum wallet on Rinkeby"
                    , Cmd.none
                    )

        _ ->
            ( model
            , Cmd.none
            )


handleWalletStateLocked : Msg -> Model -> ( Model, Cmd Msg )
handleWalletStateLocked msg model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            case msg of
                WalletStatus walletSentry_ ->
                    let
                        _ =
                            Debug.log "ws in locked" walletSentry_
                    in
                    ( AppOps SR.Types.WalletStateLocked AllEmpty SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord, Cmd.none )

                UsersReceived userList ->
                    ( model, Cmd.none )

                GlobalRankingsReceived lgranking ->
                    ( model, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "msg" msg
                    in
                    ( Failure "handleWalletStateLocked"
                    , Cmd.none
                    )

        _ ->
            ( Failure "handleWalletStateLocked model"
            , Cmd.none
            )


handleWalletStateAwaitOpening : Msg -> Model -> ( Model, Cmd Msg )
handleWalletStateAwaitOpening msg model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            case msg of
                WalletStatus walletSentry_ ->
                    let
                        _ =
                            Debug.log "ws in awaitopening" walletSentry_
                    in
                    case walletSentry_.networkId of
                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    ( AppOps SR.Types.WalletStateLocked allSets SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord
                                    , Cmd.none
                                    )

                                Just uaddr ->
                                    handledWalletStateOpened msg (gotWalletAddrApplyToUser model uaddr)

                        _ ->
                            ( Failure "handleWalletStateAwaitOpening"
                            , Cmd.none
                            )

                _ ->
                    ( Failure "handleWalletStateAwaitOpening"
                    , Cmd.none
                    )

        _ ->
            ( Failure "handleWalletStateAwaitOpening"
            , Cmd.none
            )


handledWalletStateOpened : Msg -> Model -> ( Model, Cmd Msg )
handledWalletStateOpened msg model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            case msg of
                WalletStatus walletSentry_ ->
                    ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                -- there are 2 instances of this operation - necessary?
                UsersReceived userList ->
                    let
                       
                        users = (Data.Users.asUsers (EverySet.fromList (Data.Users.validatedUserList <| Data.Users.extractUsersFromWebData userList)))
                        
                        userInAppInfo = { appInfo | user = Data.Users.gotUser users appInfo.user.ethaddress }

                        newSetState = UsersFetched users appInfo.user
                        
                    in
                        ( AppOps SR.Types.WalletOpened newSetState userInAppInfo SR.Types.UIRenderAllRankings emptyTxRecord, gotRankingList )


                GlobalRankingsReceived rmtrnkingdata ->
                    case allSets of
                        UsersFetched sUsers user ->
                            let
                                globalSet = GlobalFetched (Data.Global.createdGlobal 
                                                --(Data.Rankings.extractRankingsFromWebData 
                                                rmtrnkingdata
                                                --(Data.Users.asList 
                                                sUsers) 
                                                sUsers appInfo.user
                            in 
                                ( AppOps SR.Types.WalletOpened globalSet appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                        _ ->
                                (model, Cmd.none)

                NoOp ->
                    ( model, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "handledWalletStateOpened1" msg
                    in
                    ( Failure "handledWalletStateOpened2"
                    , Cmd.none
                    )

        Failure str ->
            ( Failure "handledWalletStateOpened3", Cmd.none )


handleWalletStateOperational : Msg -> Model -> ( Model, Cmd Msg )
handleWalletStateOperational msg model =
    
    
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            case msg of
                -- nb. walletSentry is constantly being updated via the sub
                WalletStatus walletSentry_ ->
                    let
                        _ =
                            Debug.log "ws in operational" walletSentry_
                    in
                    ( model, Cmd.none )

                TxSentryMsg subMsg ->
                    let
                        _ =
                            Debug.log "handleTxSubMsg subMsg" <| handleTxSubMsg subMsg

                        ( subModel, subCmd ) =
                            Eth.Sentry.Tx.update subMsg txRec.txSentry

                       
                    in
                    if handleTxSubMsg subMsg then                      
                        ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings txRec, Cmd.none )

                    else
                        ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )

                WatchTxHash (Ok txHash) ->
                    let
                        _ =
                            Debug.log "WatchTxHash in wallet operational " "Ok - hash watched and all ok"
                    in
                    ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings { txRec | txHash = Just txHash }, Cmd.none )

                WatchTxHash (Err err) ->
                    let
                        _ =
                            Debug.log "WatchTxHash" "Err"
                    in
                    ( AppOps SR.Types.WalletStateMissing allSets appInfo SR.Types.UIRenderAllRankings { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

                WatchTx (Ok tx) ->
                    let
                        _ =
                            Debug.log "WatchTx" "tx Ok"
                    in
                    AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings { txRec | tx = Just tx } |> update (ProcessResult SR.Types.Won)

                WatchTx (Err err) ->
                    let
                        _ =
                            Debug.log "WatchTx tx err" err
                    in
                    ( AppOps SR.Types.WalletStateLocked allSets appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

                WatchTxReceipt (Ok txReceipt) ->
                    let
                        _ =
                            Debug.log "handleWalletStateOpenedAndOperational Receipt" txReceipt
                    in
                    AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings emptyTxRecord
                        |> update (ProcessResult SR.Types.Won)

                WatchTxReceipt (Err err) ->
                    let
                        _ =
                            Debug.log "tx err" err
                    in
                    ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

                TrackTx blockDepth ->
                    let
                        _ =
                            Debug.log "TrackTx" "TrackTx"
                    in
                    ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIWaitingForTxReceipt { txRec | blockDepth = Just blockDepth }, Cmd.none )

                UsersReceived userList ->
                    let
                        _ =
                            Debug.log "Operational" userList
                        
                        users = (Data.Users.asUsers (EverySet.fromList (Data.Users.extractUsersFromWebData userList)))

                        newSetState = UsersFetched users appInfo.user
                         
                        userInAppInfo =
                            { appInfo | user = Data.Users.gotUser users appInfo.user.ethaddress }

                    in
                    ( AppOps SR.Types.WalletOperational newSetState userInAppInfo SR.Types.UIRenderAllRankings emptyTxRecord, gotRankingList )

                ProcessResult result ->
                    let
                        _ =
                            Debug.log "process result" result
                    in
                    case result of
                        SR.Types.Won ->
                            case allSets of
                                Selected sSelected sUsers rnkId ->
                                    let 
                                        -- prefer UISelectedRankingUserIsPlayer, but currently have to render all
                                        handleWonTuple = Data.Selected.handleWon sSelected appInfo
                                        newSetState = Selected (Tuple.first handleWonTuple) sUsers rnkId
                                        newModel = AppOps walletState newSetState (Tuple.second handleWonTuple) SR.Types.UIRenderAllRankings txRec
                                    in
                                        ( newModel, httpPlayerList newSetState)
                                _ -> 
                                    let 
                                        _ = Debug.log "2 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                        SR.Types.Lost ->
                            case allSets of
                                Selected sSelected sUsers rnkId ->
                                    let 
                                        -- prefer UISelectedRankingUserIsPlayer, but currently have to render all
                                        handleLostTuple = Data.Selected.handleLost sSelected appInfo
                                        newSetState = Selected (Tuple.first handleLostTuple) sUsers rnkId
                                        newModel = AppOps walletState newSetState (Tuple.second handleLostTuple) SR.Types.UIRenderAllRankings txRec
                                    in
                                        ( newModel, httpPlayerList newSetState)
                                _ -> 
                                    let 
                                        _ = Debug.log "3 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                        SR.Types.Undecided ->
                            case allSets of
                                Selected sSelected sUsers rnkId ->
                                    let 
                                        -- prefer UISelectedRankingUserIsPlayer, but currently have to render all
                                        handleUndecidedTuple = Data.Selected.handleLost sSelected appInfo
                                        newSetState = Selected (Tuple.first handleUndecidedTuple) sUsers rnkId
                                        newModel = AppOps walletState newSetState (Tuple.second handleUndecidedTuple) SR.Types.UIRenderAllRankings txRec
                                    in
                                        ( newModel, httpPlayerList newSetState)
                                _ -> 
                                    let 
                                        _ = Debug.log "4 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                SentResultToWallet result ->
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
                                ( AppOps SR.Types.WalletWaitingForTransactionReceipt allSets newAppInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                                , sentryCmd
                                )
                                
                        SR.Types.Lost ->                                     
                            let
                                    newAppInfo = {appInfo | appState = SR.Types.AppStateEnterLost }
                            in
                                ( AppOps SR.Types.WalletWaitingForTransactionReceipt allSets newAppInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                                , sentryCmd
                                )
                        SR.Types.Undecided -> 
                            let
                                    newAppInfo = {appInfo | appState = SR.Types.AppStateEnterUndecided }
                            in
                                ( AppOps SR.Types.WalletOperational allSets newAppInfo SR.Types.UIEnterResultTxProblem emptyTxRecord
                                    , sentryCmd
                                    )
                    



                SentResultToJsonbin a ->
                    ( AppOps SR.Types.WalletOperational
                        allSets
                        appInfo
                        uiState
                        txRec
                    , Cmd.none
                    )

                GlobalRankingsReceived rmtrnkingdata ->
                    case allSets of 
                        UsersFetched susers user -> 
                            let 
                                newSetState = GlobalFetched (Data.Global.createdGlobal rmtrnkingdata susers) susers user
                            in 
                                ( AppOps SR.Types.WalletOperational newSetState appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )
                        _ ->
                                (model, Cmd.none)
          
                ClickedSelectedOwnedRanking rnkidstr rnkownerstr rnknamestr ->
                        case allSets of 
                            GlobalFetched sGlobal susers user ->
                                let 
                                    newAppInfo =
                                        updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr


                                -- re-factor from appInfo to AppState over time
                                    initAppState = 
                                        Data.AppState.updateAppState appInfo.user appInfo.player 
                                        appInfo.challenger (rnkidstr)

                                    newSetState = Selected Data.Selected.emptySelected susers (Internal.Types.RankingId "")
                            
                            
                                in
                                    ( AppOps SR.Types.WalletOperational newSetState newAppInfo SR.Types.UISelectedRankingUserIsOwner emptyTxRecord, 
                                    fetchedSingleRanking rnkidstr )
                            _ -> 
                                (model, Cmd.none)

                ClickedSelectedMemberRanking rnkidstr rnkownerstr rnknamestr ->
                    let
                        _ =
                            Debug.log "user clicked member" rnkidstr

                        newAppInfo =
                            updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr

                        -- re-factor from appInfo to AppState over time
                        initAppState = 
                            Data.AppState.updateAppState appInfo.user appInfo.player 
                            appInfo.challenger ( rnkidstr)
                    in
                    ( AppOps SR.Types.WalletOperational allSets newAppInfo SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord, 
                    fetchedSingleRanking rnkidstr )

                ClickedSelectedNeitherOwnerNorMember rnkidstr rnkownerstr rnknamestr ->
                    let
                        _ =
                            Debug.log "user clicked other " rnkidstr

                        newAppInfo =
                            updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr

                        -- re-factor from appInfo to AppState over time
                        initAppState = 
                            Data.AppState.updateAppState appInfo.user appInfo.player 
                            appInfo.challenger ( rnkidstr)
                    in
                    ( AppOps SR.Types.WalletOperational allSets newAppInfo SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, 
                    fetchedSingleRanking rnkidstr )

                -- this is the response from addedUserAsFirstPlayerInNewList Cmd
                -- it had the Http.expectStringResponse in it
                -- it's already created the new ranking with current player as the first entry
                -- the result now is the ranking id only at this point which was pulled out by the decoder
                SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder ->
                    case allSets of 
                        GlobalFetched globalUserRankings sUsers user ->
                            let
                                extractedRankingId = Data.Global.gotNewRankingIdFromWebData idValueFromDecoder
                                newSGlobal = Data.Global.addUserRanking globalUserRankings extractedRankingId appInfo.selectedRanking appInfo.user
                                newGlobalAsList = Data.Global.rankingsAsList newSGlobal
                                newGlobalUpdated = GlobalUpdated newSGlobal sUsers user
                            in
                                ( AppOps SR.Types.WalletOperational newGlobalUpdated appInfo SR.Types.UICreateNewLadder emptyTxRecord
                                ,
                                httpPutRequestForAddGlobal (Data.Global.newJsonEncodedList (newGlobalAsList)) newGlobalAsList
                                )
                        _ -> 
                                    let 
                                        _ = Debug.log "4 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                SentUserInfoAndDecodedResponseToNewUser serverResponse ->
                    ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetToShowGlobal ->
                    ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetRejectedNewUserToShowGlobal ->
                    let 
                        newAppInfo = {appInfo | user = SR.Defaults.emptyUser}
                    in 
                    ( AppOps SR.Types.WalletOperational allSets newAppInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetToShowSelected ->
                --todo: something like this will need to be implemented:
                    -- let
                    --     uiType =
                    --         ensuredCorrectSelectedUI appInfo allSets
                    -- in
                    ( AppOps SR.Types.WalletOperational allSets appInfo uiState emptyTxRecord, Cmd.none )

                ClickedCreateNewLadder ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        clearedNameFieldInSelectedRanking =
                            { newSelectedRanking | rankingname = "" }

                        clearedNameFieldAppInfo =
                            { appInfo | selectedRanking = clearedNameFieldInSelectedRanking }
                    in
                    ( AppOps SR.Types.WalletOperational allSets clearedNameFieldAppInfo SR.Types.UICreateNewLadder emptyTxRecord, Cmd.none )

                ClickedConfirmCreateNewLadder ->
                    case allSets of 
                        GlobalFetched sGlobal sUsers user ->
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
                                ( AppOps SR.Types.WalletWaitingForTransactionReceipt allSets newAppInfo SR.Types.UIRenderAllRankings { txRec | txSentry = newSentry }
                                --Cmd.batch [ sentryCmd, addedUserAsFirstPlayerInNewList appInfo.user ] )
                                ,sentryCmd)

                            else
                                ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRegisterNewUser emptyTxRecord, Cmd.none )
                        _ -> 
                                    let 
                                        _ = Debug.log "6 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList ->
                    -- I think the global set has already been updated
                    ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )
                    

                LadderNameInputChg namefield ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        updatedSelectedRanking =
                            { newSelectedRanking | rankingname = namefield }

                        newAppInfo =
                            { appInfo | selectedRanking = updatedSelectedRanking }
                    in
                    ( AppOps SR.Types.WalletOperational allSets newAppInfo SR.Types.UICreateNewLadder emptyTxRecord, Cmd.none )

                LadderDescInputChg descfield ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        updatedSelectedRanking =
                            { newSelectedRanking | rankingdesc = descfield }

                        newAppInfo =
                            { appInfo | selectedRanking = updatedSelectedRanking }
                    in
                    ( AppOps SR.Types.WalletOperational allSets newAppInfo SR.Types.UICreateNewLadder emptyTxRecord, Cmd.none )

                ClickedNewChallengeConfirm ->
                    case allSets of 
                        Selected sSelected sUsers rnkId ->
                            let
                                sChallengerUpdated = SelectedUpdated (Data.Selected.assignChallengerAddrsForBOTHPlayers sSelected appInfo) sUsers rnkId
                                updatedModel = AppOps walletState sChallengerUpdated appInfo SR.Types.UIRenderAllRankings txRec
                            in 
                                ( updatedModel, httpPlayerList (sChallengerUpdated))

                        _ -> 
                            let 
                                _ = Debug.log "7.1 - setsState" allSets
                            in
                                (model, Cmd.none)


                PlayersReceived lplayer ->
                    case allSets of
                        Selected sSelected sUsers _ ->
                         
                             let 
                                _ = Debug.log "lplayer" lplayer
                            in
                            ( populatedSelected model (Data.Selected.extractAndSortPlayerList lplayer (Data.Users.asList sUsers)), Cmd.none )
                            

                        _ -> 
                            let 
                                _ = Debug.log "8 - setsState" allSets
                            in
                                (model, Cmd.none)

                ChangedUIStateToEnterResult player ->
                    ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIEnterResult emptyTxRecord, Cmd.none )

                DeletedRanking uaddr ->
                    case allSets of 
                        GlobalFetched sGlobal sUsers user ->
                            let
                                userRankingToDelete = Data.Global.gotUserRanking sGlobal uaddr
                                newGlobal = Data.Global.removeUserRanking sGlobal userRankingToDelete 
                            in 
                                ( AppOps SR.Types.WalletOperational
                                    (GlobalUpdated newGlobal sUsers user)
                                    appInfo
                                    uiState
                                    txRec
                                , httpDeleteSelectedRankingFromJsonBin appInfo.selectedRanking.id
                                )
                        _ -> 
                                    let 
                                        _ = Debug.log "9 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                DeletedSingleRankingFromJsonBin result ->
                    -- nb. you haven't used the result!
                        ( AppOps SR.Types.WalletOperational
                            allSets
                            appInfo
                            uiState
                            txRec
                        , httpDeleteSelectedRankingFromJsonBin appInfo.selectedRanking.id)
                    


                ChallengeOpponentClicked opponentAsPlayer ->
                    case allSets of 
                        Selected sSelected sUsers _ -> 
                            ( updatedForChallenge model (Data.Selected.asList sSelected) opponentAsPlayer appInfo.user, Cmd.none )
                        
                        _ -> 
                                    let 
                                        _ = Debug.log "10 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                DeletedRankingFromGlobalList updatedListAfterRankingDeletedFromGlobalList ->
                    case allSets of 
                        UsersFetched sUsers user ->       
                            let
                                userRankings = Data.Global.createdGlobal (updatedListAfterRankingDeletedFromGlobalList) sUsers
                                
                                newSetState = GlobalUpdated userRankings sUsers appInfo.user
                            in
                                ( AppOps SR.Types.WalletOperational newSetState appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                        _ -> 
                                    let 
                                        _ = Debug.log "11 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                ClickedJoinSelected ->
                    case allSets of 
                        Selected sSelected sUsers rnkId ->
                            if Data.Users.isRegistered (Data.Users.asList sUsers) appInfo.user then
                                let
                                    newLUPlayer = Data.Selected.userAdded sUsers appInfo.selectedRanking.id (Data.Selected.asList sSelected) appInfo.user
                                    newSelected = Data.Selected.asSelected (EverySet.fromList newLUPlayer) sUsers rnkId
                                    
                                    newSetState = Selected newSelected sUsers rnkId
                                    updatedModel = AppOps walletState newSetState appInfo SR.Types.UIRenderAllRankings txRec
                                in
                                ( updatedModel, httpPlayerList (newSetState))
                            else
                                ( AppOps walletState allSets appInfo SR.Types.UIRegisterNewUser txRec, Cmd.none )

                        _ -> 
                                    let 
                                        _ = Debug.log "12 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                ReturnFromPlayerListUpdate response ->
                    case allSets of 
                        SelectedUpdated sSelected sUsers _ -> 
                            let
                                lplayer =
                                    Data.Selected.extractPlayersFromWebData response

                                convertedToUserPlayers =
                                    Data.Selected.convertPlayersToUserPlayers
                                        lplayer
                                        (Data.Users.asList sUsers)

                                
                            in
                            ( updateSelectedRankingPlayerList model convertedToUserPlayers, Cmd.none )
                        
                        _ -> 
                                    let 
                                        _ = Debug.log "13 - setsState" allSets
                                    in
                                        (model, Cmd.none)
                    

                ReturnFromUserListUpdate response ->
                    --( updateUserList model (Data.Users.extractUsersFromWebData response), Cmd.none )
                    let 
                        lusers = Data.Users.extractUsersFromWebData response
                        resetUserList = UsersUpdated (Data.Users.asUsers (EverySet.fromList lusers)) appInfo.user
                    in
                        (AppOps walletState resetUserList appInfo uiState txRec, Cmd.none)

                TimeUpdated posixTime ->
                    let
                        _ =
                            Debug.log "posixtime" posixTime
                    in
                    ( model, Cmd.none )

                NewUserNameInputChg updateField ->
                    ( handleNewUserInputs model (NewUserNameInputChg updateField), Cmd.none )

                NewUserDescInputChg updateField ->
                    ( handleNewUserInputs model (NewUserDescInputChg updateField), Cmd.none )

                NewUserEmailInputChg updateField ->
                    ( handleNewUserInputs model (NewUserEmailInputChg updateField), Cmd.none )

                NewUserMobileInputChg updateField ->
                    ( handleNewUserInputs model (NewUserMobileInputChg updateField), Cmd.none )

                ExistingUserNameInputChg updateField ->
                    ( handleExistingUserInputs model (ExistingUserNameInputChg updateField), Cmd.none )

                ExistingUserDescInputChg updateField ->
                    ( handleExistingUserInputs model (ExistingUserDescInputChg updateField), Cmd.none )

                ExistingUserEmailInputChg updateField ->
                    ( handleExistingUserInputs model (ExistingUserEmailInputChg updateField), Cmd.none )

                ExistingUserMobileInputChg updateField ->
                    ( handleExistingUserInputs model (ExistingUserMobileInputChg updateField), Cmd.none )

                ClickedUpdateExistingUser ->
                    ( AppOps walletState allSets appInfo SR.Types.UIUpdateExistingUser txRec, Cmd.none )

                ClickedConfirmedUpdateExistingUser ->
                    case allSets of
                        UsersUpdated sUsers user ->
                            ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRenderAllRankings txRec, updateExistingUser (Data.Users.asList sUsers) appInfo.user )
                        _ -> 
                                    let 
                                        _ = Debug.log "14 - setsState" allSets
                                    in
                                        (model, Cmd.none)

                ClickedRegister ->
                     ( AppOps SR.Types.WalletOperational allSets appInfo SR.Types.UIRegisterNewUser txRec, Cmd.none )
                
                ClickedConfirmedRegisterNewUser ->
                    ( AppOps SR.Types.WalletWaitingForTransactionReceipt allSets appInfo SR.Types.UIWaitingForTxReceipt txRec, Cmd.none )

                CreateNewUserRequested userInfo ->
                
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
                    ( AppOps SR.Types.WalletWaitingForTransactionReceipt allSets newAppInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                    , sentryCmd)

                Fail str ->
                    ( Failure str, Cmd.none )

                NoOp ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Failure str ->
            ( Failure str, Cmd.none )


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





handleWalletWaitingForUserInput : Msg -> SR.Types.WalletState -> SetState -> SR.Types.AppInfo -> TxRecord -> ( Model, Cmd Msg )
handleWalletWaitingForUserInput msg walletState setsState appInfo txRec =
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
            ( AppOps SR.Types.WalletWaitingForTransactionReceipt setsState appInfo SR.Types.UIWaitingForTxReceipt txRec
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
                        ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }
                        , Cmd.batch [subCmd, addedUserAsFirstPlayerInNewList appInfo.user] )
                    
                    SR.Types.AppStateCreateNewUser -> 
                        let 
                            _ =
                                Debug.log "in CreateNewUser" "yes"

                            userSet = case setsState of 
                                        UsersFetched users user -> 
                                            users 
                                        UsersUpdated users user ->
                                            users 
                                        _ -> 
                                            Data.Users.emptyUsers
                        in
                        ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }
                        , Cmd.batch [subCmd,  createNewUser ( Data.Users.asList userSet) appInfo.user] )






                    SR.Types.AppStateEnterWon -> 
                        let 
                            _ =
                                Debug.log "in AppStateEnterWon" "yes"

                            
                        in
                        
                             (AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Won) )
                            
                          

                    SR.Types.AppStateEnterLost -> 
                        let 
                            _ =
                                Debug.log "in AppStateEnterLost" "yes"
                        in
                        ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Lost)
                        
                        )

                    SR.Types.AppStateEnterUndecided -> 
                        let 
                            _ =
                                Debug.log "in AppStateEnterUndecided" "yes"
                        in
                        ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel } |> update (ProcessResult SR.Types.Undecided)
                        
                        )

                    _ -> 
                       ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }, subCmd ) 

            else
                ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )

        WatchTxHash (Ok txHash) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput" "watch tx hash"
            in
            ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | txHash = Just txHash }, Cmd.none )

        WatchTxHash (Err err) ->
            ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTx (Ok tx) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput" "tx ok"
            in
      
            (AppOps walletState setsState appInfo SR.Types.UIRenderAllRankings { txRec | tx = Just tx }, Cmd.none )

        WatchTx (Err err) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput tx err" err
            in
            ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTxReceipt (Ok txReceipt) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForUserInput tx ok" txReceipt
            in
            AppOps walletState setsState appInfo SR.Types.UIRenderAllRankings { txRec | txReceipt = Just txReceipt } |> update (ProcessResult SR.Types.Won)

        WatchTxReceipt (Err err) ->
            let
                _ =
                    Debug.log "tx err" err
            in
            ( AppOps SR.Types.WalletOperational setsState appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

        _ ->
            let
                _ =
                    Debug.log "wallet state " walletState
            in
            ( Failure "handleWalletStateUnknown"
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


gotWalletAddrApplyToUser : Model -> Eth.Types.Address -> Model
gotWalletAddrApplyToUser model uaddr =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            let
                newAppInfoUser =
                    appInfo.user

                newUserWithAddr =
                    { newAppInfoUser | ethaddress = Eth.Utils.addressToString uaddr }

                newAppInfo =
                    { appInfo | user = newUserWithAddr }
            in
            AppOps SR.Types.WalletOpened allSets newAppInfo SR.Types.UILoading emptyTxRecord

        _ ->
            Failure "gotWalletAddrApplyToUser"



-- assignChallengerAddrsForBOTHPlayers : Model -> ( Model, Cmd Msg )
-- assignChallengerAddrsForBOTHPlayers model =
--     case model of
--         AppOps walletState allSets appInfo uiState txRec ->
--             --let
--                 case allSets of 
--                     Selected sselected sUsers rnkId ->
--                     -- List SR.Types.UserPlayer -> SR.Types.UserPlayer -> String -> List SR.Types.UserPlayer
--                         --lassignedChallengerAddress = Data.Selected.assignChallengerAddr allSets.userPlayers appInfo.player appInfo.challenger.player.address
--                         let
--                             sUserUpdated = Data.Selected.assignChallengerAddr sselected appInfo.player appInfo.challenger.player.address
--                             sChallengerUpdated = SelectedUpdated (Data.Selected.assignChallengerAddr sUserUpdated appInfo.challenger appInfo.player.player.address) sUsers rnkId
--                             updatedModel = AppOps walletState sChallengerUpdated appInfo SR.Types.UIRenderAllRankings txRec
--                         in 
--                             ( updatedModel, httpPlayerList (sChallengerUpdated))
                



handleNewUserInputs : Model -> Msg -> Model
handleNewUserInputs model msg =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
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
                    AppOps walletState allSets newAppInfo SR.Types.UIRegisterNewUser txRec

                NewUserDescInputChg descfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | description = descfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState allSets newAppInfo SR.Types.UIRegisterNewUser txRec

                NewUserEmailInputChg emailfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | email = emailfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState allSets newAppInfo SR.Types.UIRegisterNewUser txRec

                NewUserMobileInputChg mobilefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | mobile = mobilefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState allSets newAppInfo SR.Types.UIRegisterNewUser txRec

                _ ->
                    Failure "NewUserNameInputChg"

        _ ->
            Failure "NewUserNameInputChg"


handleExistingUserInputs : Model -> Msg -> Model
handleExistingUserInputs model msg =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
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
                    AppOps walletState allSets newAppInfo SR.Types.UIUpdateExistingUser txRec

                ExistingUserEmailInputChg emailfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | email = emailfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState allSets newAppInfo SR.Types.UIUpdateExistingUser txRec

                ExistingUserMobileInputChg mobilefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | mobile = mobilefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState allSets newAppInfo SR.Types.UIUpdateExistingUser txRec

                _ ->
                    Failure "ExistingUserNameInputChg"

        _ ->
            Failure "ExistingUserNameInputChg"


updatedForChallenge : Model -> List SR.Types.UserPlayer -> SR.Types.UserPlayer -> SR.Types.User -> Model
updatedForChallenge model luplayer opponentAsPlayer user =
    case model of
        AppOps walletState allSets appInfo _ txRec ->
                case allSets of 
                    Selected sSelected sUsers rnkId -> 
                        let 
                            newAppInfoWithPlayer = { appInfo | player = Data.Selected.gotCurrentUserAsPlayerFromPlayerList luplayer user }
                
                            newAppInfoWithChallengerAndPlayer = { newAppInfoWithPlayer | challenger = opponentAsPlayer }
                            
                            newAllListsWithSelectedRankingUpdate = 
                                SelectedUpdated (Data.Selected.updateSelectedRankingOnChallenge sSelected newAppInfoWithChallengerAndPlayer) sUsers rnkId
                            
                        in
                                AppOps walletState newAllListsWithSelectedRankingUpdate newAppInfoWithChallengerAndPlayer SR.Types.UIChallenge txRec
                    
                    _ -> 
                        let 
                            _ = Debug.log "updatedForChallenge - setsState" allSets
                        in
                            model
        _ ->
            Failure <| "updatedForChallenge : "



updateSelectedRankingPlayerList : Model -> List SR.Types.UserPlayer -> Model
updateSelectedRankingPlayerList model luplayers =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
                case allSets of 
                        SelectedUpdated sSelectedUpdated sUsers rnkId ->
                            let 
                                --todo: another way around this here? Problem is ensuredCorrectSelectedUI needs the global set:
                                --newUiState = ensuredCorrectSelectedUI appInfo allSets 
                                newSelected = Data.Selected.asSelected (EverySet.fromList luplayers) sUsers rnkId
                                newSelectedUpdated = SelectedUpdated newSelected sUsers rnkId
                            in
                                AppOps walletState newSelectedUpdated appInfo uiState txRec
                        _ -> 
                            let 
                                _ = Debug.log "updateSelectedRankingPlayerList - setsState" allSets
                            in
                                model

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


populatedSelected : Model -> List SR.Types.UserPlayer -> Model
populatedSelected model luplayer =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            case allSets of 
                Selected sSelected sUsers _ ->
                    let
                        newSSelected = Data.Selected.asSelected (EverySet.fromList luplayer ) sUsers (Internal.Types.RankingId appInfo.selectedRanking.id)

                        stateToSelected = Selected newSSelected sUsers (Internal.Types.RankingId appInfo.selectedRanking.id)
                        
                        newAppPlayer = { appInfo | player = Data.Selected.gotUserPlayerFromPlayerListStrAddress luplayer appInfo.user.ethaddress }

                        newAppChallengerAndPlayer = { newAppPlayer | challenger = Data.Selected.gotUserPlayerFromPlayerListStrAddress luplayer newAppPlayer.player.player.challengeraddress }

                        --_ = Debug.log "in populatedSelected" <| stateToSelected
                    
                    in
                        AppOps walletState stateToSelected newAppChallengerAndPlayer uiState emptyTxRecord
                _ ->
                    Failure <| "populatedSelected : "
        _ ->
            Failure <| "populatedSelected : "



-- view


view : Model -> Html Msg
view model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            case uiState of
                SR.Types.UICreateNewLadder ->
                    inputNewLadderview model

                SR.Types.UISelectedRankingUserIsOwner ->
                    case allSets of 
                        Selected sSelected sUsers rnkId -> 
                            selectedUserIsOwnerView allSets appInfo
                        _ -> 
                            greetingView <| "Owner View error"
                    

                SR.Types.UISelectedRankingUserIsPlayer ->
                    case allSets of 
                        Selected sSelected sUsers rnkId -> 
                            selectedUserIsPlayerView allSets appInfo
                        _ -> 
                            greetingView <| "View error"

                SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer ->
                    case allSets of 
                        Selected sSelected sUsers rnkId -> 
                            selectedUserIsNeitherOwnerNorPlayerView allSets appInfo
                        _ -> 
                            greetingView <| "View error"

                SR.Types.UIRenderAllRankings ->
                    case allSets of 
                        GlobalFetched sGlobal sUsers user ->
                            globalResponsiveview sGlobal appInfo.user
                        GlobalUpdated sGlobal sUsers user -> 
                            globalResponsiveview sGlobal appInfo.user
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
                    greetingView <|
                        """Your Ethereum  
wallet browser
extension is LOCKED. Please 
use your wallet      
password to open it 
before continuing and
refresh the browser"""

                SR.Types.UIRegisterNewUser ->
                    -- case allSets of 
                    --     Selected sSelected sUsers rnkId -> 
                            inputNewUserview allSets appInfo
                        -- _ -> 
                        --     greetingView <| "View error"
                    

                SR.Types.UIUpdateExistingUser ->
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
        List.singleton <|
            Element.el []
                (Input.button
                    ([ Element.htmlAttribute (Html.Attributes.id "createnewrankingbtn") ]
                        
                        ++ Button.simple
                        ++ Button.fill
                        ++ Color.info
                    )
                 <|
                    { onPress = Just <| ClickedCreateNewLadder
                    , label = Element.text "Create New Ladder"
                    }
                )

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
        [ Input.button ([ Element.htmlAttribute (Html.Attributes.id "createnewrankingbtn") ] ++ Button.simple ++ Color.info) <|
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

        _ = Debug.log "mapOutRankingList" lrankinginfo
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


-- playerbuttons : Model -> Element Msg
-- playerbuttons model =
playerbuttons : SetState -> SR.Types.AppInfo -> Element Msg
playerbuttons setsState appInfo =
    case setsState of
        Selected sSelected sUsers rnkId ->
            Element.column Grid.section <|
                [ SR.Elements.selectedRankingHeaderEl appInfo.selectedRanking
                , Element.column (Card.simple ++ Grid.simple) <|
                    insertPlayerList setsState appInfo
                ]

        _ ->
            Element.text "Error"


-- configureThenAddPlayerRankingBtns : SetState -> SR.Types.AppInfo -> SR.Types.UserPlayer -> Element Msg
-- configureThenAddPlayerRankingBtns setsState  appInfo uplayer =

configureThenAddPlayerRankingBtns : Data.Selected.Selected -> SR.Types.AppInfo -> SR.Types.UserPlayer -> Element Msg
configureThenAddPlayerRankingBtns sSelected appInfo uplayer =
   -- nb. 'uplayer' is the player that's being mapped cf. appInfo.player which is current user as player (single instance)
    -- case setsState of 
    --         Selected sSelected sUsers rnkId ->
                let
                    _ = Debug.log "userPlayer in configureThenAddPlayerRankingBtns" uplayer
                    -- playerAsUser =
                    --     Data.Users.gotUser sUsers uplayer.player.address

                    -- challengerAsUser =
                    --     Data.Users.gotUser sUsers uplayer.player.challengeraddress

                    printChallengerNameOrAvailable = Data.Selected.printChallengerNameOrAvailable sSelected uplayer
                    --printChallengerNameOrAvailable = uplayer.challenger.name

                    --_ = Debug.log "playerAsUser " playerAsUser
                in
                if Data.Selected.isUserPlayerMemberOfSelectedRanking (Data.Selected.asList sSelected) appInfo.user then
                    let
                        _ = Debug.log "player is in selected ranking" "current user not yet determined"
                    in
                    --if isPlayerCurrentUser then
                    if Data.Selected.isPlayerCurrentUser appInfo.user uplayer then
                        let 
                                    _ = Debug.log "player is current user" "challenge not yet determined"
                        in
                        --if isCurrentUserInAChallenge then
                        if Data.Selected.isChallenged sSelected uplayer then
                            let 
                                    _ = Debug.log "player is current user, and in a challenge" "here"
                            in
                            Element.column Grid.simple <|
                                [ Input.button (Button.fill ++ Color.success) <|
                                    { onPress = Just <| ChangedUIStateToEnterResult appInfo.player
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
                    let 
                            _ = Debug.log "player is not current user, and is ready to be challenged if higher ranked" uplayer
                    in
                        if Data.Selected.isCurrentUserPlayerLowerRanked uplayer appInfo.challenger then 
                            Element.column Grid.simple <|
                                [ Input.button (Button.fill ++ Color.light) <|
                                    { onPress = Just <| ChallengeOpponentClicked uplayer
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

insertPlayerList : SetState -> SR.Types.AppInfo -> List (Element Msg)
insertPlayerList setsState appInfo =
    case setsState of
        Selected sselected sUsers rnkId ->
            let
                
                mapOutPlayerList =
                    List.map
                        --(configureThenAddPlayerRankingBtns setsState appInfo)
                        (configureThenAddPlayerRankingBtns sselected appInfo)
                        (Data.Selected.asList sselected)

                    -- EverySet.map
                    --     (configureThenAddPlayerRankingBtns sselected appInfo) (Data.Selected.asEverySet sselected)
            in
            --EverySet.toList mapOutPlayerList
            mapOutPlayerList

        _ ->
            [ Element.text "error" ]


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
                    { onPress = Just <| DeletedRanking user.ethaddress
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


newrankingconfirmbutton : SR.Types.AppInfo -> SetState -> Element Msg
newrankingconfirmbutton appInfo allSets =
    case allSets of 
            GlobalFetched sGlobal sUsers user ->
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
                    _ = Debug.log "newrankingconfirmbutton - setsState" allSets
                in
                    Element.text ""


confirmChallengebutton : Model -> Element Msg
confirmChallengebutton model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            
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
        AppOps walletState allSets appInfo uiState txRec ->
            case allSets of 
                Selected sSelected sUsers _ ->
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


acknoweldgeTxErrorbtn : Model -> Element Msg
acknoweldgeTxErrorbtn model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
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
                            { onPress = Just <| ResetRejectedNewUserToShowGlobal
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
            (Element.text "45 characters max")


ladderDescValidationErr : SR.Types.Ranking -> Element Msg
ladderDescValidationErr rankingInfo =
    if isLadderDescValidated rankingInfo then
        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "ladderdescValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "")

    else
        Element.el
            (List.append [ Element.htmlAttribute (Html.Attributes.id "ladderdescValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text "45 characters max")


isUserDescValidated : SR.Types.User -> Bool
isUserDescValidated user =
    if String.length user.description <= 45 then
        True

    else
        False


isLadderDescValidated : SR.Types.Ranking -> Bool
isLadderDescValidated rankingInfo =
    if String.length rankingInfo.rankingdesc <= 45 then
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
                    { onPress = Just <| CreateNewUserRequested user
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
        Utils.Validation.Validate.isUserNameValidated user luser
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
        Utils.Validation.Validate.isRankingNameValidated rnkInfo luranking
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


inputNewUser : SetState -> SR.Types.AppInfo -> Element Msg
inputNewUser setsState appInfo =
            case setsState of 
                Selected sSelected sUsers _ -> 
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


inputUpdateExistingUser : Model -> Element Msg
inputUpdateExistingUser model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
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


ladderNameValidationErr : SR.Types.AppInfo -> SetState -> Element Msg
ladderNameValidationErr appInfo setsState =
    case setsState of 
        GlobalFetched sGlobal sUsers user ->
            if Utils.Validation.Validate.isRankingNameValidated appInfo.selectedRanking (Data.Global.asList sGlobal) then
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
                    _ = Debug.log "setsState - ladderNameValidationErr" setsState
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


inputNewLadder : SR.Types.AppInfo -> SetState -> Element Msg
inputNewLadder appInfo allSets =
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
                , ladderNameValidationErr appInfo allSets
                , Input.multiline Input.simple
                    { onChange = LadderDescInputChg
                    , text = appInfo.selectedRanking.rankingdesc
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Desc:"
                    , spellcheck = False
                    }
                , Element.text "* Required"
                , ladderDescValidationErr appInfo.selectedRanking
                ]
            ]
        ]


-- globalResponsiveview : List SR.Types.UserRanking -> List SR.Types.UserRanking -> List SR.Types.UserRanking -> SR.Types.User -> Html Msg
-- globalResponsiveview lowneduranking lmemberusranking lotheruranking user =
globalResponsiveview : Data.Global.Global -> SR.Types.User -> Html Msg
globalResponsiveview sGlobal user =

    let
        userName =
            if user.username /= "" then
                user.username

            else
                "New User"

        _ = Debug.log "gotMember" <| Data.Global.gotMember sGlobal user
    in
    Framework.responsiveLayout
        []
    <|
        Element.column
            Framework.container
            [ Element.el (Heading.h5 ++ [ Element.htmlAttribute (Html.Attributes.id "globalHeader") ]) <|
                Element.text ("SportRank - " ++ userName)
            ,displayUpdateProfileBtnIfExistingUser user.username ClickedUpdateExistingUser
            --, Element.text "\n"
            , displayCreateNewLadderBtnIfExistingUser user.username (Data.Global.asList (Data.Global.gotOwned sGlobal user)) ClickedCreateNewLadder
            , displayRegisterBtnIfNewUser
                user.username
                ClickedRegister
            , ownedrankingbuttons (Data.Global.asList (Data.Global.gotOwned sGlobal user)) user
            , memberrankingbuttons (Data.Global.gotMember sGlobal user) user
            , otherrankingbuttons (Data.Global.asList (Data.Global.gotOthers sGlobal user)) user
            ]


displayUpdateProfileBtnIfExistingUser : String -> Msg -> Element Msg
displayUpdateProfileBtnIfExistingUser uname msg =
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
            { onPress = Just <| msg
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

selectedUserIsOwnerView : SetState -> SR.Types.AppInfo -> Html Msg
selectedUserIsOwnerView setsState appInfo =
    case setsState of
        Selected sSelected sUsers _ ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Owner  - " ++ appInfo.user.username
                    , selecteduserIsOwnerhomebutton appInfo.user
                    , playerbuttons setsState appInfo
                    ]

        _ ->
            Html.text "Fail selectedUserIsOwnerView"

selectedUserIsPlayerView : SetState -> SR.Types.AppInfo -> Html Msg
selectedUserIsPlayerView setsState appInfo =
    case setsState of
        Selected sSelected sUsers _ ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ appInfo.user.username
                    , selecteduserIsPlayerHomebutton appInfo.user
                    , playerbuttons setsState appInfo
                    ]

        _ ->
            Html.text "Error"


selectedUserIsNeitherOwnerNorPlayerView : SetState -> SR.Types.AppInfo -> Html Msg
selectedUserIsNeitherOwnerNorPlayerView setsState appInfo =
    case setsState of
        Selected sSelected sUsers _ ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ newOrExistingUserNameDisplay appInfo.user
                    , selecteduserIsNeitherPlayerNorOwnerHomebutton appInfo.user
                    , playerbuttons  setsState appInfo
                    ]

        _ ->
            Html.text "Error"


newOrExistingUserNameDisplay : SR.Types.User -> Element msg
newOrExistingUserNameDisplay user =
    if user.username == "" then
        Element.el Heading.h4 <| Element.text <| "SportRank - New User - Join?"

    else
        Element.el Heading.h4 <| Element.text <| "SportRank - " ++ user.username ++ " - Join?"


inputNewUserview : SetState -> SR.Types.AppInfo -> Html Msg
inputNewUserview setsState appInfo =
            case setsState of 
                UsersFetched sUsers user -> 
                    Framework.responsiveLayout [] <|
                        Element.column
                            Framework.container
                            [ Element.el Heading.h4 <| Element.text "Create New User"
                            , inputNewUser setsState appInfo
                            , newuserConfirmPanel appInfo.user (Data.Users.asList sUsers)
                            ]
                _ ->
                    Html.text "Fail inputNewUserview"


updateExistingUserView : Model -> Html Msg
updateExistingUserView model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            case allSets of 
                UsersFetched sUsers user -> 
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
        AppOps walletState allSets appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text "Create New Ladder Ranking"
                    , inputNewLadder appInfo allSets
                    , newrankingconfirmbutton appInfo allSets
                    , SR.Elements.footer
                    ]

        _ ->
            Html.text "Fail"


displayChallengeBeforeConfirmView : Model -> Html Msg
displayChallengeBeforeConfirmView model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| appInfo.user.username ++ " - Confirm Challenge"
                    , confirmChallengebutton model
                    ]

        _ ->
            Html.text "Error"


displayResultBeforeConfirmView : Model -> Html Msg
displayResultBeforeConfirmView model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
                case allSets of 
                    Selected sSelected sUsers _ ->
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
                        Html.text "Error"
        _ ->
            Html.text "Error"


txErrorView : Model -> Html Msg
txErrorView model =
    case model of
        AppOps walletState allSets appInfo uiState txRec ->
            let
                playerAsUser =
                    --SR.ListOps.gotUserFromUserList (EverySet.fromList allSets) appInfo.player.player.address
                    case allSets of 
                        UsersFetched users user ->
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
            Html.text "Error"


greetingView : String -> Html Msg
greetingView greetingMsg =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , greetingHeading greetingMsg
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        AppOps walletStatus _ _ _ txRec ->
            -- the orig code uses Ports.walletSentry ... same as here:
            case walletStatus of
                SR.Types.WalletStateUnknown ->
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                SR.Types.WalletStateMissing ->
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                SR.Types.WalletStateLocked ->
                    Sub.none

                SR.Types.WalletStateAwaitOpening ->
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                SR.Types.WalletOpened ->
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                SR.Types.WalletOperational ->
                    Sub.none

                SR.Types.WalletWaitingForTransactionReceipt ->
                    let
                        _ =
                            Debug.log "SR.Types is now WalletWaitingForTransactionReceipt :" walletStatus
                    in
                    Sub.batch
                        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                        , Eth.Sentry.Tx.listen txRec.txSentry
                        ]

                _ ->
                    let
                        _ =
                            Debug.log "walletStatus :" walletStatus
                    in
                    Sub.none

        Failure _ ->
            Sub.none



--Helper functions


-- isOpponentHigherRank : SR.Types.UserPlayer -> SR.Types.Opponent -> SR.Types.OpponentRelativeRank
-- isOpponentHigherRank uplayer opponent =
--     -- nb. if player rank is 'higher' than opponent his rank integer will actually be 'less than' opponent
--     -- we go by the integer ...
--     if uplayer.player.rank > opponent.player.rank then
--         SR.Types.OpponentRankHigher

--     else
--         SR.Types.OpponentRankLower



-- Http ops


gotUserList : Cmd Msg
gotUserList =
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


gotRankingList : Cmd Msg
gotRankingList =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> GlobalRankingsReceived) SR.Decode.rankingsDecoder
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
            , userjoinrankings = []
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


httpAddCurrentUserToPlayerList : SetState -> SR.Types.User -> Cmd Msg
httpAddCurrentUserToPlayerList setsState userRec =

    case setsState of 
        Selected sSelected sUsers rnkId -> 
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
                            _ = Debug.log "httpAddCurrentUserToPlayerList - setsState" setsState
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

httpDeleteSelectedRankingFromGlobalList : Json.Encode.Value -> List SR.Types.Ranking -> Cmd Msg
httpDeleteSelectedRankingFromGlobalList newJsonEncodedList globalListWithRankingDeleted =
    Http.request
            { body =
                Http.jsonBody <| Data.Global.newJsonEncodedList globalListWithRankingDeleted
            , expect = Http.expectJson (RemoteData.fromResult >> DeletedRankingFromGlobalList) SR.Decode.decodeNewRankingListServerResponse
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
        , expect = Http.expectJson (RemoteData.fromResult >> DeletedSingleRankingFromJsonBin) SR.Decode.decodeNewRankingListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
        , method = "DELETE"
        , timeout = Nothing
        , tracker = Nothing
        , url = "https://api.jsonbin.io/b/" ++ rankingId
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


httpPlayerList : SetState -> Cmd Msg
httpPlayerList setsState =
    case setsState of
        Selected sSelected sUsers rnkId -> 
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
                _ = Debug.log "setsState - httpPlayerList" setsState
            in
                Cmd.none
    


updateUsersJoinRankings : String -> SR.Types.User -> List SR.Types.User -> Cmd Msg
updateUsersJoinRankings rankingId user lUser =
    Http.request
        { body =
            Http.jsonBody <| SR.Encode.encodeUserList <| Data.Users.addedNewJoinedRankingIdToUser rankingId user lUser
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromUserListUpdate) SR.Decode.decodeNewUserListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlUpdateUserListAndRespond
        }


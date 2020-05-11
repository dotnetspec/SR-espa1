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
import SR.ListOps
import SR.Types
import Task
import Time exposing (Posix)
import Utils.MyUtils


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
-- current state of model


type Model
    = AppOps SR.Types.WalletState SR.Types.AllLists SR.Types.AppInfo SR.Types.UIState TxRecord
    | Failure String


init : () -> ( Model, Cmd Msg )
init _ =
    ( AppOps SR.Types.WalletStateUnknown SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UILoading emptyTxRecord
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
    | ResetToShowGlobal
    | ResetToShowSelected
    | DeletedRanking String
    | ChallengeOpponentClicked SR.Types.UserPlayer
    | ClickedJoinSelected
    | LadderNameInputChg String
    | LadderDescInputChg String
    | ClickedNewRankingRequested SR.Types.RankingInfo
    | ChangedUIStateToCreateNewLadder
    | NewChallengeConfirmClicked
    | ChangedUIStateToEnterResult SR.Types.UserPlayer
    | NewUserNameInputChg String
    | NewUserDescInputChg String
    | NewUserEmailInputChg String
    | NewUserMobileInputChg String
    | CreateNewUserRequested SR.Types.User
      -- App Only Ops
    | MissingWalletInstructions
    | OpenWalletInstructions
    | Fail String
    | NoOp
    | SentResultToJsonbin (Result Http.Error ())
    | SentUserInfoAndDecodedResponseToNewUser (RemoteData.WebData (List SR.Types.User))
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | GlobalRankingsReceived (RemoteData.WebData (List SR.Types.RankingInfo))
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | UsersReceived (RemoteData.WebData (List SR.Types.User))
    | ReturnFromPlayerListUpdate (RemoteData.WebData (List SR.Types.Player))
    | ReturnFromUserListUpdate (RemoteData.WebData (List SR.Types.User))
    | DeletedRankingFromGlobalList (RemoteData.WebData (List SR.Types.RankingInfo))
    | DeletedSingleRankingFromJsonBin (RemoteData.WebData (List SR.Types.RankingInfo))
    | SentResultToWallet SR.Types.ResultOfMatch
    | AddedNewRankingToGlobalList (RemoteData.WebData (List SR.Types.RankingInfo))
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
        AppOps walletState allLists appInfo uiState txRec ->
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
                    handleWalletWaitingForTransactionReceipt msgOfTransitonThatAlreadyHappened walletState allLists appInfo txRec

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
                            ( AppOps SR.Types.WalletStateLocked SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord
                            , Cmd.none
                            )

                        Just uaddr ->
                            --( gotWalletAddrApplyToUser model uaddr, gotUserList )
                            ( gotWalletAddrApplyToUser model uaddr, Cmd.none )

                _ ->
                    ( Failure "handleWalletStateUnknown at Rinkeby"
                    , Cmd.none
                    )

        _ ->
            ( Failure "handleWalletStateUnknown here"
            , Cmd.none
            )


handleWalletStateLocked : Msg -> Model -> ( Model, Cmd Msg )
handleWalletStateLocked msg model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            case msg of
                WalletStatus walletSentry_ ->
                    ( AppOps SR.Types.WalletStateLocked SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord, Cmd.none )

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
        AppOps walletState allLists appInfo uiState txRec ->
            case msg of
                WalletStatus walletSentry_ ->
                    case walletSentry_.networkId of
                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    ( AppOps SR.Types.WalletStateLocked SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord
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
        AppOps walletState allLists appInfo uiState txRec ->
            case msg of
                WalletStatus walletSentry_ ->
                    if SR.ListOps.isUserInListStrAddr allLists.users appInfo.user.ethaddress then
                        ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                    else
                        ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UICreateNewUser emptyTxRecord, Cmd.none )

                UsersReceived userList ->
                    let
                        userLAddedToAllLists =
                            { allLists | users = SR.ListOps.validatedUserList <| SR.ListOps.extractUsersFromWebData userList }
                    in
                    updateOnUserListReceived model userLAddedToAllLists.users

                GlobalRankingsReceived rmtrnkingdata ->
                    handleGlobalRankingsReceived rmtrnkingdata model

                NoOp ->
                    ( model, Cmd.none )

                _ ->
                    ( Failure "handledWalletStateOpened2"
                    , Cmd.none
                    )

        Failure str ->
            ( Failure "handledWalletStateOpened3", Cmd.none )


handleWalletStateOperational : Msg -> Model -> ( Model, Cmd Msg )
handleWalletStateOperational msg model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            case msg of
                -- nb. walletSentry is constantly being updated via the sub
                WalletStatus walletSentry_ ->
                    ( model, Cmd.none )

                TxSentryMsg subMsg ->
                    let
                        _ =
                            Debug.log "handleTxSubMsg subMsg" <| handleTxSubMsg subMsg

                        ( subModel, subCmd ) =
                            Eth.Sentry.Tx.update subMsg txRec.txSentry
                    in
                    if handleTxSubMsg subMsg then
                        ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIRenderAllRankings txRec, Cmd.none )

                    else
                        ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )

                WatchTxHash (Ok txHash) ->
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txHash = Just txHash }, Cmd.none )

                WatchTxHash (Err err) ->
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

                WatchTx (Ok tx) ->
                    let
                        _ =
                            Debug.log "handleWalletStateOpenedAndOperational" "tx Ok"
                    in
                    AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIRenderAllRankings { txRec | tx = Just tx } |> update (ProcessResult SR.Types.Won)

                WatchTx (Err err) ->
                    let
                        _ =
                            Debug.log "handleWalletStateOpenedAndOperational tx err" err
                    in
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

                WatchTxReceipt (Ok txReceipt) ->
                    let
                        _ =
                            Debug.log "handleWalletStateOpenedAndOperational Receipt" txReceipt
                    in
                    AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord
                        |> update (ProcessResult SR.Types.Won)

                WatchTxReceipt (Err err) ->
                    let
                        _ =
                            Debug.log "tx err" err
                    in
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

                TrackTx blockDepth ->
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | blockDepth = Just blockDepth }, Cmd.none )

                -- UsersReceived userList ->
                --     let
                --         userLAddedToAllLists =
                --             { allLists | users = SR.ListOps.validatedUserList <| SR.ListOps.extractUsersFromWebData userList }
                --     in
                --     updateOnUserListReceived model userLAddedToAllLists.users
                ProcessResult result ->
                    let
                        _ =
                            Debug.log "process result" result
                    in
                    case result of
                        SR.Types.Won ->
                            let
                                newModel =
                                    handleWon model
                            in
                            case newModel of
                                AppOps thewalletState allTheLists theAppInfo theUIState thetxRec ->
                                    ( newModel, updatePlayerList theAppInfo.selectedRanking.id allTheLists.userPlayers )

                                _ ->
                                    ( Failure "result won", Cmd.none )

                        SR.Types.Lost ->
                            let
                                newModel =
                                    handleLost model
                            in
                            case newModel of
                                AppOps thewalletState allTheLists theAppInfo theUIState thetxRec ->
                                    ( newModel, updatePlayerList theAppInfo.selectedRanking.id allTheLists.userPlayers )

                                _ ->
                                    ( Failure "result lost", Cmd.none )

                        SR.Types.Undecided ->
                            let
                                newModel =
                                    handleUndecided model
                            in
                            case newModel of
                                AppOps thewalletState allTheLists theAppInfo theUIState thetxRec ->
                                    ( newModel, updatePlayerList theAppInfo.selectedRanking.id allTheLists.userPlayers )

                                _ ->
                                    ( Failure "result lost", Cmd.none )

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
                    in
                    ( AppOps SR.Types.WalletWaitingForTransactionReceipt allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                    , sentryCmd
                    )

                SentResultToJsonbin a ->
                    ( AppOps SR.Types.WalletOperational
                        allLists
                        appInfo
                        uiState
                        txRec
                    , Cmd.none
                    )

                -- GlobalRankingsReceived rmtrnkingdata ->
                --     handleGlobalRankingsReceived rmtrnkingdata model
                -- ClickedSelectedRanking rnkidstr rnkownerstr rnknamestr ->
                --     let
                --         _ =
                --             Debug.log "on click " rnkidstr
                --         newSelectedRanking =
                --             appInfo.selectedRanking
                --         newRnkInfo =
                --             { newSelectedRanking | id = Utils.MyUtils.stringFromRankingId rnkidstr, rankingowneraddr = rnkownerstr, rankingname = rnknamestr }
                --         newAppInfo =
                --             { appInfo | selectedRanking = newRnkInfo }
                --         updatedUiState =
                --             ensuredCorrectSelectedUI newAppInfo allLists
                --     in
                --     ( AppOps SR.Types.WalletOperational allLists newAppInfo updatedUiState emptyTxRecord, fetchedSingleRanking rnkidstr )
                ClickedSelectedOwnedRanking rnkidstr rnkownerstr rnknamestr ->
                    let
                        _ =
                            Debug.log "on click owner" rnkidstr

                        newAppInfo =
                            updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr
                    in
                    ( AppOps SR.Types.WalletOperational allLists newAppInfo SR.Types.UISelectedRankingUserIsOwner emptyTxRecord, fetchedSingleRanking rnkidstr )

                ClickedSelectedMemberRanking rnkidstr rnkownerstr rnknamestr ->
                    let
                        _ =
                            Debug.log "on click member" rnkidstr

                        newAppInfo =
                            updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr
                    in
                    ( AppOps SR.Types.WalletOperational allLists newAppInfo SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord, fetchedSingleRanking rnkidstr )

                ClickedSelectedNeitherOwnerNorMember rnkidstr rnkownerstr rnknamestr ->
                    let
                        _ =
                            Debug.log "on click other " rnkidstr

                        newAppInfo =
                            updateAppInfoOnRankingSelected appInfo rnkidstr rnkownerstr rnknamestr
                    in
                    ( AppOps SR.Types.WalletOperational allLists newAppInfo SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, fetchedSingleRanking rnkidstr )

                -- this is the response from createNewPlayerListWithCurrentUser Cmd
                -- it had the Http.expectStringResponse in it
                -- it's already created the new ranking with current player as the first entry
                -- the result now is the ranking id only at this point which was pulled out by the decoder
                SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder ->
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.CreateNewLadder emptyTxRecord
                    , addedNewRankingListEntryInGlobal idValueFromDecoder
                        allLists.userRankings
                        appInfo.selectedRanking
                        appInfo.user.ethaddress
                        allLists.users
                    )

                SentUserInfoAndDecodedResponseToNewUser serverResponse ->
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetToShowGlobal ->
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetToShowSelected ->
                    -- let
                    --     uiType =
                    --         ensuredCorrectSelectedUI appInfo allLists
                    -- in
                    -- ( AppOps SR.Types.WalletOperational allLists appInfo uiType emptyTxRecord, Cmd.none )
                    ( model, Cmd.none )

                ChangedUIStateToCreateNewLadder ->
                    let
                        rankingInfoFromModel =
                            appInfo.selectedRanking

                        rankingWithFieldsCleared =
                            { rankingInfoFromModel | rankingname = "", rankingdesc = "" }

                        newAppInfo =
                            { appInfo | selectedRanking = rankingWithFieldsCleared }
                    in
                    ( AppOps SR.Types.WalletOperational allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

                AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList ->
                    let
                        extractedList =
                            SR.ListOps.ownerValidatedRankingList <| SR.ListOps.extractRankingsFromWebData updatedListAfterNewEntryAddedToGlobalList

                        allGlobal =
                            SR.ListOps.createAllUserAsOwnerGlobalRankingList extractedList allLists.users

                        addedRankingListToAllLists =
                            { allLists
                                | userRankings = allGlobal
                            }
                    in
                    ( AppOps SR.Types.WalletOperational addedRankingListToAllLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                LadderNameInputChg namefield ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        updatedSelectedRanking =
                            { newSelectedRanking | rankingname = namefield }

                        newAppInfo =
                            { appInfo | selectedRanking = updatedSelectedRanking }
                    in
                    ( AppOps SR.Types.WalletOperational allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

                LadderDescInputChg descfield ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        updatedSelectedRanking =
                            { newSelectedRanking | rankingdesc = descfield }

                        newAppInfo =
                            { appInfo | selectedRanking = updatedSelectedRanking }
                    in
                    ( AppOps SR.Types.WalletOperational allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

                ClickedNewRankingRequested newLadderRnkInfo ->
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
                            { appInfo | selectedRanking = newLadderRnkInfo }
                    in
                    ( AppOps SR.Types.WalletOperational allLists newAppInfo SR.Types.CreateNewLadder { txRec | txSentry = newSentry }, sentryCmd )

                NewChallengeConfirmClicked ->
                    createNewPlayerListWithNewChallengeAndUpdateJsonBin model

                PlayersReceived lplayer ->
                    ( updatedSelectedRankingOnPlayersReceived model (extractAndSortPlayerList lplayer allLists.users), Cmd.none )

                ChangedUIStateToEnterResult player ->
                    ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIEnterResult emptyTxRecord, Cmd.none )

                DeletedRanking uaddr ->
                    ( AppOps SR.Types.WalletOperational
                        allLists
                        appInfo
                        uiState
                        txRec
                    , deleteSelectedRankingFromJsonBin appInfo.selectedRanking.id
                    )

                DeletedSingleRankingFromJsonBin result ->
                    ( AppOps SR.Types.WalletOperational
                        allLists
                        appInfo
                        uiState
                        txRec
                    , deleteSelectedRankingFromGlobalList appInfo.selectedRanking.id (SR.ListOps.extractRankingList allLists.userRankings) allLists.users appInfo.user.ethaddress
                    )

                ChallengeOpponentClicked opponentAsPlayer ->
                    ( updatedForChallenge model allLists.userPlayers opponentAsPlayer appInfo.user, Cmd.none )

                DeletedRankingFromGlobalList updatedListAfterRankingDeletedFromGlobalList ->
                    let
                        extractedList =
                            SR.ListOps.ownerValidatedRankingList <| SR.ListOps.extractRankingsFromWebData updatedListAfterRankingDeletedFromGlobalList

                        allGlobal =
                            SR.ListOps.createAllUserAsOwnerGlobalRankingList extractedList allLists.users

                        addedRankingListToAllLists =
                            { allLists
                                | userRankings = allGlobal
                            }
                    in
                    ( AppOps SR.Types.WalletOperational addedRankingListToAllLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ClickedJoinSelected ->
                    -- this updates the player and user lists
                    ( model, Cmd.batch [ addCurrentUserToPlayerList appInfo.selectedRanking.id allLists.userPlayers appInfo.user, updateUsersJoinRankings appInfo.selectedRanking.id appInfo.user allLists.users ] )

                ReturnFromPlayerListUpdate response ->
                    let
                        lplayer =
                            SR.ListOps.extractPlayersFromWebData response

                        convertedToUserPlayers =
                            SR.ListOps.convertPlayersToUserPlayers
                                lplayer
                                allLists.users

                        -- lplayer
                        -- allLists.users
                    in
                    ( updateSelectedRankingPlayerList model convertedToUserPlayers, Cmd.none )

                ReturnFromUserListUpdate response ->
                    ( updateUserList model (SR.ListOps.extractUsersFromWebData response), Cmd.none )

                TimeUpdated posixTime ->
                    let
                        _ =
                            Debug.log "posixtime" posixTime
                    in
                    ( model, Cmd.none )

                NewUserNameInputChg namefield ->
                    ( handleNewUserInputs model (NewUserNameInputChg namefield), Cmd.none )

                NewUserDescInputChg namefield ->
                    ( handleNewUserInputs model (NewUserDescInputChg namefield), Cmd.none )

                NewUserEmailInputChg namefield ->
                    ( handleNewUserInputs model (NewUserEmailInputChg namefield), Cmd.none )

                NewUserMobileInputChg namefield ->
                    ( handleNewUserInputs model (NewUserMobileInputChg namefield), Cmd.none )

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
                            { appInfo | user = userWithUpdatedAddr }
                    in
                    ( AppOps SR.Types.WalletOperational allLists newAppInfo SR.Types.UIRenderAllRankings { txRec | txSentry = newSentry }, Cmd.batch [ sentryCmd, createNewUser allLists.users userWithUpdatedAddr, Cmd.none ] )

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


handleGlobalRankingsReceived : RemoteData.WebData (List SR.Types.RankingInfo) -> Model -> ( Model, Cmd Msg )
handleGlobalRankingsReceived rmtrnkingdata model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                extractedList =
                    SR.ListOps.ownerValidatedRankingList <| SR.ListOps.extractRankingsFromWebData rmtrnkingdata

                allUserAsOwnerGlobal =
                    SR.ListOps.createAllUserAsOwnerGlobalRankingList extractedList allLists.users

                currentUserAsPlayer =
                    SR.ListOps.gotUserIsPlayerNonUserRankingList appInfo.user extractedList

                addedRankingListToAllLists =
                    { allLists
                        | userRankings = allUserAsOwnerGlobal
                    }

                luserRankingOwner =
                    SR.ListOps.gotUserOwnedGlobalRankingList allUserAsOwnerGlobal appInfo.user

                addedlUserRankingOwnerListToAllLists =
                    { addedRankingListToAllLists
                        | lownedUserRanking = luserRankingOwner
                    }

                luserRankingPlayer =
                    SR.ListOps.createduserRankingPlayerList currentUserAsPlayer allLists.users

                addedUserRankingMemberListToAllLists =
                    { addedlUserRankingOwnerListToAllLists
                        | lmemberUserRanking = luserRankingPlayer
                    }

                ownerPlayerCombinedList =
                    luserRankingOwner ++ luserRankingPlayer

                luserRankingOther =
                    SR.ListOps.gotOthersGlobalRankingList ownerPlayerCombinedList allUserAsOwnerGlobal

                addedUserRankingOtherListToAllLists =
                    { addedUserRankingMemberListToAllLists
                        | lotherUserRanking = luserRankingOther
                    }

                allListsUpdated =
                    addedUserRankingOtherListToAllLists

                _ =
                    Debug.log "lownedUserRanking : " allListsUpdated.lownedUserRanking

                _ =
                    Debug.log "luserRankingOwner : " luserRankingOwner

                -- _ =
                --     Debug.log "combined lists : " ((luserRankingOwner ++ luserRankingPlayer) ++ luserRankingOther)
                --     _ =
                --         Debug.log "combined lists : " ((allListsUpdated.lownedUserRanking ++ allListsUpdated.lmemberUserRanking) ++ allListsUpdated.lotherUserRanking)
            in
            ( AppOps SR.Types.WalletOperational allListsUpdated appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

        Failure str ->
            ( Failure str, Cmd.none )


handleWalletWaitingForTransactionReceipt : Msg -> SR.Types.WalletState -> SR.Types.AllLists -> SR.Types.AppInfo -> TxRecord -> ( Model, Cmd Msg )
handleWalletWaitingForTransactionReceipt msg walletState allLists appInfo txRec =
    let
        _ =
            Debug.log "Msg" msg
    in
    case msg of
        WalletStatus walletSentry_ ->
            ( AppOps SR.Types.WalletWaitingForTransactionReceipt allLists appInfo SR.Types.UIWaitingForTxReceipt txRec
            , Cmd.none
            )

        TxSentryMsg subMsg ->
            let
                _ =
                    Debug.log "handleTxSubMsg subMsg" <| handleTxSubMsg subMsg
            in
            let
                ( subModel, subCmd ) =
                    Eth.Sentry.Tx.update subMsg txRec.txSentry
            in
            if handleTxSubMsg subMsg then
                ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }, subCmd )

            else
                ( AppOps walletState SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )

        WatchTxHash (Ok txHash) ->
            ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txHash = Just txHash }, Cmd.none )

        WatchTxHash (Err err) ->
            ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTx (Ok tx) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForTransactionReceipt" "tx ok"
            in
            AppOps walletState allLists appInfo SR.Types.UIRenderAllRankings { txRec | tx = Just tx } |> update (ProcessResult SR.Types.Won)

        WatchTx (Err err) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForTransactionReceipt tx err" err
            in
            ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTxReceipt (Ok txReceipt) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForTransactionReceipt tx ok" txReceipt
            in
            AppOps walletState allLists appInfo SR.Types.UIRenderAllRankings { txRec | txReceipt = Just txReceipt } |> update (ProcessResult SR.Types.Won)

        WatchTxReceipt (Err err) ->
            let
                _ =
                    Debug.log "tx err" err
            in
            ( AppOps SR.Types.WalletOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

        _ ->
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
        AppOps walletState allLists appInfo uiState txRec ->
            let
                newAppInfoUser =
                    appInfo.user

                newUserWithAddr =
                    { newAppInfoUser | ethaddress = Eth.Utils.addressToString uaddr }

                newAppInfo =
                    { appInfo | user = newUserWithAddr }
            in
            AppOps SR.Types.WalletOpened allLists newAppInfo SR.Types.UILoading emptyTxRecord

        _ ->
            Failure "gotWalletAddrApplyToUser"


handleWon : Model -> Model
handleWon model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                whoHigher =
                    isOpponentHigherRank appInfo.player appInfo.challenger
            in
            case whoHigher of
                SR.Types.OpponentRankHigher ->
                    let
                        -- update the player list for both players challenger to emptyPlayer and change rankings
                        updatedPlayerListForPlayer =
                            SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.userPlayers appInfo.player appInfo.challenger.player.rank

                        updatedPlayerListForPlayerAndChallenger =
                            SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger (appInfo.challenger.player.rank + 1)

                        --update current player now
                        newUserPlayerPlayer =
                            appInfo.player.player

                        newPlayerUpdated =
                            { newUserPlayerPlayer | address = "" }

                        currentUserPlayer =
                            appInfo.player

                        newUserPlayerUpdated =
                            { currentUserPlayer | player = newPlayerUpdated, user = appInfo.user }

                        newAppInfo =
                            { appInfo | player = newUserPlayerUpdated, challenger = SR.Defaults.emptyUserPlayer }

                        newAllLists =
                            { allLists | userPlayers = updatedPlayerListForPlayerAndChallenger }
                    in
                    --nb. higher rank is a lower number and vice versa!
                    AppOps walletState
                        newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

                SR.Types.OpponentRankLower ->
                    let
                        --no ranking change - just update the player list for both players challenger to emptyPlayer, no rank change
                        --update the player list
                        updatedPlayerListForPlayer =
                            SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.userPlayers appInfo.player appInfo.player.player.rank

                        updatedPlayerListForPlayerAndChallenger =
                            SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.player.rank

                        --update current player now
                        newUserPlayerPlayer =
                            appInfo.player.player

                        newUserPlayerPlayerUpdated =
                            { newUserPlayerPlayer | address = "" }

                        newAppInfoPlayer =
                            appInfo.player

                        newAppInfoUserPlayer =
                            { newAppInfoPlayer | player = newUserPlayerPlayerUpdated }

                        newAppInfo =
                            { appInfo | player = newAppInfoUserPlayer, challenger = SR.Defaults.emptyUserPlayer }

                        newAllLists =
                            { allLists | userPlayers = updatedPlayerListForPlayerAndChallenger }
                    in
                    --nb. higher rank is a lower number and vice versa!
                    AppOps walletState
                        newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

        _ ->
            Failure "HandleWonFail"


handleLost : Model -> Model
handleLost model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                whoHigher =
                    isOpponentHigherRank appInfo.player appInfo.challenger
            in
            case whoHigher of
                SR.Types.OpponentRankHigher ->
                    let
                        updatedPlayerListForPlayer =
                            SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.userPlayers appInfo.player appInfo.player.player.rank

                        updatedPlayerListForPlayerAndChallenger =
                            SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.player.rank

                        --update current player now
                        newUserPlayer =
                            appInfo.player

                        newUserPlayerPlayer =
                            appInfo.player.player

                        newPlayerUpdated =
                            { newUserPlayerPlayer | address = "" }

                        newUserPlayerUpdated =
                            { newUserPlayer | player = newPlayerUpdated }

                        newAppInfo =
                            { appInfo | player = newUserPlayerUpdated, challenger = SR.Defaults.emptyUserPlayer }

                        newAllLists =
                            { allLists | userPlayers = updatedPlayerListForPlayerAndChallenger }
                    in
                    --nb. higher rank is a lower number and vice versa!
                    AppOps walletState
                        newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

                SR.Types.OpponentRankLower ->
                    --nb. higher rank is a lower number and vice versa!
                    let
                        updatedPlayerListForPlayer =
                            SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.userPlayers appInfo.player (appInfo.player.player.rank + 1)

                        updatedPlayerListForPlayerAndChallenger =
                            SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.player.player.rank

                        --update current player now
                        newUserPlayer =
                            appInfo.player

                        newUserPlayerPlayer =
                            appInfo.player.player

                        newUserPlayerPlayerUpdated =
                            { newUserPlayerPlayer | address = "" }

                        newUserPlayerUpdated =
                            { newUserPlayer | player = newUserPlayerPlayerUpdated }

                        newAppInfo =
                            { appInfo | player = newUserPlayerUpdated, challenger = SR.Defaults.emptyUserPlayer }

                        newAllLists =
                            { allLists | userPlayers = updatedPlayerListForPlayerAndChallenger }
                    in
                    AppOps walletState
                        newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

        _ ->
            Failure "Fail handleLost"


handleUndecided : Model -> Model
handleUndecided model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                updatedPlayerListForPlayer =
                    SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.userPlayers appInfo.player appInfo.player.player.rank

                updatedPlayerListForPlayerAndChallenger =
                    SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.player.rank

                --update current player now
                newUserPlayer =
                    appInfo.player

                newUserPlayerPlayer =
                    appInfo.player.player

                newPlayerPlayerUpdated =
                    { newUserPlayerPlayer | address = "" }

                newUserPlayerUpdated =
                    { newUserPlayer | player = newPlayerPlayerUpdated }

                newAppInfo =
                    { appInfo | player = newUserPlayerUpdated, challenger = SR.Defaults.emptyUserPlayer }

                newAllLists =
                    { allLists | userPlayers = updatedPlayerListForPlayerAndChallenger }
            in
            AppOps walletState
                newAllLists
                newAppInfo
                SR.Types.UISelectedRankingUserIsPlayer
                txRec

        _ ->
            Failure "Fail in handleUndecided"



-- ensuredCorrectSelectedUI : SR.Types.AppInfo -> SR.Types.AllLists -> SR.Types.UIState
-- ensuredCorrectSelectedUI appInfo allLists =
--     --if SR.ListOps.isUserSelectedOwnerOfRanking appInfo.selectedRanking (SR.ListOps.extractRankingList allLists.userRankings) appInfo.user then
--     let
--         _ =
--             Debug.log "member?" SR.ListOps.isUserMemberOfSelectedRanking allLists.userPlayers appInfo.user
--     in
--     if SR.ListOps.isUserOwnerOfSelectedUserRanking appInfo.selectedRanking allLists.lownedUserRanking appInfo.user then
--         SR.Types.UISelectedRankingUserIsOwner
--     else if SR.ListOps.isUserMemberOfSelectedRanking allLists.userPlayers appInfo.user then
--         SR.Types.UISelectedRankingUserIsPlayer
--     else
--         SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer
-- if SR.ListOps.isUserOwnerOfSelectedUserRanking appInfo.selectedRanking allLists.lownedUserRanking appInfo.user then
--                 ( AppOps SR.Types.WalletOperational allLists newAppInfo updatedUiState emptyTxRecord, fetchedSingleRanking rnkidstr )
--             else if SR.ListOps.isUserMemberOfSelectedRanking allLists.userPlayers appInfo.user then
--                 ( AppOps SR.Types.WalletOperational allLists newAppInfo updatedUiState emptyTxRecord, fetchedSingleRanking rnkidstr )
--             else
--                 ( AppOps SR.Types.WalletOperational allLists newAppInfo updatedUiState emptyTxRecord, fetchedSingleRanking rnkidstr )


createNewPlayerListWithNewResultAndUpdateJsonBin : Model -> ( Model, Cmd Msg )
createNewPlayerListWithNewResultAndUpdateJsonBin model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                -- add respective challenger addresses to player and challenger (who is also a player type)
                newplayerListWithPlayerUpdated =
                    SR.ListOps.updatePlayerRankWithWonResult allLists.userPlayers appInfo.player

                challengerAsPlayer =
                    SR.ListOps.gotUserPlayerFromPlayerListStrAddress allLists.userPlayers appInfo.challenger.player.address

                newplayerListWithPlayerAndChallengerUpdated =
                    SR.ListOps.setPlayerInPlayerListWithNewChallengerAddr newplayerListWithPlayerUpdated challengerAsPlayer appInfo.player.player.address

                sortedByRankingnewplayerListWithPlayerAndChallengerUpdated =
                    SR.ListOps.sortedPlayerListByRank newplayerListWithPlayerAndChallengerUpdated

                newAllLists =
                    { allLists | userPlayers = sortedByRankingnewplayerListWithPlayerAndChallengerUpdated }
            in
            ( AppOps walletState newAllLists appInfo uiState txRec, updatePlayerList appInfo.selectedRanking.id newAllLists.userPlayers )

        _ ->
            ( Failure "createNewPlayerListWithNewResultAndUpdateJsonBin", Cmd.none )


createNewPlayerListWithNewChallengeAndUpdateJsonBin : Model -> ( Model, Cmd Msg )
createNewPlayerListWithNewChallengeAndUpdateJsonBin model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                -- add respective challenger addresses to player and challenger (who is also a player type)
                newplayerListWithPlayerUpdated =
                    SR.ListOps.setPlayerInPlayerListWithNewChallengerAddr allLists.userPlayers appInfo.player appInfo.challenger.player.address

                challengerAsPlayer =
                    SR.ListOps.gotUserPlayerFromPlayerListStrAddress allLists.userPlayers appInfo.challenger.player.address

                newplayerListWithPlayerAndChallengerUpdated =
                    SR.ListOps.setPlayerInPlayerListWithNewChallengerAddr newplayerListWithPlayerUpdated challengerAsPlayer appInfo.player.player.address

                sortedByRankingnewplayerListWithPlayerAndChallengerUpdated =
                    SR.ListOps.sortedPlayerListByRank newplayerListWithPlayerAndChallengerUpdated

                newAllLists =
                    { allLists | userPlayers = sortedByRankingnewplayerListWithPlayerAndChallengerUpdated }
            in
            ( AppOps walletState newAllLists appInfo uiState txRec, updatePlayerList appInfo.selectedRanking.id newAllLists.userPlayers )

        _ ->
            ( Failure "createNewPlayerListWithNewChallengeAndUpdateJsonBin", Cmd.none )


handleNewUserInputs : Model -> Msg -> Model
handleNewUserInputs model msg =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            case msg of
                NewUserNameInputChg namefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | username = namefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }

                        _ =
                            Debug.log "currentUformfield" .username
                    in
                    AppOps walletState allLists newAppInfo SR.Types.UICreateNewUser txRec

                NewUserDescInputChg descfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | description = descfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState allLists newAppInfo SR.Types.UICreateNewUser txRec

                NewUserEmailInputChg emailfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | email = emailfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState allLists newAppInfo SR.Types.UICreateNewUser txRec

                NewUserMobileInputChg mobilefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | mobile = mobilefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps walletState allLists newAppInfo SR.Types.UICreateNewUser txRec

                _ ->
                    Failure "NewUserNameInputChg"

        _ ->
            Failure "NewUserNameInputChg"


extractAndSortPlayerList : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.User -> List SR.Types.UserPlayer
extractAndSortPlayerList rdlPlayer luser =
    let
        lplayer =
            SR.ListOps.extractPlayersFromWebData rdlPlayer

        convertedPlayerListToUserPlayerList =
            SR.ListOps.convertPlayersToUserPlayers
                lplayer
                luser
    in
    SR.ListOps.sortedPlayerListByRank <| convertedPlayerListToUserPlayerList


updatedForChallenge : Model -> List SR.Types.UserPlayer -> SR.Types.UserPlayer -> SR.Types.User -> Model
updatedForChallenge model luplayer opponentAsPlayer user =
    case model of
        AppOps walletState allLists appInfo _ txRec ->
            let
                newAppInfoWithPlayer =
                    { appInfo | player = SR.ListOps.gotCurrentUserAsPlayerFromPlayerList luplayer user }

                newAppInfoWithChallengerAndPlayer =
                    { newAppInfoWithPlayer | challenger = opponentAsPlayer }

                newAllListsWithSelectedRankingUpdate =
                    updateSelectedRankingOnChallenge allLists newAppInfoWithChallengerAndPlayer
            in
            AppOps walletState newAllListsWithSelectedRankingUpdate newAppInfoWithChallengerAndPlayer SR.Types.UIChallenge txRec

        _ ->
            Failure <| "updatedForChallenge : "


updateSelectedRankingOnChallenge : SR.Types.AllLists -> SR.Types.AppInfo -> SR.Types.AllLists
updateSelectedRankingOnChallenge allLists appInfo =
    allLists


updateOnUserListReceived : Model -> List SR.Types.User -> ( Model, Cmd Msg )
updateOnUserListReceived model userList =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                gotUserToUpdateAddr =
                    SR.ListOps.singleUserInListStrAddr userList appInfo.user.ethaddress

                userWithUpdatedAddr =
                    { gotUserToUpdateAddr | ethaddress = appInfo.user.ethaddress }

                userUpdatedInAppInfo =
                    { appInfo | user = userWithUpdatedAddr }

                newAllLists =
                    { allLists | users = userList }
            in
            if SR.ListOps.isUserInListStrAddr userList userUpdatedInAppInfo.user.ethaddress then
                let
                    _ =
                        Debug.log "have user" userUpdatedInAppInfo.user.ethaddress
                in
                ( AppOps SR.Types.WalletOpened newAllLists userUpdatedInAppInfo SR.Types.UIRenderAllRankings emptyTxRecord, gotRankingList )

            else
                let
                    _ =
                        Debug.log "not existing user" appInfo.user.ethaddress
                in
                ( AppOps SR.Types.WalletOpened newAllLists userUpdatedInAppInfo SR.Types.UICreateNewUser txRec, Cmd.none )

        _ ->
            ( Failure "should be in AppOps", Cmd.none )


updateSelectedRankingPlayerList : Model -> List SR.Types.UserPlayer -> Model
updateSelectedRankingPlayerList model luplayers =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                resetSelectedRankingPlayerList =
                    { allLists | userPlayers = luplayers }

                -- uiState =
                --     ensuredCorrectSelectedUI appInfo allLists
            in
            AppOps walletState resetSelectedRankingPlayerList appInfo uiState txRec

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


updateUserList : Model -> List SR.Types.User -> Model
updateUserList model lusers =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                resetUserList =
                    { allLists | users = lusers }

                -- uiState =
                --     ensuredCorrectSelectedUI appInfo allLists
            in
            AppOps walletState resetUserList appInfo uiState txRec

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


updatedSelectedRankingOnPlayersReceived : Model -> List SR.Types.UserPlayer -> Model
updatedSelectedRankingOnPlayersReceived model luplayer =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                newAppPlayer =
                    { appInfo | player = SR.ListOps.gotUserPlayerFromPlayerListStrAddress luplayer appInfo.user.ethaddress }

                newAppChallengerAndPlayer =
                    { newAppPlayer | challenger = SR.ListOps.gotUserPlayerFromPlayerListStrAddress luplayer newAppPlayer.player.player.challengeraddress }

                -- current
                --
                userPlayerOwner =
                    SR.ListOps.gotRankingOwnerAsUserPlayer appInfo.selectedRanking allLists.userRankings luplayer

                _ =
                    Debug.log "userPlayerOwner" userPlayerOwner

                allListsPlayersAdded =
                    { allLists | userPlayers = luplayer }

                -- uistate =
                --     ensuredCorrectSelectedUI appInfo allLists
                _ =
                    Debug.log "userPlayers " allListsPlayersAdded.userPlayers
            in
            AppOps walletState allListsPlayersAdded newAppChallengerAndPlayer uiState emptyTxRecord

        _ ->
            Failure <| "updatedSelectedRankingOnPlayersReceived : "



-- view


view : Model -> Html Msg
view model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            case uiState of
                SR.Types.CreateNewLadder ->
                    inputNewLadderview model

                SR.Types.UISelectedRankingUserIsOwner ->
                    selectedUserIsOwnerView model

                SR.Types.UISelectedRankingUserIsPlayer ->
                    selectedUserIsPlayerView model

                SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer ->
                    selectedUserIsNeitherOwnerNorPlayerView model

                SR.Types.UIRenderAllRankings ->
                    -- let
                    --     _ =
                    --         Debug.log "allLists.lownedUserRanking" allLists.lownedUserRanking
                    -- in
                    globalResponsiveview allLists.lownedUserRanking allLists.lmemberUserRanking allLists.lotherUserRanking appInfo.user

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

                SR.Types.UICreateNewUser ->
                    inputNewUserview model

                _ ->
                    greetingView <| "Loading ... "

        Failure str ->
            greetingView <| "Model failure in view: " ++ str


greetingHeading : String -> Element Msg
greetingHeading greetingStr =
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Initializing ..."
        , Element.column Card.fill
            [ Element.el Heading.h6 <| Element.text greetingStr
            ]
        ]



-- rankingbuttons : List SR.Types.RankingInfo -> Element Msg
-- rankingbuttons rankingList =
--     Element.column Grid.section <|
--         [ Element.el Heading.h5 <| Element.text "Global Rankings"
--         , Element.column (Card.simple ++ Grid.simple) <|
--             insertRankingList rankingList
--         , Element.paragraph (Card.fill ++ Color.warning) <|
--             [ Element.el [ Font.bold ] <| Element.text "Please note: "
--             , Element.paragraph [] <|
--                 List.singleton <|
--                     Element.text "Terms and Conditions Apply"
--             ]
--         ]


ownedrankingbuttons : List SR.Types.UserRanking -> Element Msg
ownedrankingbuttons urankingList =
    let
        newRankingList =
            --SR.ListOps.gotRankingListFromUserRankingList urankingList
            SR.ListOps.extractRankingList urankingList
    in
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Your Created Rankings:"
        , Element.column (Card.simple ++ Grid.simple) <|
            insertOwnedRankingList newRankingList
        ]


memberrankingbuttons : List SR.Types.UserRanking -> Element Msg
memberrankingbuttons urankingList =
    let
        newRankingList =
            --SR.ListOps.gotRankingListFromUserRankingList urankingList
            SR.ListOps.extractRankingList urankingList
    in
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Your Member Rankings: "
        , Element.column (Card.simple ++ Grid.simple) <|
            insertMemberRankingList newRankingList
        ]


otherrankingbuttons : List SR.Types.UserRanking -> Element Msg
otherrankingbuttons urankingList =
    let
        newRankingList =
            --SR.ListOps.gotRankingListFromUserRankingList urankingList
            SR.ListOps.extractRankingList urankingList
    in
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Other Rankings: "
        , Element.column (Card.simple ++ Grid.simple) <|
            insertNeitherOwnerNorMemberRankingList newRankingList
        ]



-- addRankingInfoToAnyElText : SR.Types.RankingInfo -> Element Msg
-- addRankingInfoToAnyElText rankingobj =
--     Element.column Grid.simple <|
--         [ Input.button (Button.fill ++ Color.primary) <|
--             { onPress = Just (ClickedSelectedRanking (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr rankingobj.rankingname)
--             , label = Element.text rankingobj.rankingname
--             }
--         ]
-- insertRankingList : List SR.Types.RankingInfo -> List (Element Msg)
-- insertRankingList rnkgInfoList =
--     let
--         mapOutRankingList =
--             List.map
--                 addRankingInfoToAnyElText
--                 rnkgInfoList
--     in
--     mapOutRankingList


insertOwnedRankingList : List SR.Types.RankingInfo -> List (Element Msg)
insertOwnedRankingList rnkgInfoList =
    let
        mapOutRankingList =
            List.map
                ownedRankingInfoBtn
                rnkgInfoList
    in
    mapOutRankingList


ownedRankingInfoBtn : SR.Types.RankingInfo -> Element Msg
ownedRankingInfoBtn rankingobj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.primary) <|
            { onPress = Just (ClickedSelectedOwnedRanking (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr rankingobj.rankingname)
            , label = Element.text rankingobj.rankingname
            }
        ]


insertMemberRankingList : List SR.Types.RankingInfo -> List (Element Msg)
insertMemberRankingList rnkgInfoList =
    let
        mapOutRankingList =
            List.map
                memberRankingInfoBtn
                rnkgInfoList
    in
    mapOutRankingList


memberRankingInfoBtn : SR.Types.RankingInfo -> Element Msg
memberRankingInfoBtn rankingobj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.primary) <|
            { onPress = Just (ClickedSelectedMemberRanking (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr rankingobj.rankingname)
            , label = Element.text rankingobj.rankingname
            }
        ]


insertNeitherOwnerNorMemberRankingList : List SR.Types.RankingInfo -> List (Element Msg)
insertNeitherOwnerNorMemberRankingList rnkgInfoList =
    let
        mapOutRankingList =
            List.map
                neitherOwnerNorMemberRankingInfoBtn
                rnkgInfoList
    in
    mapOutRankingList


neitherOwnerNorMemberRankingInfoBtn : SR.Types.RankingInfo -> Element Msg
neitherOwnerNorMemberRankingInfoBtn rankingobj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.primary) <|
            { onPress = Just (ClickedSelectedNeitherOwnerNorMember (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr rankingobj.rankingname)
            , label = Element.text rankingobj.rankingname
            }
        ]


playerbuttons : Model -> Element Msg
playerbuttons model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            Element.column Grid.section <|
                [ SR.Elements.selectedRankingHeaderEl appInfo.selectedRanking
                , Element.column (Card.simple ++ Grid.simple) <|
                    insertPlayerList model
                , Element.paragraph (Card.fill ++ Color.warning) <|
                    [ Element.el [ Font.bold ] <| Element.text "Please note: "
                    , Element.paragraph [] <|
                        List.singleton <|
                            Element.text "Clicking 'Create New' interacts with your Ethereum wallet"
                    ]
                ]

        _ ->
            Element.text "Error"


configureThenAddPlayerRankingBtns : Model -> SR.Types.UserPlayer -> Element Msg
configureThenAddPlayerRankingBtns model uplayer =
    --nb. 'uplayer' is the player that's being mapped cf. appInfo.player which is current user as player (single instance)
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users uplayer.player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users uplayer.player.challengeraddress

                isChallenged =
                    if challengerAsUser.username /= "" then
                        True

                    else
                        False

                printChallengerNameOrAvailable =
                    if isChallenged then
                        challengerAsUser.username

                    else
                        "Available"

                isPlayerCurrentUser =
                    if uplayer.player.address == appInfo.user.ethaddress then
                        True

                    else
                        False

                isCurrentUserInAChallenge =
                    if appInfo.player.player.challengeraddress /= "" then
                        True

                    else
                        False

                -- isCurrentUserOwnerOfSelectedUserRanking =
                --     SR.ListOps.isUserOwnerOfSelectedUserRanking appInfo.selectedRanking allLists.userRankings appInfo.user
                -- _ =
                --     Debug.log "isPlayerSelectedRankingOwner" <| isCurrentUserOwnerOfSelectedUserRanking
            in
            if SR.ListOps.isUserMemberOfSelectedRanking allLists.userPlayers appInfo.user then
                if isPlayerCurrentUser then
                    if isCurrentUserInAChallenge then
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.success) <|
                                { onPress = Just <| ChangedUIStateToEnterResult uplayer
                                , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                                }
                            ]

                    else
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.info) <|
                                { onPress = Nothing
                                , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                                }
                            ]
                    -- else if - this uplayer isn't the current user but the current user is in a challenge so disable any other players

                else if isCurrentUserInAChallenge then
                    Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.disabled) <|
                            { onPress = Nothing
                            , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                            }
                        ]
                    -- else if - this uplayer isn't the current user but is being challenged

                else if isChallenged then
                    Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.disabled) <|
                            { onPress = Nothing
                            , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                            }
                        ]
                    -- else - this uplayer isn't the current user and isn't challenged by anyone

                else
                    Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.light) <|
                            { onPress = Just <| ChallengeOpponentClicked uplayer
                            , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                            }
                        ]
                -- the user isn't a member of this ranking so disable everything

            else
                Element.column Grid.simple <|
                    [ Input.button (Button.fill ++ Color.disabled) <|
                        { onPress = Nothing
                        , label = Element.text <| String.fromInt uplayer.player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                        }
                    ]

        _ ->
            Element.text "Failed"


insertPlayerList : Model -> List (Element Msg)
insertPlayerList model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                mapOutPlayerList =
                    List.map
                        (configureThenAddPlayerRankingBtns model)
                        allLists.userPlayers
            in
            mapOutPlayerList

        _ ->
            [ Element.text "error" ]


globalhomebutton : Element Msg
globalhomebutton =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.info ++ [ Element.htmlAttribute (Html.Attributes.id "createnewladderbtn") ]) <|
                    { onPress = Just <| ChangedUIStateToCreateNewLadder
                    , label = Element.text "Create New"
                    }
                ]
            ]
        ]


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


selecteduserIsNeitherPlayerNorOwnerHomebutton : Element Msg
selecteduserIsNeitherPlayerNorOwnerHomebutton =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.info) <|
                    { onPress = Just ClickedJoinSelected
                    , label = Element.text "Join"
                    }
                ]
            ]
        ]


newrankinhomebutton : SR.Types.User -> SR.Types.RankingInfo -> Element Msg
newrankinhomebutton user rnkInfo =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal
                    , label = Element.text "Cancel"
                    }
                , Input.button (Button.simple ++ Color.info) <|
                    { onPress = Just <| ClickedNewRankingRequested rnkInfo
                    , label = Element.text "Create New"
                    }
                ]
            ]
        , Element.paragraph (Card.fill ++ Color.warning) <|
            [ Element.el [ Font.bold ] <| Element.text "Please note: "
            , Element.paragraph [] <|
                List.singleton <|
                    Element.text "Clicking 'Create New' interacts with your Ethereum wallet"
            ]
        ]


confirmChallengebutton : Model -> Element Msg
confirmChallengebutton model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.challenger.player.address
            in
            Element.column Grid.section <|
                [ Element.el Heading.h6 <| Element.text <| " Your opponent's details: "
                , Element.paragraph (Card.fill ++ Color.info) <|
                    [ Element.el [] <| Element.text <| playerAsUser.username ++ " you are challenging " ++ challengerAsUser.username
                    ]
                , Element.el [] <| Element.text <| "Email: "
                , Element.paragraph (Card.fill ++ Color.info) <|
                    [ Element.el [] <| Element.text <| challengerAsUser.email
                    ]
                , Element.el [] <| Element.text <| "Mobile: "
                , Element.paragraph (Card.fill ++ Color.info) <|
                    [ Element.el [] <| Element.text <| challengerAsUser.mobile
                    ]
                , Element.column (Card.simple ++ Grid.simple) <|
                    [ Element.wrappedRow Grid.simple <|
                        [ Input.button (Button.simple ++ Color.simple) <|
                            { onPress = Just <| ResetToShowSelected
                            , label = Element.text "Cancel"
                            }
                        , Input.button (Button.simple ++ Color.info) <|
                            { onPress = Just <| NewChallengeConfirmClicked
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
        AppOps walletState allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.challenger.player.address
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
                        [ Input.button (Button.simple ++ Color.primary) <|
                            { --onPress = Just <| ProcessResult SR.Types.Won
                              onPress = Just <| SentResultToWallet SR.Types.Won
                            , label = Element.text "Won"
                            }
                        , Input.button (Button.simple ++ Color.primary) <|
                            { onPress = Just <| ProcessResult SR.Types.Lost
                            , label = Element.text "Lost"
                            }
                        , Input.button (Button.simple ++ Color.primary) <|
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


acknoweldgeTxErrorbtn : Model -> Element Msg
acknoweldgeTxErrorbtn model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
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
                            { onPress = Just <| ResetToShowGlobal
                            , label = Element.text "Continue ..."
                            }
                        ]
                    ]
                , SR.Elements.footer
                ]

        _ ->
            Element.text "Fail acknoweldgeTxErrorbtn"


newuserConfirmPanel : SR.Types.User -> Element Msg
newuserConfirmPanel user =
    Element.column Grid.section <|
        [ Element.paragraph (Card.fill ++ Color.warning) <|
            [ Element.el [ Font.bold ] <| Element.text "Please note: "
            , Element.paragraph [] <|
                List.singleton <|
                    Element.text "Clicking 'Register' interacts with your Ethereum wallet"
            ]
        , Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.disabled) <|
                    { onPress = Nothing
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.info) <|
                    { onPress = Just <| CreateNewUserRequested user
                    , label = Element.text "Register"
                    }
                ]
            ]
        ]


inputNewUser : Model -> Element Msg
inputNewUser model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                isValidated =
                    if String.length appInfo.user.username > 5 && String.length appInfo.user.username < 9 then
                        True

                    else
                        False

                nameChgValidationErr =
                    if isValidated then
                        Element.el [ Font.color SR.Types.colors.green, Font.center ] <| Element.text "Username OK!"

                    else
                        Element.el [ Font.color SR.Types.colors.red, Font.alignLeft ] <|
                            Element.text """Username must be unique
and between 6-8 characters"""
            in
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text "New User Details"
                , Element.wrappedRow (Card.fill ++ Grid.simple)
                    [ Element.column Grid.simple
                        [ Input.text
                            Input.simple
                            { onChange = NewUserNameInputChg
                            , text = appInfo.user.username
                            , placeholder = Nothing
                            , label = Input.labelLeft Input.label <| Element.text "Username"
                            }
                        , nameChgValidationErr
                        , Input.multiline Input.simple
                            { onChange = NewUserDescInputChg
                            , text = appInfo.user.description
                            , placeholder = Nothing
                            , label = Input.labelLeft Input.label <| Element.text "Description"
                            , spellcheck = False
                            }
                        , Input.text Input.simple
                            { onChange = NewUserEmailInputChg
                            , text = appInfo.user.email
                            , placeholder = Nothing
                            , label = Input.labelLeft Input.label <| Element.text "Email"
                            }
                        , Input.text Input.simple
                            { onChange = NewUserMobileInputChg
                            , text = appInfo.user.mobile
                            , placeholder = Nothing
                            , label = Input.labelLeft Input.label <| Element.text "Mobile"
                            }
                        ]
                    ]
                , SR.Elements.justParasimpleUserInfoText
                ]

        _ ->
            Element.text "Fail on inputNewUser"



-- inputNewLadder updates view on every text entry


inputNewLadder : SR.Types.RankingInfo -> Element Msg
inputNewLadder newladder =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "New Ladder Details"
        , Element.wrappedRow (Card.fill ++ Grid.simple)
            [ Element.column Grid.simple
                [ Input.text Input.simple
                    { onChange = LadderNameInputChg
                    , text = newladder.rankingname
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Name:"
                    }
                , Input.multiline Input.simple
                    { onChange = LadderDescInputChg
                    , text = newladder.rankingdesc
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Description:"
                    , spellcheck = False
                    }
                ]
            ]
        , SR.Elements.footer
        ]


globalResponsiveview : List SR.Types.UserRanking -> List SR.Types.UserRanking -> List SR.Types.UserRanking -> SR.Types.User -> Html Msg
globalResponsiveview lowneduranking lmemberusranking lotheruranking user =
    Framework.responsiveLayout
        []
    <|
        Element.column
            Framework.container
            [ Element.el (Heading.h5 ++ [ Element.htmlAttribute (Html.Attributes.id "globalHeader") ]) <| Element.text ("SportRank - " ++ user.username)
            , globalhomebutton
            , ownedrankingbuttons lowneduranking
            , memberrankingbuttons lmemberusranking
            , otherrankingbuttons lotheruranking
            ]


selectedUserIsOwnerView : Model -> Html Msg
selectedUserIsOwnerView model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Owner  - " ++ appInfo.user.username
                    , selecteduserIsOwnerhomebutton appInfo.user
                    , playerbuttons model
                    ]

        _ ->
            Html.text "Fail selectedUserIsOwnerView"


selectedUserIsPlayerView : Model -> Html Msg
selectedUserIsPlayerView model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ appInfo.user.username
                    , selecteduserIsPlayerHomebutton appInfo.user
                    , playerbuttons model
                    ]

        _ ->
            Html.text "Error"


selectedUserIsNeitherOwnerNorPlayerView : Model -> Html Msg
selectedUserIsNeitherOwnerNorPlayerView model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| "SportRank - " ++ appInfo.user.username ++ " - Join?"
                    , selecteduserIsNeitherPlayerNorOwnerHomebutton
                    , playerbuttons model
                    ]

        _ ->
            Html.text "Error"


inputNewUserview : Model -> Html Msg
inputNewUserview model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text "Create New User"
                    , inputNewUser model
                    , newuserConfirmPanel appInfo.user
                    ]

        _ ->
            Html.text "Fail inputNewUserview"


inputNewLadderview : Model -> Html Msg
inputNewLadderview model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text "Create New Ladder Ranking"
                    , newrankinhomebutton appInfo.user appInfo.selectedRanking
                    , inputNewLadder appInfo.selectedRanking
                    ]

        _ ->
            Html.text "Fail"


displayChallengeBeforeConfirmView : Model -> Html Msg
displayChallengeBeforeConfirmView model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.player.address
            in
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| playerAsUser.username ++ " - Confirm Challenge"
                    , confirmChallengebutton model
                    ]

        _ ->
            Html.text "Error"


displayResultBeforeConfirmView : Model -> Html Msg
displayResultBeforeConfirmView model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.player.address
            in
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| playerAsUser.username ++ " - Result"
                    , confirmResultbutton model
                    ]

        _ ->
            Html.text "Error"


txErrorView : Model -> Html Msg
txErrorView model =
    case model of
        AppOps walletState allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.player.address
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
        AppOps _ _ _ _ txRec ->
            -- the orig code uses Ports.walletSentry ... same as here:
            Sub.batch
                [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                , Eth.Sentry.Tx.listen txRec.txSentry
                ]

        Failure _ ->
            Sub.none



--Helper functions


isOpponentHigherRank : SR.Types.UserPlayer -> SR.Types.Opponent -> SR.Types.OpponentRelativeRank
isOpponentHigherRank uplayer opponent =
    -- nb. if player rank is 'higher' than opponent his rank integer will actually be 'less than' opponent
    -- we go by the integer ...
    if uplayer.player.rank > opponent.player.rank then
        SR.Types.OpponentRankHigher

    else
        SR.Types.OpponentRankLower



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


createNewPlayerListWithCurrentUser : SR.Types.User -> Cmd Msg
createNewPlayerListWithCurrentUser user =
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
    --RemoteData is used throughout the module, including update
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

        userListWithJsonObjAdded =
            newUser :: originaluserlist
    in
    --SentUserInfoAndDecodedResponseToNewUser is the Msg handled by update whenever a request is made by button click
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- decoder relates to what comes back from server. Nothing to do with above.
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewUsersList userListWithJsonObjAdded
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


addCurrentUserToPlayerList : String -> List SR.Types.UserPlayer -> SR.Types.User -> Cmd Msg
addCurrentUserToPlayerList intrankingId luPlayer userRec =
    let
        newUserPlayer =
            { player =
                { address = userRec.ethaddress
                , rank = List.length luPlayer + 1
                , challengeraddress = ""
                }
            , user = userRec
            }

        selectedRankingListWithNewPlayerJsonObjAdded =
            newUserPlayer :: luPlayer

        sortedSelectedRankingListWithNewPlayerJsonObjAdded =
            SR.ListOps.sortedPlayerListByRank selectedRankingListWithNewPlayerJsonObjAdded
    in
    --AddedNewRankingToGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewSelectedRankingPlayerList sortedSelectedRankingListWithNewPlayerJsonObjAdded
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromPlayerListUpdate) SR.Decode.decodeNewPlayerListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlStubForUpdateExistingBinAndRespond ++ intrankingId
        }


jsonEncodeNewSelectedRankingPlayerList : List SR.Types.UserPlayer -> Json.Encode.Value
jsonEncodeNewSelectedRankingPlayerList luplayers =
    let
        lplayers =
            SR.ListOps.convertUserPlayersToPlayers luplayers

        encodePlayerObj : SR.Types.Player -> Json.Encode.Value
        encodePlayerObj player =
            Json.Encode.object
                [ ( "address", Json.Encode.string (String.toLower player.address) )
                , ( "rank", Json.Encode.int player.rank )
                , ( "challengeraddress", Json.Encode.string player.challengeraddress )
                ]

        encodedList =
            Json.Encode.list encodePlayerObj lplayers
    in
    encodedList


addedNewRankingListEntryInGlobal : RemoteData.WebData SR.Types.RankingId -> List SR.Types.UserRanking -> SR.Types.RankingInfo -> String -> List SR.Types.User -> Cmd Msg
addedNewRankingListEntryInGlobal newrankingid lrankingInfo rnkInfo rankingowneraddress lusers =
    let
        _ =
            Debug.log "rankingowneraddress" rankingowneraddress

        newRankingInfo =
            { id = gotNewRankingIdFromWebData newrankingid
            , active = True
            , rankingname = rnkInfo.rankingname
            , rankingdesc = rnkInfo.rankingdesc
            , rankingowneraddr = rankingowneraddress
            }

        newOtherRankingInfo =
            { rankingInfo = newRankingInfo
            , userInfo = SR.ListOps.gotUserFromUserList lusers rankingowneraddress
            }

        globalListWithJsonObjAdded =
            newOtherRankingInfo :: lrankingInfo
    in
    --AddedNewRankingToGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewGlobalRankingList globalListWithJsonObjAdded
        , expect = Http.expectJson (RemoteData.fromResult >> AddedNewRankingToGlobalList) SR.Decode.decodeNewRankingListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.globalBinName, SR.Defaults.globalContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingUpdateLink
        }


jsonEncodeNewGlobalRankingList : List SR.Types.UserRanking -> Json.Encode.Value
jsonEncodeNewGlobalRankingList lotherrankingInfo =
    let
        newRankingInfoList =
            SR.ListOps.extractRankingList lotherrankingInfo

        encodeAglobalRankingObj : SR.Types.RankingInfo -> Json.Encode.Value
        encodeAglobalRankingObj rankingInfo =
            Json.Encode.object
                [ ( "id", Json.Encode.string rankingInfo.id )
                , ( "active", Json.Encode.bool rankingInfo.active )
                , ( "rankingname", Json.Encode.string rankingInfo.rankingname )
                , ( "rankingdesc", Json.Encode.string rankingInfo.rankingdesc )
                , ( "rankingowneraddr", Json.Encode.string rankingInfo.rankingowneraddr )
                ]

        encodedList =
            Json.Encode.list encodeAglobalRankingObj newRankingInfoList
    in
    encodedList


gotNewRankingIdFromWebData : RemoteData.WebData SR.Types.RankingId -> String
gotNewRankingIdFromWebData rankingIdremdata =
    case rankingIdremdata of
        RemoteData.Success a ->
            case a of
                b ->
                    case b of
                        SR.Types.RankingId c ->
                            c

        RemoteData.NotAsked ->
            "Initialising."

        RemoteData.Loading ->
            "Loading."

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    "Bad Url"

                Http.Timeout ->
                    "Timeout"

                Http.NetworkError ->
                    "Network Err"

                Http.BadStatus statuscode ->
                    String.fromInt <| statuscode

                Http.BadBody s ->
                    "BadBody " ++ s


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


deleteSelectedRankingFromJsonBin : String -> Cmd Msg
deleteSelectedRankingFromJsonBin rankingId =
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


deleteSelectedRankingFromGlobalList : String -> List SR.Types.RankingInfo -> List SR.Types.User -> String -> Cmd Msg
deleteSelectedRankingFromGlobalList rankingId lrankingInfo luser rankingowneraddress =
    let
        globalListWithDeletedRankingInfoRemoved =
            SR.ListOps.filterSelectedRankingOutOfGlobalList rankingId lrankingInfo

        newUserRankingList =
            SR.ListOps.createAllUserAsOwnerGlobalRankingList globalListWithDeletedRankingInfoRemoved luser
    in
    --DeletedRankingFromGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewGlobalRankingList newUserRankingList
        , expect = Http.expectJson (RemoteData.fromResult >> DeletedRankingFromGlobalList) SR.Decode.decodeNewRankingListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.globalBinName, SR.Defaults.globalContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingUpdateLink
        }


updatePlayerList : String -> List SR.Types.UserPlayer -> Cmd Msg
updatePlayerList intrankingId luPlayer =
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewSelectedRankingPlayerList luPlayer
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromPlayerListUpdate) SR.Decode.decodeNewPlayerListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlStubForUpdateExistingBinAndRespond ++ intrankingId
        }


updateUsersJoinRankings : String -> SR.Types.User -> List SR.Types.User -> Cmd Msg
updateUsersJoinRankings rankingId user lUser =
    Http.request
        { body =
            Http.jsonBody <| SR.Encode.encodeUserList <| SR.ListOps.addedNewJoinedRankingIdToUser rankingId user lUser
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromUserListUpdate) SR.Decode.decodeNewUserListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlUpdateUserListAndRespond
        }

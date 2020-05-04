module Main exposing (Model(..), Msg(..), emptyTxRecord, init, main, update, view)

import Browser
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Eth
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
import Framework.Tag as Tag
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Internal.Types
import Json.Encode
import Ports
import Process
import RemoteData
import SR.Constants
import SR.Decode
import SR.Defaults
import SR.Elements
import SR.Encode
import SR.GlobalListOps
import SR.ListOps
import SR.PlayerListOps
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
--nb: each variant added to model has to be handled e.g. do you need 'failure' if it's anyway handled by RemoteData?
--we have to have a separate VARIANT for the user to move on from wallet_status sub - avoiding looping
-- maybe that can be handled by poll block?


type Model
    = WalletOps SR.Types.WalletState SR.Types.AllLists SR.Types.AppInfo SR.Types.UIState TxRecord
    | AppOps SR.Types.AllLists SR.Types.AppInfo SR.Types.UIState TxRecord
    | Failure String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        node =
            --Net.toNetworkId networkId
            --currently hardcode
            Net.toNetworkId 4
                |> Ports.ethNode
    in
    ( WalletOps SR.Types.WalletStateUnknown SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UILoading emptyTxRecord
    , Cmd.batch
        [ Ports.log "Sending out msg from init "
        , Task.attempt PollBlock (Eth.getBlockNumber node.http)
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
    = -- WalletOps
      WalletStatus Eth.Sentry.Wallet.WalletSentry
    | MissingWalletInstructions
    | OpenWalletInstructions
    | Fail String
      -- AppOps
    | GotGlobalRankingsJson (RemoteData.WebData (List SR.Types.RankingInfo))
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | ClickedSelectedRanking Internal.Types.RankingId String String
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | SentUserInfoAndDecodedResponseToNewUser (RemoteData.WebData (List SR.Types.User))
    | ResetToShowGlobal
    | ResetToShowSelected
    | AddedNewRankingToGlobalList (RemoteData.WebData (List SR.Types.RankingInfo))
    | SentResultToWallet SR.Types.ResultOfMatch
    | ProcessResult SR.Types.ResultOfMatch
    | SentResultToJsonbin (Result Http.Error ())
    | DeletedRanking String
    | DeletedRankingFromGlobalList (RemoteData.WebData (List SR.Types.RankingInfo))
    | DeletedSingleRankingFromJsonBin (RemoteData.WebData (List SR.Types.RankingInfo))
    | ChallengeOpponentClicked SR.Types.Player
    | ClickedJoinSelected
    | ReturnFromPlayerListUpdate (RemoteData.WebData (List SR.Types.Player))
    | ReturnFromUserListUpdate (RemoteData.WebData (List SR.Types.User))
    | LadderNameInputChg String
    | LadderDescInputChg String
    | ClickedNewRankingRequested SR.Types.RankingInfo
    | ChangedUIStateToCreateNewLadder
    | NewChallengeConfirmClicked
    | ChangedUIStateToEnterResult SR.Types.Player
      -- User/AppOps
    | UsersReceived (RemoteData.WebData (List SR.Types.User))
    | NewUserNameInputChg String
    | NewUserDescInputChg String
    | NewUserEmailInputChg String
    | NewUserMobileInputChg String
    | CreateNewUserRequested SR.Types.User
    | TimeUpdated Posix
      --Wallet and User/App Ops
    | PollBlock (Result Http.Error Int)
    | WatchTxHash (Result String Eth.Types.TxHash)
    | WatchTx (Result String Eth.Types.Tx)
    | WatchTxReceipt (Result String Eth.Types.TxReceipt)
    | TrackTx Eth.Sentry.Tx.TxTracker
    | TxSentryMsg Eth.Sentry.Tx.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msgOfTransitonThatAlreadyHappened currentmodel =
    case currentmodel of
        WalletOps walletState allLists appInfo uiState txRec ->
            case walletState of
                SR.Types.WalletStateUnknown ->
                    handleWalletStateUnknown msgOfTransitonThatAlreadyHappened currentmodel

                SR.Types.WalletStateLocked ->
                    let
                        _ =
                            Debug.log "WalletStateLocked" msgOfTransitonThatAlreadyHappened
                    in
                    handleWalletStateLocked msgOfTransitonThatAlreadyHappened

                SR.Types.WalletStateAwaitOpening ->
                    let
                        _ =
                            Debug.log "WalletStateAwaitOpening appInfo" appInfo
                    in
                    handleWalletStateAwaitOpening msgOfTransitonThatAlreadyHappened currentmodel

                SR.Types.WalletOpenedAndOperational ->
                    let
                        _ =
                            Debug.log "appInfo" appInfo
                    in
                    handleWalletStateOpenedAndOperational msgOfTransitonThatAlreadyHappened currentmodel

                SR.Types.WalletWaitingForTransactionReceipt ->
                    let
                        _ =
                            Debug.log "WalletWaitingForTransactionReceipt: " "b4 WalletOps"
                    in
                    handleWalletWaitingForTransactionReceipt msgOfTransitonThatAlreadyHappened allLists appInfo txRec

                _ ->
                    ( Failure "WalletState failure", Cmd.none )

        AppOps allLists appInfo uiState txRec ->
            case msgOfTransitonThatAlreadyHappened of
                GotGlobalRankingsJson rmtrnkingdata ->
                    let
                        extractedList =
                            SR.GlobalListOps.ownerValidatedRankingList <| Utils.MyUtils.extractRankingsFromWebData rmtrnkingdata

                        --}
                        -- this was only added to get the new 'imposs' code working:
                        allGlobal =
                            SR.GlobalListOps.createAllUserAsOwnerGlobalRankingList extractedList allLists.users

                        addedRankingListToAllLists =
                            { allLists
                                | userRankings = allGlobal
                            }

                        _ =
                            Debug.log "gotUserOwnedGlobalRankingList" (SR.GlobalListOps.gotUserOwnedGlobalRankingList allGlobal appInfo.user)
                    in
                    ( AppOps addedRankingListToAllLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ClickedSelectedRanking rnkidstr rnkownerstr rnknamestr ->
                    let
                        _ =
                            Debug.log "on click " rnkidstr

                        newSelectedRanking =
                            appInfo.selectedRanking

                        newRnkInfo =
                            { newSelectedRanking | id = Utils.MyUtils.stringFromRankingId rnkidstr, rankingowneraddr = rnkownerstr, rankingname = rnknamestr }

                        newAppInfo =
                            { appInfo | selectedRanking = newRnkInfo }
                    in
                    ( AppOps allLists newAppInfo uiState emptyTxRecord, fetchedSingleRanking rnkidstr )

                -- this is the response from createNewPlayerListWithCurrentUser Cmd
                -- it had the Http.expectStringResponse in it
                -- it's already created the new ranking with current player as the first entry
                -- the result now is the ranking id only at this point which was pulled out by the decoder
                SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder ->
                    ( AppOps allLists appInfo SR.Types.CreateNewLadder emptyTxRecord
                    , addedNewRankingListEntryInGlobal idValueFromDecoder
                        allLists.userRankings
                        appInfo.selectedRanking
                        appInfo.user.ethaddress
                        allLists.users
                    )

                SentUserInfoAndDecodedResponseToNewUser serverResponse ->
                    ( AppOps allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetToShowGlobal ->
                    ( AppOps allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetToShowSelected ->
                    let
                        uiType =
                            ensuredCorrectSelectedUI appInfo allLists
                    in
                    ( AppOps allLists appInfo uiType emptyTxRecord, Cmd.none )

                ChangedUIStateToCreateNewLadder ->
                    let
                        rankingInfoFromModel =
                            appInfo.selectedRanking

                        rankingWithFieldsCleared =
                            { rankingInfoFromModel | rankingname = "", rankingdesc = "" }

                        newAppInfo =
                            { appInfo | selectedRanking = rankingWithFieldsCleared }
                    in
                    ( AppOps allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

                AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList ->
                    let
                        extractedList =
                            SR.GlobalListOps.ownerValidatedRankingList <| Utils.MyUtils.extractRankingsFromWebData updatedListAfterNewEntryAddedToGlobalList

                        allGlobal =
                            SR.GlobalListOps.createAllUserAsOwnerGlobalRankingList extractedList allLists.users

                        addedRankingListToAllLists =
                            { allLists
                                | userRankings = allGlobal
                            }
                    in
                    ( AppOps addedRankingListToAllLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                LadderNameInputChg namefield ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        updatedSelectedRanking =
                            { newSelectedRanking | rankingname = namefield }

                        newAppInfo =
                            { appInfo | selectedRanking = updatedSelectedRanking }
                    in
                    ( AppOps allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

                LadderDescInputChg descfield ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        updatedSelectedRanking =
                            { newSelectedRanking | rankingdesc = descfield }

                        newAppInfo =
                            { appInfo | selectedRanking = updatedSelectedRanking }
                    in
                    ( AppOps allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

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
                    ( AppOps allLists newAppInfo SR.Types.CreateNewLadder { txRec | txSentry = newSentry }, Cmd.batch [ sentryCmd, createNewPlayerListWithCurrentUser newAppInfo.user ] )

                NewChallengeConfirmClicked ->
                    createNewPlayerListWithNewChallengeAndUpdateJsonBin currentmodel

                Fail str ->
                    ( Failure str, Cmd.none )

                PlayersReceived players ->
                    ( updateSelectedRankingOnPlayersReceived currentmodel (extractAndSortPlayerList players), Cmd.none )

                TxSentryMsg subMsg ->
                    let
                        ( subModel, subCmd ) =
                            Eth.Sentry.Tx.update subMsg txRec.txSentry
                    in
                    ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo uiState { txRec | txSentry = subModel }, subCmd )

                ChangedUIStateToEnterResult player ->
                    ( AppOps allLists appInfo SR.Types.UIEnterResult emptyTxRecord, Cmd.none )

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
                    ( WalletOps SR.Types.WalletWaitingForTransactionReceipt allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                      --|> update (ProcessResult SR.Types.Won)
                    , sentryCmd
                    )

                SentResultToJsonbin a ->
                    ( AppOps allLists
                        appInfo
                        uiState
                        txRec
                    , Cmd.none
                    )

                DeletedRanking uaddr ->
                    ( AppOps allLists
                        appInfo
                        uiState
                        txRec
                    , deleteSelectedRankingFromJsonBin appInfo.selectedRanking.id
                    )

                DeletedSingleRankingFromJsonBin result ->
                    ( AppOps allLists
                        appInfo
                        uiState
                        txRec
                    , deleteSelectedRankingFromGlobalList appInfo.selectedRanking.id (SR.GlobalListOps.extractRankingList allLists.userRankings) allLists.users appInfo.user.ethaddress
                    )

                ChallengeOpponentClicked opponentAsPlayer ->
                    ( updatedForChallenge currentmodel allLists.players opponentAsPlayer appInfo.user, Cmd.none )

                DeletedRankingFromGlobalList updatedListAfterRankingDeletedFromGlobalList ->
                    let
                        extractedList =
                            SR.GlobalListOps.ownerValidatedRankingList <| Utils.MyUtils.extractRankingsFromWebData updatedListAfterRankingDeletedFromGlobalList

                        allGlobal =
                            SR.GlobalListOps.createAllUserAsOwnerGlobalRankingList extractedList allLists.users

                        addedRankingListToAllLists =
                            { allLists
                                | userRankings = allGlobal
                            }
                    in
                    ( AppOps addedRankingListToAllLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                -- current
                ClickedJoinSelected ->
                    -- this updates the player and user lists
                    ( currentmodel, Cmd.batch [ addCurrentUserToPlayerList appInfo.selectedRanking.id allLists.players appInfo.user, updateUsersJoinRankings appInfo.selectedRanking.id appInfo.user allLists.users ] )

                ReturnFromPlayerListUpdate response ->
                    ( updateSelectedRankingPlayerList currentmodel (Utils.MyUtils.extractPlayersFromWebData response), Cmd.none )

                ReturnFromUserListUpdate response ->
                    ( updateUserList currentmodel (Utils.MyUtils.extractUsersFromWebData response), Cmd.none )

                PollBlock (Ok blockNumber) ->
                    ( currentmodel
                    , Cmd.none
                    )

                PollBlock (Err error) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo uiState txRec, Cmd.none )

                -- old UserOps
                TimeUpdated posixTime ->
                    let
                        _ =
                            Debug.log "posixtime" posixTime
                    in
                    ( currentmodel, Cmd.none )

                UsersReceived userList ->
                    let
                        userLAddedToAllLists =
                            { allLists | users = SR.ListOps.validatedUserList <| Utils.MyUtils.extractUsersFromWebData userList }
                    in
                    if SR.ListOps.isUserInListStrAddr userLAddedToAllLists.users appInfo.user.ethaddress then
                        ( updateOnUserListReceived currentmodel userLAddedToAllLists.users, gotRankingList )

                    else
                        ( updateOnUserListReceived currentmodel userLAddedToAllLists.users, Cmd.none )

                NewUserNameInputChg namefield ->
                    ( handleNewUserInputs currentmodel (NewUserNameInputChg namefield), Cmd.none )

                NewUserDescInputChg namefield ->
                    ( handleNewUserInputs currentmodel (NewUserDescInputChg namefield), Cmd.none )

                NewUserEmailInputChg namefield ->
                    ( handleNewUserInputs currentmodel (NewUserEmailInputChg namefield), Cmd.none )

                NewUserMobileInputChg namefield ->
                    ( handleNewUserInputs currentmodel (NewUserMobileInputChg namefield), Cmd.none )

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
                    ( AppOps allLists newAppInfo SR.Types.UIRenderAllRankings { txRec | txSentry = newSentry }, Cmd.batch [ sentryCmd, createNewUser allLists.users userWithUpdatedAddr, gotRankingList ] )

                _ ->
                    let
                        _ =
                            Debug.log "AppOps: msgOfTransitonThatAlreadyHappened" msgOfTransitonThatAlreadyHappened
                    in
                    --todo: better logic. This should go to failure model rather than fall thru to UserOps
                    -- but currently logic needs to do this
                    ( AppOps allLists appInfo SR.Types.UICreateNewUser txRec, Cmd.none )

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
                            let
                                _ =
                                    Debug.log "nothing : " "set to locked"
                            in
                            ( WalletOps SR.Types.WalletStateLocked SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord
                            , Cmd.none
                            )

                        Just uaddr ->
                            ( handleGotUser model uaddr, gotUserList )

                _ ->
                    ( Failure "handleWalletStateUnknown"
                    , Cmd.none
                    )

        _ ->
            ( Failure "handleWalletStateUnknown"
            , Cmd.none
            )


handleWalletStateLocked : Msg -> ( Model, Cmd Msg )
handleWalletStateLocked msg =
    case msg of
        WalletStatus walletSentry_ ->
            ( WalletOps SR.Types.WalletStateLocked SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord, Cmd.none )

        PollBlock (Ok blockNumber) ->
            ( AppOps SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord, Cmd.none )

        PollBlock (Err error) ->
            ( AppOps SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord, Cmd.none )

        _ ->
            ( Failure "handleWalletStateLocked"
            , Cmd.none
            )


handleWalletStateAwaitOpening : Msg -> Model -> ( Model, Cmd Msg )
handleWalletStateAwaitOpening msg model =
    case model of
        WalletOps walletState allLists appInfo uiState txRec ->
            case msg of
                WalletStatus walletSentry_ ->
                    case walletSentry_.networkId of
                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    let
                                        _ =
                                            Debug.log "nothing : " "set to locked"
                                    in
                                    ( WalletOps SR.Types.WalletStateLocked SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions emptyTxRecord
                                    , Cmd.none
                                    )

                                Just uaddr ->
                                    let
                                        _ =
                                            Debug.log "handleWalletStateAwaitOpening : " uaddr
                                    in
                                    handleWalletStateOpenedAndOperational msg (handleGotUser model uaddr)

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


handleWalletWaitingForTransactionReceipt : Msg -> SR.Types.AllLists -> SR.Types.AppInfo -> TxRecord -> ( Model, Cmd Msg )
handleWalletWaitingForTransactionReceipt msg allLists appInfo txRec =
    let
        _ =
            Debug.log "Msg" msg
    in
    case msg of
        WalletStatus walletSentry_ ->
            ( WalletOps SR.Types.WalletWaitingForTransactionReceipt allLists appInfo SR.Types.UIWaitingForTxReceipt txRec
              --|> update (ProcessResult SR.Types.Won)
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
                ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }, subCmd )

            else
                ( AppOps SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )

        WatchTxHash (Ok txHash) ->
            --( { txRec | txHash = Just txHash }, Cmd.none )
            ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txHash = Just txHash }, Cmd.none )

        WatchTxHash (Err err) ->
            --( { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )
            ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTx (Ok tx) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForTransactionReceipt" "tx ok"
            in
            -- WalletOps SR.Types.WalletOpenedAndOperational { txRec | tx = Just tx }
            --     |> update (ProcessResult SR.Types.Won)
            --( UserOps SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions txRec, Cmd.none )
            AppOps allLists appInfo SR.Types.UIRenderAllRankings { txRec | tx = Just tx } |> update (ProcessResult SR.Types.Won)

        WatchTx (Err err) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForTransactionReceipt tx err" err
            in
            ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

        WatchTxReceipt (Ok txReceipt) ->
            let
                _ =
                    Debug.log "handleWalletWaitingForTransactionReceipt tx ok" txReceipt
            in
            AppOps allLists appInfo SR.Types.UIRenderAllRankings { txRec | txReceipt = Just txReceipt } |> update (ProcessResult SR.Types.Won)

        WatchTxReceipt (Err err) ->
            let
                _ =
                    Debug.log "tx err" err
            in
            ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

        _ ->
            ( Failure "handleWalletStateUnknown"
            , Cmd.none
            )


handleWalletStateOpenedAndOperational : Msg -> Model -> ( Model, Cmd Msg )
handleWalletStateOpenedAndOperational msg model =
    let
        _ =
            Debug.log "in opend and op" "with model"
    in
    case model of
        WalletOps walletState allLists appInfo uiState txRec ->
            case msg of
                WalletStatus walletSentry_ ->
                    if appInfo.user.ethaddress == "" then
                        ( AppOps allLists appInfo SR.Types.UICreateNewUser emptyTxRecord, Cmd.none )

                    else
                        ( AppOps allLists appInfo SR.Types.UILoading emptyTxRecord, gotUserList )

                PollBlock (Ok blockNumber) ->
                    ( AppOps allLists appInfo SR.Types.UIRenderAllRankings txRec, Cmd.none )

                PollBlock (Err error) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt txRec, Cmd.none )

                OpenWalletInstructions ->
                    ( WalletOps SR.Types.WalletStateLocked allLists appInfo SR.Types.UIWaitingForTxReceipt txRec, Cmd.none )

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
                        ( AppOps allLists appInfo SR.Types.UIRenderAllRankings txRec, Cmd.none )

                    else
                        ( AppOps allLists appInfo SR.Types.UIEnterResultTxProblem txRec, Cmd.none )

                WatchTxHash (Ok txHash) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txHash = Just txHash }, Cmd.none )

                WatchTxHash (Err err) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

                WatchTx (Ok tx) ->
                    let
                        _ =
                            Debug.log "handleWalletStateOpenedAndOperational" "tx Ok"
                    in
                    AppOps allLists appInfo SR.Types.UIRenderAllRankings { txRec | tx = Just tx } |> update (ProcessResult SR.Types.Won)

                WatchTx (Err err) ->
                    let
                        _ =
                            Debug.log "handleWalletStateOpenedAndOperational tx err" err
                    in
                    ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

                WatchTxReceipt (Ok txReceipt) ->
                    let
                        _ =
                            Debug.log "handleWalletStateOpenedAndOperational Receipt" txReceipt
                    in
                    AppOps allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord
                        |> update (ProcessResult SR.Types.Won)

                WatchTxReceipt (Err err) ->
                    let
                        _ =
                            Debug.log "tx err" err
                    in
                    ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

                TrackTx blockDepth ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | blockDepth = Just blockDepth }, Cmd.none )

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
                                AppOps allTheLists theAppInfo theUIState thetxRec ->
                                    ( newModel, updatePlayerList theAppInfo.selectedRanking.id allTheLists.players )

                                _ ->
                                    ( Failure "result won", Cmd.none )

                        SR.Types.Lost ->
                            let
                                newModel =
                                    handleLost model
                            in
                            case newModel of
                                AppOps allTheLists theAppInfo theUIState thetxRec ->
                                    ( newModel, updatePlayerList theAppInfo.selectedRanking.id allTheLists.players )

                                _ ->
                                    ( Failure "result lost", Cmd.none )

                        SR.Types.Undecided ->
                            let
                                newModel =
                                    handleUndecided model
                            in
                            case newModel of
                                AppOps allTheLists theAppInfo theUIState thetxRec ->
                                    ( newModel, updatePlayerList theAppInfo.selectedRanking.id allTheLists.players )

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
                    ( WalletOps SR.Types.WalletWaitingForTransactionReceipt allLists appInfo SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                    , sentryCmd
                    )

                SentResultToJsonbin a ->
                    ( AppOps allLists
                        appInfo
                        uiState
                        txRec
                    , Cmd.none
                    )

                Fail str ->
                    let
                        _ =
                            Debug.log "Fail: handleWalletStateOpenedAndOperational" msg
                    in
                    ( Failure <| "WalletOps 1" ++ str, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "Fail: msg " msg
                    in
                    ( Failure "handleWalletStateOpenedAndOperational"
                    , Cmd.none
                    )

        _ ->
            let
                _ =
                    Debug.log "Fail: msg " msg
            in
            ( Failure "handleWalletStateOpenedAndOperational"
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


handleGotUser : Model -> Eth.Types.Address -> Model
handleGotUser model uaddr =
    case model of
        WalletOps walletState allLists appInfo uiState txRec ->
            let
                newAppInfoUser =
                    appInfo.user

                newUserWithAddr =
                    { newAppInfoUser | ethaddress = Eth.Utils.addressToString uaddr }

                newAppInfo =
                    { appInfo | user = newUserWithAddr }

                _ =
                    Debug.log "newUserWithAddr" newUserWithAddr.ethaddress
            in
            if newUserWithAddr.ethaddress == "" then
                AppOps SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UICreateNewUser emptyTxRecord

            else
                AppOps SR.Defaults.emptyAllLists newAppInfo SR.Types.UILoading emptyTxRecord

        _ ->
            Failure "handleGotUser"


handleWon : Model -> Model
handleWon model =
    case model of
        AppOps allLists appInfo uiState txRec ->
            let
                whoHigher =
                    isOpponentHigherRank appInfo.player appInfo.challenger
            in
            case whoHigher of
                SR.Types.OpponentRankHigher ->
                    let
                        -- update the player list for both players challenger to emptyPlayer and change rankings
                        updatedPlayerListForPlayer =
                            SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player appInfo.challenger.rank

                        updatedPlayerListForPlayerAndChallenger =
                            SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger (appInfo.challenger.rank + 1)

                        --update current player now
                        newPlayer =
                            appInfo.player

                        newPlayerUpdated =
                            { newPlayer | address = "" }

                        newAppInfo =
                            { appInfo | player = newPlayerUpdated, challenger = SR.Defaults.emptyPlayer }

                        newAllLists =
                            { allLists | players = updatedPlayerListForPlayerAndChallenger }
                    in
                    --nb. higher rank is a lower number and vice versa!
                    AppOps newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

                SR.Types.OpponentRankLower ->
                    let
                        --no ranking change - just update the player list for both players challenger to emptyPlayer, no rank change
                        --update the player list
                        updatedPlayerListForPlayer =
                            SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player appInfo.player.rank

                        updatedPlayerListForPlayerAndChallenger =
                            SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.rank

                        --update current player now
                        newPlayer =
                            appInfo.player

                        newPlayerUpdated =
                            { newPlayer | address = "" }

                        newAppInfo =
                            { appInfo | player = newPlayerUpdated, challenger = SR.Defaults.emptyPlayer }

                        newAllLists =
                            { allLists | players = updatedPlayerListForPlayerAndChallenger }
                    in
                    --nb. higher rank is a lower number and vice versa!
                    AppOps newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

        _ ->
            Failure "HandleWonFail"


handleLost : Model -> Model
handleLost model =
    case model of
        AppOps allLists appInfo uiState txRec ->
            let
                whoHigher =
                    isOpponentHigherRank appInfo.player appInfo.challenger
            in
            case whoHigher of
                SR.Types.OpponentRankHigher ->
                    let
                        updatedPlayerListForPlayer =
                            SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player appInfo.player.rank

                        updatedPlayerListForPlayerAndChallenger =
                            SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.rank

                        --update current player now
                        newPlayer =
                            appInfo.player

                        newPlayerUpdated =
                            { newPlayer | address = "" }

                        newAppInfo =
                            { appInfo | player = newPlayerUpdated, challenger = SR.Defaults.emptyPlayer }

                        newAllLists =
                            { allLists | players = updatedPlayerListForPlayerAndChallenger }
                    in
                    --nb. higher rank is a lower number and vice versa!
                    AppOps newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

                SR.Types.OpponentRankLower ->
                    --nb. higher rank is a lower number and vice versa!
                    let
                        updatedPlayerListForPlayer =
                            SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player (appInfo.player.rank + 1)

                        updatedPlayerListForPlayerAndChallenger =
                            SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.player.rank

                        --update current player now
                        newPlayer =
                            appInfo.player

                        newPlayerUpdated =
                            { newPlayer | address = "" }

                        newAppInfo =
                            { appInfo | player = newPlayerUpdated, challenger = SR.Defaults.emptyPlayer }

                        newAllLists =
                            { allLists | players = updatedPlayerListForPlayerAndChallenger }
                    in
                    AppOps newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

        _ ->
            Failure "Fail handleLost"


handleUndecided : Model -> Model
handleUndecided model =
    case model of
        AppOps allLists appInfo uiState txRec ->
            let
                updatedPlayerListForPlayer =
                    SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player appInfo.player.rank

                updatedPlayerListForPlayerAndChallenger =
                    SR.PlayerListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.rank

                --update current player now
                newPlayer =
                    appInfo.player

                newPlayerUpdated =
                    { newPlayer | address = "" }

                newAppInfo =
                    { appInfo | player = newPlayerUpdated, challenger = SR.Defaults.emptyPlayer }

                newAllLists =
                    { allLists | players = updatedPlayerListForPlayerAndChallenger }
            in
            AppOps newAllLists
                newAppInfo
                SR.Types.UISelectedRankingUserIsPlayer
                txRec

        _ ->
            Failure "Fail in handleUndecided"


ensuredCorrectSelectedUI : SR.Types.AppInfo -> SR.Types.AllLists -> SR.Types.UIState
ensuredCorrectSelectedUI appInfo allLists =
    if SR.ListOps.isUserSelectedOwnerOfRanking appInfo.selectedRanking (SR.GlobalListOps.extractRankingList allLists.userRankings) appInfo.user then
        SR.Types.UISelectedRankingUserIsOwner

    else if SR.ListOps.isUserMemberOfSelectedRanking allLists.players appInfo.user then
        SR.Types.UISelectedRankingUserIsPlayer

    else
        SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer


createNewPlayerListWithNewResultAndUpdateJsonBin : Model -> ( Model, Cmd Msg )
createNewPlayerListWithNewResultAndUpdateJsonBin model =
    case model of
        AppOps allLists appInfo uiState txRec ->
            let
                -- add respective challenger addresses to player and challenger (who is also a player type)
                newplayerListWithPlayerUpdated =
                    SR.PlayerListOps.updatePlayerRankWithWonResult allLists.players appInfo.player

                challengerAsPlayer =
                    SR.PlayerListOps.gotPlayerFromPlayerListStrAddress allLists.players appInfo.challenger.address

                newplayerListWithPlayerAndChallengerUpdated =
                    SR.PlayerListOps.setPlayerInPlayerListWithNewChallengerAddr newplayerListWithPlayerUpdated challengerAsPlayer appInfo.player.address

                sortedByRankingnewplayerListWithPlayerAndChallengerUpdated =
                    SR.PlayerListOps.sortedPlayerListByRank newplayerListWithPlayerAndChallengerUpdated

                newAllLists =
                    { allLists | players = sortedByRankingnewplayerListWithPlayerAndChallengerUpdated }
            in
            ( AppOps newAllLists appInfo uiState txRec, updatePlayerList appInfo.selectedRanking.id newAllLists.players )

        _ ->
            ( Failure "createNewPlayerListWithNewResultAndUpdateJsonBin", Cmd.none )


createNewPlayerListWithNewChallengeAndUpdateJsonBin : Model -> ( Model, Cmd Msg )
createNewPlayerListWithNewChallengeAndUpdateJsonBin model =
    case model of
        AppOps allLists appInfo uiState txRec ->
            let
                -- add respective challenger addresses to player and challenger (who is also a player type)
                newplayerListWithPlayerUpdated =
                    SR.PlayerListOps.setPlayerInPlayerListWithNewChallengerAddr allLists.players appInfo.player appInfo.challenger.address

                challengerAsPlayer =
                    SR.PlayerListOps.gotPlayerFromPlayerListStrAddress allLists.players appInfo.challenger.address

                newplayerListWithPlayerAndChallengerUpdated =
                    SR.PlayerListOps.setPlayerInPlayerListWithNewChallengerAddr newplayerListWithPlayerUpdated challengerAsPlayer appInfo.player.address

                sortedByRankingnewplayerListWithPlayerAndChallengerUpdated =
                    SR.PlayerListOps.sortedPlayerListByRank newplayerListWithPlayerAndChallengerUpdated

                newAllLists =
                    { allLists | players = sortedByRankingnewplayerListWithPlayerAndChallengerUpdated }
            in
            ( AppOps newAllLists appInfo uiState txRec, updatePlayerList appInfo.selectedRanking.id newAllLists.players )

        _ ->
            ( Failure "createNewPlayerListWithNewChallengeAndUpdateJsonBin", Cmd.none )


handleNewUserInputs : Model -> Msg -> Model
handleNewUserInputs currentmodel msg =
    case currentmodel of
        AppOps allLists appInfo uiState txRec ->
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
                    AppOps allLists newAppInfo SR.Types.UICreateNewUser txRec

                NewUserDescInputChg descfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | description = descfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps allLists newAppInfo SR.Types.UICreateNewUser txRec

                NewUserEmailInputChg emailfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | email = emailfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps allLists newAppInfo SR.Types.UICreateNewUser txRec

                NewUserMobileInputChg mobilefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | mobile = mobilefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    AppOps allLists newAppInfo SR.Types.UICreateNewUser txRec

                _ ->
                    Failure "NewUserNameInputChg"

        _ ->
            Failure "NewUserNameInputChg"


extractAndSortPlayerList : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.Player
extractAndSortPlayerList rdlPlayer =
    SR.PlayerListOps.sortedPlayerListByRank <| Utils.MyUtils.extractPlayersFromWebData rdlPlayer


updatedForChallenge : Model -> List SR.Types.Player -> SR.Types.Player -> SR.Types.User -> Model
updatedForChallenge currentmodel lplayer opponentAsPlayer user =
    case currentmodel of
        AppOps allLists appInfo _ txRec ->
            let
                newAppInfoWithPlayer =
                    { appInfo | player = SR.PlayerListOps.gotCurrentUserAsPlayerFromPlayerList lplayer user }

                newAppInfoWithChallengerAndPlayer =
                    { newAppInfoWithPlayer | challenger = opponentAsPlayer }

                newAllListsWithSelectedRankingUpdate =
                    updateSelectedRankingOnChallenge allLists newAppInfoWithChallengerAndPlayer
            in
            AppOps newAllListsWithSelectedRankingUpdate newAppInfoWithChallengerAndPlayer SR.Types.UIChallenge txRec

        _ ->
            Failure <| "updatedForChallenge : "


updateSelectedRankingOnChallenge : SR.Types.AllLists -> SR.Types.AppInfo -> SR.Types.AllLists
updateSelectedRankingOnChallenge allLists appInfo =
    allLists


updateOnUserListReceived : Model -> List SR.Types.User -> Model
updateOnUserListReceived model userList =
    case model of
        AppOps allLists appInfo uiState txRec ->
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
                AppOps newAllLists userUpdatedInAppInfo SR.Types.UIRenderAllRankings emptyTxRecord

            else
                let
                    _ =
                        Debug.log "no user" appInfo.user.ethaddress
                in
                AppOps newAllLists userUpdatedInAppInfo SR.Types.UICreateNewUser txRec

        _ ->
            Failure "should be in AppOps"


updateSelectedRankingPlayerList : Model -> List SR.Types.Player -> Model
updateSelectedRankingPlayerList currentmodel lplayers =
    case currentmodel of
        AppOps allLists appInfo _ txRec ->
            let
                resetSelectedRankingPlayerList =
                    { allLists | players = lplayers }

                uiState =
                    ensuredCorrectSelectedUI appInfo allLists
            in
            AppOps resetSelectedRankingPlayerList appInfo uiState txRec

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


updateUserList : Model -> List SR.Types.User -> Model
updateUserList currentmodel lusers =
    case currentmodel of
        AppOps allLists appInfo _ txRec ->
            let
                resetUserList =
                    { allLists | users = lusers }

                uiState =
                    ensuredCorrectSelectedUI appInfo allLists
            in
            AppOps resetUserList appInfo uiState txRec

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


updateSelectedRankingOnPlayersReceived : Model -> List SR.Types.Player -> Model
updateSelectedRankingOnPlayersReceived currentmodel lplayers =
    case currentmodel of
        AppOps allLists appInfo uiState txRec ->
            let
                newAppPlayer =
                    { appInfo | player = SR.PlayerListOps.gotPlayerFromPlayerListStrAddress lplayers appInfo.user.ethaddress }

                newAppChallengerAndPlayer =
                    { newAppPlayer | challenger = SR.PlayerListOps.gotPlayerFromPlayerListStrAddress lplayers newAppPlayer.player.challengeraddress }

                allListsPlayersAdded =
                    { allLists | players = lplayers }

                uistate =
                    ensuredCorrectSelectedUI appInfo allLists
            in
            AppOps allListsPlayersAdded newAppChallengerAndPlayer uistate emptyTxRecord

        _ ->
            Failure <| "updateSelectedRankingOnPlayersReceived : "



-- view


view : Model -> Html Msg
view model =
    case model of
        AppOps allLists appInfo uiState txRec ->
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
                    globalResponsiveview (SR.GlobalListOps.extractRankingList allLists.userRankings) appInfo.user

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

        WalletOps walletState allLists appInfo uiState txRec ->
            case walletState of
                SR.Types.WalletStateUnknown ->
                    greetingView <| "Wallet State unknown"

                SR.Types.Missing ->
                    greetingView "MissingWalletInstructions"

                SR.Types.WalletStateLocked ->
                    case uiState of
                        SR.Types.UIDisplayWalletLockedInstructions ->
                            let
                                _ =
                                    Debug.log "ui wallet loack " "here"
                            in
                            greetingView "OpenWalletInstructions"

                        _ ->
                            greetingView "OpenWalletInstructions"

                SR.Types.WalletStateAwaitOpening ->
                    greetingView "OpenWalletInstructions"

                SR.Types.WalletOpenedWithoutUserCheck uaddr ->
                    greetingView "User unchecked "

                SR.Types.WalletOpenedAndOperational ->
                    greetingView "WalletOpenedAndOperational"

                SR.Types.WalletWaitingForTransactionReceipt ->
                    greetingView "Please wait while the transaction is mined"

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


rankingbuttons : List SR.Types.RankingInfo -> Element Msg
rankingbuttons rankingList =
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Global Rankings"
        , Element.column (Card.simple ++ Grid.simple) <|
            insertRankingList rankingList
        , Element.paragraph (Card.fill ++ Color.warning) <|
            [ Element.el [ Font.bold ] <| Element.text "Please note: "
            , Element.paragraph [] <|
                List.singleton <|
                    Element.text "Clicking 'Create New' interacts with your Ethereum wallet"
            ]
        ]


addRankingInfoToAnyElText : SR.Types.RankingInfo -> Element Msg
addRankingInfoToAnyElText rankingobj =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.primary) <|
            { onPress = Just (ClickedSelectedRanking (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr rankingobj.rankingname)
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


playerbuttons : Model -> Element Msg
playerbuttons model =
    case model of
        AppOps allLists appInfo uiState txRec ->
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


addPlayerInfoToAnyElText : Model -> SR.Types.Player -> Element Msg
addPlayerInfoToAnyElText model player =
    --nb. 'player' is the player that's being mapped cf. appInfo.player which is current user as player (single instance)
    case model of
        AppOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users player.challengeraddress

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
                    if player.address == appInfo.user.ethaddress then
                        True

                    else
                        False

                isCurrentUserInAChallenge =
                    if appInfo.player.challengeraddress /= "" then
                        True

                    else
                        False
            in
            if SR.ListOps.isUserMemberOfSelectedRanking allLists.players appInfo.user then
                -- let
                --     _ =
                --         Debug.log "isCurrentUserInAChallenge" isCurrentUserInAChallenge
                -- in
                if isPlayerCurrentUser then
                    if isCurrentUserInAChallenge then
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.success) <|
                                { onPress = Just <| ChangedUIStateToEnterResult player
                                , label = Element.text <| String.fromInt player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                                }
                            ]

                    else
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.info) <|
                                { onPress = Nothing
                                , label = Element.text <| String.fromInt player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                                }
                            ]
                    -- else if - this player isn't the current user but the current user is in a challenge so disable any other players

                else if isCurrentUserInAChallenge then
                    Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.disabled) <|
                            { onPress = Nothing
                            , label = Element.text <| String.fromInt player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                            }
                        ]
                    -- else if - this player isn't the current user but is being challenged

                else if isChallenged then
                    Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.disabled) <|
                            { onPress = Nothing
                            , label = Element.text <| String.fromInt player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                            }
                        ]
                    -- else - this player isn't the current user and isn't challenged by anyone

                else
                    Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.primary) <|
                            { onPress = Just <| ChallengeOpponentClicked player
                            , label = Element.text <| String.fromInt player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                            }
                        ]
                -- the user isn't a member of this ranking so disable everything

            else
                Element.column Grid.simple <|
                    [ Input.button (Button.fill ++ Color.disabled) <|
                        { onPress = Nothing
                        , label = Element.text <| String.fromInt player.rank ++ ". " ++ playerAsUser.username ++ " vs " ++ printChallengerNameOrAvailable
                        }
                    ]

        _ ->
            Element.text "Failed"


insertPlayerList : Model -> List (Element Msg)
insertPlayerList model =
    case model of
        AppOps allLists appInfo uiState txRec ->
            let
                mapOutPlayerList =
                    List.map
                        (addPlayerInfoToAnyElText model)
                        allLists.players
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


selectedhomebuttons : Element Msg
selectedhomebuttons =
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
        AppOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.challenger.address
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
        AppOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.challenger.address
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
        AppOps allLists appInfo uiState txRec ->
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
        AppOps allLists appInfo uiState txRec ->
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


globalResponsiveview : List SR.Types.RankingInfo -> SR.Types.User -> Html Msg
globalResponsiveview rankingList user =
    Framework.responsiveLayout
        []
    <|
        Element.column
            Framework.container
            [ Element.el (Heading.h5 ++ [ Element.htmlAttribute (Html.Attributes.id "globalHeader") ]) <| Element.text ("SportRank - " ++ user.username)
            , globalhomebutton
            , rankingbuttons rankingList
            ]


selectedUserIsOwnerView : Model -> Html Msg
selectedUserIsOwnerView model =
    case model of
        AppOps allLists appInfo uiState txRec ->
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
        AppOps allLists appInfo uiState txRec ->
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
        AppOps allLists appInfo uiState txRec ->
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
        AppOps allLists appInfo uiState txRec ->
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
        AppOps allLists appInfo uiState txRec ->
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
        AppOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.address
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
        AppOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.address
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
        AppOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserList allLists.users appInfo.player.address
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
        WalletOps _ _ _ _ txRec ->
            Sub.batch
                [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                , Eth.Sentry.Tx.listen txRec.txSentry
                ]

        AppOps _ _ _ _ ->
            Sub.none

        Failure _ ->
            Sub.none



--Helper functions


isOpponentHigherRank : SR.Types.Player -> SR.Types.Opponent -> SR.Types.OpponentRelativeRank
isOpponentHigherRank player opponent =
    -- nb. if player rank is 'higher' than opponent his rank integer will actually be 'less than' opponent
    -- we go by the integer ...
    if player.rank > opponent.rank then
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

        --, url = "https://api.jsonbin.io/b/" ++ rankingId ++ "/latest"
        , url = SR.Constants.jsonbinUrlForCreateNewBinAndRespond ++ rankingId ++ "/latest"
        }


gotRankingList : Cmd Msg
gotRankingList =
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> GotGlobalRankingsJson) SR.Decode.rankingsDecoder
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


addCurrentUserToPlayerList : String -> List SR.Types.Player -> SR.Types.User -> Cmd Msg
addCurrentUserToPlayerList intrankingId lPlayer userRec =
    let
        newPlayer =
            { address = userRec.ethaddress
            , rank = List.length lPlayer + 1
            , challengeraddress = ""
            }

        selectedRankingListWithNewPlayerJsonObjAdded =
            newPlayer :: lPlayer

        sortedSelectedRankingListWithNewPlayerJsonObjAdded =
            SR.PlayerListOps.sortedPlayerListByRank selectedRankingListWithNewPlayerJsonObjAdded
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


jsonEncodeNewSelectedRankingPlayerList : List SR.Types.Player -> Json.Encode.Value
jsonEncodeNewSelectedRankingPlayerList lplayers =
    let
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
            SR.GlobalListOps.extractRankingList lotherrankingInfo

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
            SR.GlobalListOps.createAllUserAsOwnerGlobalRankingList globalListWithDeletedRankingInfoRemoved luser
    in
    --DeletedRankingFromGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            --Http.jsonBody <| jsonEncodeNewGlobalRankingList globalListWithDeletedRankingInfoRemoved
            Http.jsonBody <| jsonEncodeNewGlobalRankingList newUserRankingList
        , expect = Http.expectJson (RemoteData.fromResult >> DeletedRankingFromGlobalList) SR.Decode.decodeNewRankingListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.globalBinName, SR.Defaults.globalContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingUpdateLink
        }


updatePlayerList : String -> List SR.Types.Player -> Cmd Msg
updatePlayerList intrankingId lPlayer =
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewSelectedRankingPlayerList lPlayer
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromPlayerListUpdate) SR.Decode.decodeNewPlayerListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.selectedBinName, SR.Defaults.selectedContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlStubForUpdateExistingBinAndRespond ++ intrankingId
        }


updateUsersJoinRankings : String -> SR.Types.User -> List SR.Types.User -> Cmd Msg
updateUsersJoinRankings rankingId user lUser =
    let
        -- update the user list with the new selected ranking id they want to join
        newUserList =
            lUser
    in
    Http.request
        { body =
            Http.jsonBody <| SR.Encode.encodeUserList newUserList
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromUserListUpdate) SR.Decode.decodeNewUserListServerResponse
        , headers = [ SR.Defaults.secretKey, SR.Defaults.userBinName, SR.Defaults.userContainerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlUpdateUserListAndRespond
        }

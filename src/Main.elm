module Main exposing (main)

--import Html exposing (Html)

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
import Form exposing (Form)
import Form.Input
import Form.Validate as Validate exposing (..)
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
import Process
import RemoteData
import SR.Constants
import SR.Decode
import SR.Defaults
import SR.Elements
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
--{ form : Form () SR.Types.UserForm }


type Model
    = WalletOps SR.Types.WalletState TxRecord
    | UserOps SR.Types.AllLists Eth.Types.Address SR.Types.AppInfo SR.Types.UIState (Form () SR.Types.UserForm) TxRecord
    | RankingOps SR.Types.AllLists SR.Types.AppInfo SR.Types.UIState TxRecord
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
    ( WalletOps SR.Types.Missing emptyTxRecord
    , Cmd.batch
        [ Ports.log "Sending out msg from init "
        , Task.attempt PollBlock (Eth.getBlockNumber node.http)
        ]
    )


validate : Validation () SR.Types.UserForm
validate =
    succeed SR.Types.UserForm
        |> andMap (field "username" string)
        |> andMap (field "description" string)
        |> andMap (field "email" email)
        |> andMap (field "mobile" string)


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
      -- RankingOps
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
    | LadderNameInputChg String
    | LadderDescInputChg String
    | ClickedNewRankingRequested SR.Types.RankingInfo
    | ChangedUIStateToCreateNew
    | NewChallengeConfirmClicked
    | ChangedUIStateToEnterResult SR.Types.Player
      -- UserOps
    | UsersReceived (RemoteData.WebData (List SR.Types.User))
    | NewUserNameInputChg String
    | NewUserDescInputChg String
    | NewUserEmailInputChg String
    | NewUserMobileInputChg String
    | NewUserRequested SR.Types.User
    | TimeUpdated Posix
    | FormMsg Form.Msg
      -- Multiple
    | PollBlock (Result Http.Error Int)
    | WatchTxHash (Result String Eth.Types.TxHash)
    | WatchTx (Result String Eth.Types.Tx)
    | WatchTxReceipt (Result String Eth.Types.TxReceipt)
    | TrackTx Eth.Sentry.Tx.TxTracker
    | TxSentryMsg Eth.Sentry.Tx.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msgOfTransitonThatAlreadyHappened currentmodel =
    case currentmodel of
        WalletOps walletState txRec ->
            case msgOfTransitonThatAlreadyHappened of
                WalletStatus walletSentry_ ->
                    case walletSentry_.networkId of
                        Mainnet ->
                            case walletSentry_.account of
                                Nothing ->
                                    ( UserOps SR.Defaults.emptyAllLists (Internal.Types.Address "") SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions (Form.initial [] validate) txRec, Cmd.none )

                                Just uaddr ->
                                    ( UserOps SR.Defaults.emptyAllLists uaddr SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletInfoToUser (Form.initial [] validate) txRec, gotUserList )

                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    ( UserOps SR.Defaults.emptyAllLists (Internal.Types.Address "") SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions (Form.initial [] validate) txRec, Cmd.none )

                                Just uaddr ->
                                    case walletState of
                                        SR.Types.Missing ->
                                            let
                                                _ =
                                                    Debug.log "In : " "Wallet Missing"
                                            in
                                            ( UserOps SR.Defaults.emptyAllLists uaddr SR.Defaults.emptyAppInfo SR.Types.UIWalletMissingInstructions (Form.initial [] validate) txRec, gotUserList )

                                        SR.Types.Locked ->
                                            let
                                                _ =
                                                    Debug.log "In : " "Wallet Locked"
                                            in
                                            ( UserOps SR.Defaults.emptyAllLists uaddr SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletInfoToUser (Form.initial [] validate) txRec, gotUserList )

                                        SR.Types.WalletOpenedWithoutUserCheck useraddr ->
                                            let
                                                _ =
                                                    Debug.log "In : WalletOpenedWithoutUserCheck" useraddr
                                            in
                                            ( UserOps SR.Defaults.emptyAllLists useraddr SR.Defaults.emptyAppInfo SR.Types.UICreateNewUser (Form.initial [] validate) txRec, gotUserList )

                                        SR.Types.WalletWaitingForTransactionReceipt ->
                                            let
                                                _ =
                                                    Debug.log "WalletWaitingForTransactionReceipt: " "b4 WalletOps"
                                            in
                                            ( WalletOps SR.Types.WalletWaitingForTransactionReceipt txRec
                                              --|> update (ProcessResult SR.Types.Won)
                                            , Cmd.none
                                            )

                                        SR.Types.WalletOpenedAndOperational ->
                                            let
                                                _ =
                                                    Debug.log "In : " "WalletOpenedAndOperational"
                                            in
                                            ( UserOps SR.Defaults.emptyAllLists uaddr SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletInfoToUser (Form.initial [] validate) txRec, gotUserList )

                        _ ->
                            let
                                _ =
                                    Debug.log "Gave MissingWalletInstructions: " "but actually a networkId fall thru"
                            in
                            ( UserOps SR.Defaults.emptyAllLists (Internal.Types.Address "") SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions (Form.initial [] validate) txRec, Cmd.none )

                OpenWalletInstructions ->
                    ( WalletOps SR.Types.Locked emptyTxRecord, Cmd.none )

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
                        ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | txSentry = subModel }, subCmd )

                    else
                        ( RankingOps SR.Defaults.emptyAllLists SR.Defaults.emptyAppInfo SR.Types.UIEnterResultTxProblem emptyTxRecord, Cmd.none )

                PollBlock (Ok blockNumber) ->
                    -- ( { txRec | blockNumber = Just blockNumber }
                    -- , Task.attempt PollBlock <|
                    --     Task.andThen (\_ -> Eth.getBlockNumber txRec.node.http) (Process.sleep 1000)
                    -- )
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | blockNumber = Just blockNumber }
                    , Task.attempt PollBlock <|
                        Task.andThen (\_ -> Eth.getBlockNumber txRec.node.http) (Process.sleep 1000)
                    )

                PollBlock (Err error) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational txRec, Cmd.none )

                WatchTxHash (Ok txHash) ->
                    --( { txRec | txHash = Just txHash }, Cmd.none )
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | txHash = Just txHash }, Cmd.none )

                WatchTxHash (Err err) ->
                    --( { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )

                WatchTx (Ok tx) ->
                    let
                        _ =
                            Debug.log "tx ok" "tx was Ok"
                    in
                    -- WalletOps SR.Types.WalletOpenedAndOperational { txRec | tx = Just tx }
                    --     |> update (ProcessResult SR.Types.Won)
                    ( UserOps SR.Defaults.emptyAllLists (Internal.Types.Address "") SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletLockedInstructions (Form.initial [] validate) txRec, Cmd.none )

                WatchTx (Err err) ->
                    let
                        _ =
                            Debug.log "tx ok" err
                    in
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

                --( { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )
                WatchTxReceipt (Ok txReceipt) ->
                    let
                        _ =
                            Debug.log "tx ok" txReceipt
                    in
                    WalletOps SR.Types.WalletOpenedAndOperational { txRec | txReceipt = Just txReceipt }
                        |> update (ProcessResult SR.Types.Won)

                WatchTxReceipt (Err err) ->
                    let
                        _ =
                            Debug.log "tx err" err
                    in
                    -- ( { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

                -- TrackTx blockDepth ->
                --     ( { txRec | blockDepth = Just blockDepth }, Cmd.none )
                Fail str ->
                    let
                        _ =
                            Debug.log "msgOfTransitonThatAlreadyHappened" msgOfTransitonThatAlreadyHappened
                    in
                    ( Failure <| "WalletOps 1" ++ str, Cmd.none )

                _ ->
                    ( Failure "WalletOps 2", Cmd.none )

        --         FormMsg formMsg ->
        -- { model | form = Form.update validate formMsg form }
        UserOps allLists uaddr appInfo uiState uForm txRec ->
            case msgOfTransitonThatAlreadyHappened of
                FormMsg formMsg ->
                    let
                        newUForm =
                            Form.update validate formMsg uForm
                    in
                    ( UserOps
                        allLists
                        uaddr
                        appInfo
                        SR.Types.UIDisplayWalletLockedInstructions
                        newUForm
                        txRec
                    , Cmd.none
                    )

                PollBlock (Ok blockNumber) ->
                    let
                        _ =
                            Debug.log "userops poll block" "yes"
                    in
                    ( UserOps
                        allLists
                        uaddr
                        appInfo
                        SR.Types.UICreateNewUser
                        (Form.initial [] validate)
                        txRec
                    , Cmd.none
                    )

                PollBlock (Err error) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational txRec, Cmd.none )

                TimeUpdated posixTime ->
                    let
                        _ =
                            Debug.log "posixtime" posixTime
                    in
                    ( currentmodel, Cmd.none )

                UsersReceived userList ->
                    let
                        userLAddedToAllLists =
                            { allLists | users = Utils.MyUtils.extractUsersFromWebData userList }
                    in
                    if SR.ListOps.isUserInList userLAddedToAllLists.users uaddr then
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

                NewUserRequested userInfo ->
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
                    ( RankingOps allLists newAppInfo SR.Types.UIRenderAllRankings { txRec | txSentry = newSentry }, Cmd.batch [ sentryCmd, createNewUser allLists.users userWithUpdatedAddr, gotRankingList ] )

                _ ->
                    let
                        _ =
                            Debug.log "msgOfTransitonThatAlreadyHappened" msgOfTransitonThatAlreadyHappened

                        -- newUForm =
                        --     { form }
                        -- Form.update
                        --     validate
                        --     formMsg
                    in
                    --todo: better logic. This should go to failure model rather than fall thru to UserOps
                    -- but currently logic needs to do this
                    ( UserOps allLists uaddr appInfo SR.Types.UICreateNewUser uForm txRec, Cmd.none )

        --( Failure "in UserOps", Cmd.none )
        RankingOps allLists appInfo uiState txRec ->
            case msgOfTransitonThatAlreadyHappened of
                GotGlobalRankingsJson rmtrnkingdata ->
                    let
                        addedRankingListToAllLists =
                            { allLists | globalRankings = SR.GlobalListOps.ownerValidatedRankingList <| Utils.MyUtils.extractRankingsFromWebData rmtrnkingdata }
                    in
                    ( RankingOps addedRankingListToAllLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

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
                    ( RankingOps allLists newAppInfo uiState emptyTxRecord, fetchedSingleRanking rnkidstr )

                -- this is the response from createNewPlayerListWithCurrentUser Cmd
                -- it had the Http.expectStringResponse in it
                -- it's already created the new ranking with current player as the first entry
                -- the result now is the ranking id only at this point which was pulled out by the decoder
                SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder ->
                    ( RankingOps allLists appInfo SR.Types.CreateNewLadder emptyTxRecord
                    , addedNewRankingListEntryInGlobal idValueFromDecoder allLists.globalRankings appInfo.selectedRanking appInfo.user.ethaddress
                    )

                SentUserInfoAndDecodedResponseToNewUser serverResponse ->
                    ( RankingOps allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetToShowGlobal ->
                    ( RankingOps allLists appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ResetToShowSelected ->
                    let
                        uiType =
                            ensuredCorrectSelectedUI appInfo allLists
                    in
                    ( RankingOps allLists appInfo uiType emptyTxRecord, Cmd.none )

                ChangedUIStateToCreateNew ->
                    let
                        rankingInfoFromModel =
                            appInfo.selectedRanking

                        rankingWithFieldsCleared =
                            { rankingInfoFromModel | rankingname = "", rankingdesc = "" }

                        newAppInfo =
                            { appInfo | selectedRanking = rankingWithFieldsCleared }
                    in
                    ( RankingOps allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

                AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList ->
                    let
                        resetGlobalRankingListForAddedNewEntry =
                            { allLists | globalRankings = Utils.MyUtils.extractRankingsFromWebData <| updatedListAfterNewEntryAddedToGlobalList }
                    in
                    ( RankingOps resetGlobalRankingListForAddedNewEntry appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                LadderNameInputChg namefield ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        updatedSelectedRanking =
                            { newSelectedRanking | rankingname = namefield }

                        newAppInfo =
                            { appInfo | selectedRanking = updatedSelectedRanking }
                    in
                    ( RankingOps allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

                LadderDescInputChg descfield ->
                    let
                        newSelectedRanking =
                            appInfo.selectedRanking

                        updatedSelectedRanking =
                            { newSelectedRanking | rankingdesc = descfield }

                        newAppInfo =
                            { appInfo | selectedRanking = updatedSelectedRanking }
                    in
                    ( RankingOps allLists newAppInfo SR.Types.CreateNewLadder emptyTxRecord, Cmd.none )

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
                    ( RankingOps allLists newAppInfo SR.Types.CreateNewLadder { txRec | txSentry = newSentry }, Cmd.batch [ sentryCmd, createNewPlayerListWithCurrentUser newAppInfo.user ] )

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
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | txSentry = subModel }, subCmd )

                ChangedUIStateToEnterResult player ->
                    ( RankingOps allLists appInfo SR.Types.UIEnterResult emptyTxRecord, Cmd.none )

                ProcessResult result ->
                    case result of
                        SR.Types.Won ->
                            let
                                -- ensure that updatePlayerList gets the updated lists
                                newModel =
                                    handleWon currentmodel
                            in
                            case newModel of
                                RankingOps allTheLists theAppInfo theUIState thetxRec ->
                                    ( newModel, updatePlayerList theAppInfo.selectedRanking.id allTheLists.players )

                                _ ->
                                    ( Failure "result won", Cmd.none )

                        SR.Types.Lost ->
                            let
                                newModel =
                                    handleLost currentmodel
                            in
                            case newModel of
                                RankingOps allTheLists theAppInfo theUIState thetxRec ->
                                    ( newModel, updatePlayerList theAppInfo.selectedRanking.id allTheLists.players )

                                _ ->
                                    ( Failure "result lost", Cmd.none )

                        SR.Types.Undecided ->
                            let
                                newModel =
                                    handleUndecided currentmodel
                            in
                            case newModel of
                                RankingOps allTheLists theAppInfo theUIState thetxRec ->
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
                    ( WalletOps SR.Types.WalletWaitingForTransactionReceipt { txRec | txSentry = newSentry }
                      --|> update (ProcessResult SR.Types.Won)
                    , sentryCmd
                    )

                SentResultToJsonbin a ->
                    ( RankingOps allLists
                        appInfo
                        uiState
                        txRec
                    , Cmd.none
                    )

                DeletedRanking uaddr ->
                    ( RankingOps allLists
                        appInfo
                        uiState
                        txRec
                    , deleteSelectedRankingFromJsonBin appInfo.selectedRanking.id
                    )

                DeletedSingleRankingFromJsonBin result ->
                    ( RankingOps allLists
                        appInfo
                        uiState
                        txRec
                    , deleteSelectedRankingFromGlobalList appInfo.selectedRanking.id allLists.globalRankings appInfo.user.ethaddress
                    )

                ChallengeOpponentClicked opponentAsPlayer ->
                    ( updatedForChallenge currentmodel allLists.players opponentAsPlayer appInfo.user, Cmd.none )

                DeletedRankingFromGlobalList updatedListAfterRankingDeletedFromGlobalList ->
                    let
                        resetGlobalRankingList =
                            { allLists | globalRankings = Utils.MyUtils.extractRankingsFromWebData <| updatedListAfterRankingDeletedFromGlobalList }
                    in
                    ( RankingOps resetGlobalRankingList appInfo SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )

                ClickedJoinSelected ->
                    ( currentmodel, addCurrentUserToPlayerList appInfo.selectedRanking.id allLists.players appInfo.user )

                ReturnFromPlayerListUpdate response ->
                    ( updateSelectedRankingPlayerList currentmodel (Utils.MyUtils.extractPlayersFromWebData response), Cmd.none )

                PollBlock (Ok blockNumber) ->
                    ( currentmodel
                    , Cmd.none
                    )

                PollBlock (Err error) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational txRec, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "msgOfTransitonThatAlreadyHappened" msgOfTransitonThatAlreadyHappened
                    in
                    ( Failure <| "Fall thru in RankingOps: ", Cmd.none )

        Failure str ->
            ( Failure <| "Model failure in RankingOps: " ++ str, Cmd.none )


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


handleWon : Model -> Model
handleWon model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
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
                    RankingOps newAllLists
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
                    RankingOps newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

        _ ->
            Failure "HandleWonFail"


handleLost : Model -> Model
handleLost model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
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
                    RankingOps newAllLists
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
                    RankingOps newAllLists
                        newAppInfo
                        SR.Types.UISelectedRankingUserIsPlayer
                        txRec

        _ ->
            Failure "Fail handleLost"


handleUndecided : Model -> Model
handleUndecided model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
            -- let
            --     whoHigher =
            --         isOpponentHigherRank appInfo.player appInfo.challenger
            -- in
            -- case whoHigher of
            --SR.Types.OpponentRankHigher ->
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
            RankingOps newAllLists
                newAppInfo
                SR.Types.UISelectedRankingUserIsPlayer
                txRec

        -- SR.Types.OpponentRankLower ->
        --     --nb. higher rank is a lower number and vice versa!
        --     let
        --         updatedPlayerListForPlayer =
        --             SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player (appInfo.player.rank + 1)
        --         updatedPlayerListForPlayerAndChallenger =
        --             SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.player.rank
        --         --update current player now
        --         newPlayer =
        --             appInfo.player
        --         newPlayerUpdated =
        --             { newPlayer | address = "" }
        --         newAppInfo =
        --             { appInfo | player = newPlayerUpdated, challenger = SR.Defaults.emptyPlayer }
        --         newAllLists =
        --             { allLists | players = updatedPlayerListForPlayerAndChallenger }
        --     in
        --     RankingOps newAllLists
        --         newAppInfo
        --         SR.Types.UISelectedRankingUserIsPlayer
        --         txRec
        _ ->
            Failure "Fail in handleUndecided"


ensuredCorrectSelectedUI : SR.Types.AppInfo -> SR.Types.AllLists -> SR.Types.UIState
ensuredCorrectSelectedUI appInfo allLists =
    if SR.ListOps.isUserSelectedOwnerOfRanking appInfo.selectedRanking allLists.globalRankings appInfo.user then
        SR.Types.UISelectedRankingUserIsOwner

    else if SR.ListOps.isUserMemberOfSelectedRanking allLists.players appInfo.user then
        SR.Types.UISelectedRankingUserIsPlayer

    else
        SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer


createNewPlayerListWithNewResultAndUpdateJsonBin : Model -> ( Model, Cmd Msg )
createNewPlayerListWithNewResultAndUpdateJsonBin model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
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
            ( RankingOps newAllLists appInfo uiState txRec, updatePlayerList appInfo.selectedRanking.id newAllLists.players )

        _ ->
            ( Failure "createNewPlayerListWithNewResultAndUpdateJsonBin", Cmd.none )


createNewPlayerListWithNewChallengeAndUpdateJsonBin : Model -> ( Model, Cmd Msg )
createNewPlayerListWithNewChallengeAndUpdateJsonBin model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
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
            ( RankingOps newAllLists appInfo uiState txRec, updatePlayerList appInfo.selectedRanking.id newAllLists.players )

        _ ->
            ( Failure "createNewPlayerListWithNewChallengeAndUpdateJsonBin", Cmd.none )


handleNewUserInputs : Model -> Msg -> Model
handleNewUserInputs currentmodel msg =
    case currentmodel of
        UserOps allLists uaddr appInfo uiState uForm txRec ->
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
                    UserOps allLists uaddr newAppInfo SR.Types.UICreateNewUser uForm txRec

                NewUserDescInputChg descfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | description = descfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    UserOps allLists uaddr newAppInfo SR.Types.UICreateNewUser uForm txRec

                NewUserEmailInputChg emailfield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | email = emailfield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    UserOps allLists uaddr newAppInfo SR.Types.UICreateNewUser uForm txRec

                NewUserMobileInputChg mobilefield ->
                    let
                        newUser =
                            appInfo.user

                        updatedNewUser =
                            { newUser | mobile = mobilefield }

                        newAppInfo =
                            { appInfo | user = updatedNewUser }
                    in
                    UserOps allLists uaddr newAppInfo SR.Types.UICreateNewUser uForm txRec

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
        RankingOps allLists appInfo _ txRec ->
            let
                newAppInfoWithPlayer =
                    { appInfo | player = SR.PlayerListOps.gotCurrentUserAsPlayerFromPlayerList lplayer user }

                newAppInfoWithChallengerAndPlayer =
                    { newAppInfoWithPlayer | challenger = opponentAsPlayer }

                newAllListsWithSelectedRankingUpdate =
                    updateSelectedRankingOnChallenge allLists newAppInfoWithChallengerAndPlayer
            in
            RankingOps newAllListsWithSelectedRankingUpdate newAppInfoWithChallengerAndPlayer SR.Types.UIChallenge txRec

        _ ->
            Failure <| "updatedForChallenge : "


updateSelectedRankingOnChallenge : SR.Types.AllLists -> SR.Types.AppInfo -> SR.Types.AllLists
updateSelectedRankingOnChallenge allLists appInfo =
    allLists


updateOnUserListReceived : Model -> List SR.Types.User -> Model
updateOnUserListReceived model userList =
    case model of
        UserOps allLists uaddr appInfo uiState uForm txRec ->
            let
                gotUserToUpdateAddr =
                    SR.ListOps.singleUserInList userList uaddr

                userWithUpdatedAddr =
                    { gotUserToUpdateAddr | ethaddress = Eth.Utils.addressToString uaddr }

                userUpdatedInAppInfo =
                    { appInfo | user = userWithUpdatedAddr }

                newAllLists =
                    { allLists | users = userList }
            in
            if SR.ListOps.isUserInList userList uaddr then
                RankingOps newAllLists userUpdatedInAppInfo SR.Types.UIRenderAllRankings emptyTxRecord

            else
                let
                    _ =
                        Debug.log "no user" uaddr
                in
                UserOps newAllLists uaddr userUpdatedInAppInfo SR.Types.UICreateNewUser uForm txRec

        _ ->
            Failure "should be in UserOps"


updateSelectedRankingPlayerList : Model -> List SR.Types.Player -> Model
updateSelectedRankingPlayerList currentmodel lplayers =
    case currentmodel of
        RankingOps allLists appInfo _ txRec ->
            let
                resetSelectedRankingPlayerList =
                    { allLists | players = lplayers }

                uiState =
                    ensuredCorrectSelectedUI appInfo allLists
            in
            RankingOps resetSelectedRankingPlayerList appInfo uiState txRec

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


updateSelectedRankingOnPlayersReceived : Model -> List SR.Types.Player -> Model
updateSelectedRankingOnPlayersReceived currentmodel lplayers =
    case currentmodel of
        RankingOps allLists appInfo uiState txRec ->
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
            RankingOps allListsPlayersAdded newAppChallengerAndPlayer uistate emptyTxRecord

        _ ->
            Failure <| "updateSelectedRankingOnPlayersReceived : "



-- view


view : Model -> Html Msg
view model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
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
                    globalResponsiveview allLists.globalRankings appInfo.user

                SR.Types.UIEnterResult ->
                    displayResultBeforeConfirmView model

                SR.Types.UIEnterResultTxProblem ->
                    txErrorView model

                SR.Types.UIChallenge ->
                    displayChallengeBeforeConfirmView model

                _ ->
                    greetingView <| "Wrong variant"

        WalletOps walletState txRec ->
            case walletState of
                SR.Types.Locked ->
                    greetingView "OpenWalletInstructions"

                SR.Types.Missing ->
                    greetingView "MissingWalletInstructions"

                SR.Types.WalletOpenedWithoutUserCheck uaddr ->
                    greetingView "User unchecked "

                SR.Types.WalletOpenedAndOperational ->
                    greetingView "WalletOpenedAndOperational"

                SR.Types.WalletWaitingForTransactionReceipt ->
                    greetingView "Please wait while the transaction is mined"

        UserOps allLists uaddr appInfo uiState uForm txRec ->
            case uiState of
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

                --Html.map FormMsg (formView uForm)
                _ ->
                    greetingView <| "Loading ... "

        Failure str ->
            greetingView <| "Model failure in view: " ++ str


formView : Form () SR.Types.UserForm -> Html Form.Msg
formView form =
    let
        -- error presenter
        errorFor field =
            case field.liveError of
                Just error ->
                    -- replace toString with your own translations
                    div [ class "error" ] [ text (Debug.toString error) ]

                Nothing ->
                    text ""

        -- fields states
        bar =
            Form.getFieldAsString "bar" form

        baz =
            Form.getFieldAsBool "baz" form
    in
    div []
        [ label [] [ text "Bar" ]
        , Form.Input.textInput bar []

        --, Form.Input.textInput
        , errorFor bar
        , label []
            [ Form.Input.checkboxInput baz []
            , text "Baz"
            ]
        , errorFor baz
        , button
            [ onClick Form.Submit ]
            [ text "Submit" ]
        ]


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
        RankingOps allLists appInfo uiState txRec ->
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
        RankingOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users player.challengeraddress

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
        RankingOps allLists appInfo uiState txRec ->
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
                , Input.button (Button.simple ++ Color.info) <|
                    { onPress = Just <| ChangedUIStateToCreateNew
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


selecteduserIsOwnerhomebutton : List SR.Types.RankingInfo -> SR.Types.User -> Element Msg
selecteduserIsOwnerhomebutton rankingList user =
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


selecteduserIsPlayerHomebutton : List SR.Types.RankingInfo -> SR.Types.User -> Element Msg
selecteduserIsPlayerHomebutton rankingList user =
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


newrankinhomebutton : List SR.Types.RankingInfo -> SR.Types.User -> SR.Types.RankingInfo -> Element Msg
newrankinhomebutton rankingList user rnkInfo =
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
        RankingOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users appInfo.player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users appInfo.challenger.address
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
        RankingOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users appInfo.player.address

                challengerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users appInfo.challenger.address
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
        RankingOps allLists appInfo uiState txRec ->
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
                    { onPress = Just <| NewUserRequested user
                    , label = Element.text "Register"
                    }
                ]
            ]
        ]


inputNewUser : SR.Types.User -> Element Msg
inputNewUser user =
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "New User Details"
        , Element.wrappedRow (Card.fill ++ Grid.simple)
            [ Element.column Grid.simple
                [ Input.text
                    Input.simple
                    { onChange = NewUserNameInputChg
                    , text = user.username
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Username"
                    }
                , Input.multiline Input.simple
                    { onChange = NewUserDescInputChg
                    , text = user.description
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Description"
                    , spellcheck = False
                    }
                , Input.text Input.simple
                    { onChange = NewUserEmailInputChg
                    , text = user.email
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Email"
                    }
                , Input.text Input.simple
                    { onChange = NewUserMobileInputChg
                    , text = user.mobile
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Mobile"
                    }
                ]
            ]
        , SR.Elements.justParasimpleUserInfoText
        ]



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
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h5 <| Element.text ("SportRank - " ++ user.username)
            , globalhomebutton
            , rankingbuttons rankingList
            ]


selectedUserIsOwnerView : Model -> Html Msg
selectedUserIsOwnerView model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Owner  - " ++ appInfo.user.username
                    , selecteduserIsOwnerhomebutton allLists.globalRankings appInfo.user
                    , playerbuttons model
                    ]

        _ ->
            Html.text "Fail selectedUserIsOwnerView"


selectedUserIsPlayerView : Model -> Html Msg
selectedUserIsPlayerView model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ appInfo.user.username
                    , selecteduserIsPlayerHomebutton allLists.globalRankings appInfo.user
                    , playerbuttons model
                    ]

        _ ->
            Html.text "Error"


selectedUserIsNeitherOwnerNorPlayerView : Model -> Html Msg
selectedUserIsNeitherOwnerNorPlayerView model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
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
        UserOps allLists uaddr appInfo uiState uForm txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text "Create New User"
                    , inputNewUser appInfo.user
                    , newuserConfirmPanel appInfo.user
                    ]

        _ ->
            Html.text "Fail inputNewUserview"


inputNewLadderview : Model -> Html Msg
inputNewLadderview model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
            Framework.responsiveLayout [] <|
                Element.column
                    Framework.container
                    [ Element.el Heading.h4 <| Element.text "Create New Ladder Ranking"
                    , newrankinhomebutton allLists.globalRankings appInfo.user appInfo.selectedRanking
                    , inputNewLadder appInfo.selectedRanking
                    ]

        _ ->
            Html.text "Fail"


displayChallengeBeforeConfirmView : Model -> Html Msg
displayChallengeBeforeConfirmView model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users appInfo.player.address
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
        RankingOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users appInfo.player.address
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
        RankingOps allLists appInfo uiState txRec ->
            let
                playerAsUser =
                    SR.ListOps.gotUserFromUserListStrAddress allLists.users appInfo.player.address
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
        WalletOps _ txRec ->
            Sub.batch
                [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                , Eth.Sentry.Tx.listen txRec.txSentry
                ]

        UserOps _ _ _ _ _ _ ->
            Sub.none

        RankingOps _ _ _ _ ->
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
        , url = SR.Constants.jsonbinUrlUpdateWithNewUserAndRespond
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
                ]

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


addedNewRankingListEntryInGlobal : RemoteData.WebData SR.Types.RankingId -> List SR.Types.RankingInfo -> SR.Types.RankingInfo -> String -> Cmd Msg
addedNewRankingListEntryInGlobal newrankingid lrankingInfo rnkInfo rankingowneraddress =
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

        globalListWithJsonObjAdded =
            newRankingInfo :: lrankingInfo
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


jsonEncodeNewGlobalRankingList : List SR.Types.RankingInfo -> Json.Encode.Value
jsonEncodeNewGlobalRankingList lrankingInfo =
    let
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
            Json.Encode.list encodeAglobalRankingObj lrankingInfo
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


deleteSelectedRankingFromGlobalList : String -> List SR.Types.RankingInfo -> String -> Cmd Msg
deleteSelectedRankingFromGlobalList rankingId lrankingInfo rankingowneraddress =
    let
        globalListWithDeletedRankingInfoRemoved =
            SR.ListOps.filterSelectedRankingOutOfGlobalList rankingId lrankingInfo
    in
    --DeletedRankingFromGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewGlobalRankingList globalListWithDeletedRankingInfoRemoved
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

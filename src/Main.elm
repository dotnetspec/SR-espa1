module Main exposing (main)

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
import Html exposing (Html)
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
import SR.ListOps
import SR.Types
import Task
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


type Model
    = WalletOps SR.Types.WalletState TxRecord
    | UserOps SR.Types.UserState SR.Types.AllLists Eth.Types.Address SR.Types.AppInfo SR.Types.UIState TxRecord
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



-- Msg is a description of the transition that already happened
-- Messages that delivered the response (orign doc says 'will deliver')
-- The messages use RemoteData. The model does not (strip out)


type Msg
    = WalletStatus Eth.Sentry.Wallet.WalletSentry
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | SentUserInfoAndDecodedResponseToNewUser (RemoteData.WebData (List SR.Types.User))
    | ChangedUIStateToCreateNew
    | ClickedNewRankingRequested SR.Types.RankingInfo
    | NewChallengeConfirmClicked
    | PollBlock (Result Http.Error Int)
    | WatchTxHash (Result String Eth.Types.TxHash)
    | WatchTx (Result String Eth.Types.Tx)
    | WatchTxReceipt (Result String Eth.Types.TxReceipt)
    | TrackTx Eth.Sentry.Tx.TxTracker
    | TxSentryMsg Eth.Sentry.Tx.Msg
    | AddedNewRankingToGlobalList (RemoteData.WebData (List SR.Types.RankingInfo))
    | DeletedRanking String
    | DeletedRankingFromGlobalList (RemoteData.WebData (List SR.Types.RankingInfo))
    | DeletedSingleRankingFromJsonBin (RemoteData.WebData (List SR.Types.RankingInfo))
    | GotGlobalRankingsJson (RemoteData.WebData (List SR.Types.RankingInfo))
    | ClickedSelectedRanking Internal.Types.RankingId String String
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | UsersReceived (RemoteData.WebData (List SR.Types.User))
    | MissingWalletInstructions
    | OpenWalletInstructions
    | NewUser
    | ResetToShowGlobal
    | LadderNameInputChg String
    | LadderDescInputChg String
    | ProcessResult SR.Types.ResultOfMatch
    | SentResultToJsonbin (Result Http.Error ())
    | NewUserNameInputChg String
    | NewUserDescInputChg String
    | NewUserEmailInputChg String
    | NewUserMobileInputChg String
    | NewUserRequested SR.Types.User
    | ClickedJoinSelected
    | ReturnFromPlayerListUpdate (RemoteData.WebData (List SR.Types.Player))
    | ChallengeOpponentClicked SR.Types.Player
    | NewUserInputs Msg
    | Fail String


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
                                    ( WalletOps SR.Types.Locked txRec, Cmd.none )

                                Just uaddr ->
                                    ( UserOps (SR.Types.NewUser <| SR.Defaults.emptyUser) SR.Defaults.emptyAllLists uaddr SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletInfoToUser txRec, gotUserList )

                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    ( WalletOps SR.Types.Locked txRec, Cmd.none )

                                Just uaddr ->
                                    ( UserOps (SR.Types.NewUser <| SR.Defaults.emptyUser) SR.Defaults.emptyAllLists uaddr SR.Defaults.emptyAppInfo SR.Types.UIDisplayWalletInfoToUser txRec, gotUserList )

                        _ ->
                            let
                                _ =
                                    Debug.log "MissingWalletInstructions " "str"
                            in
                            ( WalletOps SR.Types.Missing emptyTxRecord, Cmd.none )

                OpenWalletInstructions ->
                    ( WalletOps SR.Types.Locked emptyTxRecord, Cmd.none )

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
                    WalletOps SR.Types.WalletOpenedAndOperational { txRec | tx = Just tx }
                        |> update (ProcessResult SR.Types.Won)

                WatchTx (Err err) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )

                --( { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )
                WatchTxReceipt (Ok txReceipt) ->
                    WalletOps SR.Types.WalletOpenedAndOperational { txRec | txReceipt = Just txReceipt }
                        |> update (ProcessResult SR.Types.Won)

                WatchTxReceipt (Err err) ->
                    -- ( { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )

                -- TrackTx blockDepth ->
                --     ( { txRec | blockDepth = Just blockDepth }, Cmd.none )
                Fail str ->
                    ( Failure <| "WalletOps 1" ++ str, Cmd.none )

                _ ->
                    ( Failure "WalletOps 2", Cmd.none )

        UserOps userState allLists uaddr appInfo uiState txRec ->
            case msgOfTransitonThatAlreadyHappened of
                PollBlock (Ok blockNumber) ->
                    ( UserOps
                        (SR.Types.NewUser appInfo.user)
                        allLists
                        uaddr
                        appInfo
                        SR.Types.CreateNewUser
                        txRec
                    , Cmd.none
                    )

                PollBlock (Err error) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational txRec, Cmd.none )

                UsersReceived userList ->
                    let
                        userLAddedToAllLists =
                            { allLists | users = Utils.MyUtils.extractUsersFromWebData userList }
                    in
                    if SR.ListOps.isUserInList userLAddedToAllLists.users uaddr then
                        ( updateOnUserListReceived currentmodel userLAddedToAllLists.users, gotRankingList )

                    else
                        ( updateOnUserListReceived currentmodel userLAddedToAllLists.users, Cmd.none )

                NewUserNameInputChg newUserInputsMsg ->
                    ( handleNewUserInputs currentmodel (NewUserNameInputChg newUserInputsMsg), Cmd.none )

                NewUserDescInputChg newUserInputsMsg ->
                    ( handleNewUserInputs currentmodel (NewUserDescInputChg newUserInputsMsg), Cmd.none )

                NewUserEmailInputChg newUserInputsMsg ->
                    ( handleNewUserInputs currentmodel (NewUserEmailInputChg newUserInputsMsg), Cmd.none )

                NewUserMobileInputChg newUserInputsMsg ->
                    ( handleNewUserInputs currentmodel (NewUserMobileInputChg newUserInputsMsg), Cmd.none )

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
                    --todo: better logic. This should go to failure model rather than fall thru to UserOps
                    -- but currently logic needs to do this
                    ( UserOps (SR.Types.NewUser SR.Defaults.emptyUser) allLists uaddr appInfo SR.Types.CreateNewUser txRec, Cmd.none )

        RankingOps allLists appInfo uiState txRec ->
            case msgOfTransitonThatAlreadyHappened of
                GotGlobalRankingsJson rmtrnkingdata ->
                    let
                        addedRankingListToAllLists =
                            { allLists | globalRankings = Utils.MyUtils.extractRankingsFromWebData rmtrnkingdata }
                    in
                    ( RankingOps addedRankingListToAllLists appInfo uiState emptyTxRecord, Cmd.none )

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
                    in
                    let
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

                ProcessResult result ->
                    let
                        whoHigher =
                            isOpponentHigherRank appInfo.player appInfo.challenger
                    in
                    case result of
                        SR.Types.Won ->
                            case whoHigher of
                                SR.Types.OpponentRankHigher ->
                                    let
                                        -- update the player list for both players challenger to emptyPlayer and change rankings
                                        updatedPlayerListForPlayer =
                                            SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player (appInfo.challenger.rank + 1)

                                        updatedPlayerListForPlayerAndChallenger =
                                            SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger (appInfo.challenger.rank - 1)

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
                                    ( RankingOps newAllLists
                                        newAppInfo
                                        uiState
                                        txRec
                                    , Cmd.none
                                    )

                                SR.Types.OpponentRankLower ->
                                    let
                                        --no ranking change - just update the player list for both players challenger to emptyPlayer, no rank change
                                        --update the player list
                                        updatedPlayerListForPlayer =
                                            SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player appInfo.player.rank

                                        updatedPlayerListForPlayerAndChallenger =
                                            SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.rank

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
                                    ( RankingOps newAllLists
                                        newAppInfo
                                        uiState
                                        txRec
                                    , Cmd.none
                                    )

                        SR.Types.Lost ->
                            case whoHigher of
                                SR.Types.OpponentRankHigher ->
                                    let
                                        updatedPlayerListForPlayer =
                                            SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player appInfo.player.rank

                                        updatedPlayerListForPlayerAndChallenger =
                                            SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.rank

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
                                    ( RankingOps newAllLists
                                        newAppInfo
                                        uiState
                                        txRec
                                    , Cmd.none
                                    )

                                SR.Types.OpponentRankLower ->
                                    --nb. higher rank is a lower number and vice versa!
                                    let
                                        updatedPlayerListForPlayer =
                                            SR.ListOps.setPlayerInPlayerListWithChallengeResult allLists.players appInfo.player appInfo.player.rank

                                        updatedPlayerListForPlayerAndChallenger =
                                            SR.ListOps.setPlayerInPlayerListWithChallengeResult updatedPlayerListForPlayer appInfo.challenger appInfo.challenger.rank

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
                                    ( RankingOps newAllLists
                                        newAppInfo
                                        uiState
                                        txRec
                                    , Cmd.none
                                    )

                        SR.Types.Undecided ->
                            ( RankingOps allLists
                                appInfo
                                uiState
                                txRec
                            , Cmd.none
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

                _ ->
                    ( Failure <| "Fall thru failure : ", Cmd.none )

        Failure str ->
            ( Failure <| "Model failure in selected ranking: " ++ str, Cmd.none )


createNewPlayerListWithNewChallengeAndUpdateJsonBin : Model -> ( Model, Cmd Msg )
createNewPlayerListWithNewChallengeAndUpdateJsonBin model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
            let
                -- add respective challenger addresses to player and challenger (who is also a player type)
                newplayerListWithPlayerUpdated =
                    SR.ListOps.setPlayerInPlayerListWithNewChallengerAddr allLists.players appInfo.player appInfo.challenger.address

                challengerAsPlayer =
                    SR.ListOps.gotPlayerFromPlayerListStrAddress allLists.players appInfo.challenger.address

                newplayerListWithPlayerAndChallengerUpdated =
                    SR.ListOps.setPlayerInPlayerListWithNewChallengerAddr newplayerListWithPlayerUpdated challengerAsPlayer appInfo.player.address

                sortedByRankingnewplayerListWithPlayerAndChallengerUpdated =
                    SR.ListOps.sortPlayerListByRank newplayerListWithPlayerAndChallengerUpdated

                newAllLists =
                    { allLists | players = sortedByRankingnewplayerListWithPlayerAndChallengerUpdated }
            in
            ( RankingOps newAllLists appInfo uiState txRec, updatePlayerList appInfo.selectedRanking.id newAllLists.players )

        _ ->
            ( Failure "createNewPlayerListWithNewChallengeAndUpdateJsonBin", Cmd.none )


handleNewUserInputs : Model -> Msg -> Model
handleNewUserInputs currentmodel msg =
    case currentmodel of
        UserOps userState allLists uaddr appInfo uiState txRec ->
            case msg of
                NewUserNameInputChg namefield ->
                    case userState of
                        SR.Types.NewUser user ->
                            UserOps (SR.Types.NewUser { user | username = namefield }) allLists uaddr appInfo SR.Types.CreateNewUser txRec

                        SR.Types.ExistingUser _ ->
                            Failure "NewUserNameInputChg"

                NewUserDescInputChg descfield ->
                    case userState of
                        SR.Types.NewUser user ->
                            UserOps (SR.Types.NewUser { user | description = descfield }) allLists uaddr appInfo SR.Types.CreateNewUser txRec

                        SR.Types.ExistingUser _ ->
                            Failure "NewUserNameInputChg"

                NewUserEmailInputChg emailfield ->
                    case userState of
                        SR.Types.NewUser user ->
                            UserOps (SR.Types.NewUser { user | email = emailfield }) allLists uaddr appInfo SR.Types.CreateNewUser txRec

                        SR.Types.ExistingUser _ ->
                            Failure "NewUserEmailInputChg"

                NewUserMobileInputChg mobilefield ->
                    case userState of
                        SR.Types.NewUser user ->
                            UserOps (SR.Types.NewUser { user | mobile = mobilefield }) allLists uaddr appInfo SR.Types.CreateNewUser txRec

                        SR.Types.ExistingUser _ ->
                            Failure "NewUserMobileInputChg"

                _ ->
                    Failure "NewUserNameInputChg"

        _ ->
            Failure "NewUserNameInputChg"


extractAndSortPlayerList : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.Player
extractAndSortPlayerList rdlPlayer =
    SR.ListOps.sortPlayerListByRank <| Utils.MyUtils.extractPlayersFromWebData rdlPlayer


updatedForChallenge : Model -> List SR.Types.Player -> SR.Types.Player -> SR.Types.User -> Model
updatedForChallenge currentmodel lplayer opponentAsPlayer user =
    case currentmodel of
        RankingOps allLists appInfo _ txRec ->
            let
                newAppInfoWithPlayer =
                    { appInfo | player = SR.ListOps.gotCurrentUserAsPlayerFromPlayerList lplayer user }

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
        UserOps userState allLists uaddr appInfo uiState txRec ->
            let
                --     extractedUsersFromWebData =
                --         Utils.MyUtils.extractUsersFromWebData allLists
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
                UserOps (SR.Types.NewUser userWithUpdatedAddr) newAllLists uaddr userUpdatedInAppInfo SR.Types.CreateNewUser txRec

        _ ->
            Failure "should be in UserOps"


updateSelectedRankingPlayerList : Model -> List SR.Types.Player -> Model
updateSelectedRankingPlayerList currentmodel lplayers =
    case currentmodel of
        RankingOps allLists appInfo _ txRec ->
            let
                resetSelectedRankingPlayerList =
                    { allLists | players = lplayers }
            in
            RankingOps resetSelectedRankingPlayerList appInfo SR.Types.UISelectedRankingUserIsPlayer txRec

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


updateSelectedRankingOnPlayersReceived : Model -> List SR.Types.Player -> Model
updateSelectedRankingOnPlayersReceived currentmodel lplayers =
    case currentmodel of
        RankingOps allLists appInfo uiState txRec ->
            let
                newAppPlayer =
                    { appInfo | player = SR.ListOps.gotPlayerFromPlayerListStrAddress lplayers appInfo.user.ethaddress }

                newAppChallengerAndPlayer =
                    { newAppPlayer | challenger = SR.ListOps.gotPlayerFromPlayerListStrAddress lplayers newAppPlayer.player.challengeraddress }

                allListsPlayersAdded =
                    { allLists | players = lplayers }
            in
            if SR.ListOps.isUserSelectedOwnerOfRanking appInfo.selectedRanking allLists.globalRankings appInfo.user then
                RankingOps allListsPlayersAdded newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsOwner emptyTxRecord

            else if SR.ListOps.isUserMemberOfSelectedRanking lplayers appInfo.user then
                let
                    _ =
                        Debug.log "isUserMemberOfSelectedRanking"
                in
                RankingOps allListsPlayersAdded newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord

            else
                RankingOps allListsPlayersAdded newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord

        _ ->
            Failure <| "updateSelectedRankingOnPlayersReceived : "



-- view


view : Model -> Html Msg
view model =
    case model of
        RankingOps allLists appInfo uiState txRec ->
            case uiState of
                SR.Types.CreateNewUser ->
                    inputNewUserview appInfo.user

                SR.Types.CreateNewLadder ->
                    --inputNewLadderview allLists.globalRankings appInfo.user appInfo.selectedRanking
                    inputNewLadderview model

                SR.Types.UISelectedRankingUserIsOwner ->
                    selectedUserIsOwnerView model

                SR.Types.UISelectedRankingUserIsPlayer ->
                    selectedUserIsPlayerView model

                SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer ->
                    selectedUserIsNeitherOwnerNorPlayerView model

                SR.Types.UIRenderAllRankings ->
                    globalResponsiveview allLists.globalRankings appInfo.user

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

                SR.Types.WalletOpenedUserCheckDone user uaddr ->
                    if user.username == "" then
                        inputNewUserview user

                    else
                        greetingView <| "Welcome back " ++ user.username

                SR.Types.WalletOpenedAndOperational ->
                    greetingView "WalletOpenedAndOperational"

        UserOps userState userList uaddr uname uiState _ ->
            case uiState of
                SR.Types.CreateNewUser ->
                    case userState of
                        SR.Types.NewUser user ->
                            inputNewUserview user

                        _ ->
                            greetingView <| "Loading ... "

                _ ->
                    greetingView <| "Loading ... "

        Failure str ->
            greetingView <| "Model failure in view: " ++ str


greetingHeading : String -> Element Msg
greetingHeading greetingStr =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Initializing ..."
        , Element.column Card.fill
            [ Element.el Heading.h4 <| Element.text greetingStr
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
                    if isPlayerCurrentUser && isChallenged then
                        let
                            _ =
                                Debug.log "challengerAsUser.username" challengerAsUser.username
                        in
                        True

                    else
                        False
            in
            if SR.ListOps.isUserMemberOfSelectedRanking allLists.players appInfo.user then
                if isPlayerCurrentUser then
                    if isCurrentUserInAChallenge then
                        Element.column Grid.simple <|
                            [ Input.button (Button.fill ++ Color.success) <|
                                { onPress = Nothing
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
                    , label = Element.text "Home"
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
                            { onPress = Just <| ResetToShowGlobal
                            , label = Element.text "Home"
                            }
                        , Input.button (Button.simple ++ Color.info) <|
                            { onPress = Just <| NewChallengeConfirmClicked

                            --onPress = Nothing
                            , label = Element.text "Confirm"
                            }
                        ]
                    ]
                ]

        _ ->
            Element.text "Fail"


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

                    --, text = user.username
                    , text = ""
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Username"
                    }
                , Input.multiline Input.simple
                    { onChange = NewUserDescInputChg

                    --, text = user.description
                    , text = ""
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Description"
                    , spellcheck = False
                    }
                , Input.text Input.simple
                    { onChange = NewUserEmailInputChg

                    --, text = user.email
                    , text = ""
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Email"
                    }
                , Input.text Input.simple
                    { onChange = NewUserMobileInputChg

                    --, text = user.mobile
                    , text = ""
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
            Html.text "Fail"


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


inputNewUserview : SR.Types.User -> Html Msg
inputNewUserview user =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , inputNewUser user
            , newuserConfirmPanel user
            ]


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


addedUAddrToNewEmptyUser : Eth.Types.Address -> SR.Types.User
addedUAddrToNewEmptyUser uaddr =
    let
        newEmptyUser =
            SR.Defaults.emptyUser

        newUser =
            { newEmptyUser | ethaddress = Eth.Utils.addressToString uaddr }
    in
    newUser



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
        idJsonObj : Json.Encode.Value
        idJsonObj =
            Json.Encode.list
                Json.Encode.object
                [ [ ( "datestamp", Json.Encode.int 123456 )
                  , ( "active", Json.Encode.bool True )
                  , ( "address", Json.Encode.string (String.toLower user.ethaddress) )
                  , ( "rank", Json.Encode.int 1 )
                  , ( "name", Json.Encode.string user.username )
                  , ( "id", Json.Encode.int 1 )
                  , ( "isplayercurrentlychallenged", Json.Encode.bool False )
                  , ( "email", Json.Encode.string user.email )
                  , ( "mobile", Json.Encode.string user.mobile )
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
            Http.jsonBody <| idJsonObj
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
            { --datestamp = 12345
              --, active = True
              address = userRec.ethaddress
            , rank = List.length lPlayer + 1

            --, name = userRec.username
            --, id = List.length lPlayer + 1
            --, isplayercurrentlychallenged = False
            , challengeraddress = ""
            }

        selectedRankingListWithNewPlayerJsonObjAdded =
            newPlayer :: lPlayer

        sortedSelectedRankingListWithNewPlayerJsonObjAdded =
            SR.ListOps.sortPlayerListByRank selectedRankingListWithNewPlayerJsonObjAdded
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

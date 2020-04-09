module Main exposing (jsonEncodeNewGlobalRankingList, main)

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
import Framework.Group as Group
import Framework.Heading as Heading
import Framework.Input as Input
import Html exposing (Html)
import Http
import Internal.Types
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Ports
import Process
import RemoteData
import SR.Constants
import SR.Decode
import SR.Defaults
import SR.ListOps
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
--we have to have a separate VARIANT for the user to move on from wallet_status sub - avoiding looping


type Model
    = WalletOps SR.Types.WalletState TxRecord
    | UserOps SR.Types.UserState (List SR.Types.User) Eth.Types.Address SR.Types.User SR.Types.UIState TxRecord
    | GlobalRankings (List SR.Types.RankingInfo) SR.Types.RankingInfo SR.Types.UIState (List SR.Types.User) SR.Types.User TxRecord
    | SelectedRanking (List SR.Types.RankingInfo) (List SR.Types.Player) Internal.Types.RankingId SR.Types.User SR.Types.Challenge SR.Types.UIState TxRecord
    | Failure String



--init : Commands msg -> Flags -> ( Model, Cmd Msg )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        node =
            --Net.toNetworkId networkId
            --currently hardcode
            Net.toNetworkId 4
                |> Ports.ethNode
    in
    ( --WalletOps [] (SR.Types.NewUser <| addedUAddrToNewEmptyUser <| Internal.Types.Address "") SR.Types.Missing SR.Types.UIRenderAllRankings
      WalletOps SR.Types.Missing emptyTxRecord
    , Cmd.batch
        [ Ports.log "Sending out msg from init "
        , Task.attempt PollBlock (Eth.getBlockNumber node.http)

        --, Cmd.none
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
    | ChangedUIStateToCreateNew (List SR.Types.RankingInfo) SR.Types.User
    | NewRankingRequestedByConfirmBtnClicked SR.Types.RankingInfo
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
    | GotRankingId Internal.Types.RankingId
    | GotRankingIdAndRankingOwnerAddr Internal.Types.RankingId String
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | UsersReceived (RemoteData.WebData (List SR.Types.User))
    | MissingWalletInstructions
    | OpenWalletInstructions
    | NewUser
    | ResetToShowGlobal (List SR.Types.RankingInfo) SR.Types.User
    | ExistingUser Eth.Types.Address
    | LadderNameInputChg String
    | LadderDescInputChg String
    | ProcessResult SR.Types.ResultOfMatch
    | SentResultToJsonbin (Result Http.Error ())
    | NewUserNameInputChg String
    | NewUserDescInputChg String
    | NewUserRequested SR.Types.User
    | ClickedJoinSelected
    | ReturnFromJoin (RemoteData.WebData (List SR.Types.Player))
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
                                    ( UserOps (SR.Types.NewUser <| SR.Defaults.emptyUser) [] uaddr SR.Defaults.emptyUser SR.Types.DisplayWalletInfoToUser txRec, gotUserList )

                        Rinkeby ->
                            case walletSentry_.account of
                                Nothing ->
                                    ( WalletOps SR.Types.Locked txRec, Cmd.none )

                                Just uaddr ->
                                    ( UserOps (SR.Types.NewUser <| SR.Defaults.emptyUser) [] uaddr SR.Defaults.emptyUser SR.Types.DisplayWalletInfoToUser txRec, gotUserList )

                        --( WalletOps (SR.Types.WalletOpenedWithoutUserCheck uaddr) { txRec | account = walletSentry_.account, node = Ports.ethNode walletSentry_.networkId } challenge, gotUserList )
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

        UserOps userState userList uaddr updatedUser uiState txRec ->
            case msgOfTransitonThatAlreadyHappened of
                PollBlock (Ok blockNumber) ->
                    ( UserOps
                        (SR.Types.NewUser updatedUser)
                        userList
                        uaddr
                        updatedUser
                        SR.Types.CreateNewUser
                        txRec
                    , Cmd.none
                    )

                PollBlock (Err error) ->
                    ( WalletOps SR.Types.WalletOpenedAndOperational txRec, Cmd.none )

                UsersReceived userlist ->
                    let
                        updateUserAddr =
                            SR.Defaults.emptyActiveUser

                        userWithUpdatedAddr =
                            { updateUserAddr | ethaddress = Eth.Utils.addressToString uaddr }
                    in
                    if isUserInList userlist uaddr then
                        ( GlobalRankings [] SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings (Utils.MyUtils.extractUsersFromWebData userlist) (SR.ListOps.singleUserInList userlist uaddr) emptyTxRecord, gotRankingList )

                    else
                        ( UserOps (SR.Types.NewUser userWithUpdatedAddr) (Utils.MyUtils.extractUsersFromWebData userlist) uaddr userWithUpdatedAddr SR.Types.CreateNewUser txRec, Cmd.none )

                NewUserNameInputChg namefield ->
                    case userState of
                        SR.Types.NewUser user ->
                            ( UserOps (SR.Types.NewUser { user | username = namefield }) userList uaddr SR.Defaults.emptyUser SR.Types.CreateNewUser txRec, Cmd.none )

                        SR.Types.ExistingUser _ ->
                            ( Failure "NewUserNameInputChg", Cmd.none )

                NewUserDescInputChg descfield ->
                    case userState of
                        SR.Types.NewUser user ->
                            ( UserOps (SR.Types.NewUser { user | description = descfield }) userList uaddr SR.Defaults.emptyUser SR.Types.CreateNewUser txRec, Cmd.none )

                        SR.Types.ExistingUser _ ->
                            ( Failure "NewUserNameInputChg", Cmd.none )

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
                    in
                    ( GlobalRankings [] SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings [] userInfo { txRec | txSentry = newSentry }, Cmd.batch [ sentryCmd, createNewUser userList userWithUpdatedAddr, gotRankingList ] )

                _ ->
                    --todo: better logic. This should go to failure model rather than fall thru to UserOps
                    -- but currently logic needs to do this
                    ( UserOps (SR.Types.NewUser SR.Defaults.emptyUser) [] uaddr SR.Defaults.emptyUser SR.Types.CreateNewUser txRec, Cmd.none )

        GlobalRankings lrankingInfo rnkInfo uiState userList user txRec ->
            case msgOfTransitonThatAlreadyHappened of
                GotGlobalRankingsJson rmtrnkingdata ->
                    ( GlobalRankings (Utils.MyUtils.extractRankingsFromWebData rmtrnkingdata) SR.Defaults.emptyRankingInfo uiState userList user emptyTxRecord, Cmd.none )

                GotRankingId rnkidstr ->
                    ( SelectedRanking lrankingInfo [] rnkidstr user SR.Defaults.emptyChallenge uiState emptyTxRecord, fetchedSingleRanking rnkidstr )

                GotRankingIdAndRankingOwnerAddr rnkidstr rnkownerstr ->
                    ( SelectedRanking lrankingInfo [] rnkidstr user SR.Defaults.emptyChallenge uiState emptyTxRecord, fetchedSingleRanking rnkidstr )

                --( updateSelectedRankingUIState rnkidstr rnkownerstr currentmodel, fetchedSingleRanking rnkidstr )
                -- this is the response from createNewPlayerListWithCurrentUser Cmd
                -- it had the Http.expectStringResponse in it
                -- it's already created the new ranking with current player as the first entry
                -- the result now is the ranking id only at this point which was pulled out by the decoder
                -- the lrankingInfo is preserved
                SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder ->
                    --SR.Types.ExistingLadder rnkInfo ->
                    ( GlobalRankings lrankingInfo SR.Defaults.emptyRankingInfo SR.Types.CreateNewLadder userList user emptyTxRecord
                    , addedNewRankingListEntryInGlobal idValueFromDecoder lrankingInfo rnkInfo user.ethaddress
                    )

                SentUserInfoAndDecodedResponseToNewUser serverResponse ->
                    ( GlobalRankings lrankingInfo SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings userList user emptyTxRecord, Cmd.none )

                ResetToShowGlobal lrankingInfoForReset userRec ->
                    ( GlobalRankings lrankingInfoForReset SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings userList userRec emptyTxRecord, Cmd.none )

                ChangedUIStateToCreateNew lrankingInfoChgToCreateNew userRec ->
                    ( GlobalRankings lrankingInfoChgToCreateNew SR.Defaults.emptyRankingInfo SR.Types.CreateNewLadder userList userRec emptyTxRecord, Cmd.none )

                AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList ->
                    ( GlobalRankings (Utils.MyUtils.extractRankingsFromWebData <| updatedListAfterNewEntryAddedToGlobalList) SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings userList user emptyTxRecord, Cmd.none )

                LadderNameInputChg namefield ->
                    --case ladderState of
                    --SR.Types.NewLadder ladder ->
                    ( GlobalRankings lrankingInfo { rnkInfo | rankingname = namefield } SR.Types.CreateNewLadder userList user emptyTxRecord, Cmd.none )

                -- SR.Types.ExistingLadder ladder ->
                --     ( GlobalRankings lrankingInfo (SR.Types.NewLadder { ladder | rankingname = namefield }) SR.Types.CreateNewLadder  userList user emptyTxRecord, Cmd.none )
                LadderDescInputChg descfield ->
                    --case ladderState of
                    --SR.Types.NewLadder ladder ->
                    ( GlobalRankings lrankingInfo { rnkInfo | rankingdesc = descfield } SR.Types.CreateNewLadder userList user emptyTxRecord, Cmd.none )

                -- SR.Types.ExistingLadder ladder ->
                --     ( GlobalRankings lrankingInfo (SR.Types.NewLadder { ladder | rankingdesc = descfield }) SR.Types.CreateNewLadder  userList user emptyTxRecord, Cmd.none )
                NewRankingRequestedByConfirmBtnClicked newLadderRnkInfo ->
                    let
                        _ =
                            Debug.log "confirm " "btn"

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
                        _ =
                            Debug.log "NewRankingRequestedByConfirmBtnClicked user " user.ethaddress
                    in
                    ( GlobalRankings lrankingInfo newLadderRnkInfo SR.Types.CreateNewLadder userList user { txRec | txSentry = newSentry }, Cmd.batch [ sentryCmd, createNewPlayerListWithCurrentUser user ] )

                Fail str ->
                    let
                        _ =
                            Debug.log "GlobalRankings fail " str
                    in
                    ( GlobalRankings lrankingInfo SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings userList user emptyTxRecord, Cmd.none )

                _ ->
                    ( GlobalRankings lrankingInfo SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings userList user emptyTxRecord, Cmd.none )

        SelectedRanking lrankingInfo lPlayer intrankingId userRec challenge uiState txRec ->
            case msgOfTransitonThatAlreadyHappened of
                PlayersReceived players ->
                    --( updateSelectedRankingUIState intrankingId currentmodel (Utils.MyUtils.extractPlayersFromWebData players), Cmd.none )
                    ( updateSelectedRankingUIState intrankingId currentmodel (extractAndSortPlayerList players), Cmd.none )

                ResetToShowGlobal _ user ->
                    ( GlobalRankings lrankingInfo SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings [ SR.Defaults.emptyUser ] user emptyTxRecord, Cmd.none )

                TxSentryMsg subMsg ->
                    let
                        ( subModel, subCmd ) =
                            Eth.Sentry.Tx.update subMsg txRec.txSentry
                    in
                    ( WalletOps SR.Types.WalletOpenedAndOperational { txRec | txSentry = subModel }, subCmd )

                ProcessResult result ->
                    let
                        whoHigher =
                            isOpponentHigherRank challenge.player challenge.opponent

                        _ =
                            Debug.log "made it to process result!" 8
                    in
                    case result of
                        SR.Types.Won ->
                            case whoHigher of
                                SR.Types.OpponentRankHigher ->
                                    --nb. higher rank is a lower number and vice versa!
                                    ( SelectedRanking lrankingInfo
                                        lPlayer
                                        intrankingId
                                        userRec
                                        { challenge
                                            | playerRank = challenge.opponentRank
                                            , opponentRank = challenge.opponentRank + 1
                                            , playerStatus = SR.Types.Available
                                            , opponentStatus = SR.Types.Available
                                        }
                                        uiState
                                        txRec
                                    , Cmd.none
                                    )

                                SR.Types.OpponentRankLower ->
                                    --nb. higher rank is a lower number and vice versa!
                                    ( SelectedRanking lrankingInfo
                                        lPlayer
                                        intrankingId
                                        userRec
                                        { challenge | playerStatus = SR.Types.Available, opponentStatus = SR.Types.Available }
                                        uiState
                                        txRec
                                    , Cmd.none
                                    )

                        SR.Types.Lost ->
                            case whoHigher of
                                SR.Types.OpponentRankHigher ->
                                    --nb. higher rank is a lower number and vice versa!
                                    ( SelectedRanking lrankingInfo
                                        lPlayer
                                        intrankingId
                                        userRec
                                        { challenge | playerStatus = SR.Types.Available, opponentStatus = SR.Types.Available }
                                        uiState
                                        txRec
                                    , Cmd.none
                                    )

                                SR.Types.OpponentRankLower ->
                                    --nb. higher rank is a lower number and vice versa!
                                    ( SelectedRanking lrankingInfo
                                        lPlayer
                                        intrankingId
                                        userRec
                                        { challenge
                                            | opponentRank = challenge.playerRank
                                            , playerRank = challenge.opponentRank + 1
                                            , playerStatus = SR.Types.Available
                                            , opponentStatus = SR.Types.Available
                                        }
                                        uiState
                                        txRec
                                    , Cmd.none
                                    )

                        SR.Types.Undecided ->
                            ( SelectedRanking lrankingInfo
                                lPlayer
                                intrankingId
                                userRec
                                challenge
                                uiState
                                txRec
                            , Cmd.none
                            )

                SentResultToJsonbin a ->
                    ( SelectedRanking lrankingInfo
                        lPlayer
                        intrankingId
                        userRec
                        challenge
                        uiState
                        txRec
                    , Cmd.none
                    )

                DeletedRanking uaddr ->
                    ( SelectedRanking lrankingInfo
                        lPlayer
                        intrankingId
                        userRec
                        challenge
                        uiState
                        txRec
                    , deleteSelectedRankingFromJsonBin intrankingId
                    )

                DeletedSingleRankingFromJsonBin result ->
                    ( SelectedRanking lrankingInfo
                        lPlayer
                        intrankingId
                        userRec
                        challenge
                        uiState
                        txRec
                    , deleteSelectedRankingFromGlobalList intrankingId lrankingInfo userRec.ethaddress
                    )

                Fail str ->
                    ( Failure <| "Fail failure : " ++ str, Cmd.none )

                DeletedRankingFromGlobalList updatedListAfterRankingDeletedFromGlobalList ->
                    ( GlobalRankings (Utils.MyUtils.extractRankingsFromWebData <| updatedListAfterRankingDeletedFromGlobalList) SR.Defaults.emptyRankingInfo SR.Types.UIRenderAllRankings [ SR.Defaults.emptyUser ] userRec emptyTxRecord, Cmd.none )

                ClickedJoinSelected ->
                    ( currentmodel, addCurrentUserToPlayerList intrankingId lPlayer userRec )

                ReturnFromJoin response ->
                    ( updateSelectedRankingPlayerList currentmodel (Utils.MyUtils.extractPlayersFromWebData response), Cmd.none )

                _ ->
                    ( Failure <| "Fall thru failure : ", Cmd.none )

        Failure str ->
            ( Failure <| "Model failure in selected ranking: " ++ str, Cmd.none )


extractAndSortPlayerList : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.Player
extractAndSortPlayerList rdlPlayer =
    SR.ListOps.sortPlayerListByRank <| Utils.MyUtils.extractPlayersFromWebData rdlPlayer


updateSelectedRankingPlayerList : Model -> List SR.Types.Player -> Model
updateSelectedRankingPlayerList currentmodel lplayers =
    case currentmodel of
        SelectedRanking lrankingInfo _ intrankingId userRec _ _ txRec ->
            SelectedRanking lrankingInfo lplayers intrankingId userRec SR.Defaults.emptyChallenge SR.Types.UISelectedRankingUserIsPlayer txRec

        _ ->
            Failure <| "updateSelectedRankingPlayerList : "


updateSelectedRankingUIState : Internal.Types.RankingId -> Model -> List SR.Types.Player -> Model
updateSelectedRankingUIState rnkid currentmodel lplayers =
    case currentmodel of
        SelectedRanking lrankingInfo lPlayer intrankingId userRec challenge uiState txRec ->
            if SR.ListOps.isUserSelectedOwnerOfRanking rnkid lrankingInfo userRec then
                SelectedRanking lrankingInfo lplayers rnkid userRec SR.Defaults.emptyChallenge SR.Types.UISelectedRankingUserIsOwner emptyTxRecord

            else if SR.ListOps.isUserMemberOfSelectedRanking lplayers userRec then
                SelectedRanking lrankingInfo lplayers rnkid userRec SR.Defaults.emptyChallenge SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord

            else
                SelectedRanking lrankingInfo lplayers rnkid userRec SR.Defaults.emptyChallenge SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord

        _ ->
            Failure <| "updateSelectedRankingUIState : "



-- view


view : Model -> Html Msg
view model =
    case model of
        GlobalRankings lrankingInfo rnkInfo uiState userlist userRec txRec ->
            case uiState of
                SR.Types.CreateNewUser ->
                    inputNewUserview userRec

                SR.Types.CreateNewLadder ->
                    inputNewLadderview lrankingInfo userRec rnkInfo

                _ ->
                    globalResponsiveview lrankingInfo userRec

        SelectedRanking lrankingInfo playerList rnkid userRec connect uiState txRec ->
            case uiState of
                SR.Types.UISelectedRankingUserIsOwner ->
                    selectedUserIsOwnerView lrankingInfo playerList rnkid userRec

                SR.Types.UISelectedRankingUserIsPlayer ->
                    selectedUserIsPlayerView lrankingInfo playerList rnkid userRec

                _ ->
                    selectedRankingView lrankingInfo playerList rnkid userRec

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


globalHeading : SR.Types.User -> Element Msg
globalHeading user =
    Element.column Grid.section <|
        [ Element.el Heading.h5 <| Element.text "Global Rankings"
        , Element.column Card.fill
            [ Element.el Heading.h4 <| Element.text user.username
            ]
        ]


selectedHeading : SR.Types.User -> SR.Types.RankingInfo -> Element Msg
selectedHeading user rnkInfo =
    let
        _ =
            Debug.log "rank id " rnkInfo.id
    in
    Element.column Grid.section <|
        [ Element.el Heading.h5 <|
            Element.text (user.username ++ " you selected ranking")
        , Element.column Card.fill
            [ Element.el Heading.h4 <|
                Element.text rnkInfo.rankingname
            , Element.text rnkInfo.rankingdesc
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
        [ Input.button (Button.fill ++ Color.info) <|
            { onPress = Just (GotRankingIdAndRankingOwnerAddr (Internal.Types.RankingId rankingobj.id) rankingobj.rankingowneraddr)
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


playerbuttons : SR.Types.User -> List SR.Types.Player -> Element Msg
playerbuttons user playerInfoList =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Selected Ranking"
        , Element.column (Card.simple ++ Grid.simple) <|
            insertPlayerList user playerInfoList
        , Element.paragraph (Card.fill ++ Color.warning) <|
            [ Element.el [ Font.bold ] <| Element.text "Please note: "
            , Element.paragraph [] <|
                List.singleton <|
                    Element.text "Clicking 'Create New' interacts with your Ethereum wallet"
            ]
        ]


addPlayerInfoToAnyElText : SR.Types.User -> SR.Types.Player -> Element Msg
addPlayerInfoToAnyElText user playerObj =
    let
        playerAvailability =
            if playerObj.currentchallengername == "" then
                "Available"

            else
                playerObj.currentchallengername

        isPlayerCurrentUser =
            if playerObj.address == user.ethaddress then
                True

            else
                False
    in
    if isPlayerCurrentUser then
        Element.column Grid.simple <|
            [ Input.button (Button.fill ++ Color.disabled) <|
                { onPress = Nothing
                , label = Element.text <| String.fromInt playerObj.rank ++ ". " ++ playerObj.name ++ " vs " ++ playerAvailability
                }
            ]

    else
        Element.column Grid.simple <|
            [ Input.button (Button.fill ++ Color.info) <|
                { onPress = Just (GotRankingId (Internal.Types.RankingId <| String.fromInt playerObj.id))
                , label = Element.text <| String.fromInt playerObj.rank ++ ". " ++ playerObj.name ++ " vs " ++ playerAvailability
                }
            ]


insertPlayerList : SR.Types.User -> List SR.Types.Player -> List (Element Msg)
insertPlayerList user playerInfoList =
    let
        mapOutPlayerList =
            List.map
                (addPlayerInfoToAnyElText
                    user
                )
                playerInfoList
    in
    mapOutPlayerList


globalhomebutton : List SR.Types.RankingInfo -> SR.Types.User -> Element Msg
globalhomebutton rankingList user =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal rankingList user
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ChangedUIStateToCreateNew rankingList user
                    , label = Element.text "Create New"
                    }
                ]
            ]
        ]


selectedhomebuttons : List SR.Types.RankingInfo -> SR.Types.User -> Element Msg
selectedhomebuttons rankingList user =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal rankingList user
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just ClickedJoinSelected
                    , label = Element.text "Join"
                    }
                ]
            ]
        , Element.column Grid.simple <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text "Button attributes can be combined with other attributes."
            ]
        ]


selecteduserIsOwnerhomebutton : List SR.Types.RankingInfo -> SR.Types.User -> Element Msg
selecteduserIsOwnerhomebutton rankingList user =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal rankingList user
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.danger) <|
                    { onPress = Just <| DeletedRanking user.ethaddress
                    , label = Element.text "Delete"
                    }
                ]
            ]
        , Element.column Grid.simple <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text "Button attributes can be combined with other attributes."
            ]
        ]


selecteduserIsPlayerHomebutton : List SR.Types.RankingInfo -> SR.Types.User -> Element Msg
selecteduserIsPlayerHomebutton rankingList user =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal rankingList user
                    , label = Element.text "Home"
                    }
                ]
            ]
        , Element.column Grid.simple <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text "Button attributes can be combined with other attributes."
            ]
        ]


newrankinhomebutton : List SR.Types.RankingInfo -> SR.Types.User -> SR.Types.RankingInfo -> Element Msg
newrankinhomebutton rankingList user rnkInfo =
    Element.column Grid.section <|
        [ Element.el Heading.h6 <| Element.text "Click to continue ..."
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button (Button.simple ++ Color.simple) <|
                    { onPress = Just <| ResetToShowGlobal rankingList user
                    , label = Element.text "Home"
                    }
                , Input.button (Button.simple ++ Color.info) <|
                    { onPress = Just <| NewRankingRequestedByConfirmBtnClicked rnkInfo
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
                [ Input.text Input.simple
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
                ]
            ]
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Input attributes can be combined with other attributes."
        ]


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
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Input attributes can be combined with other attributes."
        ]


globalResponsiveview : List SR.Types.RankingInfo -> SR.Types.User -> Html Msg
globalResponsiveview rankingList user =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , globalHeading user
            , globalhomebutton rankingList user
            , rankingbuttons rankingList
            ]


selectedRankingView : List SR.Types.RankingInfo -> List SR.Types.Player -> Internal.Types.RankingId -> SR.Types.User -> Html Msg
selectedRankingView lrankingInfo playerList rnkid user =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , selectedHeading user <| gotRankingFromRankingList lrankingInfo rnkid
            , selectedhomebuttons lrankingInfo user
            , playerbuttons user playerList
            ]


selectedUserIsOwnerView : List SR.Types.RankingInfo -> List SR.Types.Player -> Internal.Types.RankingId -> SR.Types.User -> Html Msg
selectedUserIsOwnerView lrankingInfo playerList rnkid user =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank - Owner"
            , selectedHeading user <| gotRankingFromRankingList lrankingInfo rnkid
            , selecteduserIsOwnerhomebutton lrankingInfo user
            , playerbuttons user playerList
            ]


selectedUserIsPlayerView : List SR.Types.RankingInfo -> List SR.Types.Player -> Internal.Types.RankingId -> SR.Types.User -> Html Msg
selectedUserIsPlayerView lrankingInfo playerList rnkid user =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank - Owner"
            , selectedHeading user <| gotRankingFromRankingList lrankingInfo rnkid
            , selecteduserIsPlayerHomebutton lrankingInfo user
            , playerbuttons user playerList
            ]


inputNewUserview : SR.Types.User -> Html Msg
inputNewUserview user =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "SportRank"
            , inputNewUser user
            , newuserConfirmPanel user
            ]


inputNewLadderview : List SR.Types.RankingInfo -> SR.Types.User -> SR.Types.RankingInfo -> Html Msg
inputNewLadderview rankingList user rnkInfo =
    Framework.responsiveLayout [] <|
        Element.column
            Framework.container
            [ Element.el Heading.h4 <| Element.text "Create New Ladder Ranking"
            , newrankinhomebutton rankingList user rnkInfo
            , inputNewLadder rnkInfo
            ]


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

        GlobalRankings _ _ _ _ _ _ ->
            Sub.none

        SelectedRanking _ _ _ _ _ _ _ ->
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


isUserInList : RemoteData.WebData (List SR.Types.User) -> Eth.Types.Address -> Bool
isUserInList userlist uaddr =
    let
        gotSingleUserFromList =
            SR.ListOps.singleUserInList userlist uaddr
    in
    if gotSingleUserFromList.ethaddress == "" then
        False

    else
        True


addedUAddrToNewEmptyUser : Eth.Types.Address -> SR.Types.User
addedUAddrToNewEmptyUser uaddr =
    let
        newEmptyUser =
            SR.Defaults.emptyUser

        newUser =
            { newEmptyUser | ethaddress = Eth.Utils.addressToString uaddr }
    in
    newUser


gotRankingFromRankingList : List SR.Types.RankingInfo -> Internal.Types.RankingId -> SR.Types.RankingInfo
gotRankingFromRankingList rankingList (Internal.Types.RankingId rnkid) =
    let
        existingRanking =
            List.head <|
                List.filter (\r -> r.id == String.toLower rnkid)
                    rankingList
    in
    case existingRanking of
        Nothing ->
            SR.Defaults.emptyRankingInfo

        Just a ->
            a



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
        , url = "https://api.jsonbin.io/b/" ++ rankingId ++ "/latest"
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
                  , ( "currentchallengername", Json.Encode.string "Available" )
                  , ( "currentchallengerid", Json.Encode.int 0 )
                  , ( "address", Json.Encode.string (String.toLower user.ethaddress) )
                  , ( "rank", Json.Encode.int 1 )
                  , ( "name", Json.Encode.string user.username )
                  , ( "playerid", Json.Encode.int 1 )
                  , ( "currentchallengeraddress", Json.Encode.string (String.toLower "") )
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

        _ =
            Debug.log "encode the list: " encodedList
    in
    encodedList



--intrankingId lPlayer userRec
-- addCurrentUserToPlayerList : RemoteData.WebData SR.Types.RankingId -> List SR.Types.RankingInfo -> SR.Types.RankingInfo -> String -> Cmd Msg
-- addCurrentUserToPlayerList newrankingid lrankingInfo rnkInfo rankingowneraddress =


addCurrentUserToPlayerList : Internal.Types.RankingId -> List SR.Types.Player -> SR.Types.User -> Cmd Msg
addCurrentUserToPlayerList (Internal.Types.RankingId intrankingId) lPlayer userRec =
    let
        newPlayer =
            { datestamp = 12345
            , active = True
            , currentchallengername = "Available"
            , currentchallengerid = 0
            , address = userRec.ethaddress
            , rank = List.length lPlayer + 1
            , name = userRec.username
            , id = List.length lPlayer + 1
            , currentchallengeraddress = ""
            }

        selectedRankingListWithNewPlayerJsonObjAdded =
            newPlayer :: lPlayer

        sortedSelectedRankingListWithNewPlayerJsonObjAdded =
            SR.ListOps.sortPlayerListByRank selectedRankingListWithNewPlayerJsonObjAdded

        _ =
            Debug.log "sortedSelectedRankingListWithNewPlayerJsonObjAdded" sortedSelectedRankingListWithNewPlayerJsonObjAdded
    in
    --AddedNewRankingToGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewSelectedRankingPlayerList sortedSelectedRankingListWithNewPlayerJsonObjAdded
        , expect = Http.expectJson (RemoteData.fromResult >> ReturnFromJoin) SR.Decode.decodeNewPlayerListServerResponse
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
                [ ( "datestamp", Json.Encode.int 123456 )
                , ( "active", Json.Encode.bool True )
                , ( "currentchallengername", Json.Encode.string "Available" )
                , ( "currentchallengerid", Json.Encode.int 0 )
                , ( "address", Json.Encode.string (String.toLower player.address) )
                , ( "rank", Json.Encode.int player.rank )
                , ( "name", Json.Encode.string player.name )
                , ( "playerid", Json.Encode.int player.id )
                , ( "currentchallengeraddress", Json.Encode.string (String.toLower "") )
                ]

        encodedList =
            Json.Encode.list encodePlayerObj lplayers

        _ =
            Debug.log "encode the list: " encodedList
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

        _ =
            Debug.log "encode the list: " encodedList
    in
    encodedList


gotRankingListFromRemData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
gotRankingListFromRemData globalList =
    case globalList of
        RemoteData.Success a ->
            a

        RemoteData.NotAsked ->
            [ SR.Defaults.emptyRankingInfo
            ]

        RemoteData.Loading ->
            [ SR.Defaults.emptyRankingInfo
            ]

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.Timeout ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.NetworkError ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.BadStatus statuscode ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.BadBody s ->
                    [ SR.Defaults.emptyRankingInfo
                    ]


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


deleteSelectedRankingFromJsonBin : Internal.Types.RankingId -> Cmd Msg
deleteSelectedRankingFromJsonBin (Internal.Types.RankingId rankingId) =
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


deleteSelectedRankingFromGlobalList : Internal.Types.RankingId -> List SR.Types.RankingInfo -> String -> Cmd Msg
deleteSelectedRankingFromGlobalList (Internal.Types.RankingId rankingId) lrankingInfo rankingowneraddress =
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

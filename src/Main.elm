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
import SR.Defaults
import SR.Elements
import SR.Types
import Task
import Time exposing (Posix)
import Utils.MyUtils
import Utils.Validation.Validate
import Validate
import Data.Selected
import EverySet exposing (EverySet)
import Data.Users
import Data.Global
import Data.Rankings
import Data.Players
import Widget exposing (..)
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
    = AppOps DataState Data.Users.User SR.Types.UIState TxRecord
    | Failure String



type DataState
  = AllEmpty
  | Fetched Data.Users.Users Data.Rankings.Rankings DataKind
  | Updated Data.Users.Users Data.Rankings.Rankings DataKind

type DataKind
  =
  Global Data.Global.Global
  | Selected Data.Selected.Selected


init : () -> ( Model, Cmd Msg )
init _ =
    -- UIEnterResultTxProblem is wrong here, part of uiState rf
    -- WalletWaitingForTransactionReceipt same
    ( AppOps  AllEmpty (Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General) (SR.Types.GeneralUI  SR.Types.Loading) emptyTxRecord
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
    ClickedSelectedRanking Data.Global.UserRanking
    | ClickedRegister
    | ClickedConfirmedRegisterNewUser
    | ClickedUpdateExistingUser
    | ClickedConfirmedUpdateExistingUser
    | ClickedCreateNewLadder
    | ClickedConfirmCreateNewRanking Data.Rankings.Ranking
    | ClickedNewChallengeConfirm String
    | ClickedChallengeOpponent Data.Selected.UserPlayer (Maybe Data.Selected.UserPlayer)
    | ClickedJoinSelected
    | ClickedChangedUIStateToEnterResult Data.Selected.UserPlayer
    | ClickedDeleteRanking
    | ClickedDeleteRankingConfirmed
    | ClickedRemoveFromUserMemberRankings
    | ClickedEnableEthereum
    | ClickedDisplayGlobalOnly
    | Cancel
    | ResetToShowSelected
    | LadderNameInputChg String
    | LadderDescInputChg String
    | UserNameInputChg String
    | UserPasswordInputChg String
    | UserDescInputChg String
    | UserEmailInputChg String
    | UserMobileInputChg String
    | ClickedLogInUser
    | InitiallyLoggedInUser (Result (GQLHttp.Error Bridge.LoginResult) Bridge.LoginResult)
    | LoggedInUser (Result (GQLHttp.Error Data.Users.Token) Data.Users.Token)
    | RegisteredNewUser (Result (GQLHttp.Error Bridge.LoginResult) Bridge.LoginResult)
    | ReceivedUserNames (Result (GQLHttp.Error (List String)) (List String))
    | ReceivedUsers (Result (GQLHttp.Error (Maybe (List (Maybe Data.Users.FUser)))) (Maybe (List (Maybe Data.Users.FUser))))
    | ReceivedRankings (Result (GQLHttp.Error (Maybe (List (Maybe Data.Rankings.FRanking)))) (Maybe (List (Maybe Data.Rankings.FRanking))))
    --| ReceivedPlayers (Result (GQLHttp.Error (Maybe (List (Maybe Data.Players.FPlayer)))) (Maybe (List (Maybe Data.Players.FPlayer))))
    | ReceivedPlayersByRankingId (Result (GQLHttp.Error (Maybe (List (Maybe Data.Players.FPlayer))))  (Maybe (List (Maybe Data.Players.FPlayer)))) 
        
    | CreatedNewRanking (Result (GQLHttp.Error (Data.Rankings.FRanking)) (Data.Rankings.FRanking))
    | CreatedGlobal
      -- App Only Ops
    | MissingWalletInstructions
    | OpenWalletInstructions
    | NoOp
    | SentResultToJsonbin (Result Http.Error ())
    | SentUserInfoAndDecodedResponseToNewUser (RemoteData.WebData (List Data.Users.User))
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | PlayersReceived (RemoteData.WebData (List Data.Players.Player))
    | ReturnFromPlayerListUpdate (RemoteData.WebData (List Data.Players.Player))
    | ReturnFromUserListUpdate (RemoteData.WebData (List Data.Users.User))
    | ReturnedFromDeletedSelectedRankingFromJsonBin (RemoteData.WebData ( SR.Types.DeleteBinResponse))
    --| ReturnedFromDeletedRankingFromGlobalList (RemoteData.WebData (List Data.Rankings.Ranking))
    --| ReturnedFromDeletedRankingFromGlobalList (Result Http.Error SR.Types.)
    | ReturnedFromDeletedRankingFromGlobalList (Result (GQLHttp.Error (Maybe (List (Maybe Data.Rankings.FRanking)))) (Maybe (List (Maybe Data.Rankings.FRanking))))
    | SentResultToWallet Data.Selected.ResultOfMatch
    | AddedNewRankingToGlobalList (RemoteData.WebData (List Data.Rankings.Ranking))
    | TimeUpdated Posix
    | ProcessResult Data.Selected.ResultOfMatch
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
        ( WalletStatus walletSentry_, AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec ) ->
            (model, Cmd.none)

        ( WalletStatus walletSentry_, AppOps dataState (Data.Users.Registered userInfo userState) uiState txRec ) ->
            case walletSentry_.networkId of
                Rinkeby ->
                    case walletSentry_.account of
                        Nothing ->
                        
                            ( --AppOps dataState (Data.Users.Registered userInfo userState) uiState emptyTxRecord
                            model
                            , Cmd.none
                            )

                        Just uaddr ->
                        -- and here:
                            (AppOps dataState 
                                --(gotWalletAddrApplyToUser (Data.Users.Registered userInfo userState) uaddr)
                                (Data.Users.Registered {userInfo | walletState = Data.Users.NoCredit uaddr} userState)
                                    uiState emptyTxRecord, Cmd.none)
                _ ->
                    (model, Cmd.none)


                -- SR.Types.WalletStopSub ->
                --     let 
                --         _ = Debug.log "in walletstopsub" "here5"
                --     in
                --     (AppOps SR.Types.WalletStateLocked dataState user uiState   txRec, Cmd.none)

                -- SR.Types.WalletOpened ->
                --     (model, Cmd.none)


                -- SR.Types.WalletWaitingForTransactionReceipt ->
                    
                --     handleWalletWaitingForUserInput msg walletState dataState user txRec

                -- _ ->
                --     let 
                --         _ = Debug.log "fell thru at: " "update - walletState"
                --     in
                    
                --             ( AppOps SR.Types.WalletStopSub AllEmpty user SR.Types.UIDisplayWalletLockedInstructions  emptyTxRecord
                --             , Cmd.none
                --             )
        ( WalletStatus _, Failure _ ) ->
            (model, Cmd.none)

        (ClickedEnableEthereum, AppOps dataState user uiState txRec ) ->
            case user of
                Data.Users.Spectator userInfo userState ->
                    (AppOps dataState user uiState txRec, Cmd.none)
                Data.Users.Registered _ _->
                    (AppOps dataState user uiState txRec, Ports.log "eth_requestAccounts")
            

        (ClickedRemoveFromUserMemberRankings, AppOps dataState user uiState txRec ) ->
            case dataState of
                Fetched sUsers sRankings dKind ->
                    case dKind of
                        Selected sSelected ->
                            case user of
                                Data.Users.Spectator userInfo userState ->
                                    (Failure "Err", Cmd.none)
                                (Data.Users.Registered userInfo userState) ->
                                    let 
                                        --_ = Debug.log "User: " user
                                        --newUser = Data.Rankings.removedDeletedRankingsFromUserJoined user rankings
                                        --todo: replace with a real set of rankings
                                        newUser = Data.Users.removedDeletedRankingsFromUserJoined user Data.Rankings.empty
                                        _ = Debug.log "newUser: " newUser
                                        updatedsUsers = Data.Users.updatedUserInSet sUsers newUser
                                    in
                                     (AppOps dataState user uiState txRec, httpUpdateUsers updatedsUsers)  
                        _ ->
                            (model, Cmd.none)

                _ ->
                    (model, Cmd.none)


        (ClickedConfirmedRegisterNewUser, AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec) ->
            ( AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec, registerUser userInfo )
                
        -- the only user UserState can be in here is General:
        (ClickedRegister, AppOps dataState user uiState txRec ) ->
            (AppOps dataState user (SR.Types.GeneralUI SR.Types.Register) txRec, Cmd.none)
            --(AppOps dataState (Data.Users.Registered "" "" Data.Users.emptyUserInfo Data.Users.Updating) uiState txRec, Cmd.none)

        (PlayersReceived response, AppOps dataState user uiState txRec )  ->
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
            --                 Fetched sUsers sRankings dKind -> 
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
            --                                                             m_userPlayer = Data.Selected.gotUserAsPlayer newSSelected (Eth.Utils.unsafeToAddress newAppPlayer.player.player.challengerid)
            --                                                         in
            --                                                         case m_userPlayer of
            --                                                             Nothing ->
            --                                                                 (model, Cmd.none)
            --                                                             Just userPlayer ->
            --                                                                 let
            --                                                                     newAppChallengerAndPlayer = { newAppPlayer | challenger = userPlayer }
            --                                                                     newDataKind = Selected newSSelected rnkId user status rankings
            --                                                                     newDataState = Fetched sUsers sRankings newDataKind
            --                                                                 in
            --                                                                     case status of 
            --                                                                         Data.Selected.Owner ->     
            --                                                                             (AppOps newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsOwner emptyTxRecord, Cmd.none)
            --                                                                         Data.Selected.Member  ->
            --                                                                             (AppOps newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord, Cmd.none)
            --                                                                         Data.Selected.Other ->
            --                                                                             (AppOps newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, Cmd.none)
                    
            --                             _ ->
            --                                 (model, Cmd.none)

                            -- Updated sUsers sRankings dKind -> 
                            --     case dKind of 
                            --             Selected sSelected ->
                            --                 case user.m_ethaddress of 
                            --                     Nothing -> 
                            --                         (model, Cmd.none)
                            --                     Just addr ->
                            --                         let 
                            --                             newSSelected = Data.Selected.createdSelected (Data.Players.asList sPlayers) sUsers rnkId
                                            
                            --                             newAppPlayer = { appInfo | player = Data.Selected.gotUserAsPlayer newSSelected addr }

                            --                             m_userPlayer = Data.Selected.gotUserAsPlayer newSSelected (Eth.Utils.unsafeToAddress newAppPlayer.player.player.challengerid)
     
                            --                         in
                            --                             case m_userPlayer of 
                            --                                 Nothing ->
                            --                                     (model, Cmd.none)
                            --                                 Just userPlayer ->
                            --                                     let 
                            --                                         newAppChallengerAndPlayer = { newAppPlayer | challenger = userPlayer }
                            --                                         newDataKind = Selected newSSelected rnkId user status rankings
                            --                                         newDataState = Fetched sUsers sRankings newDataKind
                            --                                     in
                            --                                         case status of 
                            --                                             Data.Selected.Owner ->     
                            --                                                 (AppOps newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsOwner emptyTxRecord, Cmd.none)
                            --                                             Data.Selected.Member  ->
                            --                                                 (AppOps newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord, Cmd.none)
                            --                                             Data.Selected.Other ->
                            --                                                 (AppOps newDataState newAppChallengerAndPlayer SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer emptyTxRecord, Cmd.none)
                                                                            
                            --             _ ->
                            --                     (model, Cmd.none)
                                

                            -- AllEmpty ->
                            --     (model, Cmd.none)
                    
                    -- (sPlayers, "404") ->
                    --     let 
                    --         _ = Debug.log " 404" "here"
                    --     in 
                    --     case dataState of
                    --         Fetched sUsers sRankings dKind -> 
                    --             case dKind of 
                    --                     Selected sSelected ->
                    --                         (AppOps dataState user SR.Types.UIOwnerDeletedRanking emptyTxRecord, Cmd.none)
                    --                     _ ->
                    --                         (model, Cmd.none)
                    --         _ ->
                    --             (model, Cmd.none)

                    -- (sPlayers, "422") ->
                    --     let 
                    --         _ = Debug.log " 422" "here"
                    --     in 
                    --     case dataState of
                    --         Fetched sUsers sRankings dKind -> 
                    --             case dKind of 
                    --                     Selected sSelected ->
                    --                         (AppOps dataState user SR.Types.UIOwnerDeletedRanking emptyTxRecord, Cmd.none)
                    --                     _ ->
                    --                         (model, Cmd.none)
                    --         _ ->
                    --             (model, Cmd.none)

                    -- (_, _) ->
                    --     (model, Cmd.none)

        ( PlayersReceived _, Failure _ ) ->
            (model, Cmd.none)

        (ClickedDisplayGlobalOnly, AppOps dataState user uiState txRec ) ->
                    ( AppOps dataState user (SR.Types.GlobalUI SR.Types.All) emptyTxRecord, Cmd.none )


        (ClickedSelectedRanking uR, AppOps 
            (Fetched sUsers sRankings _)
                user uiState txRec ) ->
                        case uR.rankingtype of 
                            Data.Global.Owned ->
                                let
                                    newDataKind = Selected <| Data.Selected.created [] sUsers user uR.rankingInfo Data.Selected.Owner
                                    newDataState = Fetched sUsers sRankings newDataKind
                                in
                                (AppOps newDataState
                                    user (SR.Types.SelectedUI SR.Types.Owned) txRec,
                                        fetchedSingleRanking uR)
                            
                            Data.Global.Member ->
                                let
                                    newDataKind = Selected <| Data.Selected.created [] sUsers user uR.rankingInfo Data.Selected.Member
                                    newDataState = Fetched sUsers sRankings newDataKind
                                in
                                (AppOps newDataState
                                    user (SR.Types.SelectedUI SR.Types.Member) txRec,
                                        fetchedSingleRanking uR)
                            
                            Data.Global.Other ->
                                let
                                    newDataKind = Selected <| Data.Selected.created [] sUsers user uR.rankingInfo Data.Selected.Other
                                    newDataState = Fetched sUsers sRankings newDataKind
                                in
                                (AppOps newDataState
                                    user (SR.Types.SelectedUI SR.Types.Other) txRec,
                                        fetchedSingleRanking uR)


        (ClickedChangedUIStateToEnterResult player, AppOps dataState user uiState txRec)  ->
            ( AppOps dataState user SR.Types.UIEnterResult emptyTxRecord, Cmd.none )


        (SentResultToWallet result, AppOps dataState user uiState txRec)  ->
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
                    Debug.log "about to switch to " ""

                -- todo: we may need to make a change here:
                -- newDataKind = Selected (EverySet UserPlayer) Internal.Types.RankingId SelectedOwnerStatus Data.Players.Players SelectedState
                --         newDataState = Updated sUsers sRankings newDataKind
            in
            case (result) of
                (Data.Selected.Won playerUP challengerUP) ->
                --SR.Types.WalletWaitingForTransactionReceipt used to be used here
                        ( AppOps dataState user 
                        SR.Types.UIWaitingForTxReceipt { txRec | txSentry = newSentry }
                        , sentryCmd
                        )
                        
                (Data.Selected.Lost playerUP challengerUP) ->
                -- SR.Types.WalletWaitingForTransactionReceipt used to be used here                              
                        ( AppOps dataState user SR.Types.UIWaitingForTxReceipt 
                         { txRec | txSentry = newSentry }
                        , sentryCmd
                        )
                
                (Data.Selected.Undecided playerUP challengerUP) -> 
                        ( AppOps dataState user (SR.Types.SelectedUI SR.Types.ConfirmedResult) 
                         emptyTxRecord
                            , sentryCmd
                            )

                Data.Selected.NoResult ->
                    (Failure "No Result", Cmd.none)


        (ProcessResult result, AppOps dataState user uiState txRec )  ->               
            let
                _ =
                    Debug.log "process result" result
            in
            case result of
                (Data.Selected.Won playerUP challengerUP) ->
                    case dataState of
                        Updated sUsers sRankings dKind -> 
                            case dKind of 
                                Selected sSelected ->
                                    let 
                                        newDataKind = Selected (Data.Selected.handleWon sSelected playerUP challengerUP sUsers)
                                        newDataState = Updated sUsers sRankings newDataKind
                                        newModel = 
                                                AppOps newDataState user
                                                --(Data.Selected.resultView (Data.Selected.gotStatus sSelected)) 
                                                uiState
                                                txRec
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


                (Data.Selected.Lost playerUP challengerUP) ->
                    case dataState of
                        Updated sUsers sRankings dKind -> 
                            case dKind of 
                                Selected sSelected ->
                                    let 
                                        newDataKind = Selected (Data.Selected.handleLost sSelected playerUP challengerUP sUsers)
                                        newDataState = Updated sUsers sRankings newDataKind
                                        newModel = 
                                                AppOps newDataState  
                                                user 
                                                uiState
                                                txRec
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


                (Data.Selected.Undecided playerUP challengerUP) ->
                    case dataState of
                        Updated sUsers sRankings dKind -> 
                            case dKind of 
                                Selected sSelected ->
                                    let
                                        newDataKind = Selected (Data.Selected.handleUndecided sSelected playerUP challengerUP)
                                        newDataState = Updated sUsers sRankings newDataKind
                                        newModel = 
                                                AppOps newDataState user
                                                uiState
                                                --(Data.Selected.resultView (Data.Selected.gotStatus sSelected)) 
                                                txRec
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

                Data.Selected.NoResult ->
                    (Failure "No Result", Cmd.none)

        -- (ClickedCreateNewLadder, AppOps (Fetched sUsers sRankings (Global (Data.Global.Global (esUR) 
        --             (Data.Global.DisplayGlobalOnly)))) 
        --     (Data.Users.Registered userInfo userState) 
        --         uiState txRec) ->
        --             let
        --                 newDataKind = Global <| Data.Global.Global esUR
        --                     (Data.Global.CreatingNewRanking (Data.Rankings.Ranking "" True "" Nothing userInfo.id))
        --                 newDataState = Fetched sUsers sRankings newDataKind
        --                 newModel = 
        --                         AppOps newDataState (Data.Users.Registered userInfo userState)
        --                         uiState 
        --                          emptyTxRecord
        --             in
        --                 (newModel, Cmd.none)

        (ClickedCreateNewLadder, AppOps
            (Fetched sUsers sRankings (Global g))
            (Data.Users.Registered userInfo userState) 
                uiState txRec) ->
                    let
                        newg = Data.Global.createdNewUR g (Data.Users.Registered userInfo userState)
                        newDataState = Fetched sUsers sRankings (Global newg)
                        newModel = 
                                AppOps newDataState (Data.Users.Registered userInfo userState)
                                (SR.Types.GlobalUI SR.Types.NewLadderCreate) 
                                 emptyTxRecord
                    in
                        (newModel, Cmd.none)
                

        -- (Cancel, AppOps 
        --     (Fetched sUsers sRankings 
        --         (Global (Data.Global.Global (esUR) 
        --             (Data.Global.DisplayGlobalOnly))))
        --             (Data.Users.Spectator userInfo Data.Users.Updating) 
        --                 uiState txRec ) ->
        --                 let
        --                     newDataKind = Global <| Data.Global.Global esUR Data.Global.DisplayGlobalLogin
        --                     newDataState = Fetched sUsers sRankings newDataKind

        --                 in
        --                     ( AppOps 
        --                        newDataState
        --                             (Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General) 
        --                                 SR.Types.GeneralUI emptyTxRecord, Cmd.none )

        -- (Cancel, AppOps 
        --     (Fetched sUsers sRankings 
        --         (Global (Data.Global.Global (esUR) 
        --             (Data.Global.DisplayGlobalOnly))))
        --             (Data.Users.Spectator userInfo Data.Users.CreatingNew) 
        --                 uiState txRec ) ->
        --                 let
        --                     newDataKind = Global <| Data.Global.Global esUR Data.Global.DisplayGlobalLogin
        --                     newDataState = Fetched sUsers sRankings newDataKind

        --                 in
        --                     ( AppOps 
        --                        newDataState
        --                             (Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General) 
        --                                 SR.Types.GeneralUI emptyTxRecord, Cmd.none )

        -- (Cancel, AppOps 
        --     (Fetched sUsers sRankings 
        --         (Global (Data.Global.Global (esUR) 
        --             (Data.Global.DisplayGlobalLogin))))
        --             (Data.Users.Spectator userInfo Data.Users.CreatingNew) 
        --                 uiState txRec ) ->
        --                 let
        --                     newDataKind = Global <| Data.Global.Global esUR Data.Global.DisplayGlobalLogin
        --                     newDataState = Fetched sUsers sRankings newDataKind

        --                 in
        --                     ( AppOps 
        --                        newDataState
        --                             (Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General) 
        --                                 SR.Types.GeneralUI emptyTxRecord, Cmd.none )

        -- (Cancel, AppOps 
        --     (Fetched sUsers sRankings 
        --         (Global (Data.Global.Global (esUR) 
        --             (Data.Global.DisplayGlobalOnly))))
        --             (Data.Users.Spectator userInfo Data.Users.General) 
        --                 uiState txRec ) ->
        --                 let
        --                     newDataKind = Global <| Data.Global.Global esUR Data.Global.DisplayGlobalLogin
        --                     newDataState = Fetched sUsers sRankings newDataKind

        --                 in
        --                     ( AppOps 
        --                        newDataState
        --                             (Data.Users.Spectator userInfo Data.Users.General) 
        --                                 SR.Types.GeneralUI emptyTxRecord, Cmd.none )

        -- (Cancel, AppOps 
        --     (Fetched sUsers sRankings 
        --         (Global (Data.Global.Global (esUR) 
        --             (Data.Global.DisplayGlobalOnly))))
        --             user uiState txRec ) ->
        --                 let
        --                     newDataKind = Global <| Data.Global.Global esUR Data.Global.DisplayLoggedIn
        --                     newDataState = Fetched sUsers sRankings newDataKind
        --                 in
        --                     ( AppOps 
        --                        newDataState
        --                             user SR.Types.GeneralUI emptyTxRecord, Cmd.none )



        -- (Cancel, AppOps 
        --     (Fetched sUsers sRankings 
        --         (Selected (Data.Selected.Selected playerUP rnkId _ _ 
        --             Data.Selected.DisplayRanking name ))) 
        --                 user uiState txRec ) ->
        --                 let
        --                     -- rf?: currently having to re-create Global here
        --                     newDataKind = Global <| Data.Global.created sRankings sUsers user
                                                    
        --                     newDataState = Fetched sUsers sRankings newDataKind

        --                 in
        --                     ( AppOps newDataState user (SR.Types.GlobalUI SR.Types.All) emptyTxRecord, Cmd.none )

        -- (Cancel, AppOps 
        --     (Fetched sUsers sRankings 
        --         (Selected (Data.Selected.Selected playerUP rnkId selectedOwnerStatus sPlayers 
        --             Data.Selected.CreatingChallenge name))) 
        --                 user uiState txRec ) ->
        --                 let
        --                     newDataKind = Selected    <| (Data.Selected.Selected playerUP rnkId selectedOwnerStatus sPlayers Data.Selected.DisplayRanking name)   
        --                     newDataState = Fetched sUsers sRankings newDataKind
        --                 in
        --                     ( AppOps newDataState user (SR.Types.SelectedUI SR.Types.Selected) emptyTxRecord, Cmd.none )
        
        (Cancel, AppOps (Fetched sUsers sRankings dKind) user uiState txRec ) ->
            let
                newDataKind = Global (Data.Global.created sRankings sUsers user)               
                newDataState = Fetched sUsers sRankings newDataKind
            in
            ( AppOps newDataState user
                (SR.Types.GlobalUI SR.Types.All) 
                    emptyTxRecord, Cmd.none )

        (ResetToShowSelected, AppOps dataState user uiState txRec ) ->
            case dataState of 
                Fetched sUsers sRankings dKind ->
                    case dKind of
                        Selected sSelected ->
                            (model, Cmd.none)
                            --(AppOps dataState user (SR.Types.SelectedUI SR.Types.Selected) emptyTxRecord, Cmd.none )
                            
                            -- case (Data.Selected.gotStatus sSelected) of 
                            --     Data.Selected.Owner ->
                            --         (AppOps dataState user SR.Types.UISelectedRankingUserIsOwner emptyTxRecord, Cmd.none )
                            --     Data.Selected.Member ->
                            --         (AppOps dataState user SR.Types.UISelectedRankingUserIsPlayer emptyTxRecord, Cmd.none )
                            --     Data.Selected.Other ->
                            --         (AppOps dataState user SR.Types.SelectedUI SR.Types. emptyTxRecord, Cmd.none )
                        _ -> 
                            (model, Cmd.none)
                _ -> 
                    (model, Cmd.none)



        (ClickedUpdateExistingUser, AppOps dataState user uiState txRec ) ->
            -- todo: fix
            (model, Cmd.none)
            -- let 
            --     _ = Debug.log "ClickedUpdateExistingUser " walletState
            --     --newDataState = Fetched sUsers sRankings dKind
            --     --
            -- in
            -- -- if user already did an update, need to ensure we start with Fetched again
            -- case dataState of
            --     Updated sUsers sRankings dKind ->
            --         let 
            --             newDataState = Fetched sUsers sRankings user
            --         in
            --             ( AppOps newDataState appInfo SR.Types.UIUpdateExistingUser txRec, Cmd.none )
            --     _ ->
            --         ( AppOps dataState user SR.Types.UIUpdateExistingUser txRec, Cmd.none )


        (LadderNameInputChg namefield
            , AppOps ( Fetched sUsers sRankings (Global g)) user
                uiState txRec ) ->
            let
                newg = Data.Global.updateRankingName g namefield
                newDataState = Fetched sUsers sRankings (Global newg)
            in
            ( AppOps newDataState user uiState emptyTxRecord, Cmd.none )


        (LadderDescInputChg descfield
            , AppOps ( Fetched sUsers sRankings (Global g)) user
                uiState txRec ) ->
            let
                newg = Data.Global.updateRankingDesc g descfield
                newDataState = Fetched sUsers sRankings (Global newg)
            in
            ( AppOps newDataState user uiState emptyTxRecord, Cmd.none )

        (UserNameInputChg updateField, AppOps dataState 
            (Data.Users.Spectator userInfo userState) uiState txRec) ->
                (AppOps dataState (Data.Users.Spectator {userInfo | username = updateField} userState) 
                    uiState txRec, Cmd.none)

        
        (UserNameInputChg updateField, AppOps dataState 
            (Data.Users.Registered userInfo userState) uiState txRec) ->
                (AppOps dataState (Data.Users.Registered {userInfo | username = updateField} userState) uiState txRec, Cmd.none)

        
        (UserPasswordInputChg updateField, 
            AppOps dataState 
                (Data.Users.Spectator userInfo userState) uiState txRec) ->
                    (AppOps dataState (Data.Users.Spectator {userInfo | password =  updateField} userState) uiState txRec, Cmd.none)

        (UserPasswordInputChg updateField, AppOps dataState (Data.Users.Registered userInfo userState) uiState txRec) ->
            (AppOps dataState (Data.Users.Registered {userInfo | password = updateField} userState) uiState txRec, Cmd.none)

        (UserDescInputChg updateField, AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec) ->
           (AppOps dataState (Data.Users.Spectator (Data.Users.updatedDesc userInfo updateField) userState) uiState txRec, Cmd.none)

        (UserEmailInputChg updateField, AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec) ->
            (AppOps dataState (Data.Users.Spectator (Data.Users.updatedEmail userInfo updateField) userState) uiState txRec, Cmd.none)

        (UserMobileInputChg updateField, AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec) ->
            (AppOps dataState (Data.Users.Spectator (Data.Users.updatedMobile userInfo updateField) userState) uiState txRec, Cmd.none)

        -- currently if the User is not 'Registered' do nothing
        (UserMobileInputChg updateField, AppOps dataState _ uiState txRec) ->
            (model, Cmd.none)


        (ClickedConfirmedUpdateExistingUser, AppOps dataState user uiState txRec )  ->
            --todo: fix
            (model, Cmd.none)
            -- case dataState of
            --     Fetched sUsers sRankings user ->
            --         let 
            --                     _ = Debug.log "14.1" dataState
            --                     newDataState = Updated sUsers sRankings user
            --         in
            --         case user of
            --             Nothing ->
            --                 (model, Cmd.none)
            --             Just userVal ->
            --                 --( AppOps newDataState user SR.Types.UIRenderAllRankings txRec, updateExistingUser (Data.Users.asList sUsers) userVal )
            --                 ( AppOps newDataState user SR.Types.UIRenderAllRankings txRec, 
            --                 --updateExistingUser (Data.Users.asList sUsers) userVal
            --                 updateExistingUser <| Data.Users.updatedUserInSet sUsers userVal
            --                 )
                    
            --     _ -> 
            --                 let 
            --                     _ = Debug.log "14.3 - dataState" dataState
            --                 in
            --                     (model, Cmd.none)



        (SentUserInfoAndDecodedResponseToNewUser serverResponse, AppOps dataState user uiState txRec )  ->
            --(AppOps dataState user SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )
            (model, Cmd.none)


        (ClickedChallengeOpponent opponentAsUserPlayer userAsUserPlayer,
            AppOps (Fetched sUsers sRankings (Selected sSelected))
                (Data.Users.Spectator userInfo userState) uiState txRec ) ->
                (Failure "Spectator should'nt be able to challenge", Cmd.none)
        
        (ClickedChallengeOpponent opponentAsUserPlayer userAsUserPlayer,
            AppOps (Fetched sUsers sRankings (Selected sSelected))
                (Data.Users.Registered userInfo userState) uiState txRec ) ->
                    case userAsUserPlayer of 
                        Nothing ->
                            (Failure "User couldn't be found as UP!", Cmd.none)
                        
                        Just uAsUP ->
                            let
                                newDataKind = Selected (Data.Selected.assignedChallengerUIDForBOTHPlayers sSelected uAsUP opponentAsUserPlayer)
                                newDataState = Fetched sUsers sRankings newDataKind
                            in
                                case userInfo.walletState of 
                                    --not yet implemented:
                                    _ -> 
                                        (AppOps newDataState (Data.Users.Registered userInfo userState) uiState txRec, Cmd.none)


        (ClickedDeleteRanking, AppOps dataState user uiState txRec )  ->
            case dataState of 
                Fetched sUsers sRankings dKind ->
                    case dKind of 
                        Selected sSelected ->
                            case user of 
                                Data.Users.Spectator userInfo userState ->
                                    (Failure "Err", Cmd.none)

                                (Data.Users.Registered userInfo userState) ->
                                    let 
                                        newsUsers = Data.Users.updatedUserInSet sUsers user
                                            --(Data.Users.removedRankindIdFromUser (Data.Rankings.stringFromRankingId  "" appInfo.user)
                                         
                                        --removedRanking = Data.Rankings.removedById rnkId sRanking
                                        --todo: replace with a real set of rankings
                                        removedRanking = Data.Rankings.removedById (Data.Selected.gotRankingId sSelected) Data.Rankings.empty
                                        newDataKind = Global (Data.Global.created removedRanking sUsers user)
                                        
                                        newDataState = Updated newsUsers sRankings newDataKind
                                        _ = Debug.log "ranking should have been removed from rankings" removedRanking
                                    in
                                        ( AppOps
                                            newDataState
                                            user
                                            SR.Types.UIDeleteRankingConfirm
                                            
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
        
        (ClickedDeleteRankingConfirmed, AppOps dataState user uiState txRec )  ->
            case dataState of 
                Updated sUsers sRankings dKind ->
                    case dKind of 
                        Selected sSelected ->
                             ( AppOps
                                    dataState
                                    user
                                    uiState
                                    
                                    txRec
                                , 
                                    --httpDeleteSelectedRankingFromJsonBin (Data.Rankings.stringFromRankingId  ""
                                    -- todo: fix for fauna
                                    Cmd.none
                            )
                        
                        Global sGlobal  ->
                           (Failure "Selected?", Cmd.none)
                _ ->
                    ( model, Cmd.none )


        (ReturnedFromDeletedSelectedRankingFromJsonBin result, AppOps dataState user uiState txRec )  ->
            -- nb. you haven't used the result
            let 
                _= Debug.log "result"  result
            in
        
            case dataState of
                Updated sUsers sRankings dKind ->
                    case dKind of 
                        Global sGlobal  ->
                            let
                                -- disabled due to refacroring
                                --newGlobal = Data.Global.removedUserRankingByRankingId sGlobal rnkId
                                newDataKind = Global Data.Global.empty
                                newDataState = Updated sUsers sRankings newDataKind           
                            in 
                                ( AppOps
                                    newDataState
                                    user
                                    uiState
                                    
                                    txRec
                                --, httpDeleteSelectedRankingFromGlobalList newGlobal
                                , Cmd.none
                                )
                        _ ->
                            ( model, Cmd.none )
                _ ->
                    ( model, Cmd.none )


        (ReturnedFromDeletedRankingFromGlobalList response, AppOps dataState user uiState txRec )  ->
            (model, Cmd.none)
            -- todo: fix for fauna
            -- let 
            --     _ = Debug.log "Result response " response
            -- in
            -- case (Data.Rankings.handleServerDeletedRanking response) of
            --     (sRanking, "Success") ->
            --         case dataState of 
            --             Updated sUsers sRankings dKind ->
            --                 case dKind of
            --                     Global sGlobal  ->
            --                                 let
            --                                     newDataKind = Global sGlobal 
            --                                     newDataState = Fetched sUsers sRankings newDataKind
                                                
            --                                     _ = Debug.log "Ranking removed on return from list updated? " Data.Global.asList sGlobal
                                                
            --                                 in
            --                                     ( AppOps walletState newDataState user SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )
                                        
            --                     _ -> 
            --                                 let 
            --                                     _ = Debug.log "11 - dataState should be Global" dataState
            --                                 in
            --                                     (model, Cmd.none)
            --             _ -> 
            --                 (model, Cmd.none)

            --     (sRanking, "404") ->
            --             case dataState of
            --                 Updated sUsers sRankings dKind -> 
            --                     case dKind of 
            --                             Global sGlobal  ->
            --                                 (AppOps dataState user SR.Types.UIUnableToFindGlobalRankings emptyTxRecord, Cmd.none)
            --                             _ ->
            --                                 (model, Cmd.none)
            --                 _ ->
            --                     (model, Cmd.none)
                
            --     -- todo: add more error conditions
            --     (_, _) ->
            --         (model, Cmd.none)    
                

        (WatchTxHash (Ok txHash), AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
                    -- let
                    --     _ =
                    --         Debug.log "WatchTxHash in wallet operational " "Ok - hash watched and all ok"
                    -- in
                    -- case walletState of 
                    --     SR.Types.WalletOperational -> 
                    --         ( AppOps dataState user SR.Types.UIRenderAllRankings  { txRec | txHash = Just txHash }, Cmd.none )
                    --     _ ->
                    --         (model, Cmd.none)

        (WatchTxHash (Err err),  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- let
            --     _ =
            --         Debug.log "WatchTxHash" "Err"
            -- in
            --     case walletState of 
            --         SR.Types.WalletOperational -> 
            --             ( AppOps SR.Types.WalletStateMissing dataState user SR.Types.UIRenderAllRankings  { txRec | errors = ("Error Retrieving TxHash: " ++ err) :: txRec.errors }, Cmd.none )
            --         _ -> 
            --             (model, Cmd.none)
        
        
        (WatchTx (Ok tx),  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- let
            --     _ =
            --         Debug.log "WatchTx" "tx Ok"
            -- in
            -- case (walletState, dataState) of
            --     (SR.Types.WalletOperational, 
            --         Fetched  sUsers sRankings 
            --             (Selected (Data.Selected.Selected esUP rnkId PlayerStatus sPlayers (Data.Selected.EnteredResult resultOfMatch)))) -> 
            --                 AppOps dataState user SR.Types.UIRenderAllRankings  { txRec | tx = Just tx } |> update (ProcessResult resultOfMatch)
                    
            --     (SR.Types.WalletOperational, _ ) ->
            --         (Failure "WatchTxReceipt", Cmd.none)
                
            --     (_, _) -> 
            --         (Failure "WatchTxReceipt", Cmd.none)

        (WatchTx (Err err),  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- let
            --     _ =
            --         Debug.log "WatchTx tx err" err
            -- in
            -- case walletState of 
            --     SR.Types.WalletOperational ->
            --         ( AppOps SR.Types.WalletStateLocked dataState user SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving Tx: " ++ err) :: txRec.errors }, Cmd.none )
            --     _ -> 
            --         (model, Cmd.none)
        
        
        (WatchTxReceipt (Ok txReceipt),  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- let
            --     _ =
            --         Debug.log "handleWalletStateOpenedAndOperational Receipt" txReceipt
            -- in
            -- case (walletState, dataState) of 
            --     -- n.b. you might need dataState to be updated here:
            --     (SR.Types.WalletOperational, 
            --         Fetched  sUsers sRankings 
            --             (Selected (Data.Selected.Selected esUP rnkId PlayerStatus sPlayers (Data.Selected.EnteredResult resultOfMatch)))) ->
            --             AppOps dataState user SR.Types.UIRenderAllRankings  emptyTxRecord
            --                 |> update (ProcessResult resultOfMatch )

            --     (SR.Types.WalletOperational, _ ) ->
            --         (Failure "WatchTxReceipt", Cmd.none)
                
            --     (_, _) -> 
            --         (Failure "WatchTxReceipt", Cmd.none)

        (WatchTxReceipt (Err err),  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- let
            --     _ =
            --         Debug.log "tx err" err
            -- in
            -- case walletState of 
            --     SR.Types.WalletOperational ->
            --         ( AppOps dataState user SR.Types.UIWaitingForTxReceipt { txRec | errors = ("Error Retrieving TxReceipt: " ++ err) :: txRec.errors }, Cmd.none )
            --     _ ->
            --         (model, Cmd.none)
        
        (TrackTx blockDepth,  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- let
            --     _ =
            --         Debug.log "TrackTx" "TrackTx"
            -- in
            -- case walletState of 
            --     SR.Types.WalletOperational ->
            --         ( AppOps dataState user SR.Types.UIWaitingForTxReceipt { txRec | blockDepth = Just blockDepth }, Cmd.none )
            --     _ -> 
            --         (model, Cmd.none)

        -- this is the response from addedUserAsFirstPlayerInNewList Cmd
        -- it had the Http.expectStringResponse in it
        -- it's already created the new ranking with current player as the first entry
        -- the result now is the ranking id only at this point which was pulled out by the decoder
        (SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder,  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- case walletState of 
            --     SR.Types.WalletOperational ->
            --         case dataState of 
            --                 Fetched sUsers sRankings dKind ->
            --                     case dKind of 
            --                         Global sGlobal  ->
            --                             case user of
            --                             -- todo: fix
            --                                 Data.Users.Spectator userInfo userState ->
            --                                     (model, Cmd.none)
            --                                 (Data.Users.Registered userInfo userState) ->
            --                                     --below just here for ref ... it never worked like this:
            --                                     Nothing ->
            --                                     (model, Cmd.none)
            --                                     Just userVal ->
            --                                         let
            --                                             extractedRankingId = Data.Global.gotNewRankingIdFromWebData idValueFromDecoder
            --                                             newSGlobal = Data.Global.addUserRanking sGlobal extractedRankingId appInfo.selectedRanking userVal
            --                                             newGlobalAsList = Data.Global.rankingsAsList newSGlobal
            --                                             newGlobalUpdated = Global newSGlobal
            --                                             newDataState = Updated sUsers sRankings newGlobalUpdated
            --                                         in
            --                                             ( AppOps SR.Types.WalletOperational newDataState appInfo SR.Types.UICreateNewLadder  emptyTxRecord
            --                                             ,
            --                                             httpPutRequestForAddGlobal (Data.Global.newJsonEncodedList (newGlobalAsList)) newGlobalAsList
            --                                             )
            --                                 (Data.Users.NoWallet userInfo userState) ->
            --                                     (model, Cmd.none)
            --                                 (Data.Users.NoCredit userInfo userState) ->
            --                                     (model, Cmd.none)
            --                                 (Data.Users.Credited userInfo userState) ->
            --                                     (model, Cmd.none)
                                            
            --                         _ -> 
            --                             let 
            --                                 _ = Debug.log "dataState should be Global" dataState
            --                             in
            --                                 (model, Cmd.none)

            --                 Updated sUsers sRankings dKind ->
            --                     case dKind of 
            --                         Global sGlobal  -> 
            --                             let 
            --                                 _ = Debug.log "6 - dataState SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId" dataState
            --                             in
            --                                 (AppOps SR.Types.WalletOpened dataState user SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none)
            --                         _ -> 
            --                             (model, Cmd.none)

            --                 AllEmpty ->
            --                     (model, Cmd.none)
                
            --     _ -> 
            --         (model, Cmd.none)

        -- (ResetRejectedNewUserToShowGlobal,  AppOps dataState user uiState txRec ) ->
        --     let 
        --         newAppInfo = {appInfo | user = Data.Users.Spectator}
        --     in 
        --     case walletState of 
        --         SR.Types.WalletOperational ->
        --             ( AppOps SR.Types.WalletOperational dataState newAppInfo SR.Types.UIRenderAllRankings emptyTxRecord, allRankings )
        --         _ -> 
        --             (model, Cmd.none)


        (ClickedConfirmCreateNewRanking _,  AppOps _
            (Data.Users.Spectator userInfo userState) 
                _ _ ) ->
                (Failure "Cannot create a ladder as a spectator", Cmd.none)


        (ClickedConfirmCreateNewRanking ranking,  AppOps (Fetched sUsers sRankings dKind)
            --(Global (Data.Global.Global esUserRanking (Data.Global.CreatingNewRanking ranking))))
            (Data.Users.Registered userInfo userState) 
                (SR.Types.GlobalUI (SR.Types.NewLadderCreate)) txRec ) ->
                case userInfo.walletState of 
                    Data.Users.Credited _ ->
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
                        
                            newesRankings = Data.Rankings.addRanking ranking sRankings 
                            newg = Data.Global.created newesRankings sUsers (Data.Users.Registered userInfo userState)
                            newDataState = Fetched sUsers newesRankings (Global newg)
                        in
                            ( AppOps newDataState (Data.Users.Registered userInfo 
                            --Data.Users.WalletWaitingForTransactionReceipt
                            Data.Users.Subscribe
                            ) 
                                (SR.Types.GlobalUI SR.Types.NewLadderConfirmed) { txRec | txSentry = newSentry }
                                    ,sentryCmd)
                
                -- this may change according to the business logic
                    _ ->
                        let
                            newesRankings = Data.Rankings.addRanking ranking sRankings 
                            newg = Data.Global.created newesRankings sUsers (Data.Users.Registered userInfo userState)
                            newDataState = Fetched sUsers newesRankings (Global newg)
                        in
                            ( AppOps newDataState (Data.Users.Registered userInfo userState)
                                (SR.Types.GlobalUI SR.Types.NewLadderConfirmed) txRec
                                    , createNewRanking ranking)

        

        (AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList,  AppOps dataState user uiState txRec ) ->
            -- I think the global set has already been updated
            -- case walletState of 
            --     SR.Types.WalletOperational ->
            --         ( AppOps SR.Types.WalletOperational dataState user SR.Types.UIRenderAllRankings emptyTxRecord, Cmd.none )
            --     _ ->
                    (model, Cmd.none)
            
        (ClickedNewChallengeConfirm challengerUID,  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- case user of 
            --     Data.Users.Spectator _ _ ->
            --         (model, Cmd.none)

            --     _ ->
            --         case dataState of
            --             Fetched sUsers sRankings dKind -> 
            --                 case dKind of 
            --                     Selected sSelected ->
            --                         let
            --                             newDataKind = Selected (Data.Selected.assignedChallengerUIDForBOTHPlayers sSelected user challengerUID)
            --                             newDataState = Updated sUsers sRankings newDataKind
            --                             updatedModel = AppOps walletState newDataState user SR.Types.UIRenderAllRankings txRec
            --                         in 
            --                             ( updatedModel, httpPlayerList (newDataState))

            --                     _ -> 
            --                         let 
            --                             _ = Debug.log "7.1 - dataState shuld be Selected" dataState
            --                         in
            --                             (model, Cmd.none)
            --             _ -> 
            --                         let 
            --                             _ = Debug.log "7.1 - dataState" dataState
            --                         in
            --                             (model, Cmd.none)


        (ClickedJoinSelected,  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
        -- todo: re-implement when fauna ready
            -- let 
            --     --_ = Debug.log "ClickedJoinSelected" "here"
            --     _ = Debug.log "walletstatein ClickedJoinSelected" walletState
            -- in
            -- case walletState of 
            --     SR.Types.WalletOpened ->
            --         case dataState of
            --             Fetched sUsers sRankings dKind -> 
            --                 case dKind of 
            --                     Selected sSelected ->
            --                         case accountState of
            --                             Data.Users.Spectator userInfo userState -> 
            --                                 ( AppOps dataState user SR.Types.UIRegisterNewUser txRec, Cmd.none )
            --                             Data.Users.Registered ->
            --                                 ( AppOps dataState user SR.Types.UIEthAlreadyEnabled txRec, Cmd.none )

            --                             SR.Types.EthEnabled ->
            --                                 ( AppOps dataState user SR.Types.UIRegisterNewUser txRec, Cmd.none )

            --                             SR.Types.EthEnabledAndRegistered ->
            --                                 case user of
            --                                     Nothing ->
            --                                         (model, Cmd.none)
            --                                     Just userVal ->
            --                                         let
            --                                             newLUPlayer = Data.Selected.userAdded sUsers appInfo.selectedRanking.id_ (Data.Selected.asList sSelected) userVal
            --                                             newSelected = Data.Selected.asSelected (EverySet.fromList newLUPlayer)
                                                        
            --                                             newDataKind = Selected newSelected rnkId user Data.Selected.Member Data.Players.empty
            --                                             newDataState = Updated sUsers sRankings newDataKind
            --                                             updatedModel = AppOps walletState newDataState user SR.Types.UIRenderAllRankings txRec
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
                --     ( AppOps dataState user SR.Types.UIEnableEthereum txRec, Cmd.none )

                -- SR.Types.WalletStateLocked ->
                --     ( AppOps dataState user SR.Types.UIEnableEthereum txRec, Cmd.none )

                -- SR.Types.WalletOpenedNoUserAccount ->
                --     ( AppOps dataState user SR.Types.UIRegisterNewUser txRec, Cmd.none )

                -- _ -> 
                --     let 
                --         _ = Debug.log "walletState in ClickedJoinSelected : " walletState
                --     in
                --     (model, Cmd.none)

        (ReturnFromPlayerListUpdate response, AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- let
            --     _ = Debug.log "ReturnFromPlayerListUpdate" walletState
            -- in
            -- case walletState of 
            --     SR.Types.WalletOpened ->
            --         case dataState of 
            --             Updated sUsers sRankings dKind ->
            --                 case dKind of 
            --                     Selected sSelected ->  
            --                         let
            --                             lplayer =
            --                                 Data.Players.extractPlayersFromWebData response

            --                             --addedNewJoinedRankingId : String -> Data.Users.User -> List Data.Users.User -> List Data.Users.User
            --                             --newUserList = Data.Users.addedNewJoinedRankingId (Data.Rankings.stringFromRankingId rnkId) user (Data.Users.asList sUsers)
            --                             newUserList = Data.Users.asList (Data.Selected.asUsers sSelected)

            --                             convertedToUserPlayers =
            --                                 Data.Selected.convertPlayersToUserPlayers
            --                                     lplayer
            --                                     --(Data.Users.asList sUsers)
            --                                     newUserList

            --                             _ = Debug.log "ReturnFromPlayerListUpdate fetched selected" convertedToUserPlayers

            --                             --httpUpdateUsersJoinRankings is the http cmd that we need to do here
                                        
            --                         in
            --                             case user of
            --                             -- todo: fix
            --                                 Data.Users.Spectator userInfo userState ->
            --                                     (model, Cmd.none)
            --                                 (Data.Users.Registered userInfo userState) ->
            --                                      ( updateSelectedRankingPlayerList model convertedToUserPlayers
            --                                      -- todo: fix:
            --                                     , httpUpdateUsersJoinRankings "" user newUserList)
            --                                     --(Data.Rankings.stringListToRankingIdList  "" 
                                                
                                
            --                                 (Data.Users.NoWallet userInfo userState) ->
            --                                     (model, Cmd.none)
            --                                 (Data.Users.NoCredit userInfo userState) ->
            --                                     (model, Cmd.none)
            --                                 (Data.Users.Credited userInfo userState) ->
            --                                     (model, Cmd.none)
            --                     _ -> 
            --                         let 
            --                             _ = Debug.log "13 - dataState should be Selected" dataState
            --                         in
            --                             (model, Cmd.none)
            --             _ -> 
            --                 let 
            --                     _ = Debug.log "13 - dataState" dataState
            --                 in
            --                     (model, Cmd.none)
            --     _ -> 
            --         (model, Cmd.none)

            

        (ReturnFromUserListUpdate response,  AppOps dataState user uiState txRec ) ->
            (model, Cmd.none)
            -- let 
            --     _ =
            --         Debug.log "ReturnFromUserListUpdate" walletState
            -- in
            -- case walletState of 
            --     SR.Types.WalletOpened ->
            --         case dataState of 
            --             Fetched sUsers sRankings dKind ->
            --                 case dKind of 
            --                     Global sGlobal  ->
            --                         let 
            --                             lusers = Data.Users.extractUsersFromWebData response
            --                             newGlobal = Data.Global.created (Data.Global.asRankings sGlobal) (Data.Users.asUsers (EverySet.fromList lusers))
            --                             newDataKind = Global newGlobal
            --                             newDataState = Fetched (Data.Users.asUsers (EverySet.fromList lusers)) sRankings newDataKind
            --                         in
            --                         (AppOps walletState newDataState user SR.Types.UIRenderAllRankings txRec, Cmd.none)
            --                     _ -> 
            --                         (model, Cmd.none)
                        
            --             Updated sUsers sRankings dKind ->
            --                 case dKind of 
            --                         Global sGlobal  ->
            --                             let 
            --                                 lusers = Data.Users.extractUsersFromWebData response
            --                                 newGlobal = Data.Global.created (Data.Global.asRankings sGlobal) (Data.Users.asUsers (EverySet.fromList lusers))
            --                                 newDataKind = Global newGlobal
            --                                 newDataState = Updated (Data.Users.asUsers (EverySet.fromList lusers)) sRankings newDataKind
            --                             in
            --                             (AppOps walletState newDataState user SR.Types.UIRenderAllRankings txRec, Cmd.none)
            --                         _ -> 
            --                             (model, Cmd.none)
            --             AllEmpty ->
            --             --_ ->
            --                 (model, Cmd.none)

                -- _ ->
                --     (model, Cmd.none)

        (TimeUpdated posixTime,  AppOps dataState user uiState txRec ) ->
            let
                _ =
                    Debug.log "posixtime" posixTime
            in
            ( model, Cmd.none )
        

        -- TxSentryMsg updates when user clicks 'Confirm' in the wallet
        (TxSentryMsg subMsg,  AppOps dataState user uiState txRec ) ->
            let
                _ =
                    Debug.log "handleTxSubMsg subMsg" <| handleTxSubMsg subMsg
            
                ( subModel, subCmd ) =
                    Eth.Sentry.Tx.update subMsg txRec.txSentry
            in
            if handleTxSubMsg subMsg then
                    (Failure "uncomment below to fix (user walletStatus filed change)", Cmd.none)
                -- case (user, dataState) of 
                --     (Data.Users.Credited userInfo userState
                --         , Fetched sUsers sRankings
                --         --nb. 'Selected' is a variant defined in Main (just a box or label), it is NOT a Set
                --         -- you're only specifying it to distinguish from Global as a dKind
                --         -- sSelected is the variable you're pattern matching on here
                --             (Selected  (Data.Selected.Selected sSelected rnkId PlayerStatus sPlayers (Data.Selected.EnteredResult resultEntered) name ))) ->
                --             case resultEntered of 
                --                 Data.Selected.Won _ _ ->
                --                     (AppOps dataState user SR.Types.UIWaitingForTxReceipt 
                --                      { txRec | txSentry = subModel } |> update (ProcessResult resultEntered) )

                --                 Data.Selected.Lost _ _ ->
                --                     -- wallet was operational here
                --                     (AppOps dataState user SR.Types.UIWaitingForTxReceipt 
                --                      { txRec | txSentry = subModel } |> update (ProcessResult resultEntered))
                                    
                --                 Data.Selected.Undecided _ _ ->
                --                     -- wallet was operational here
                --                     ( AppOps dataState user 
                --                     SR.Types.UIWaitingForTxReceipt 
                --                      { txRec | txSentry = subModel } |> update (ProcessResult resultEntered))
                                
                --                 Data.Selected.NoResult ->
                --                     (Failure "Tx problem Should have been a result", Cmd.none)
                        
                --     (Data.Users.Credited userInfo Data.Users.Updating, Fetched sUsers sRankings dKind ) ->
                --         ( AppOps dataState (Data.Users.Credited userInfo Data.Users.WalletOperational) 
                --         SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }
                --         , Cmd.batch [subCmd,  createNewUser sUsers (Data.Users.Credited userInfo Data.Users.WalletOperational)])
                    
                --     -- (Data.Users.Credited userInfo Data.Users.Updating, Fetched sUsers sRankings dKind ) ->
                --     --     ( AppOps SR.Types.WalletOperational dataState user SR.Types.UIWaitingForTxReceipt { txRec | txSentry = subModel }
                --     --     , Cmd.batch [subCmd, addedUserAsFirstPlayerInNewList user] )
            
                --     (_, _) ->
                --         (Failure "No credit", Cmd.none)
            
            else
                ( AppOps dataState user (SR.Types.GlobalUI SR.Types.All) txRec, Cmd.none )


                
        (ClickedLogInUser, model_) ->
            case model of 
                AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec ->
                    (model, loginUser userInfo.username userInfo.password)
                AppOps dataState _ uiState txRec ->
                    (Failure "Only Spectator can login", Cmd.none)
                    
                Failure _ ->
                    (model, Cmd.none)
        

        (InitiallyLoggedInUser response, modelReDef) ->
            ( loginResponse modelReDef response
               , Cmd.none 
            )

        (LoggedInUser response, modelReDef) ->
            ( registeredResponse modelReDef response
               , commandFromLoggedInUser response 
            )
            
        (RegisteredNewUser response, modelReDef) ->
            (
                loginResponse modelReDef response
               , Cmd.none 
            )

        (ReceivedUsers response, modelReDef) ->
            ( updateWithReceivedUsers modelReDef response
            , Cmd.none
            )

        (ReceivedRankings response, modelReDef) ->
            ( updateWithReceivedRankings modelReDef response
            , Cmd.none
            )

        (ReceivedPlayersByRankingId response, modelReDef) ->
            ( updateWithReceivedPlayersByRankingId modelReDef response
            , Cmd.none
            )

        (CreatedGlobal, _) ->
            (updateGlobal model, Cmd.none)

        (ReceivedUserNames response, modelReDef) ->
            ( receivedUserNamesFaunaTest modelReDef response
            , Cmd.none
            )

            
        (CreatedNewRanking response, modelReDef) ->
            ( createNewRankingResponse modelReDef response
               , Cmd.none 
            )
        
        (NoOp, _) ->
            let
                _ =
                    Debug.log "handledWalletStateOpened no op" msg
            in
            ( model, Cmd.none )


        (_, _) ->
            ( Failure "fell through update, use debugger to find/create the state pattern"
            , Cmd.none
            )

-- model handlers
handleClickedRegister : Data.Users.User -> Data.Users.User
handleClickedRegister user = 
    case user of 
        Data.Users.Spectator userInfo _ ->
            Data.Users.Spectator userInfo Data.Users.Updating 
        _ ->
            user
        

-- GQL commands

loginUser : String -> String -> Cmd Msg
loginUser user_name password =
    GQLHttp.send InitiallyLoggedInUser (Bridge.requestLoginUser user_name password)
    --Cmd.none

fetchedSingleRanking : Data.Global.UserRanking -> Cmd Msg
fetchedSingleRanking uR =
    GQLHttp.send ReceivedPlayersByRankingId (Bridge.requestgotPlayersByRankingId (Data.Global.gotRankingId uR))
       

registerUser : Data.Users.UserInfo -> Cmd Msg
registerUser userInfo =
    GQLHttp.send RegisteredNewUser (Bridge.requestCreateNewUser  userInfo )

createNewRanking : Data.Rankings.Ranking -> Cmd Msg
createNewRanking ranking =
    GQLHttp.send CreatedNewRanking (Bridge.requestCreateNewRanking  ranking )

commandFromLoggedInUser : Result (GQLHttp.Error (String)) (Data.Users.Token) -> Cmd Msg
commandFromLoggedInUser response =
    case response of
        Ok token ->
            allUserNames token

        Err _ ->
            Cmd.none


allUserNames : Data.Users.Token -> Cmd Msg
allUserNames token =
    GQLHttp.send ReceivedUserNames (Bridge.requestAllUserNames token)

allUsers : Cmd Msg
allUsers  =
    GQLHttp.send ReceivedUsers (Bridge.requestAllUsers)

allRankings : Cmd Msg
allRankings  =
    GQLHttp.send ReceivedRankings (Bridge.requestAllRankings)

       
-- model handlers

updateGlobal : Model -> Model 
updateGlobal model = 
    case model of 
        AppOps dataState user uiState txRec ->
            case dataState of 
                AllEmpty ->
                    model
                Fetched sUsers sRankings dKind ->
                    case dKind of 
                        Global _ ->
                            let
                             
                                newDataKind = Global (Data.Global.created sRankings sUsers user)
                                newDataState = Updated sUsers sRankings newDataKind
                            in
                                AppOps newDataState user uiState txRec

                        Selected _ ->
                            Failure "updateGlobal"

                Updated _ _ _ ->
                    model

        Failure str ->
            Failure "updateGlobal"

-- todo: change the name here as it's not actually updating the model - just testing faunadb
receivedUserNamesFaunaTest : Model -> Result (GQLHttp.Error (List String)) (List String) -> Model
receivedUserNamesFaunaTest model response =
    case response of
        Ok lusernames ->
                model

        Err _ ->
            model


updateWithReceivedUsers : Model -> Result (GQLHttp.Error (Maybe (List (Maybe Data.Users.FUser)))) (Maybe (List (Maybe Data.Users.FUser))) -> Model
updateWithReceivedUsers model response =
    case (model, response) of -- AllEmpty, so fill the User set
        (AppOps AllEmpty user uiState txRec, Ok lusers)  ->
                    let
                        filteredFUserList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lusers)
                        -- need to convert from FRanking to Ranking (id_ needs to be a String)
                        lFromFToUser = List.map Data.Users.convertFUserToUser filteredFUserList
                        --_ = Debug.log "lFromFToUser : " lFromFToUser
                        sUsers = Data.Users.asUsers (EverySet.fromList lFromFToUser)
                        newDataState = Fetched sUsers Data.Rankings.empty (Global Data.Global.empty)
                        
                    in
                        AppOps newDataState user uiState txRec
        
        (AppOps (Fetched sUsers sRankings  (Global _)) user uiState txRec, Ok lusers) ->
                if Data.Rankings.isEmpty sRankings then -- just fill the User set
                    let
                        filteredFUserList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lusers)
                        lFromFToUser = List.map Data.Users.convertFUserToUser filteredFUserList
                        newsUsers = Data.Users.asUsers (EverySet.fromList lFromFToUser)
                        newDataState = Fetched sUsers sRankings (Global Data.Global.empty)
                    in
                        AppOps newDataState user uiState txRec

                else --if sRankings isn't empty we can populate Global now
                    let
                        
                        filteredFUserList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lusers)
                        --lFromFToUser = List.map Data.Users.convertFUserToUser filteredFUserList
                        lFromFToUser = List.map Data.Users.convertFUserToUser filteredFUserList
                        newsUsers = Data.Users.asUsers (EverySet.fromList lFromFToUser)
                        
                        newDataKind = Global (Data.Global.created sRankings newsUsers user)
                        
                        newDataState = Fetched newsUsers sRankings newDataKind
                        
                    in
                        AppOps newDataState user (SR.Types.GeneralUI SR.Types.LogIn) txRec


        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Ok lusers) ->
            model

        ( AppOps (Fetched _ _ (Selected _)) _ _ _ , Ok _ ) ->
            (Failure "updateWithReceivedUsers8")

        (AppOps AllEmpty user uiState txRec, Err _ )  ->
            (Failure "Unable to obtain User data. \nPlease check your network connection ...")

        (AppOps (Fetched sUsers sRankings dKind) user uiState txRec, Err _)  ->
            (Failure "Network problem ... please re-load or change your location2")

        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Err _ ) ->
            (Failure "updateWithReceivedUsers11")

        (Failure _, Ok lusers) ->
            (Failure "Network problem ... please re-load or change your location3")

        (Failure _, Err str) ->
            (Failure "Unable to obtain User data. \nPlease check your network connection ...")

updateWithReceivedPlayersByRankingId : Model -> 
    Result ((GQLHttp.Error (Maybe (List (Maybe Data.Players.FPlayer)))))
         (Maybe (List (Maybe Data.Players.FPlayer))) -> Model
updateWithReceivedPlayersByRankingId model response =
    case (model, response) of
        ( AppOps (Fetched sUsers sRankings 
            (Selected s))
                user uiState txRec
                , Ok lplayers ) ->
                    let
                        -- filterMap?:
                        filteredFPlayerList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lplayers)
                        lFromFToPlayer = List.map Data.Players.convertPlayerFromFPlayer filteredFPlayerList
                        -- todo: change createdSelected to accept a Set instead of a list
                        --newsSelected = Data.Selected.created lFromFToPlayer sUsers  (Data.Selected.gotUP s) (Data.Selected.gotRanking s)
                        newsSelected = Data.Selected.created lFromFToPlayer sUsers user (Data.Selected.gotRanking s) (Data.Selected.gotStatus s)
                        newDataKind = Selected newsSelected
                        newDataState = Fetched sUsers sRankings newDataKind
                    in
                        AppOps newDataState user uiState txRec

        (AppOps AllEmpty user uiState txRec, Ok lplayers)  ->
            (Failure "updateWithReceivedPlayersByRankingId10")

        (AppOps (Fetched sUsers  sRankings  (Global _ )) user uiState txRec, Ok lplayers) ->
            -- this only happens when the data comes in after a cancel, just return the model
            model

        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Ok lplayers) ->
            (Failure "updateWithReceivedPlayersByRankingId13")

        (AppOps AllEmpty user uiState txRec, Err _ )  ->
            (Failure "Unable to obtain Rankings data. Please check your network connection ...")

        -- nodb:
        (AppOps (Fetched sUsers sRankings (Selected s)) user uiState txRec, Err _)  ->
            --(Failure "updateWithReceivedPlayersByRankingId15")
            let
                        -- filteredFPlayerList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lplayers)
                        -- lFromFToPlayer = List.map Data.Players.convertPlayerFromFPlayer filteredFPlayerList
                        -- todo: change createdSelected to accept a Set instead of a list

                        --newsSelected = Data.Selected.created lFromFToPlayer sUsers user (Data.Selected.gotRanking s) playerStatus
                        --nodb (replacing line above):
                        newsSelected = Data.Selected.created Data.Players.dummyPlayers sUsers user (Data.Selected.gotRanking s) (Data.Selected.gotStatus s)
                        newDataKind = Selected newsSelected
                        newDataState = Fetched sUsers sRankings newDataKind
                    in
                        AppOps newDataState user uiState txRec

        ( AppOps (Fetched _ _ (Global _)) _ _ _, Err _ ) ->
            (Failure "should be selected here")

        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Err _ ) ->
            (Failure "updateWithReceivedPlayersByRankingId16")

        (Failure _, Ok lusers) ->
            (Failure "updateWithReceivedPlayersByRankingId17")

        (Failure _, Err _) ->
            (Failure "updateWithReceivedPlayersByRankingId18")


updateWithReceivedRankings : Model -> Result (GQLHttp.Error (Maybe (List (Maybe Data.Rankings.FRanking)))) (Maybe (List (Maybe Data.Rankings.FRanking))) -> Model
updateWithReceivedRankings model response =
     case (model, response) of -- AllEmpty, so fill the Ranking set
        (AppOps AllEmpty user uiState txRec, Ok lrankings)  ->
            let
                filteredFRankingList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lrankings)
                -- need to convert from FRanking to Ranking (id_ needs to be a String)
                lFromFToRanking = List.map Data.Rankings.convertFRankingToRanking filteredFRankingList
                --_ = Debug.log "lFromFToRanking : " lFromFToRanking
                --newsRankings = Data.Rankings.asRankings (EverySet.fromList lFromFToRanking)
                -- nodb:
                newsRankings = Data.Rankings.asRankings (EverySet.fromList Data.Rankings.testRankings)
                newDataState = Fetched Data.Users.empty newsRankings (Global Data.Global.empty)
                
            in
                AppOps newDataState user uiState txRec

        (AppOps (Fetched sUsers sRankings  (Global _)) user uiState txRec, Ok lrankings) ->
                if Data.Users.isEmpty sUsers then -- just fill the Ranking set
                    let
                        filteredFRankingList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lrankings)
                        lFromFToRanking = List.map Data.Rankings.convertFRankingToRanking filteredFRankingList
                        --newsRankings = Data.Rankings.asRankings (EverySet.fromList lFromFToRanking)
                        newsRankings = Data.Rankings.asRankings (EverySet.fromList Data.Rankings.testRankings)
                        newDataState = Fetched sUsers  sRankings (Global Data.Global.empty)
                    in
                        AppOps newDataState user uiState txRec

                else --if sUsers isn't empty we can populate Global now
                    let
                        filteredFRankingList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lrankings)
                        lFromFToRanking = List.map Data.Rankings.convertFRankingToRanking filteredFRankingList
                        --newsRankings = Data.Rankings.asRankings (EverySet.fromList lFromFToRanking)
                        newsRankings = Data.Rankings.asRankings (EverySet.fromList Data.Rankings.testRankings)
                        newDataKind = Global (Data.Global.created newsRankings sUsers user)
                        newDataState = Fetched sUsers newsRankings newDataKind
                    in
                        AppOps newDataState user uiState txRec

        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Ok lrankings) ->
            model

        ( AppOps (Fetched _ _ (Selected _)) _ _ _ , Ok _ ) ->
            (Failure "updateWithReceivedRankings13")

        (AppOps AllEmpty user uiState txRec, Err _ )  ->
            --(Failure "Unable to obtain Rankings data. Please check your network connection ...")
            -- todo: fix put back above - just while sorting fauna relations issues
            let
                newsRankings = Data.Rankings.asRankings (EverySet.fromList Data.Rankings.testRankings)
                newDataKind = Global (Data.Global.created newsRankings Data.Users.empty user)
                newDataState = Fetched Data.Users.empty  newsRankings newDataKind
            in
                AppOps newDataState user uiState txRec

        (AppOps (Fetched sUsers sRankings dKind) user uiState txRec, Err _)  ->
            --(Failure "Essential data missing. Please check network and re-try")
            let
                newsRankings = Data.Rankings.asRankings (EverySet.fromList Data.Rankings.testRankings)
                newDataKind = Global (Data.Global.created newsRankings Data.Users.empty Data.Users.emptyUser)
                newDataState = Fetched Data.Users.empty newsRankings newDataKind
            in
                -- AppOps newDataState Data.Users.emptyUser (SR.Types.GeneralUI SR.Types.Loading) emptyTxRecord
                --(Failure "Unable to obtain Rankings data. Please check your network connection ...")
            -- todo: fix put back above - just while sorting fauna relations issues
                AppOps newDataState Data.Users.emptyUser (SR.Types.GeneralUI SR.Types.LogIn) emptyTxRecord

        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Err _ ) ->
            (Failure "updateWithReceivedRankings16")

        (Failure _, Ok lusers) ->
            (Failure "updateWithReceivedRankings17.1")

        (Failure _, Err _) ->
            --(Failure "updateWithReceivedRankings18")
            -- todo: fix put back above - just while sorting fauna relations issues
            let
                newsRankings = Data.Rankings.asRankings (EverySet.fromList Data.Rankings.testRankings)
                newDataKind = Global (Data.Global.created newsRankings Data.Users.empty Data.Users.emptyUser)
                newDataState = Fetched Data.Users.empty  newsRankings newDataKind
            in
                AppOps newDataState Data.Users.emptyUser (SR.Types.SelectedUI SR.Types.Other) emptyTxRecord



updateWithReceivedPlayers : Model -> Result (GQLHttp.Error (Maybe (List (Maybe Data.Players.FPlayer)))) (Maybe (List (Maybe Data.Players.FPlayer))) 
    -> Data.Selected.PlayerStatus -> Model
updateWithReceivedPlayers model response playerStatus =
     case (model, response) of
        (AppOps AllEmpty user uiState txRec, Ok lplayers)  ->
            (Failure "No network connection ...")

        (AppOps (Fetched sUsers sRankings  (Global _)) user uiState txRec, Ok lplayers) ->
             (Failure "updateWithReceivedPlayers1")
                
        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Ok lplayers) ->
            (Failure "updateWithReceivedPlayers2")

        ( AppOps (Fetched sUsers sRankings 
            --(Selected (Data.Selected.Selected esUP rnkId selectedOwnerStatus sPlayers selectedState "")))
            (Selected s))
                user uiState txRec
                , Ok lplayers ) ->

                    let
                        filteredFPlayerList = Utils.MyUtils.removeNothingFromList (Maybe.withDefault [] lplayers)
                        lFromFToPlayer = List.map Data.Players.convertPlayerFromFPlayer filteredFPlayerList
                        _ = Debug.log "players" lFromFToPlayer
                        newDataKind = Selected <| Data.Selected.created lFromFToPlayer sUsers user (Data.Selected.gotRanking s) playerStatus
                        newDataState = Fetched sUsers sRankings newDataKind
                    in
                        AppOps newDataState user uiState txRec

        -- ( AppOps (Fetched _ _ (Selected (Data.Selected.Selected _ _ _ _ _ _))) _ _ _, Ok _ ) ->
        --     (Failure "tbc in updateWithReceivedPlayers ")

        (AppOps AllEmpty user uiState txRec, Err _ )  ->
            (Failure "Unable to obtain Players data. Please check your network connection ...")

        (AppOps (Fetched sUsers sRankings dKind) user uiState txRec, Err _)  ->
            (Failure "updateWithReceivedRankings15")

        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Err _ ) ->
            (Failure "updateWithReceivedRankings16")

        (Failure _, Ok lusers) ->
            (Failure "updateWithReceivedRankings17")

        (Failure _, Err _) ->
            (Failure "updateWithReceivedRankings18")


 
updateWithReceivedRankingById : Model -> Result (GQLHttp.Error (Maybe Data.Rankings.FRanking)) (Maybe Data.Rankings.FRanking) -> Model
updateWithReceivedRankingById model response =
     case (model, response) of -- AllEmpty, so fill the Ranking set
        (AppOps AllEmpty user uiState txRec, Ok _)  ->
            Failure "Err"

        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Ok lrankings) ->
            model

        (AppOps (Fetched sUsers sRankings (Global sGlobal)) user uiState txRec, Ok franking) ->
            let
                --filteredFRanking = Maybe.withDefault (Data.Rankings.Ranking 0 True "" Nothing "") franking
                -- need to convert from FRanking to Ranking (id_ needs to be a String)

                --ethaddr = Maybe.withDefault "" (Just (Eth.Utils.addressToString user.m_ethaddress))
                --fromFToRanking = SR.Types.newRanking filteredFRanking
                -- below just getting to compile
                fromFToRanking = (Data.Rankings.Ranking "" True "" Nothing "")
                -- --_ = Debug.log "lFromFToRanking : " lFromFToRanking
                
                --change dataKind to Selected (Rankings below is just to get it to compile)
                --newDataKind = Selected ((Data.Selected.gotUserAsPlayer Data.Selected.empty ethaddr) sUsers fromFToRanking.id_)
                
                --todo: I think we'll be using the selected ranking fromFToRanking to build a Selected (set of UserPlayers)
                newDataState = Fetched sUsers sRankings (Selected Data.Selected.empty)
            in
                AppOps newDataState user uiState txRec


        ( AppOps (Fetched _ _ (Selected _)) _ _ _ , Ok _ ) ->
            (Failure "updateWithReceivedUsers1")

        (AppOps AllEmpty user uiState txRec, Err _ )  ->
            (Failure "updateWithReceivedUsers2")

        (AppOps (Fetched sUsers sRankings dKind) user uiState txRec, Err _)  ->
            (Failure "updateWithReceivedUsers3")

        (AppOps (Updated sUsers sRankings dKind) user uiState txRec, Err _ ) ->
            (Failure "updateWithReceivedUsers4")

        (Failure _, Ok lusers) ->
            (Failure "updateWithReceivedUsers5")

        (Failure _, Err _) ->
            (Failure "updateWithReceivedUsers6")




loginResponse: Model -> Result (GQLHttp.Error (Bridge.LoginResult)) (Bridge.LoginResult) -> Model
loginResponse model response =
    case (model, response) of
        (AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec
            , Ok loginResult) ->
                let 
                    --convertedUser = (Data.Users.convertFUserToUser (Maybe.withDefault (Data.Users.emptyFUser) loginResult.user))
                    convertedUser = Data.Users.dummyUserWithUserJoinedRankings
                in
                AppOps (createGlobal dataState convertedUser) convertedUser uiState txRec
        
        ( AppOps _ (_) _ _ 
            , Ok loginResult) ->
                Failure "Only a Spectator should be able \nlogin or register. A user has \nbeen created in Fauna!"

        (AppOps dataState (Data.Users.Spectator userInfo _) uiState txRec
            , Err _) ->
                let 
                    --convertedUser = (Data.Users.convertFUserToUser (Maybe.withDefault (Data.Users.emptyFUser) loginResult.user))
                    convertedUser = Data.Users.dummyUserWithUserJoinedRankings
                in
                AppOps (createGlobal dataState convertedUser) convertedUser uiState txRec
                --AppOps dataState (Data.Users.Spectator userInfo (Data.Users.LoginError)) uiState txRec
        
        ( AppOps _ (_) _ _ , Err _ ) ->
            Failure "Only a Spectator should be able \nlogin or register."

        (Failure _, _) ->
            model

createGlobal : DataState -> Data.Users.User -> DataState 
createGlobal dataState user = 
    case (dataState, user) of 
        (Fetched sUsers sRankings _, Data.Users.Spectator _ _) ->
            let
                newDataKind = Global <| Data.Global.created sRankings sUsers user
                newDataState = Fetched sUsers sRankings newDataKind
            in 
            newDataState
        
        (Fetched sUsers sRankings _, Data.Users.Registered _ _) ->
            let
                newDataKind = Global <| Data.Global.created sRankings sUsers user
                newDataState = Fetched sUsers sRankings newDataKind
            in 
                newDataState

        (_, _) ->
            dataState


createNewRankingResponse: Model -> Result (GQLHttp.Error (Data.Rankings.FRanking)) (Data.Rankings.FRanking) -> Model
createNewRankingResponse model response =
    case (model, response) of
        (AppOps dataState (Data.Users.Spectator userInfo userState) uiState txRec
            , Ok createNewRankingResult) ->
                Failure "Spectator can't create a ranking!"
        
        ( AppOps (Fetched sUsers sRankings dKind) (Data.Users.Registered userInfo userState) uiState txRec 
            , Ok createNewRankingResult) ->
                let 
                    newRanking = Data.Rankings.convertFRankingToRanking createNewRankingResult
                    firstPlayer = Data.Players.IndividualPlayer (Data.Players.PlayerInfo newRanking.id_  userInfo.id 1)
                                Data.Players.Available
                    firstUserPlayer = {  player = firstPlayer, user = Data.Users.Registered userInfo userState}

                    newDataKind = Selected <| Data.Selected.created [firstPlayer]
                                 sUsers (Data.Users.Registered userInfo userState) newRanking Data.Selected.Owner
                    newDataState = Fetched sUsers sRankings newDataKind
                in 
                    case userInfo.walletState of
                        -- todo: complete for walletStates:
                        _ ->
                            AppOps newDataState  (Data.Users.Registered userInfo userState) uiState txRec

        
        ( AppOps AllEmpty (Data.Users.Registered _ _) _ _, Ok _ ) ->
            Failure "All empty"
        ( AppOps (Updated _ _ _) (Data.Users.Registered _ _) _ _, Ok _ )->
            Failure "Maybe use updated?"
 

        (AppOps dataState (Data.Users.Spectator userInfo _) uiState txRec
            , Err _) ->
                Failure "Spectator can't create a ranking!"
        
        -- rf: maybe create a userState for this?:
        ( AppOps _ (_) _ _ , Err _ ) ->
            Failure "There was a problem creating the ranking ... please try again"

        (Failure _, _) ->
            model


registeredResponse: Model -> Result (GQLHttp.Error (String)) (Data.Users.Token) -> Model
registeredResponse model response =
    case (model, response) of
        (AppOps dataState user uiState txRec, Ok token) ->
            case user of
                Data.Users.Spectator userInfo userState ->
                    let
                        --updated_user = Data.Users.Registered loginResult userInfo userState
                        -- todo: fix
                        updated_user = Data.Users.Registered userInfo userState
                    in
                        AppOps dataState updated_user uiState txRec
                (Data.Users.Registered userInfo userState) ->
                    model

        (AppOps dataState user uiState txRec, Err _) ->
                AppOps dataState user uiState txRec

        (Failure _, _) ->
            model


updateFromRegisteredNewUser: Model -> Result (GQLHttp.Error Data.Users.Token) Data.Users.Token -> Model
updateFromRegisteredNewUser model response =
    case (response, model) of
        (Ok token, AppOps dataState user uiState txRec) ->
            case user of
                Data.Users.Spectator userInfo userState ->
                    model
                (Data.Users.Registered userInfo userState) ->
                    let
                        updated_user = Data.Users.Registered userInfo userState
                    in
                        AppOps dataState updated_user uiState txRec

        ( Ok _, Failure _ ) ->
            (Failure "New user registered, model failure")

        (Err str, _) ->
            (Failure "Problem registering new user")


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


updateUserName : String -> Data.Users.UserInfo -> Data.Users.UserInfo
updateUserName newusername userInfo =
    {userInfo | username = newusername}


updateAppInfoUserDesc : String -> Data.Users.UserInfo -> Data.Users.UserInfo
updateAppInfoUserDesc descfield userInfo =
    let
        existingExtraUserInfo = userInfo.extrauserinfo
        newExtraInfo = {existingExtraUserInfo | description = descfield}
        newUserInfo = {userInfo | extrauserinfo = newExtraInfo}
        
    in
        newUserInfo

updateAppInfoUserEmail : String -> Data.Users.UserInfo -> Data.Users.UserInfo 
updateAppInfoUserEmail emailfield userInfo =
    let
        existingExtraUserInfo = userInfo.extrauserinfo
        newExtraInfo = {existingExtraUserInfo | email = emailfield}
        newUserInfo = {userInfo | extrauserinfo = newExtraInfo}
    in
        newUserInfo


updateUserMobile : String -> Data.Users.UserInfo -> Data.Users.UserInfo
updateUserMobile mobilefield userInfo =
    let
        existingExtraUserInfo = userInfo.extrauserinfo
        newExtraInfo = {existingExtraUserInfo | mobile = mobilefield}
        newUserInfo = {userInfo | extrauserinfo = newExtraInfo}
    in
        newUserInfo


updateSelectedRankingPlayerList : Model -> List Data.Selected.UserPlayer -> Model
updateSelectedRankingPlayerList model luplayers =
    case model of
        AppOps dataState user uiState txRec ->
            case dataState of
                Updated sUsers sRankings dKind -> 
                    case dKind of 
                        Selected s ->
                            let 
                            --todo: I think this means we lose the update - need to do differently ...
                                -- newDataKind = Selected ((Data.Selected.created (Data.Selected.convertUserPlayersToPlayers luplayers) sUsers
                                --     (rnkId) "") selectedOwnerStatus)
                                newDataKind = Selected <| Data.Selected.created (Data.Selected.convertUserPlayersToPlayers luplayers) 
                                    sUsers user (Data.Selected.gotRanking s) (Data.Selected.gotStatus s)
                                newDataState = Updated sUsers sRankings newDataKind 
                            in
                                AppOps newDataState user uiState txRec

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

-- view

view : Model -> Html Msg
view model =
    case model of
        AppOps dataState user uiState txRec ->
            -- might find no need to pattern match on user when UI state fixed:
            case (dataState, user, uiState) of 
                (AllEmpty, _ , (SR.Types.GeneralUI SR.Types.Loading)) ->
                    Html.text ("Loading ...")

                -- Global -- Spectator

                (Fetched sUsers sRankings (Global g), _ , (SR.Types.GlobalUI SR.Types.All)) ->
                            -- this may be on a 'Cancel'
                           globalView user sUsers (Global g) ""

                (Fetched sUsers sRankings (Selected s), _ , (SR.Types.GlobalUI SR.Types.All))
                    -- Data.Users.Spectator userInfo 
                    --     Data.Users.General) 
                        ->
                            -- this may be on a 'Cancel'
                            -- globalView 
                            --     --user sUsers (Data.Global.Global (esUR) Data.Global.DisplayLoggedIn)
                            --     user sUsers g
                            --     ""
                                Html.text ("Selected tbc2")
                ( Updated _ _ _, _, _ ) ->
                    Html.text ("deleted updataed?")

                ( AllEmpty, _, _) ->
                    Html.text ("Selected tbc3")
            
                ( Fetched sUsers sRankings (Global g), _, SR.Types.GeneralUI SR.Types.LogIn ) ->
                     globalView user sUsers (Global g) ""
                ( Fetched _ _ (Global _), _, SR.Types.GeneralUI SR.Types.Loading ) ->
                    Html.text ("Loading ...")
                ( Fetched sUsers _ (Global _), _, SR.Types.GeneralUI SR.Types.Register ) ->
                    registerNewUserView user sUsers
                    -- todo: fix
                    -- Framework.responsiveLayout [] <| Element.column Framework.container <|
                    --     [displayRegisterNewUser user sUsers]
                ( Fetched _ _ (Global _), _, SR.Types.GeneralUI SR.Types.UserInfo ) ->
                    Html.text ("tbc")
                ( Fetched _ _ (Global _), _, SR.Types.GeneralUI SR.Types.UpdateUser ) ->
                    Html.text ("tbc")
                ( Fetched _ _ (Global _), _, SR.Types.SelectedUI _ ) ->
                    Html.text ("Selected tbc10")
                ( Fetched _ _ (Global _), _, SR.Types.UIUpdateExistingUser ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Global _), _, SR.Types.UIEnterResult ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Global _), _, SR.Types.UIWaitingForTxReceipt ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Global _), _, SR.Types.UIDeleteRankingConfirm ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Global _), _, SR.Types.UIOwnerDeletedRanking ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Global _), _, SR.Types.UIUnableToFindGlobalRankings ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Selected _), _, SR.Types.GeneralUI _ ) ->
                    Html.text ("Selected tbc11")

                ( Fetched _ _ (Selected s), _, SR.Types.SelectedUI viewType ) ->
                    selectedView s viewType

                ( Fetched _ _ (Selected _), _, SR.Types.UIUpdateExistingUser ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Selected _), _, SR.Types.UIEnterResult ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Selected _), _, SR.Types.UIWaitingForTxReceipt ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Selected _), _, SR.Types.UIDeleteRankingConfirm ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Selected _), _, SR.Types.UIOwnerDeletedRanking ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Selected _), _, SR.Types.UIUnableToFindGlobalRankings ) ->
                    Html.text ("Selected tbc")

                ( Fetched sUser sRankings (Global g), _, SR.Types.GlobalUI SR.Types.NewLadderCreate ) ->
                    inputNewLadderview sRankings (Data.Global.gotRanking g) user

                ( Fetched _ _ (Global _), _, SR.Types.GlobalUI SR.Types.NewLadderConfirmed ) ->
                    Html.text ("Selected tbc")
                ( Fetched _ _ (Selected _), _, SR.Types.GlobalUI SR.Types.NewLadderCreate ) ->
                    Html.text ("Selected tbc12")
                ( Fetched _ _ (Selected _), _, SR.Types.GlobalUI SR.Types.NewLadderConfirmed ) ->
                    Html.text ("Should be global before createnew")

                -- (Fetched sUsers sRankings dKind, 
                --     Data.Users.Spectator userInfo 
                --         Data.Users.Updating) ->
                --             Html.text ("Spectator cannot update!")

                -- (Fetched sUsers sRankings dKind, 
                --     Data.Users.Spectator userInfo 
                --         Data.Users.CreatingNew) ->
                --             inputUserDetailsView (Fetched sUsers sRankings dKind) user

                -- ( Fetched _ _ 
                --     (Global (Data.Global.Global esUR (Data.Global.CreatingNewRanking _ )))
                --     , Data.Users.Spectator _ Data.Users.General ) ->
                --     Html.text ("Not yet implemented")
                
                -- ( Fetched _ _ 
                --     (Global (Data.Global.Global (esUR) (Data.Global.CreatedNewRanking ranking)))
                --     , Data.Users.Spectator _ Data.Users.General ) ->
                --     Html.text ("Not yet implemented")
                
                -- ( Fetched sUsers _ 
                --     (Global (Data.Global.Global (esUR) Data.Global.DisplayGlobalOnly))
                --     , Data.Users.Spectator _ Data.Users.LoginError ) ->
                --         globalView
                --             user sUsers (Data.Global.Global (esUR) Data.Global.DisplayGlobalLogin) "Not found. Register?:"
                
                -- ( Fetched sUsers _ 
                --     (Global (Data.Global.Global (esUR) 
                --     (Data.Global.CreatingNewRanking _ )))
                --     , Data.Users.Spectator _ Data.Users.LoginError ) ->
                --         globalView
                --             user sUsers (Data.Global.Global (esUR) Data.Global.DisplayGlobalLogin) "Please register \nto create a new ladder:"

                -- ( Fetched sUsers sRankings 
                --     (Global (Data.Global.Global (esUR) 
                --     (Data.Global.CreatingNewRanking ranking)))
                --     , (Data.Users.Registered _ _) as userVal ) ->
                --         inputNewLadderview sRankings ranking userVal
                
                -- ( Fetched sUsers _ 
                --     (Global (Data.Global.Global (esUR) (Data.Global.CreatedNewRanking ranking)))
                --     , Data.Users.Spectator _ Data.Users.LoginError ) ->
                --         globalView
                --             user sUsers (Data.Global.Global (esUR) Data.Global.DisplayGlobalLogin) "Please register \nto create a new ladder:"

                -- ( Fetched sUsers _ (
                --         Global (Data.Global.Global (esUR) Data.Global.DisplayGlobalOnly))
                --     , Data.Users.Spectator _ Data.Users.General ) ->
                --     globalView user sUsers (Data.Global.Global(esUR) Data.Global.DisplayGlobalOnly) ""

                -- ( Fetched sUsers _ (
                --         Global (Data.Global.Global (esUR) 
                --         Data.Global.DisplayGlobalLogin))
                --     , Data.Users.Spectator _ Data.Users.General ) ->
                --     globalView user sUsers (Data.Global.Global(esUR) Data.Global.DisplayGlobalOnly) ""

                -- -- Global -- Registered

                -- ( Fetched sUsers _ 
                --     (Global (Data.Global.Global (esUR) Data.Global.DisplayGlobalLogin))
                --     , Data.Users.Registered userInfo userState ) ->
                --     globalView 
                --         (Data.Users.Registered userInfo userState ) sUsers 
                --         (Data.Global.Global (esUR) Data.Global.DisplayLoggedIn)
                --         ""

                -- ( Fetched sUsers sRankings 
                --     (Global (Data.Global.Global (esUR) Data.Global.DisplayGlobalOnly) )
                --     , Data.Users.Registered _ _ ) ->
                --     globalView user sUsers (Data.Global.Global(esUR) Data.Global.DisplayGlobalOnly)
                --     ""

                
                -- ( Fetched sUsers sRankings 
                --     (Global (Data.Global.Global (esUR) (Data.Global.CreatedNewRanking ranking)))
                --     , Data.Users.Registered userInfo userState ) ->
                --     globalView 
                --         (Data.Users.Registered userInfo userState) sUsers 
                --             (Data.Global.Global (esUR) (Data.Global.CreatedNewRanking ranking))
                --             ""


                -- ( Fetched sUsers sRankings 
                --     (Global (Data.Global.Global (esUR) Data.Global.DisplayLoggedIn)), userVal) ->
                --     globalView 
                --         userVal sUsers (Data.Global.Global (esUR) (Data.Global.DisplayLoggedIn ))
                        --""

                -- ( Fetched _ _ (Global (Data.Global.Global (_) Data.Global.DisplayGlobalLogin)), Data.Users.Spectator _ Data.Users.Updated ) ->
                --     Html.text ("User Updated")
                -- ( Fetched _ _ (Global (Data.Global.Global (_) Data.Global.DisplayGlobalOnly)), Data.Users.Spectator _ Data.Users.Updated ) ->
                --     Html.text ("User Updated")
                -- ( Fetched _ _ (Global (Data.Global.Global (_) (Data.Global.CreatingNewRanking _ ))), Data.Users.Spectator _ Data.Users.Updated ) ->
                --     Html.text ("User Updated")
                -- ( Fetched _ _ (Global (Data.Global.Global (_) (Data.Global.CreatedNewRanking ranking))), Data.Users.Spectator _ Data.Users.Updated ) ->
                --     Html.text ("User Updated")

                -- ( Fetched _ _ (Global _), Data.Users.NoWallet _ _ ) ->
                --     Html.text ("Not yet implemented")
                -- ( Fetched _ _ (Global _), Data.Users.NoCredit _ _ ) ->
                --     Html.text ("Not yet implemented")
                -- ( Fetched _ _ (Global _), Data.Users.Credited _ _ ) ->
                --     Html.text ("Not yet implemented")

                -- ( Fetched _ _ (Global (Data.Global.Global (_) Data.Global.DisplayGlobalLogin)), Data.Users.Spectator _ _ ) ->
                --     Html.text ("Not yet implemented")
                
                -- ( Fetched _ _ (Global (Data.Global.Global (_) Data.Global.DisplayGlobalOnly)), Data.Users.Spectator _ _ ) ->
                --      Html.text ("Not yet implemented")
                
                -- ( Fetched _ _ (Global (Data.Global.Global (_) (Data.Global.CreatingNewRanking _ ))), Data.Users.Spectator _ _ ) ->
                --     Html.text ("Not yet implemented")
                
                -- ( Fetched _ _ (Global (Data.Global.Global (_) (Data.Global.CreatedNewRanking ranking))), Data.Users.Spectator _ _ ) ->
                --      Html.text ("Not yet implemented")
                

                -- Selected
                -- (Fetched sUsers sRankings 
                --     (Selected sSelected), Data.Users.Spectator _ _ ) ->
                --         Framework.responsiveLayout [] <| Element.column Framework.container
                --             [ Element.el Heading.h4 <| Element.text <| "SportRank - Spectator "
                --             , infoBtn "Cancel" Cancel
                --             , Element.text "\n"
                --             , playerbuttons sSelected sUsers user
                --             ]
                
                -- ( Fetched sUsers sRankings 
                --     (Selected (Data.Selected.Selected esUP rnkId 
                --         Data.Selected.Owner 
                --         sPlayers selectedState name)  )
                --         , Data.Users.Registered userInfo userState ) ->
                --             Framework.responsiveLayout [] <| Element.column Framework.container
                --             [ Element.el Heading.h4 <| Element.text <| "SportRank - " ++ userInfo.username
                --             , playerbuttons (Data.Selected.Selected esUP rnkId 
                --                     Data.Selected.Owner 
                --                     sPlayers selectedState name)        
                --                     sUsers user
                --             , infoBtn "Delete" ClickedDeleteRanking
                --             , Element.text "\n"
                --             , infoBtn "Cancel" Cancel
                --             ]

                -- ( Fetched sUsers sRankings (Selected (Data.Selected.Selected esUP rnkId Data.Selected.Member sPlayers selectedState name))
                --     , Data.Users.Registered userInfo userState ) ->
                --             Framework.responsiveLayout [] <| Element.column Framework.container
                --             [ Element.el Heading.h4 <| Element.text <| "SportRank - " ++ userInfo.username
                --             , playerbuttons (Data.Selected.Selected esUP rnkId 
                --                     Data.Selected.Owner 
                --                     sPlayers selectedState name)       
                --                     sUsers user
                --             , Element.text "\n"
                --             , infoBtn "Cancel" Cancel
                --             ]
                            
                -- ( Fetched sUsers sRankings (Selected (Data.Selected.Selected esUP rnkId Data.Selected.Other sPlayers selectedState name))
                --     , Data.Users.Registered userInfo userState  ) ->
                --     Framework.responsiveLayout [] <| Element.column Framework.container
                --     [ Element.el Heading.h4 <| Element.text <| "SportRank - " ++ userInfo.username
                --     , playerbuttons (Data.Selected.Selected esUP rnkId 
                --             Data.Selected.Owner 
                --             sPlayers selectedState name)       
                --             sUsers user
                --     , Element.text "\n"
                --     , infoBtn "Cancel" Cancel
                --     ]

                -- ( Fetched _ _ (Selected _), Data.Users.NoWallet _ _ ) ->
                --     Html.text "selected user3"

                -- ( Fetched _ _ (Selected _), Data.Users.NoCredit _ _ ) ->
                --     Html.text "selected user"

                -- ( Fetched _ _ (Selected _), Data.Users.Credited _ _ ) ->
                --     Html.text "selected user"

                -- (Updated _ _ _, _) ->
                --     Html.text ("No User - No Update")

           
        Failure str ->
           failureView str

-- view helpers

-- resultView : Data.Selected.SelectedOwnerStatus -> SR.Types.UIState
-- resultView  status = 
--     case status of
--             Data.Selected.Owner -> 
--                 SR.Types.UISelectedRankingUserIsOwner

--             Data.Selected.Member -> 
--                 SR.Types.UISelectedRankingUserIsPlayer

--             Data.Selected.Other -> 
--                 SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer



globalView : Data.Users.User -> Data.Users.Users -> DataKind -> String -> Html Msg 
globalView userVal sUsers dKind errorMsg =
    case dKind of
        Global g -> 
            Framework.responsiveLayout [] <| Element.column Framework.container <|
                case userVal of
                    Data.Users.Spectator _ _ ->
                        [Element.el (Heading.h5) <| (Element.text ("SportRank - Welcome Spectator"))
                        , displayEnableEthereumBtn
                        , displayRankingBtns userVal (Global g) errorMsg]

                    Data.Users.Registered userInfo userState ->
                        [Element.el (Heading.h5) <| (Element.text ("SportRank -" ++ userInfo.username) )
                        , displayEnableEthereumBtn
                        , Element.text ("\n")
                        , 
                            case (Data.Global.isEmpty <| Data.Global.fetchedOwned g) of 
                                True ->
                                    Element.text ("")
                                False ->
                                    displayCreateNewRankingBtn

                        , displayRankingBtns userVal (Global g) errorMsg
                        ]
                            
        Selected _ -> 
            Framework.responsiveLayout [] <| Element.column Framework.container <|
                [Element.text ("Should be Global view only here")]
        


registerNewUserView : Data.Users.User -> Data.Users.Users -> Html Msg 
registerNewUserView userVal sUsers = 
    case userVal of
        Data.Users.Spectator userInfo userState ->
            Framework.responsiveLayout [] <|
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                , Element.wrappedRow (Card.fill ++ Grid.simple)
                    [ Element.column
                        Grid.simple
                        [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                            { onChange = UserNameInputChg
                            , text = userInfo.username
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username*")
                            }
                        , nameValidView userInfo sUsers
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                            { onChange = UserPasswordInputChg
                            , text = userInfo.password
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password*")
                            }
                        , passwordValidView userInfo
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userDescription") ])
                            { onChange = UserDescInputChg
                            , text = userInfo.extrauserinfo.description
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Description")
                            }
                        , userDescValidationErr userInfo.extrauserinfo.description
                        , Input.email (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userEmail") ])
                            { onChange = UserEmailInputChg
                            , text = userInfo.extrauserinfo.email
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Email")
                            }
                        , emailValidationErr userInfo.extrauserinfo.email
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userMobile") ])
                            { onChange = UserMobileInputChg
                            , text = Utils.Validation.Validate.validatedMaxTextLength userInfo.extrauserinfo.mobile 25
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Mobile \n(inc. Int code\neg.+65)")
                            }
                        , mobileValidationErr userInfo.extrauserinfo.mobile
                        ]
                    ]
                , Element.text "* required"
                , SR.Elements.justParasimpleUserInfoText
                , userDetailsConfirmPanel userVal sUsers
                ]
        (Data.Users.Registered userInfo userState) ->
            Framework.responsiveLayout [] <|
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                , Element.wrappedRow (Card.fill ++ Grid.simple)
                    [ Element.column
                        Grid.simple
                        [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                            { onChange = UserNameInputChg
                            , text = userInfo.username
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username*")
                            }
                        , nameValidView userInfo sUsers
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                            { onChange = UserPasswordInputChg
                            , text = userInfo.password
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password*")
                            }
                        , passwordValidView userInfo
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userDescription") ])
                            { onChange = UserDescInputChg
                            , text = userInfo.extrauserinfo.description
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Description")
                            }
                        , userDescValidationErr userInfo.extrauserinfo.description
                        , Input.email (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userEmail") ])
                            { onChange = UserEmailInputChg
                            , text = userInfo.extrauserinfo.email
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Email")
                            }
                        , emailValidationErr userInfo.extrauserinfo.email
                        , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userMobile") ])
                            { onChange = UserMobileInputChg
                            , text = Utils.Validation.Validate.validatedMaxTextLength userInfo.extrauserinfo.mobile 25
                            , placeholder = Nothing
                            , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Mobile \n(inc. Int code\n e.g.+65)")
                            }
                        , mobileValidationErr userInfo.extrauserinfo.mobile
                        ]
                    ]
                , Element.text "* required"
                , SR.Elements.justParasimpleUserInfoText
                , userDetailsConfirmPanel userVal sUsers
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
                    --, displayRankingBtns userVal sGlobal
                    -- if the UI following is an issue needing branching
                    -- do it in a separate function like dispalyForToken
                    , infoBtn "Log In" ClickedLogInUser
                    , Element.text ("\n")
                    , infoBtn "Register" ClickedRegister  
                ]


displayRankingBtns : Data.Users.User -> DataKind-> String -> Element Msg 
displayRankingBtns userVal dKind errorMsg = 
    case dKind of 
        (Global g) ->
            case userVal of
                Data.Users.Spectator userInfo userState ->
                    -- Err
                    Element.column Grid.section <|
                        [ Element.el [] <| Element.text " Please login or view \n lists as spectator (below):"
                        --Heading.h5 <| Element.text "Please Enter Your User \nDetails And Click 'Register' below:"
                        , Element.wrappedRow (Card.fill ++ Grid.simple)
                            [ Element.column
                                Grid.simple
                                [ Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "userName") ] ++ [ Input.focusedOnLoad ])
                                    { onChange = UserNameInputChg
                                    , text = userInfo.username
                                    --, placeholder = Input.placeholder <| [Element.Attribute "Username"]
                                    , placeholder = Nothing
                                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Username")
                                    }
                                --, nameValidView appInfo sUsers
                                , Input.text (Input.simple ++ [ Element.htmlAttribute (Html.Attributes.id "Password") ])
                                    { onChange = UserPasswordInputChg
                                    , text = userInfo.password
                                    , placeholder = Nothing
                                    , label = Input.labelLeft (Input.label ++ [ Element.moveLeft 11.0 ]) (Element.text "Password")
                                    }
                                ]
                            ]
                        , infoBtn "Log In" ClickedLogInUser
                        , SR.Elements.warningText errorMsg
                        , infoBtn "Register" ClickedRegister
                        ]
                    
                (Data.Users.Registered userInfo userState) ->
                    Element.column Grid.section <|
                        [ rankingbtns (Global g) userVal ]

        (Selected s) ->
            Element.text " Should be in Global here"
  

greetingHeading : String -> Element Msg
greetingHeading greetingStr =
    Element.column Grid.section <|
        [ Element.el (List.append Heading.h5 [ Element.htmlAttribute (Html.Attributes.id "greetingInitStr") ]) <| Element.text "Initializing ..."
        , Element.column Card.fill
            [ Element.el (List.append Heading.h5 [ Element.htmlAttribute (Html.Attributes.id "greetingHeadingStr") ]) <|
                Element.text greetingStr
            ]
        ]


rankingbtns: DataKind -> Data.Users.User -> Element Msg
rankingbtns dKind user =
    case dKind of 
        (Global g) ->
            case user of 
                Data.Users.Spectator userInfo userState ->
                    Element.column Grid.section <|
                            [ Element.el Heading.h5 <|  Element.text "If you register you can \ncreate your own rankings"
                            , infoBtn "Cancel" Cancel
                            , Element.text "\n"
                            , Element.column (Card.simple ++ Grid.simple)
                                (List.map rankingBtn (Data.Global.asList (Data.Global.fetchedOther g)))
                            ]

                (Data.Users.Registered userInfo userState) ->
                        Element.column Grid.section <|
                            [ Element.el Heading.h5 <| Element.text "Your Created Rankings:"
                            , Element.column (Card.simple ++ Grid.simple) <| 
                                (case Data.Global.isEmpty (Data.Global.fetchedOwned g) of
                                    True ->
                                        [infoBtn "Create New Ladder" ClickedCreateNewLadder]
                                    False ->
                                        List.map rankingBtn (Data.Global.asList (Data.Global.fetchedOwned g))
                                )
                            , Element.el Heading.h5 <| Element.text "Your Member Rankings: " 
                            , Element.column (Card.simple ++ Grid.simple) <| 
                                (case Data.Global.isEmpty (Data.Global.fetchedMember g) of
                                    True ->
                                        [infoBtn "Join A Ladder?" ClickedDisplayGlobalOnly]
                                    False ->
                                        List.map rankingBtn (Data.Global.asList (Data.Global.fetchedMember g))
                                )
                            , Element.el Heading.h5 <|  Element.text "All Other Rankings: "
                            , Element.column (Card.simple ++ Grid.simple)
                                (List.map rankingBtn (Data.Global.asList (Data.Global.fetchedOther g)))
                            ]

        (Selected s) ->
            Element.text " should be global, I think"
                    

rankingBtn : Data.Global.UserRanking -> Element Msg
rankingBtn uR =
    Element.column Grid.simple <|
        [ Input.button (Button.fill ++ Color.primary) <|
            { onPress = Just <| ClickedSelectedRanking uR
            , label = Element.text uR.rankingInfo.rankingname
            }
        ]


playerbuttons : Data.Selected.Selected -> Element Msg
playerbuttons s =
    Element.column Grid.section <|
        [ 
            Element.column (Card.simple ++ Grid.simple) <|
            List.map (configureThenAddPlayerRankingBtns s )
                (Data.Selected.asList s)
        ]

configureThenAddPlayerRankingBtns : Data.Selected.Selected -> Data.Selected.UserPlayer -> Element Msg
configureThenAddPlayerRankingBtns s uplayer =
   -- nb. 'uplayer' is the player that's being mapped cf. user which is current user (single instance)
    let
        challorAvail = Data.Selected.challorAvail s uplayer
    in
        -- all players are considered Registered (only)
        case (uplayer.user, uplayer.player) of
            (Data.Users.Registered userInfo userState, Data.Players.IndividualPlayer playerInfo playerStatus) ->
                if not (Data.Selected.isChallenged uplayer) then
                    if Data.Selected.isRegisteredPlayerCurrentUser (Data.Selected.gotUP s).user uplayer then
                        --not challenged, is current user:
                        Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.disabled) <|
                            { onPress = Nothing
                            , label = Element.text <| String.fromInt playerInfo.rank ++ ". " ++ userInfo.username ++ " vs " ++ challorAvail
                            }
                        ]
                    
                    else
                        --available to challenge, not current user.
                        -- uplayer for ClickedChallengeOpponent here is the opponent:
                        Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.info) <|
                            { onPress = Just <| ClickedChallengeOpponent uplayer (Data.Selected.gotUserPlayerByUserId s userInfo.id)
                            , label = Element.text <| String.fromInt playerInfo.rank ++ ". " ++ userInfo.username ++ " vs " ++ challorAvail
                            }
                        ]
                else
                    if Data.Selected.isRegisteredPlayerCurrentUser (Data.Selected.gotUP s).user uplayer then
                        -- already in a challenge, is current user and therefore ready to enter a result
                        Element.column Grid.simple <|
                        [ Input.button (Button.fill ++ Color.success) <|
                            { 
                                --nb. this was appInfo.player - uplayer.player might not be quite correct:
                                onPress = Just <| ClickedChangedUIStateToEnterResult uplayer
                            , label = Element.text <| String.fromInt playerInfo.rank ++ ". " ++ userInfo.username ++ " vs " ++ challorAvail
                            }
                        ]

                    else
                    -- already in a challenge, not current user
                    Element.column Grid.simple <|
                    [ Input.button (Button.fill ++ Color.disabled) <|
                        { onPress = Nothing
                        , label = Element.text <| String.fromInt playerInfo.rank ++ ". " ++ userInfo.username ++ " vs " ++ challorAvail
                        }
                    ]

            (_) ->
                Element.text "Unregistered user!"
        
joinBtn : Data.Users.User -> Element Msg
joinBtn user  =
    case user of
        Data.Users.Spectator userInfo userState ->
            Input.button ([ Element.htmlAttribute (Html.Attributes.id "existingUserJoinbtn") ] ++ Button.simple ++ Color.disabled) <|
            { onPress = Just ClickedRegister
            , label = Element.text "Join"
            }
        (Data.Users.Registered userInfo userState) ->
            Input.button ([ Element.htmlAttribute (Html.Attributes.id "newUserJoinbtn") ] ++ Button.simple ++ Color.disabled) <|
            { onPress = Nothing
            , label = Element.text "Join"
            }


-- confirmDelRankingBtn : SR.Types.AppInfo -> DataState -> Element Msg
-- confirmDelRankingBtn appInfo dataState =
    
--     case dataState of 
--             Updated sUsers sRankings dKind ->
--                  case dKind of 
--                     Global sGlobal  ->
--                         let 
--                             m_userRanking = Data.Global.gotUserRankingByRankingId sGlobal appInfo.selectedRanking.id_
--                         in
--                             case m_userRanking of
--                                 Nothing ->
--                                     Element.text "Cannot find the ranking"
--                                 Just userRanking ->
--                                     Element.column Grid.section <|
--                                         [ 
--                                         Element.el Heading.h5 <| Element.text userRanking.rankingInfo.rankingname
--                                         , Element.el Heading.h6 <| Element.text "Click to continue ..."
--                                         , Element.column (Card.simple ++ Grid.simple) <|
--                                             [ Element.wrappedRow Grid.simple <|
--                                                 [ Input.button (Button.simple ++ Color.simple) <|
--                                                     { onPress = Just <| Cancel
--                                                     , label = Element.text "Cancel"
--                                                     }
--                                                     , Input.button Button.simple <|
--                                                     { onPress = Just <| ClickedDeleteRankingConfirmed
--                                                     , label = Element.text "Confirm"
--                                                     }
--                                                 ]
--                                             ]
--                                         , SR.Elements.permanentlyDeleteWarnPara
--                                         ]
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

-- continueAfterDelRankingBtn : SR.Types.AppInfo -> DataState -> Element Msg
-- continueAfterDelRankingBtn appInfo dataState =
--     case dataState of 
--             Updated sUsers sRankings dKind ->
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
                                        
--                                     --     { onPress = Just <| ClickedConfirmCreateNewRanking
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
        AppOps dataState user uiState txRec ->
            case (user, dataState) of
                (Data.Users.Spectator _ _, _) ->
                    Element.text <| " No User3"
                (Data.Users.Registered userInfo userState, AllEmpty) ->
                    Element.text <| " No Data"
                (Data.Users.Registered userInfo userState, Fetched sUsers sRankings (Selected sSelected)) ->
                    Element.text <| "implement here"

                     -- Element.column Grid.section <|
                    --     [ Element.el Heading.h6 <| Element.text <| " Your opponent's details: "
                    --     , Element.paragraph (Card.fill ++ Color.info) <|
                    --         [ Element.el [] <| Element.text <| userInfo.username ++ " you are challenging " ++ challengerInfo.username
                    --         ]
                    --     , Element.el [] <| Element.text <| "Email: "
                    --     , Element.paragraph (Card.fill ++ Color.info) <|
                    --         [ Element.el [] <| Element.text <| challengerInfo.extrauserinfo.email
                    --         ]
                    --     , Element.el [] <| Element.text <| "Mobile: "
                    --     , Element.paragraph (Card.fill ++ Color.info) <|
                    --         [ Element.el [] <| Element.text <| challengerInfo.extrauserinfo.mobile
                    --         ]
                    --     , Element.column (Card.simple ++ Grid.simple) <|
                    --         [ Element.wrappedRow Grid.simple <|
                    --             [ Input.button (Button.simple ++ Color.simple) <|
                    --                 { onPress = Just <| ResetToShowSelected
                    --                 , label = Element.text "Cancel"
                    --                 }
                    --             , Input.button (Button.simple ++ Color.info) <|
                    --                 { onPress = Just <| ClickedNewChallengeConfirm
                    --                 , label = Element.text "Confirm"
                    --                 }
                    --             ]
                    --         ]
                    --     ]
                (Data.Users.Registered userInfo userState, Fetched sUsers sRankings (Global sGlobal)) ->
                    Element.text <| " Not in Selected"
                
                ( Data.Users.Registered _ _, _) ->
                    Element.text "No challenger"
                    
        _ ->
            Element.text "Fail confirmChallengebutton"


confirmResultbutton : Model -> Element Msg
confirmResultbutton model =
    -- todo: fix
    Element.text "Fix confirmResultbutton"
    -- case model of
    --     AppOps dataState user uiState txRec ->
    --         case dataState of
    --             Fetched sUsers sRankings dKind -> 
    --                 case dKind of 
    --                     Selected sSelected ->
    --                         let
    --                             m_playerAsUser = Data.Users.gotUser sUsers appInfo.player.player.uid
    --                         in
    --                         case m_playerAsUser of
    --                             Nothing ->
    --                                 Element.text "No player"
    --                             Just playerAsUser ->
    --                                 let
    --                                     m_challengerAsUser = Data.Users.gotUser sUsers appInfo.challenger.player.uid
    --                                 in
    --                                 case m_challengerAsUser of
    --                                     Nothing ->
    --                                         Element.text "No challenger"
    --                                     Just challengerAsUser ->
    --                                         -- challenger should always be registered
    --                                         case (playerAsUser, challengerAsUser) of
    --                                         (Data.Users.Spectator _ _, _) ->
    --                                             Element.text "No challenger"
    --                                         (Data.Users.Registered _ _ playerUserInfo, Data.Users.Registered challengerUserInfo) ->
    --                                             Element.column Grid.section <|
    --                                             [ Element.column (Card.simple ++ Grid.simple) <|
    --                                                 [ Element.wrappedRow Grid.simple <|
    --                                                     [ Input.button (Button.simple ++ Color.simple) <|
    --                                                         { onPress = Just <| ResetToShowSelected
    --                                                         , label = Element.text "Cancel"
    --                                                         }
    --                                                     ]
    --                                                 ]
    --                                             , Element.paragraph (Card.fill ++ Color.info) <|
    --                                                 [ Element.el [] <| Element.text <| playerUserInfo.username 
    --                                                     ++ " you had a challenge match vs " ++ challengerUserInfo.username
    --                                                 ]
    --                                             , Element.el Heading.h6 <| Element.text <| "Please confirm your result: "
    --                                             , Element.column (Card.simple ++ Grid.simple) <|
    --                                                 [ Element.column Grid.simple <|
    --                                                     [ Input.button (Button.simple  ++ Button.fill ++ Color.primary) <|
    --                                                         { 
    --                                                         onPress = Just <| SentResultToWallet Data.Selected.Won
    --                                                         , label = Element.text "Won"
    --                                                         }
    --                                                     , Input.button (Button.simple  ++ Button.fill ++ Color.primary) <|
    --                                                         { onPress = Just <| SentResultToWallet Data.Selected.Lost
    --                                                         , label = Element.text "Lost"
    --                                                         }
    --                                                     , Input.button (Button.simple  ++ Button.fill ++ Color.primary) <|
    --                                                         { onPress = Just <| ProcessResult Data.Selected.Undecided
    --                                                         , label = Element.text "Undecided"
    --                                                         }
    --                                                     ]
    --                                                 ]
    --                                             , SR.Elements.ethereumWalletWarning
    --                                             , SR.Elements.footer
    --                                             ]
    --                                         (Data.Users.NoWallet userInfo userState, _) ->
    --                                             Element.text "No challenger"
    --                                         (Data.Users.NoCredit userInfo userState, _) ->
    --                                             Element.text "No challenger"
    --                                         (Data.Users.Credited userInfo userState, _) ->
    --                                             Element.text "No challenger"
    --                                         ( Data.Users.Registered _ _, Data.Users.Spectator ) ->
    --                                             Element.text "No challenger"
    --                                         ( Data.Users.Registered _ _, Data.Users.NoWallet _ _ )->
    --                                             Element.text "No challenger"
    --                                         ( Data.Users.Registered _ _, Data.Users.NoCredit _ _ )->
    --                                             Element.text "No challenger"
    --                                         ( Data.Users.Registered _ _, Data.Users.Credited _ _ )->
    --                                             Element.text "No challenger"
                                            
    --                     _ ->
    --                         Element.text "Fail confirmResultbutton"
    --             _ ->
    --                 Element.text "Fail confirmResultbutton"
    --     _ ->
    --         Element.text "Fail confirmResultbutton"


acknoweldgeTxErrorbtn : Model -> Element Msg
acknoweldgeTxErrorbtn model =
    -- todo: fix
    Element.text "fix"
    -- case model of
    --     AppOps dataState user uiState txRec ->
    --         Element.column Grid.section <|
    --             [ 
    --             Element.paragraph (Card.fill ++ Color.info) <|
    --                 [ Element.el [] <| Element.text """ There was an error 
    --                                                     processing your transaction. 
    --                                                     It is unlikely to be 
    --                                                     an issue with this 
    --                                                     application 
    --                                                     but rather your 
    --                                                     wallet setup. Your results are unaffected and
    --                                                     there will have been no charge against your wallet """
    --                 ]
    --             , Element.el Heading.h6 <| Element.text <| "Please click below to continue ... "
    --             , Element.column (Card.simple ++ Grid.simple) <|
    --                 [ Element.column Grid.simple <|
    --                     [ Input.button (Button.simple ++ Color.primary) <|
    --                         { onPress = Just <| ResetToShowSelected
    --                         , label = Element.text "Continue ..."
    --                         }
    --                     ]
    --                 ]
    --             , SR.Elements.footer
    --             ]

    --     _ ->
    --         Element.text "Fail acknoweldgeTxErrorbtn"


userDescValidationErr : String -> Element Msg
userDescValidationErr str =
    if Data.Users.isDescValid str then
        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "descValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "")

    else
        Element.el
            (List.append [ Element.htmlAttribute (Html.Attributes.id "descValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text "20 characters max")


ladderDescValidation : Data.Rankings.Ranking -> Element Msg
ladderDescValidation rankingInfo =
    if isLadderDescValidated rankingInfo then
        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "ladderdescValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "")

    else
        Element.el
            (List.append [ Element.htmlAttribute (Html.Attributes.id "ladderdescValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text "20 characters max")


isLadderDescValidated : Data.Rankings.Ranking -> Bool
isLadderDescValidated rankingInfo =
    case rankingInfo.rankingdesc of
        Nothing ->
            True 
        Just rnkDesc ->
            if String.length rnkDesc <= 20 then
                True

            else
                False


emailValidationErr : String -> Element Msg
emailValidationErr str =
    if Data.Users.isEmailValid str then
        Element.el
            (List.append
                [ Font.color SR.Types.colors.green, Font.alignLeft ]
                [ Element.moveLeft 1.0 ]
            )
            (Element.text "Email OK!")

    else if String.length str > 0 then
        Element.el (List.append [ Font.color SR.Types.colors.red, Font.alignLeft ] [ Element.moveLeft 7.0 ])
            (Element.text """ Email, if
 entered, must be valid""")

    else
        Element.el [] <| Element.text ""


mobileValidationErr : String -> Element Msg
mobileValidationErr str =
    if Data.Users.isMobileValid str then
        Element.el (List.append [ Font.color SR.Types.colors.green, Font.alignLeft ] [ Element.htmlAttribute (Html.Attributes.id "userMobileValid") ]) (Element.text "Mobile OK!")

    else if String.length str > 0 then
        Element.el (List.append [ Font.color SR.Types.colors.red, Font.alignLeft ] [ Element.htmlAttribute (Html.Attributes.id "userMobileInvalid") ] ++ [ Element.moveLeft 5.0 ])
            (Element.text """ Mobile number, if
 entered, must be valid (+ not 00)""")

    else
        Element.el [] <| Element.text ""


userDetailsConfirmPanel : Data.Users.User -> Data.Users.Users -> Element Msg
userDetailsConfirmPanel  user sUsers =
        case user of
        Data.Users.Spectator userInfo userState ->
            if 
                (List.isEmpty <| Data.Users.asList sUsers)
                || 
                (not <| Data.Users.isNameValid userInfo.username sUsers)
                || 
                (not <| Data.Users.isPasswordValid userInfo.password)
            
            then
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
                                , Input.button (Button.simple ++ enableButton (isValidatedForAllUserDetailsInput 
                                user userInfo sUsers)) <|
                                    { onPress = Just <| ClickedConfirmedRegisterNewUser
                                    , label = Element.text "Register"
                                    }
                        
                            ]
                        ]
                    ]

        (Data.Users.Registered userInfo userState) ->
            if List.isEmpty <| Data.Users.asList sUsers then
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
                            , Input.button (Button.simple ++ enableButton (isValidatedForAllUserDetailsInput
                            user userInfo sUsers)) <|
                                { onPress = Just <| ClickedConfirmedUpdateExistingUser
                                , label = Element.text "Update"
                                }
                            ]
                        ]
                    ]


isValidatedForAllUserDetailsInput : Data.Users.User -> Data.Users.UserInfo -> Data.Users.Users -> Bool
isValidatedForAllUserDetailsInput user userInfo sUsers =
    case user of 
        Data.Users.Spectator _ _ ->
            if
                Data.Users.isNameValid (Data.Users.gotName user) sUsers
                && Data.Users.isDescValid userInfo.extrauserinfo.description
                && Data.Users.isEmailValid userInfo.extrauserinfo.email
                && Data.Users.isMobileValid userInfo.extrauserinfo.mobile
            then
                True

            else
                False

        _ ->
            if
                Data.Users.isDescValid userInfo.extrauserinfo.description
                    && Data.Users.isEmailValid userInfo.extrauserinfo.email
                    && Data.Users.isMobileValid userInfo.extrauserinfo.mobile
            then
                True

            else False
        
isValidatedForAllLadderDetailsInput : Data.Rankings.Ranking -> Data.Rankings.Rankings -> Bool
isValidatedForAllLadderDetailsInput ranking sRanking =
    Data.Rankings.isRankingNameValid ranking.rankingname sRanking
        && isLadderDescValidated ranking

enableButton : Bool -> List (Element.Attribute msg)
enableButton enable =
    if enable then
        Color.info
    else
        Color.disabled

nameValidView : Data.Users.UserInfo -> Data.Users.Users -> Element Msg
nameValidView userInfo sUsers =
    if Data.Users.isNameValid userInfo.username sUsers then 
        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "usernameValidMsg") ] [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "Username OK!")

    else
        Element.el
            (List.append [ Element.htmlAttribute (Html.Attributes.id "usernameValidMsg") ] [ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text """Unique and 5-8 continuous chars""")


passwordValidView : Data.Users.UserInfo -> Element Msg
passwordValidView userInfo =
    if Data.Users.isPasswordValid userInfo.password then 
        Element.el ([ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) (Element.text "Password OK!")

    else
        Element.el
            ([ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text """5-8 continuous chars""")


ladderNameValidation :  Data.Rankings.Ranking -> Data.Rankings.Rankings -> Element Msg
ladderNameValidation  ranking sRankings =
    if Data.Rankings.isRankingNameValid ranking.rankingname sRankings then
        Element.el (List.append [ Element.htmlAttribute (Html.Attributes.id "laddernameValidMsg") ] 
        [ Font.color SR.Types.colors.green, Font.alignLeft ] ++ [ Element.moveLeft 1.0 ]) 
        (Element.text "Ladder name OK!")

    else
        Element.el
            (List.append [ Element.htmlAttribute (Html.Attributes.id "laddernameValidMsg") ] 
            [ Font.color SR.Types.colors.red, Font.alignLeft ]
                ++ [ Element.moveLeft 0.0 ]
            )
            (Element.text """Must be unique (4-8 continuous chars)""")


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
            onPress = Just <| ClickedUpdateExistingUser
            , label = Element.text "Update Profile"
            }
     ]


displayCreateNewLadderBtnIfExistingUser : String -> List Data.Global.UserRanking -> Msg -> Element Msg
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

-- displayRegisterBtnIfNewUser : String -> Msg -> Element Msg
-- displayRegisterBtnIfNewUser uname msg =
--     if uname /= "" then
--         Element.text ""

--     else
--         Input.button
--             (Button.simple ++ Button.fill ++ Color.info ++ [ Element.htmlAttribute (Html.Attributes.id "registerbtn") ])
--         <|
--             { onPress = Just <| msg
--             , label = Element.text "Register"
--             }

displayCreateNewRankingBtn : Element Msg
displayCreateNewRankingBtn = 
    Input.button
            (Button.simple ++ Button.fill ++ Color.info)
        <|
            { onPress = Just ClickedCreateNewLadder
            , label = Element.text "Create New Ladder"
            }

displayEnableEthereumBtn : Element Msg
displayEnableEthereumBtn = 
    Input.button
            (Button.simple ++ Button.fill ++ Color.warning ++ [ Element.htmlAttribute (Html.Attributes.id "enableEthereumButton") ] ++ [ Element.htmlAttribute (Html.Attributes.class "enableEthereumButton") ])
        <|
            { onPress = Just ClickedEnableEthereum
            , label = Element.text "Enable Ethereum"
            }

selectedView : Data.Selected.Selected -> SR.Types.Selected -> Html Msg
selectedView s viewType =
    Framework.responsiveLayout [] <| Element.column
        Framework.container
        [ 
            SR.Elements.selectedRankingHeaderEl s
            , Element.text "\n"
            , infoBtn "Cancel" Cancel
            , Element.text "\n"
            , case viewType of 
                SR.Types.Owned ->
                    infoBtn "Delete" ClickedDeleteRanking
                SR.Types.Member ->
                    infoBtn "Leave" Cancel
                SR.Types.Other ->
                    Element.text "Other"
                SR.Types.CreatingChallenge ->
                    Element.text "CreatingChallenge"
                SR.Types.ConfirmedChallenge ->
                    Element.text "ConfirmedChallenge"
                SR.Types.EnteringResult ->
                    Element.text "EnteringResult"
  -- should ResultOfMatch be in a view type?
                SR.Types.ConfirmedResult ->
                    Element.text "ConfirmedResult"
                    -- don't currently know how to use one viewType:
             , case viewType of 
                SR.Types.Owned ->
                    playerbuttons s
                SR.Types.Member ->
                    playerbuttons s
                SR.Types.Other ->
                    playerbuttons s
                SR.Types.CreatingChallenge ->
                    Element.text "CreatingChallenge"
                SR.Types.ConfirmedChallenge ->
                    Element.text "ConfirmedChallenge"
                SR.Types.EnteringResult ->
                    Element.text "EnteringResult"
  -- should ResultOfMatch be in a view type?
                SR.Types.ConfirmedResult ->
                    Element.text "ConfirmedResult"
        ]

--         Updated sUsers sRankings dKind -> 
--             case dKind of 
--                 Selected sSelected ->
--                     case user of
--                             Data.Users.Spectator userInfo userState ->
--                                 Framework.responsiveLayout [] <| Element.column Framework.container
--                                     [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - No User13" 
--                                     , Element.text <| "No User17"
--                                     ]
--                             (Data.Users.Registered userInfo userState) ->
--                                 Framework.responsiveLayout [] <| Element.column Framework.container
--                                     [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ userInfo.username
--                                     , Element.text <| "Challenge is On! Good luck!"
--                                     , selecteduserIsPlayerHomebutton appInfo.user
--                                     , playerbuttons dataState appInfo
--                                     ]
--                             (Data.Users.NoWallet userInfo userState) ->
--                                 Framework.responsiveLayout [] <| Element.column Framework.container
--                                     [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ userInfo.username
--                                     , Element.text <| "Challenge is On! Good luck!"
--                                     , selecteduserIsPlayerHomebutton appInfo.user
--                                     , playerbuttons dataState appInfo
--                                     ]
--                             (Data.Users.NoCredit userInfo userState) ->
--                                 Framework.responsiveLayout [] <| Element.column Framework.container
--                                     [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ userInfo.username
--                                     , Element.text <| "Challenge is On! Good luck!"
--                                     , selecteduserIsPlayerHomebutton appInfo.user
--                                     , playerbuttons dataState appInfo
--                                     ]
--                             (Data.Users.Credited userInfo userState) ->
--                                 Framework.responsiveLayout [] <| Element.column Framework.container
--                                     [ Element.el Heading.h4 <| Element.text <| "SportRank - Player - " ++ userInfo.username
--                                     , Element.text <| "Challenge is On! Good luck!"
--                                     , selecteduserIsPlayerHomebutton appInfo.user
--                                     , playerbuttons dataState appInfo
--                                     ]
                                    
--                 _ ->
--                     Html.text "Error4"

--         AllEmpty ->
--                     Html.text "Please refresh your browser"




-- updateExistingUserView : Model -> Html Msg
-- updateExistingUserView model =
--     case model of
--         AppOps dataState user uiState txRec ->
--             case dataState of 
--                 Fetched sUsers sRankings dKind -> 
--                     case user of
--                         Nothing ->
--                             Html.text "No User18"
--                         Just userVal ->
--                             Framework.responsiveLayout [] <|
--                                 Element.column
--                                     Framework.container
--                                     [ Element.el Heading.h4 <| Element.text "Update User Profile"
--                                     , inputUpdateExistingUser model
--                                     , existingUserConfirmPanel userVal (Data.Users.asList sUsers)
--                                     ]
--                 _ ->
--                     Html.text "Fail updateExistingUserView"
--         _ ->
--             Html.text "Fail updateExistingUserView"


inputNewLadderview : Data.Rankings.Rankings -> Data.Rankings.Ranking -> Data.Users.User -> Html Msg
inputNewLadderview sRankings ranking user =
            Framework.responsiveLayout [] <| Element.column Framework.container
                [ Element.el Heading.h4 <| Element.text "Create New Ladder Ranking"
                , Element.column Grid.section <|
                    [ Element.el Heading.h6 <| Element.text "New Ladder Details"
                    , Element.wrappedRow (Card.fill ++ Grid.simple)
                        [ Element.column Grid.simple
                            [ Input.text Input.simple
                                { onChange = LadderNameInputChg
                                , text = ranking.rankingname
                                , placeholder = Nothing
                                , label = Input.labelLeft Input.label <| Element.text "Name*:"
                                }
                            , ladderNameValidation ranking sRankings
                                , Input.multiline Input.simple 
                                    {onChange = LadderDescInputChg
                                    , text =  Utils.Validation.Validate.validatedMaxTextLength (Maybe.withDefault "" ranking.rankingdesc) 20
                                    , placeholder = Nothing
                                    , label = Input.labelLeft Input.label <| Element.text "Desc:"
                                    , spellcheck = False
                                    }
                            , Element.text "* Required"
                            , ladderDescValidation ranking
                            ]
                        ]
                    ]
                , Element.column Grid.section <|
                    [ Element.el Heading.h6 <| Element.text "Click to continue ..."
                    , Element.column (Card.simple ++ Grid.simple) <|
                        [ Element.wrappedRow Grid.simple <|
                            [infoBtn "Cancel" Cancel
                            , Input.button (Button.simple ++ enableButton (isValidatedForAllLadderDetailsInput ranking sRankings)) <|
                                { onPress = Just <| ClickedConfirmCreateNewRanking ranking
                                , label = Element.text "Confirm"
                                }
                            ]
                        ]
                    ]
                ,
                -- nb. the footer couldn't be added to the e.g. warningParagraph
                -- each element needs it's own comma (,) and then pattern match
                case user of 
                    (Data.Users.Spectator _ _) ->
                        Element.text "" 
                
                    (Data.Users.Registered userInfo _) ->
                        case userInfo.walletState of 
                            Data.Users.Credited _ ->
                                SR.Elements.warningParagraph
                            _ ->
                                 Element.text "" 
                
                 , SR.Elements.footer
                ]

-- deleteRankingview : Model -> Html Msg
-- deleteRankingview model =
--     case model of
--         AppOps dataState user uiState txRec ->
--             Framework.responsiveLayout [] <|
--                 Element.column
--                     Framework.container
--                     [ Element.el Heading.h4 <| Element.text "Delete Ranking"
--                     --, inputNewLadder appInfo dataState
--                     , confirmDelRankingBtn appInfo dataState
--                     --, newrankingconfirmbutton appInfo dataState
--                     , SR.Elements.footer
--                     ]

--         _ ->
--             Html.text "Fail"



displayChallengeBeforeConfirmView : Model -> Html Msg
displayChallengeBeforeConfirmView model =
    case model of
        AppOps dataState user uiState txRec ->
            case user of
                Data.Users.Spectator userInfo userState ->
                    Html.text "No User19"
                (Data.Users.Registered userInfo userState) ->
                    Framework.responsiveLayout [] <| Element.column Framework.container
                    [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " - Confirm Challenge"
                    , confirmChallengebutton model
                    ]

        _ ->
            Html.text "Error5"


displayResultBeforeConfirmView : Model -> Html Msg
displayResultBeforeConfirmView model =
    -- todo: fix
    Html.text "Error displayResultBeforeConfirmView"
    -- case model of
    --     AppOps dataState user uiState txRec ->
    --         case dataState of
    --             Fetched sUsers sRankings (Selected sSelected) -> 
    --                 let
    --                     m_playerAsUser = Data.Users.gotUser sUsers appInfo.player.player.uid
    --                 in
    --                     case m_playerAsUser of
    --                         Nothing ->
    --                             Html.text "No Player"
    --                         Just playerasuser ->
    --                             case playerasuser of 
    --                                 Data.Users.Spectator userInfo userState ->
    --                                     Html.text "No Player"
    --                                 (Data.Users.Registered userInfo userState) ->
    --                                     Framework.responsiveLayout [] <| Element.column Framework.container
    --                                         [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " - Result"
    --                                         , confirmResultbutton model
    --                                         ]
    --                                 (Data.Users.NoWallet userInfo userState) ->
    --                                     Framework.responsiveLayout [] <| Element.column Framework.container
    --                                         [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " - Result"
    --                                         , confirmResultbutton model
    --                                         ]
    --                                 (Data.Users.NoCredit userInfo userState) ->
    --                                     Framework.responsiveLayout [] <| Element.column Framework.container
    --                                         [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " - Result"
    --                                         , confirmResultbutton model
    --                                         ]
    --                                 (Data.Users.Credited userInfo userState) ->
    --                                     Framework.responsiveLayout [] <| Element.column Framework.container
    --                                         [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " - Result"
    --                                         , confirmResultbutton model
    --                                         ]
                                
    --             _ ->
    --                 Html.text "Error6.2"
    --     _ ->
    --         Html.text "Error6.2"


txErrorView : Model -> Html Msg
txErrorView model =
-- todo: fix
    Html.text "Error txErrorView"
    -- case model of
    --     AppOps dataState user uiState txRec ->
    --         let
    --             m_playerAsUser =
    --                 --SR.ListOps.gotUserFromUserList (EverySet.fromList dataState) appInfo.player.player.uid
    --                 case dataState of 
    --                     Fetched users rankings dKind ->
    --                         Data.Users.gotUser users appInfo.player.player.uid
    --                     _ ->
    --                         Data.Users.gotUser Data.Users.empty appInfo.player.player.uid

    --         in
    --             case m_playerAsUser of 
    --                 Nothing ->
    --                     Framework.responsiveLayout [] <|
    --                         Element.column
    --                             Framework.container
    --                             [ Element.el Heading.h4 <| Element.text " Transaction Error"
    --                             , acknoweldgeTxErrorbtn model
    --                             ]

    --                 Just playerAsUser ->
    --                     case playerAsUser of 
    --                         Data.Users.Spectator userInfo userState ->
    --                             Framework.responsiveLayout [] <| Element.column Framework.container
    --                                 [ Element.el Heading.h4 <| Element.text " Transaction Error"
    --                                 , acknoweldgeTxErrorbtn model
    --                                 ]

    --                         (Data.Users.Registered userInfo userState) ->
    --                             Framework.responsiveLayout [] <| Element.column Framework.container
    --                                 [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " Transaction Error"
    --                                 , acknoweldgeTxErrorbtn model
    --                                 ]
    --                         (Data.Users.NoWallet userInfo userState) ->
    --                             Framework.responsiveLayout [] <| Element.column Framework.container
    --                                 [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " Transaction Error"
    --                                 , acknoweldgeTxErrorbtn model
    --                                 ]
    --                         (Data.Users.NoCredit userInfo userState) ->
    --                             Framework.responsiveLayout [] <| Element.column Framework.container
    --                                 [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " Transaction Error"
    --                                 , acknoweldgeTxErrorbtn model
    --                                 ]
    --                         (Data.Users.Credited userInfo userState) ->
    --                             Framework.responsiveLayout [] <| Element.column Framework.container
    --                                 [ Element.el Heading.h4 <| Element.text <| userInfo.username ++ " Transaction Error"
    --                                 , acknoweldgeTxErrorbtn model
    --                                 ]
                        

    --     _ ->
    --         Html.text "Error7"


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
        AppOps (Fetched sUsers sRankings dKind)
            (Data.Users.Registered userInfo userState) 
                uiState txRec ->
                    case userInfo.walletState of
                        Data.Users.Credited _ -> 
                            Sub.batch
                                [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
                                , Eth.Sentry.Tx.listen txRec.txSentry
                                ]
                        _ ->
                            Sub.none
        _ ->
            Sub.none


--Helper functions



-- Http ops

addedUserAsFirstPlayerInNewList : Data.Users.User -> Cmd Msg
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
    --               , ( "challengerid", Json.Encode.string "" )
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


createNewUser : Data.Users.Users -> Data.Users.User -> Cmd Msg
createNewUser sUsers newuserinfo =
    Cmd.none
    -- let
    --     newUserToAdd : Data.Users.User
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

httpPutRequestForAddGlobal : Json.Encode.Value -> List Data.Rankings.Ranking -> Cmd Msg
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
--     Updated sUsers sRankings dKind -> 
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
--                         , url = SR.Constants.jsonbinUrlStubForUpdateExistingBinAndRespond ++ (Data.Rankings.stringFromRankingId rnkId)
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
    


httpUpdateUsersJoinRankings : String -> Data.Users.User -> List Data.Users.User -> Cmd Msg
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


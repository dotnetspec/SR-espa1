port module Ports exposing (..)

import Json.Encode as Json exposing (Value)
    -- ( EthNode
    -- , Model
    -- , Msg(..)
    -- , ethNode
    -- , init
    -- , main
    -- , subscriptions
    -- , txIn
    -- , txOut
    -- , update
    -- , view
    -- , viewThing
    -- , walletSentry
    -- )

import Eth
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (..)
import Eth.Units exposing (gwei)
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Value)
import Process
import Task



type alias Model =
    { txSentry : TxSentry Msg
    , account : Maybe Address
    , node : EthNode
    , blockNumber : Maybe Int
    , txHash : Maybe TxHash
    , tx : Maybe Tx
    , txReceipt : Maybe TxReceipt
    , blockDepth : Maybe TxTracker
    , errors : List String
    , incomingData : String
    }


init : Int -> ( Model, Cmd Msg )
init networkId =
    let
        node =
            --Net.toNetworkId networkId
            --currently hardcode
            Net.toNetworkId 4
                |> ethNode
    in
    ( { txSentry = TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
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
    , Task.attempt PollBlock (Eth.getBlockNumber node.http)
    )


type alias EthNode =
    { http : HttpProvider
    , ws : WebsocketProvider
    }


ethNode : NetworkId -> EthNode
ethNode networkId =
    case networkId of
        Mainnet ->
            EthNode "https://mainnet.infura.io/" "wss://mainnet.infura.io/ws"

        Ropsten ->
            EthNode "https://ropsten.infura.io/" "wss://ropsten.infura.io/ws"

        Rinkeby ->
            EthNode "https://rinkeby.infura.io/" "wss://rinkeby.infura.io/ws"

        _ ->
            EthNode "UnknownEthNetwork" "UnknownEthNetwork"


type Msg
    = TxSentryMsg TxSentry.Msg
    | WalletStatus WalletSentry
    | PollBlock (Result Http.Error Int)
    | InitTx
    | WatchTxHash (Result String TxHash)
    | WatchTx (Result String Tx)
    | WatchTxReceipt (Result String TxReceipt)
    | TrackTx TxTracker
    | Fail String
    | NoOp
    | ReceivedDataFromJS String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TxSentryMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( { model | txSentry = subModel }, subCmd )

        WalletStatus walletSentry_ ->
            ( { model
                | account = walletSentry_.account
                , node = ethNode walletSentry_.networkId
              }
            , Cmd.none
            )

        PollBlock (Ok blockNumber) ->
            ( { model | blockNumber = Just blockNumber }
            , Task.attempt PollBlock <|
                Task.andThen (\_ -> Eth.getBlockNumber model.node.http) (Process.sleep 1000)
            )

        PollBlock (Err error) ->
            ( model, Cmd.none )

        InitTx ->
            let
                txParams =
                    { to = model.account
                    , from = model.account
                    , gas = Nothing
                    , gasPrice = Just <| gwei 4
                    , value = Just <| gwei 1
                    , data = Nothing
                    , nonce = Nothing
                    }

                ( newSentry, sentryCmd ) =
                    TxSentry.customSend
                        model.txSentry
                        { onSign = Just WatchTxHash
                        , onBroadcast = Just WatchTx
                        , onMined = Just ( WatchTxReceipt, Just { confirmations = 3, toMsg = TrackTx } )
                        }
                        txParams
            in
            ( { model | txSentry = newSentry }, sentryCmd )

        WatchTxHash (Ok txHash) ->
            ( { model | txHash = Just txHash }, Cmd.none )

        WatchTxHash (Err err) ->
            ( { model | errors = ("Error Retrieving TxHash: " ++ err) :: model.errors }, Cmd.none )

        WatchTx (Ok tx) ->
            ( { model | tx = Just tx }, Cmd.none )

        WatchTx (Err err) ->
            ( { model | errors = ("Error Retrieving Tx: " ++ err) :: model.errors }, Cmd.none )

        WatchTxReceipt (Ok txReceipt) ->
            ( { model | txReceipt = Just txReceipt }, Cmd.none )

        WatchTxReceipt (Err err) ->
            ( { model | errors = ("Error Retrieving TxReceipt: " ++ err) :: model.errors }, Cmd.none )

        TrackTx blockDepth ->
            ( { model | blockDepth = Just blockDepth }, Cmd.none )

        Fail str ->
            let
                _ =
                    Debug.log str
            in
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ReceivedDataFromJS data ->
            let
                _ =
                    Debug.log "data is: " data
            in
                ( { model | incomingData = data }, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ incoming decodeValue
        , walletSentry (WalletSentry.decodeToMsg Fail WalletStatus)
        , TxSentry.listen model.txSentry
        ]

--decodeValue just used to help understanding of incoming port functionality
decodeValue : Value -> Msg
decodeValue x =
    let
        result =
            Decode.decodeValue Decode.string x
    in
        case result of
            Ok string ->
                ReceivedDataFromJS string            
            Err _ -> 
                ReceivedDataFromJS "Silly JavaScript, you can't kill me!"



-- Ports
-- Test ports

port incoming : (Value -> msg) -> Sub msg

port outgoing : { action : String, data : Json.Value } -> Cmd msg


--web3 ports

port walletSentry : (Value -> msg) -> Sub msg

port output : Value -> Cmd msg

port input : (Value -> msg) -> Sub msg

port txOut : Value -> Cmd msg

port txIn : (Value -> msg) -> Sub msg



-- Helpers


maybeToString : (a -> String) -> String -> Maybe a -> String
maybeToString toString onNothing mVal =
    case mVal of
        Nothing ->
            onNothing

        Just a ->
            toString a


boolToString : Bool -> String
boolToString b =
 if b == True then
      "True"

  else if b == False then
      "False"

  else
      "There was a problem converting this string!!"


log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = Json.string message
        }

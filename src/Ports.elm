port module Ports exposing (..)

import Json.Decode exposing (Value)
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

--import Browser exposing (document)
import Eth
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (..)
import Eth.Units exposing (gwei)
import Eth.Utils
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Process
import Task





-- this Model is just to get test incoming working


-- type alias Model =
--     String



--elm-spa example port



log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = Json.string message
        }



-- main : Program Int Model Msg
-- main =
--     Browser.element
--         { init = init
--         , view = view
--         , update = update
--         , subscriptions = subscriptions
--         }


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



-- View


-- view : Model -> Html Msg
-- view model =
--     div []
--         [ div []
--             (List.map viewThing
--                 [ ( "Current Block", maybeToString String.fromInt "No blocknumber found yet" model.blockNumber )
--                 , ( "--------------------", "" )
--                 , ( "TxHash", maybeToString Eth.Utils.txHashToString "No TxHash yet" model.txHash )
--                 ]
--             )
--         , viewTxTracker model.blockDepth
--         , div [] [ button [ onClick InitTx ] [ text "Send 0 value Tx to yourself as a test" ] ]
--         , div [] (List.map (\e -> div [] [ text e ]) model.errors)
--         ]


-- viewThing : ( String, String ) -> Html Msg
-- viewThing ( name, val ) =
--     div []
--         [ div [] [ text name ]
--         , div [] [ text val ]
--         ]



-- Update


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
        [ incoming (decodeValue)
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
-- test incoming port

--port incoming : (Model -> msg) -> Sub msg
port incoming : (Value -> msg) -> Sub msg

port outgoing : { action : String, data : Json.Value } -> Cmd msg


--web3 ports


port walletSentry : (Value -> msg) -> Sub msg


port output : Value -> Cmd msg


port input : (Value -> msg) -> Sub msg


--port input : (Model -> Msg) -> Sub msg
--port input : { action : String, data : Json.Value } -> Sub msg


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


-- viewTxTracker : Maybe TxTracker -> Html msg
-- viewTxTracker mTxTracker =
--     case mTxTracker of
--         Nothing ->
--             text "Waiting for tx to be sent or mined...."

--         Just txTracker ->
--             [ " TxTracker"
--             , "    { currentDepth : " ++ String.fromInt txTracker.currentDepth
--             , "    , minedInBlock : " ++ String.fromInt txTracker.minedInBlock
--             , "    , stopWatchingAtBlock : " ++ String.fromInt txTracker.stopWatchingAtBlock
--             , "    , lastCheckedBlock : " ++ String.fromInt txTracker.lastCheckedBlock
--             , "    , txHash : " ++ Eth.Utils.txHashToString txTracker.txHash
--             , "    , doneWatching : " ++ boolToString txTracker.doneWatching
--             , "    , reOrg : " ++ boolToString txTracker.reOrg
--             , "    }"
--             , ""
--             ]
--                 |> List.map (\n -> div [] [ text n ])
--                 |> div []


boolToString : Bool -> String
boolToString b =
    case b of
        True ->
            "True"

        False ->
            "False"

module Global exposing
    ( EthNode
    , Flags
    , Model
    , Msg(..)
    , ethNode
    , init
    , subscriptions
    , update
    )

import Eth
import Eth.Net as EthNet exposing (NetworkId(..))
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (..)
import Eth.Units exposing (gwei)
import Eth.Utils
import Generated.Routes as Routes exposing (Route)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Ports exposing (..)
import Process
import Task



--import Utils.Spa exposing (Page, PageContext)


type alias Flags =
    Int



--type alias Model =
--    { --browserEnv : BrowserEnv
--      --, settings : Maybe SettingsData
--    }


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



-- the orig init from Main.elm
--init : Int -> ( Model, Cmd Msg )
--init networkId =
--    let
--        node =
--            Net.toNetworkId networkId
--                |> ethNode
--    in
--    ( { txSentry = TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
--      , account = Nothing
--      , node = node
--      , blockNumber = Nothing
--      , txHash = Nothing
--      , tx = Nothing
--      , txReceipt = Nothing
--      , blockDepth = Nothing
--      , errors = []
--      }
--    , Task.attempt PollBlock (Eth.getBlockNumber node.http)
--    )


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    --{ --  browserEnv = Nothing
    --    --, settings = Nothing
    --  }
    let
        node =
            --Net.toNetworkId networkId
            --hard code networkId for now
            EthNet.toNetworkId 4
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
    , Cmd.none
    , Ports.log "Hello there!"
    )



--subscriptions : Model -> Sub Msg
--subscriptions _ =
--    Sub.batch
--        [ --pageSubscriptions model.page
--          Ports.walletSentry (WalletSentry.decodeToMsg Fail WalletStatus)
--        --, EventSentry.listen model.eventSentry
--        --, TxSentry.listen model.txSentry
--        ]


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
      --| EventSentryMsg EventSentry.Msg
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



--update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
--update _ _ model =
--    ( model
--    , Cmd.none
--    , Cmd.none
--    )


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    case msg of
        TxSentryMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( { model | txSentry = subModel }, subCmd, Cmd.none )

        WalletStatus walletSentry_ ->
            ( { model
                | account = walletSentry_.account
                , node = ethNode walletSentry_.networkId
              }
            , Cmd.none
            , Cmd.none
            )

        PollBlock (Ok blockNumber) ->
            ( { model | blockNumber = Just blockNumber }
            , Task.attempt PollBlock <|
                Task.andThen (\_ -> Eth.getBlockNumber model.node.http) (Process.sleep 1000)
            , Cmd.none
            )

        PollBlock (Err error) ->
            ( model, Cmd.none, Cmd.none )

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
            ( { model | txSentry = newSentry }, sentryCmd, Cmd.none )

        WatchTxHash (Ok txHash) ->
            ( { model | txHash = Just txHash }, Cmd.none, Cmd.none )

        WatchTxHash (Err err) ->
            ( { model | errors = ("Error Retrieving TxHash: " ++ err) :: model.errors }, Cmd.none, Cmd.none )

        WatchTx (Ok tx) ->
            ( { model | tx = Just tx }, Cmd.none, Cmd.none )

        WatchTx (Err err) ->
            ( { model | errors = ("Error Retrieving Tx: " ++ err) :: model.errors }, Cmd.none, Cmd.none )

        WatchTxReceipt (Ok txReceipt) ->
            ( { model | txReceipt = Just txReceipt }, Cmd.none, Cmd.none )

        WatchTxReceipt (Err err) ->
            ( { model | errors = ("Error Retrieving TxReceipt: " ++ err) :: model.errors }, Cmd.none, Cmd.none )

        TrackTx blockDepth ->
            ( { model | blockDepth = Just blockDepth }, Cmd.none, Cmd.none )

        Fail str ->
            let
                _ =
                    Debug.log str
            in
            ( model, Cmd.none, Cmd.none )

        NoOp ->
            ( model, Cmd.none, Cmd.none )

        --update will update whatever needs to be updated
        --in this case, a port
        --ReceivedDataFromJS data ->
        --    ( data, Cmd.none, Cmd.none )
        ReceivedDataFromJS data ->
            let
                _ =
                    Debug.log "data is: " data
            in
            ( { model | incomingData = data }, Cmd.none, Cmd.none )



--Msg ->
--    ( model, Cmd.none, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ 
        Ports.incoming (decodeValue)
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


maybeToString : (a -> String) -> String -> Maybe a -> String
maybeToString toString onNothing mVal =
    case mVal of
        Nothing ->
            onNothing

        Just a ->
            toString a


boolToString : Bool -> String
boolToString b =
    case b of
        True ->
            "True"

        False ->
            "False"



--viewTxTracker : Maybe TxTracker -> Html msg
--viewTxTracker mTxTracker =
--    case mTxTracker of
--        Nothing ->
--            text "Hi there! Waiting for tx to be sent or mined...."
--        Just txTracker ->
--            [ " TxTracker"
--            , "    { currentDepth : " ++ String.fromInt txTracker.currentDepth
--            , "    , minedInBlock : " ++ String.fromInt txTracker.minedInBlock
--            , "    , stopWatchingAtBlock : " ++ String.fromInt txTracker.stopWatchingAtBlock
--            , "    , lastCheckedBlock : " ++ String.fromInt txTracker.lastCheckedBlock
--            , "    , txHash : " ++ Eth.Utils.txHashToString txTracker.txHash
--            , "    , doneWatching : " ++ boolToString txTracker.doneWatching
--            , "    , reOrg : " ++ boolToString txTracker.reOrg
--            , "    }"
--            , ""
--            ]
--                |> List.map (\n -> div [] [ text n ])
--                |> div []
-- View
--view : Model -> Html Msg
--view model =
--    div []
--        [ div []
--            (List.map viewThing
--                [ ( "Current Block", maybeToString String.fromInt "No blocknumber found yet" model.blockNumber )
--                , ( "--------------------", "" )
--                , ( "TxHash", maybeToString Eth.Utils.txHashToString "No TxHash yet" model.txHash )
--                ]
--            )
--        , viewTxTracker model.blockDepth
--        , div [] [ button [ onClick InitTx ] [ text "Send 0 value Tx to yourself as a test" ] ]
--        , div [] (List.map (\e -> div [] [ text e ]) model.errors)
--        ]
--viewThing : ( String, String ) -> Html Msg
--viewThing ( name, val ) =
--    div []
--        [ div [] [ text name ]
--        , div [] [ text val ]
--        ]
-- Update
--type Msg
--    = Msg
--      -- Port/Sub Related Msgs
--    | WalletStatus WalletSentry
--    | EventSentryMsg EventSentry.Msg
--    | TxSentryMsg TxSentry.Msg
--    | Fail String

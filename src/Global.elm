module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Generated.Routes as Routes exposing (Route)
import Ports exposing (..)
import Debug
import Json.Decode as Decode exposing (Value)
import Http
--import Eth
--import Eth.Net as EthNet exposing (NetworkId(..))

--import Eth.Sentry.Tx as TxSentry exposing (..)
--import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
--import Eth.Types exposing (..)
--import Eth.Units exposing (gwei)
--import Eth.Utils
--import Html exposing (..)
--import Html.Events exposing (onClick)
--import Process
--import Task


type alias Flags =
    ()


type alias Model =
    {
        --txSentry : TxSentry Msg
    incomingData : String}


type Msg
    = 
    --Msg
    -- TxSentryMsg TxSentry.Msg
    -- | 
    ReceivedDataFromJS String


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    --    let
    --     node =
    --         --Net.toNetworkId networkId
    --         --hard code networkId for now
    --         EthNet.toNetworkId 4
    --             |> ethNode 
    -- in
    ( {incomingData = ""}
    --, txSentry = TxSentry.init ( txOut, txIn ) TxSentryMsg node.http}
    , Cmd.none
    , Cmd.none
    )

-- type alias EthNode =
--     { http : HttpProvider
--     , ws : WebsocketProvider
--     }

-- ethNode : NetworkId -> EthNode
-- ethNode networkId =
--     case networkId of
--         Mainnet ->
--             EthNode "https://mainnet.infura.io/" "wss://mainnet.infura.io/ws"

--         Ropsten ->
--             EthNode "https://ropsten.infura.io/" "wss://ropsten.infura.io/ws"

--         Rinkeby ->
--             EthNode "https://rinkeby.infura.io/" "wss://rinkeby.infura.io/ws"

--         _ ->
--             EthNode "UnknownEthNetwork" "UnknownEthNetwork"


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    case msg of
    -- TxSentryMsg subMsg ->
    --         let
    --             ( subModel, subCmd ) =
    --                 TxSentry.update subMsg model.txSentry
    --         in
    --         ( { model | txSentry = subModel }, subCmd, Cmd.none )

    ReceivedDataFromJS data ->
            let
                _ =
                    Debug.log "data is: " data
            in
            ( { model | incomingData = data }, Cmd.none, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    incoming decodeValue

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

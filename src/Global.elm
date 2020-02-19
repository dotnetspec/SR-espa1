--standard elm-spa file + handle js messages
-- currently using both flags and port to receive data from js
module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Generated.Routes as Routes exposing (Route)
import Ports
import Debug
import Json.Decode as Decode exposing (Value)
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Utils
import Eth.Net as Net exposing (NetworkId(..))
import List
import Utils.MyUtils


type alias Flags =
      {networkid : Int
      , useraccounts: List String
      , comment : String}


type alias Model =
    {
    username : String
    , accountForUserLookup : String
    , account : Maybe Eth.Types.Address
    , node : Ports.EthNode
    , incomingData : String}


type Msg =  
    ReceivedDataFromJS String
    | WalletStatus Eth.Sentry.Wallet.WalletSentry
    | Fail String
    --| GotJSAddress (Result String Eth.Types.Address)


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ flags =
            let
                --dataStrVal = String.fromInt flags.networkid ++ flags.comment
                -- have to take this as a flag from js - global.context is too slow for Top init
                dataStrVal = Utils.MyUtils.stringFromMaybeString (List.head flags.useraccounts) 

                _ =
                    Debug.log "flag data is : " dataStrVal
                node =
                    --Net.toNetworkId networkId
                    Net.toNetworkId 4
                        |> Ports.ethNode

                --accountNumber = Utils.MyUtils.stringFromMaybeString (List.head flags.useraccounts) 
                --     Ports.retrieveAddress Ports.Model
            in
        ( { 
            username = tempAddressToNameLookup  dataStrVal
        , accountForUserLookup = dataStrVal
        , account = Nothing
        , node = node
        , incomingData = String.fromInt flags.networkid
        }
            , Cmd.none
            , Cmd.none
            )


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    case msg of
    ReceivedDataFromJS data ->
            let
                _ =
                    Debug.log "data is: " data

                networkName = if data == "4" then "I see you are using Rinkeby" else "Not sure which network you are on"
            in
            ( { model | incomingData = networkName }, Cmd.none, Cmd.none)

    WalletStatus walletSentry_ ->
        ( { model
            | account = walletSentry_.account
            , node = Ports.ethNode walletSentry_.networkId
            }
        , Cmd.none, Cmd.none
        )


    Fail str ->
                    let
                        _ =
                            Debug.log str
                    in
                    ( model, Cmd.none, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
    --incoming decodeValue
    --   Sub.batch
    --     [ incoming decodeValue
    --     , Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
    --     --, Eth.Sentry.Tx.listen model.txSentry
    --     ]

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



tempAddressToNameLookup : String -> String 
tempAddressToNameLookup str = 
    if str == "0x847700b781667abdd98e1393420754e503dca5b7" then "Philip" else "New User"

-- gotResultFromJSAddress : Result error value -> Msg 
-- gotResultFromJSAddress result = 
--     case result of 
--         Err error -> 
--              GotJSAddress error
--         Ok string ->
--             GotJSAddress string

-- just used below to help to understand Result type and it's args
-- isReasonableAge : Maybe String -> Result String String
-- --isReasonableAge : Result String String
-- isReasonableAge str =
--   case str of
--     Nothing ->
--         Err "error"

--     Just addr ->
--         Ok addr
            


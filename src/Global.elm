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



tempAddressToNameLookup : String -> String 
tempAddressToNameLookup str = 
    if str == "0x847700b781667abdd98e1393420754e503dca5b7" then "Philip" else "New User"

            


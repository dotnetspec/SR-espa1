--standard elm-spa file + handle js messages
-- currently using both flags and port to receive data from js


module Global exposing
    (  Flags
       --, Model(GlobalVariant(..), Failure(..))

    , Model(..)
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Debug
import Eth.Defaults
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Utils
import Generated.Routes as Routes exposing (Route)
import Internal.Types
import List
import Ports
import SR.Types
import Utils.MyUtils


type alias Flags =
    { networkid : Int
    , useraccounts : List String
    , comment : String
    }



-- type alias Model =
--     {
--     username : String
--     , accountForUserLookup : String
--     , account : Maybe Eth.Types.Address
--     , node : Ports.EthNode
--     , incomingData : String}


type
    Model
    --= GlobalVariant Eth.Sentry.Wallet.WalletSentry SR.Types.Username Eth.Types.Address
    = GlobalVariant SR.Types.WalletState SR.Types.UserState
    | Failure String


type Msg
    = -- ReceivedDataFromJS String
      WalletStatus Eth.Sentry.Wallet.WalletSentry
    | Fail String



--| GotJSAddress (Result String Eth.Types.Address)


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ flags =
    let
        --         --dataStrVal = String.fromInt flags.networkid ++ flags.comment
        --         -- have to take this as a flag from js - global.context is too slow for Top init
        --         dataStrVal = Utils.MyUtils.stringFromMaybeString (List.head flags.useraccounts)
        --         _ =
        --             Debug.log "flag data is : " dataStrVal
        node =
            --Net.toNetworkId networkId
            Net.toNetworkId 4
                |> Ports.ethNode

        --         --accountNumber = Utils.MyUtils.stringFromMaybeString (List.head flags.useraccounts)
        --         --     Ports.retrieveAddress Ports.Model
    in
    -- ( {
    --     username = tempAddressToNameLookup  dataStrVal
    -- , accountForUserLookup = dataStrVal
    -- , account = Nothing
    -- , node = node
    -- , incomingData = String.fromInt flags.networkid
    --}
    ( GlobalVariant SR.Types.Missing (SR.Types.ExistingUser "Philip")
    , Cmd.none
    , Cmd.none
    )



-- Update needs to take two things: a message Msg (which
-- is a description of the transition that needs to happen),
--  and the model Model (which is the model before the update is applied),
--  and it will return a new model.
-- branching on Model (just a reflection of current state)
-- is branching on something you don't need to change
-- (but you might further down that section of case(s).
-- Only branching on Msg will actually change a value.


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    let
        --     walletSentry = {
        --         account = walletSentry_.account
        --         , node = Ports.ethNode walletSentry_.networkId
        --     }
        addressFromMaybeAddress addr =
            case addr of
                Nothing ->
                    Eth.Defaults.invalidAddress

                Just a ->
                    a
    in
    case msg of
        -- ReceivedDataFromJS data ->
        --     let
        --         _ =
        --             Debug.log "data is: " data
        --         networkName =
        --             if data == "4" then
        --                 "I see you are using Rinkeby"
        --             else
        --                 "Not sure which network you are on"
        --     in
        --     ( { model | incomingData = networkName }, Cmd.none, Cmd.none )
        -- walletSentry_ consists of an account number and a networkId
        WalletStatus walletSentry_ ->
            -- let
            --     walletSentry =
            --         { account = walletSentry_.account
            --         , node = Ports.ethNode walletSentry_.networkId
            --         }
            -- in
            case walletSentry_.account of
                Nothing ->
                    let
                        _ =
                            Debug.log "Nothing" walletSentry_.account
                    in
                    ( GlobalVariant SR.Types.Missing SR.Types.NewUser, Cmd.none, Cmd.none )

                Just a ->
                    let
                        _ =
                            Debug.log "Just a" a
                    in
                    ( GlobalVariant SR.Types.Opened
                        (SR.Types.ExistingUser (tempAddressToNameLookup (Eth.Utils.addressToString a)))
                    , Cmd.none
                    , Cmd.none
                    )

        -- case walletState of
        --         SR.Types.Missing ->
        --             case userState of
        --                 -- perhaps makes no diff if new/existing user here[?]
        --                 SR.Types.NewUser ->
        --                     case msg of
        --                         GetAWallet ->
        --                             ( Greeting SR.Types.MissingWalletDialogOpen SR.Types.NewUser SR.Types.Missing, Cmd.none, Cmd.none )
        --                         _ ->
        --                             ( Failure "unexpected message received while wallet missing, new user state", Cmd.none, Cmd.none )
        --                 SR.Types.ExistingUser ->
        --                     case msg of
        --                         GetAWallet ->
        --                             ( Greeting SR.Types.MissingWalletDialogOpen SR.Types.ExistingUser SR.Types.Missing, Cmd.none, Cmd.none )
        --                         _ ->
        --                             ( Failure "unexpected message received while wallet missing, existing user state", Cmd.none, Cmd.none )
        --         SR.Types.Locked ->
        --             case uiState of
        --                 SR.Types.DialogClosed ->
        --                     case msg of
        --                         OpenWalletInstructions ->
        --                             ( Greeting SR.Types.MissingWalletDialogOpen SR.Types.NewUser SR.Types.Locked, Cmd.none, Cmd.none )
        --                         _ ->
        --                             ( Failure "unexpected message received while wallet locked, new user state", Cmd.none, Cmd.none )
        --                 _ ->
        --                     ( Greeting SR.Types.LockedWalletDialogOpen SR.Types.NewUser SR.Types.Locked, Cmd.none, Cmd.none )
        --         SR.Types.Opened ->
        --             case userState of
        --                 SR.Types.NewUser ->
        --                     case msg of
        --                         CloseDialog ->
        --                             case uiState of
        --                                 SR.Types.MissingWalletDialogOpen ->
        --                                     ( Greeting SR.Types.DialogClosed SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )
        --                                 _ ->
        --                                     ( Greeting SR.Types.DialogClosed SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )
        --                         _ ->
        --                             ( Greeting SR.Types.DialogClosed SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )
        --                 SR.Types.ExistingUser ->
        --                     case msg of
        --                         CloseDialog ->
        --                             ( Greeting SR.Types.DialogClosed SR.Types.ExistingUser SR.Types.Opened, Cmd.none, Cmd.none )
        --                         _ ->
        --                             ( Greeting SR.Types.DialogClosed SR.Types.ExistingUser SR.Types.Opened, Cmd.none, Cmd.none )
        Fail str ->
            let
                _ =
                    Debug.log "Failed" str
            in
            ( model, Cmd.none, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)


tempAddressToNameLookup : String -> String
tempAddressToNameLookup str =
    if str == "0x847700b781667abdd98e1393420754e503dca5b7" then
        "Philip"

    else
        "New User"

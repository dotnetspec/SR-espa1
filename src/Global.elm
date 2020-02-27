--standard elm-spa file. If needed to handle js messages could do it here
-- currently using port to receive data from wallet


module Global exposing
    ( Flags
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


type Model
    = GlobalVariant SR.Types.WalletState SR.Types.UserState
    | Failure String


type Msg
    = WalletStatus Eth.Sentry.Wallet.WalletSentry
    | Fail String


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    ( GlobalVariant SR.Types.Missing SR.Types.NewUser
    , Cmd.none
    , Cmd.none
    )



-- Update needs to take two things: a message Msg (which
-- is a description of the transition that needs to happen),
--  and the model Model (which is the model before the update is applied),
--  and it will return a new model.
-- branching on Model is just a reflection of current state
-- Only branching on Msg will actually change a value or the state


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    case msg of
        WalletStatus walletSentry_ ->
            case walletSentry_.networkId of
                Mainnet ->
                    case walletSentry_.account of
                        Nothing ->
                            let
                                _ =
                                    Debug.log "You're on Mainnet - Locked!!!!!!" walletSentry_.account
                            in
                            ( GlobalVariant SR.Types.Locked SR.Types.NewUser, Cmd.none, Cmd.none )

                        Just a ->
                            let
                                _ =
                                    Debug.log "You're on Mainnet!!!!!!" a
                            in
                            ( GlobalVariant SR.Types.Opened
                                (SR.Types.ExistingUser (tempAddressToNameLookup (Eth.Utils.addressToString a)))
                            , Cmd.none
                            , Cmd.none
                            )

                Rinkeby ->
                    case walletSentry_.account of
                        Nothing ->
                            let
                                _ =
                                    Debug.log "Rinkeby Locked" walletSentry_.account
                            in
                            ( GlobalVariant SR.Types.Locked SR.Types.NewUser, Cmd.none, Cmd.none )

                        Just a ->
                            let
                                _ =
                                    Debug.log "Rinkeby" a
                            in
                            ( GlobalVariant SR.Types.Opened
                                (SR.Types.ExistingUser (tempAddressToNameLookup (Eth.Utils.addressToString a)))
                            , Cmd.none
                            , Cmd.none
                            )

                _ ->
                    ( GlobalVariant SR.Types.Missing SR.Types.NewUser, Cmd.none, Cmd.none )

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

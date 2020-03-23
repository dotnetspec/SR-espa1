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

--n.b 'as Routes' alias was rm here:

import Eth.Net exposing (NetworkId(..))
import Eth.Sentry.Wallet
import Eth.Types
import Generated.Routes exposing (Route)
import Ports
import SR.Types



-- if you needed to you could define a GlobalModel variant here
-- that could be accessed via a PageContext which might be defined as context.global


type Model
    = GlobalModel SR.Types.UserState
    | Failure String


type alias Flags =
    {}


type Msg
    = Fail String
    | NoOp
      --| GotExistingUserAddressInGlobal SR.Types.UserState
    | WalletStatus Eth.Sentry.Wallet.WalletSentry


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    ( Failure ""
      --GlobalModel (SR.Types.ExistingUser (Eth.Types.Address "0x847700B781667abdD98E1393420754E503dca5b7"))
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
        -- GotExistingUserAddressInGlobal useraddress ->
        --     ( GlobalModel useraddress, Cmd.none, Cmd.none )
        WalletStatus walletSentry_ ->
            case walletSentry_.networkId of
                Mainnet ->
                    case walletSentry_.account of
                        Nothing ->
                            ( GlobalModel SR.Types.NewUser, Cmd.none, Cmd.none )

                        Just useraddress ->
                            --handleMsg (SR.Types.NewUser a)
                            ( GlobalModel (SR.Types.ExistingUser useraddress), Cmd.none, Cmd.none )

                Rinkeby ->
                    case walletSentry_.account of
                        Nothing ->
                            ( GlobalModel SR.Types.NewUser, Cmd.none, Cmd.none )

                        Just useraddress ->
                            --handleMsg (SR.Types.ExistingUser a)
                            ( GlobalModel (SR.Types.ExistingUser useraddress), Cmd.none, Cmd.none )

                _ ->
                    ( GlobalModel SR.Types.NewUser, Cmd.none, Cmd.none )

        Fail str ->
            ( Failure str, Cmd.none, Cmd.none )

        NoOp ->
            ( Failure "NoOp", Cmd.none, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    --Sub.none
    Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)

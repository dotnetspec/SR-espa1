module Pages.Top exposing (Model, Msg, page)

--'Homepage' of the app. Gathers username and networkid to determine how to handle user

import Dialog
import Element exposing (..)
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Utils
import Generated.Params as Params
import Global exposing (..)
import Ports
import SR.Types
import Spa.Page
import Task
import Ui
import Utils.Spa exposing (Page)


page : Page Params.Top Model Msg model msg appMsg
page =
    Spa.Page.component
        { title = always "Top"
        , init = init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
        }


type Model
    = Greeting SR.Types.UserState SR.Types.WalletState
    | Failure String


init : Utils.Spa.PageContext -> Params.Top -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    ( Greeting SR.Types.NewUser SR.Types.Missing
    , Cmd.none
    , Cmd.none
    )



-- UPDATE
-- Msg is a description of the transition that needs to happen


type Msg
    = GetAWalletInstructions
    | OpenWalletInstructions
    | CloseDialog
    | NewUser
    | ExistingUser Eth.Types.Address
    | WalletStatus Eth.Sentry.Wallet.WalletSentry
    | Fail String
    | NoOp



-- Update needs to take two things: a message (which
-- is a description of the transition that needs to happen),
--  and the model (which is the model before the update is applied),
--  and it will return a new model.


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        WalletStatus walletSentry_ ->
            case walletSentry_.networkId of
                Mainnet ->
                    case walletSentry_.account of
                        Nothing ->
                            handleMsg OpenWalletInstructions

                        Just a ->
                            handleMsg (ExistingUser a)

                Rinkeby ->
                    case walletSentry_.account of
                        Nothing ->
                            handleMsg OpenWalletInstructions

                        Just a ->
                            handleMsg (ExistingUser a)

                _ ->
                    handleMsg GetAWalletInstructions

        GetAWalletInstructions ->
            ( Greeting SR.Types.NewUser SR.Types.Missing, Cmd.none, Cmd.none )

        OpenWalletInstructions ->
            ( Greeting SR.Types.NewUser SR.Types.Locked, Cmd.none, Cmd.none )

        NewUser ->
            ( Greeting SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )

        ExistingUser uname ->
            --( Greeting SR.Types.DialogClosed (SR.Types.ExistingUser a) SR.Types.Opened, Cmd.none, Cmd.none )
            ( Greeting (SR.Types.ExistingUser uname) SR.Types.Opened, Cmd.none, Cmd.none )

        Fail str ->
            ( Failure str, Cmd.none, Cmd.none )

        NoOp ->
            ( Failure "NoOp", Cmd.none, Cmd.none )

        _ ->
            ( Failure "Something wrong with Msg", Cmd.none, Cmd.none )


handleMsg : Msg -> ( Model, Cmd Msg, Cmd Global.Msg )
handleMsg msg =
    case msg of
        GetAWalletInstructions ->
            ( Greeting SR.Types.NewUser SR.Types.Missing, Cmd.none, Cmd.none )

        OpenWalletInstructions ->
            ( Greeting SR.Types.NewUser SR.Types.Locked, Cmd.none, Cmd.none )

        NewUser ->
            ( Greeting SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )

        ExistingUser uname ->
            ( Greeting (SR.Types.ExistingUser uname) SR.Types.Opened, Cmd.none, Cmd.none )

        _ ->
            ( Failure "Something wrong with Msg", Cmd.none, Cmd.none )


failure : String -> Element Msg
failure message =
    Element.text message


tempAddressToNameLookup : String -> String
tempAddressToNameLookup str =
    if str == "0x847700b781667abdd98e1393420754e503dca5b7" then
        "Philip"

    else
        "New User"


addressToString : Maybe Eth.Types.Address -> String
addressToString addr =
    case addr of
        Nothing ->
            "No address"

        Just a ->
            Eth.Utils.addressToString a



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)


view : Utils.Spa.PageContext -> Model -> Element Msg
view context model =
    let
        -- currently Dialog module not compatible with Elm 0.19.1!
        config msg =
            { closeMessage = Just msg
            , maskAttributes = []
            , containerAttributes = [ centerX, centerY, padding 10 ]
            , headerAttributes = []
            , bodyAttributes = []
            , footerAttributes = []
            , header = Just (text "Hello world")
            , body = Just modalBody
            , footer = Nothing
            }
    in
    case model of
        Failure message ->
            failure message

        Greeting userState walletState ->
            case walletState of
                SR.Types.Locked ->
                    Element.el [ inFront (Dialog.view (Just (config OpenWalletInstructions))) ]
                        (Ui.hero
                            { title = "SPORTRANK", description = "Hello New User", buttons = [ ( "Wallet Locked ...", "/" ) ] }
                        )

                SR.Types.Missing ->
                    Element.el [ inFront (Dialog.view (Just (config GetAWalletInstructions))) ]
                        (Ui.hero
                            { title = "SPORTRANK", description = "Hello New User", buttons = [ ( "Missing Wallet ...", "/" ) ] }
                        )

                SR.Types.Opened ->
                    case userState of
                        SR.Types.NewUser ->
                            Ui.hero { title = "SPORTRANK", description = "Hello New User", buttons = [ ( "Register ...", "/" ) ] }

                        SR.Types.ExistingUser a ->
                            Ui.hero { title = "SPORTRANK", description = "Welcome Back " ++ tempAddressToNameLookup (Eth.Utils.addressToString a), buttons = [ ( "Continue ...", "/rankings" ) ] }


modalBody : Element Msg
modalBody =
    Element.text "hi there"

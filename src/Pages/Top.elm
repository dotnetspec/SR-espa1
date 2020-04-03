module Pages.Top exposing (Model, Msg, page)

--'Homepage' of the app. Gathers username and networkid to determine how to handle user

import Dialog
import Element exposing (..)
import Eth.Net exposing (NetworkId(..))
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Utils
import Generated.Params as Params
import Global
import Http
import Ports
import RemoteData
import SR.Constants
import SR.Decode
import SR.Defaults
import SR.Types
import Spa.Page
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


type
    Model
    --= Greeting SR.Types.UserListState SR.Types.UserState SR.Types.WalletState
    = Greeting SR.Types.UserState SR.Types.WalletState
    | Failure String


init : Utils.Spa.PageContext -> Params.Top -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    ( --Greeting SR.Types.Loading SR.Types.NewUser SR.Types.Missing
      Greeting SR.Types.NewUser SR.Types.Missing
    , gotPlayersList
    , Cmd.none
    )



-- UPDATE
-- Msg is a description of the transition that needed to happen


type Msg
    = GetAWalletInstructions
    | OpenWalletInstructions
    | CloseDialog
    | NewUser
    | ExistingUser Eth.Types.Address
    | WalletStatus Eth.Sentry.Wallet.WalletSentry
    | GotJsonbinUsers (RemoteData.WebData (List SR.Types.User))
    | Fail String
    | NoOp



-- Update needs to take two things: a message (which
-- is a description of the transition that needed to happen),
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
            ( Greeting (SR.Types.ExistingUser uname) SR.Types.Opened, Cmd.none, Cmd.none )

        GotJsonbinUsers rmtdata ->
            case rmtdata of
                RemoteData.Success a ->
                    case model of
                        Greeting b c ->
                            ( Greeting b c, Cmd.none, Cmd.none )

                        Failure str ->
                            ( Failure str, Cmd.none, Cmd.none )

                RemoteData.Failure e ->
                    case model of
                        Greeting b c ->
                            ( Greeting b c, Cmd.none, Cmd.none )

                        Failure str ->
                            ( Failure str, Cmd.none, Cmd.none )

                RemoteData.NotAsked ->
                    case model of
                        Greeting b c ->
                            ( Greeting b c, Cmd.none, Cmd.none )

                        Failure str ->
                            ( Failure str, Cmd.none, Cmd.none )

                RemoteData.Loading ->
                    case model of
                        Greeting b c ->
                            ( Greeting b c, Cmd.none, Cmd.none )

                        Failure str ->
                            ( Failure str, Cmd.none, Cmd.none )

        Fail str ->
            ( Failure str, Cmd.none, Cmd.none )

        NoOp ->
            ( Failure "NoOp", Cmd.none, Cmd.none )

        _ ->
            ( Failure "Msg Err - is it list as a variant?", Cmd.none, Cmd.none )



-- it might not be enough to just use RemoteData.Loading here
-- it might overwrite the model ref to List Players


handleMsg : Msg -> ( Model, Cmd Msg, Cmd Global.Msg )
handleMsg msg =
    case msg of
        GetAWalletInstructions ->
            ( Greeting SR.Types.NewUser SR.Types.Missing, Cmd.none, Cmd.none )

        OpenWalletInstructions ->
            ( Greeting SR.Types.NewUser SR.Types.Locked, Cmd.none, Cmd.none )

        NewUser ->
            ( Greeting SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )

        ExistingUser uaddr ->
            ( Greeting (SR.Types.ExistingUser uaddr) SR.Types.Opened, Cmd.none, Cmd.none )

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
            , containerAttributes = []
            , headerAttributes = []
            , bodyAttributes = []
            , footerAttributes = []
            , header = Just (text "Wallet Missing/Locked")
            , body = Just modalBody
            , footer = Nothing
            }
    in
    case model of
        Failure message ->
            failure message

        -- Greeting userList userState walletState ->
        Greeting userState walletState ->
            -- let
            --     -- usrlist =
            --     --     gotUserListFromRemData (SR.Types.Success userList)
            --     _ =
            --         Debug.log "user list : " userList
            -- in
            case walletState of
                SR.Types.Locked ->
                    Element.el [ centerX, centerY, inFront (Dialog.view (Just (config OpenWalletInstructions))) ]
                        (Ui.hero
                            { title = "SPORTRANK", description = "Hello New User", buttons = [ ( "Wallet Locked ...", "/" ) ] }
                        )

                SR.Types.Missing ->
                    Element.el [ centerX, centerY, inFront (Dialog.view (Just (config GetAWalletInstructions))) ]
                        (Ui.hero
                            { title = "SPORTRANK", description = "Hello New User", buttons = [ ( "Missing Wallet ...", "/" ) ] }
                        )

                SR.Types.Opened ->
                    case userState of
                        SR.Types.NewUser ->
                            Ui.hero { title = "SPORTRANK", description = "Hello New User. Please click to register", buttons = [ ( "Register ...", "/" ) ] }

                        SR.Types.ExistingUser a ->
                            Ui.hero { title = "SPORTRANK", description = "Welcome Back " ++ tempAddressToNameLookup (Eth.Utils.addressToString a), buttons = [ ( "Continue ...", "/rankings" ) ] }


modalBody : Element Msg
modalBody =
    Element.text "Please install and unlock \nan Ethereum wallet extension in \nyour browser to continue"



-- Http ops


gotPlayersList : Cmd Msg
gotPlayersList =
    let
        secretKey =
            Http.header
                "secret-key"
                "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"

        binName =
            Http.header
                "name"
                "Users"

        containerId =
            Http.header
                "collection-id"
                "5e4cf4ba4d073155b0dca8b8"
    in
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> GotJsonbinUsers) SR.Decode.listOfUsersDecoder
        , headers = [ secretKey, binName, containerId ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUsersReadLink
        }


gotUserListFromRemData : RemoteData.WebData (List SR.Types.User) -> List SR.Types.User
gotUserListFromRemData userList =
    case userList of
        RemoteData.Success a ->
            a

        RemoteData.NotAsked ->
            [ SR.Defaults.emptyUser
            ]

        RemoteData.Loading ->
            [ SR.Defaults.emptyUser
            ]

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.Timeout ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.NetworkError ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.BadStatus statuscode ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.BadBody s ->
                    [ SR.Defaults.emptyUser
                    ]

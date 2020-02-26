--'Homepage' of the app. Gathers username and networkid to determine how to handle user


module Pages.Top exposing (Model, Msg, page)

import Dialog
import Element exposing (..)
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Utils
import Generated.Params as Params
import Global
import Ports
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



-- INIT


type UserAccountConnection
    = NoConnection
    | NoAccountNumber
    | NoUserDetails
    | Success


type Model
    = Greeting SR.Types.UIState SR.Types.UserState SR.Types.WalletState
    | Failure String



-- type alias Model =
--     { account : Maybe Eth.Types.Address
--     , node : Ports.EthNode
--     , user : User
--     , showDialog : Bool
--     }
--until can figure a better way to return account to Address type
--use string so can do a lookup


type alias User =
    { name : String
    , description : String
    , contact : String
    , email : String
    , active : Bool
    , datestamp : Int
    }


type alias Config msg =
    { closeMessage : Maybe msg
    , maskAttributes : List (Attribute msg)
    , containerAttributes : List (Attribute msg)
    , headerAttributes : List (Attribute msg)
    , bodyAttributes : List (Attribute msg)
    , footerAttributes : List (Attribute msg)
    , header : Maybe (Element msg)
    , body : Maybe (Element msg)
    , footer : Maybe (Element msg)
    }


init : Utils.Spa.PageContext -> Params.Top -> ( Model, Cmd Msg, Cmd Global.Msg )
init context _ =
    --     let
    --         node =
    --             --Net.toNetworkId networkId
    --             Net.toNetworkId 4
    --                 |> Ports.ethNode
    --         _ =
    --             Debug.log "global context" context.global.accountForUserLookup
    --         -- the account number has to come through as a js value as update or [?] is too slow in global.elm
    --         -- for init
    --     in
    ( Greeting SR.Types.LockedWalletDialogOpen SR.Types.NewUser SR.Types.Opened
      --{
      --account = Nothing
      --       , node = node
      --       , user =
      --             { name = ""
      --             , description = ""
      --             , contact = ""
      --             , email = ""
      --             , active = False
      --             , datestamp = 123456
      --             }
      --       , showDialog = False
      --}
    , Cmd.none
    , Cmd.none
    )



-- UPDATE
-- Msg is a description of the transition that needs to happen


type Msg
    = --WalletStatus Eth.Sentry.Wallet.WalletSentry
      --       Fail String
      --     | NoOp
      --     | CloseDialog
      --     | GotUserStatus UserAccountConnection
      --     | Missing
      --     | Unlock
      --     | Open
      GetAWallet
    | OpenWallet
    | CloseDialog


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case model of
        Greeting uiState userState walletState ->
            case walletState of
                SR.Types.Missing ->
                    case userState of
                        -- perhaps makes no diff if new/existing user here[?]
                        SR.Types.NewUser ->
                            case msg of
                                GetAWallet ->
                                    ( Greeting SR.Types.MissingWalletDialogOpen SR.Types.NewUser SR.Types.Missing, Cmd.none, Cmd.none )

                                _ ->
                                    ( Failure "unexpected message received while wallet missing, new user state", Cmd.none, Cmd.none )

                        SR.Types.ExistingUser ->
                            case msg of
                                GetAWallet ->
                                    ( Greeting SR.Types.MissingWalletDialogOpen SR.Types.ExistingUser SR.Types.Missing, Cmd.none, Cmd.none )

                                _ ->
                                    ( Failure "unexpected message received while wallet missing, existing user state", Cmd.none, Cmd.none )

                SR.Types.Locked ->
                    case msg of
                        OpenWallet ->
                            case uiState of
                                SR.Types.DialogClosed ->
                                    case msg of
                                        OpenWallet ->
                                            ( Greeting SR.Types.MissingWalletDialogOpen SR.Types.NewUser SR.Types.Locked, Cmd.none, Cmd.none )

                                        _ ->
                                            ( Failure "unexpected message received while wallet locked, new user state", Cmd.none, Cmd.none )

                                _ ->
                                    ( Greeting SR.Types.LockedWalletDialogOpen SR.Types.NewUser SR.Types.Locked, Cmd.none, Cmd.none )

                        _ ->
                            ( Failure "unexpected message received while wallet was in Locked state", Cmd.none, Cmd.none )

                SR.Types.Opened ->
                    case userState of
                        -- perhaps makes no diff if new/existing user here[?]
                        SR.Types.NewUser ->
                            case msg of
                                CloseDialog ->
                                    case uiState of
                                        SR.Types.MissingWalletDialogOpen ->
                                            ( Greeting SR.Types.DialogClosed SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )

                                        _ ->
                                            ( Greeting SR.Types.DialogClosed SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )

                                _ ->
                                    ( Greeting SR.Types.DialogClosed SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )

                        SR.Types.ExistingUser ->
                            case msg of
                                CloseDialog ->
                                    ( Greeting SR.Types.DialogClosed SR.Types.ExistingUser SR.Types.Opened, Cmd.none, Cmd.none )

                                _ ->
                                    ( Greeting SR.Types.DialogClosed SR.Types.ExistingUser SR.Types.Opened, Cmd.none, Cmd.none )

                --Transaction to be confirmed if nec
                SR.Types.Transaction ->
                    case msg of
                        OpenWallet ->
                            case uiState of
                                SR.Types.DialogClosed ->
                                    ( Greeting SR.Types.DialogClosed SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )

                                _ ->
                                    ( Greeting SR.Types.DialogClosed SR.Types.NewUser SR.Types.Opened, Cmd.none, Cmd.none )

                        _ ->
                            ( Failure "unexpected message received while wallet was in Transaction state", Cmd.none, Cmd.none )

        Failure errorMessage ->
            ( model, Cmd.none, Cmd.none )



-- case msg of
--     GotUserStatus acctconn ->
--         case acctconn of
--             NoConnection ->
--                 ( { model
--                     | showDialog = True
--                   }
--                 , Cmd.none
--                 , Cmd.none
--                 )
--             NoAccountNumber ->
--                 ( { model
--                     | showDialog = True
--                   }
--                 , Cmd.none
--                 , Cmd.none
--                 )
--             NoUserDetails ->
--                 ( { model
--                     | showDialog = True
--                   }
--                 , Cmd.none
--                 , Cmd.none
--                 )
--             Success ->
--                 ( { model
--                     | showDialog = False
--                   }
--                 , Cmd.none
--                 , Cmd.none
--                 )
--     CloseDialog ->
--         ( { model
--             | showDialog = False
--           }
--         , Cmd.none
--         , Cmd.none
--         )
--     Fail str ->
--         let
--             _ =
--                 Debug.log str
--         in
--         ( model, Cmd.none, Cmd.none )
--     NoOp ->
--         ( model, Cmd.none, Cmd.none )
-- determineUserStatus: Model -> Msg
-- determineUserStatus model =
--     if model.account == Nothing then
--         GotUserStatus NoAccountNumber
--     else
--         GotUserStatus Success
--TODO: re-factor - repeated code - to MyUtils?


failure message =
    Element.text "A failure!"


addressToString : Maybe Eth.Types.Address -> String
addressToString addr =
    case addr of
        Nothing ->
            "No address"

        Just a ->
            Eth.Utils.addressToString a



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Utils.Spa.PageContext -> Model -> Element Msg
view context model =
    let
        config =
            { closeMessage = Just CloseDialog
            , maskAttributes = []
            , containerAttributes = [ centerX, centerY, padding 10 ]
            , headerAttributes = []
            , bodyAttributes = []
            , footerAttributes = []
            , header = Just (text "Hello world")
            , body = Just modalBody
            , footer = Nothing
            }

        -- dialogConfig =
        --     if model.showDialog then
        --         Just config
        --     else
        --         Nothing
        descTxt =
            "Welcome " ++ context.global.username
    in
    case model of
        Failure message ->
            failure message

        --Greeting SR.Types.UIState SR.Types.UserState SR.Types.WalletState
        Greeting uiState userState walletState ->
            case uiState of
                SR.Types.LockedWalletDialogOpen ->
                    Element.el [ inFront (Dialog.view (Just config)) ]
                        (Ui.hero
                            { title = "SPORTRANK", description = descTxt, buttons = [ ( "Continue ...", "/rankings" ) ] }
                        )

                _ ->
                    --text "nothing"
                    --Element.el [ inFront (Dialog.view dialogConfig) ]
                    Ui.hero { title = "SPORTRANK", description = descTxt, buttons = [ ( "Continue ...", "/rankings" ) ] }


modalBody : Element Msg
modalBody =
    Element.text "hi there"

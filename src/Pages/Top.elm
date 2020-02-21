--'Homepage' of the app. Gathers username and networkid to determine how to handle user
module Pages.Top exposing (Model, Msg, page)

import Spa.Page
import Element exposing (..)
import Generated.Params as Params
import Global
import Utils.Spa exposing (Page)
import Ui
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Utils
import Ports
import Eth.Net as Net exposing (NetworkId(..))
import Dialog


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

type UserAccountConnection = 
    NoConnection
    | NoAccountNumber
    | NoUserDetails
    | Success


type alias Model =
    {  
    account : Maybe Eth.Types.Address
    , node : Ports.EthNode
    , user : User
    , showDialog : Bool
    }

--until can figure a better way to return account to Address type
--use string so can do a lookup
type alias User = 
    {  
    name : String
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
  let
        node =
            --Net.toNetworkId networkId
            Net.toNetworkId 4
                |> Ports.ethNode
        _ = 
            Debug.log "global context" context.global.accountForUserLookup
        -- the account number has to come through as a js value as update or [?] is too slow in global.elm
        -- for init
    in
    ( { 
    account = Nothing
    , node = node
    , user =  {  
            name = ""
            , description = ""
            , contact = "" 
            , email = ""
            , active = False
            , datestamp = 123456
        } 
    , showDialog = False
    }
    , Cmd.none
    , Cmd.none
    )



-- UPDATE


type Msg
    = 
    --WalletStatus Eth.Sentry.Wallet.WalletSentry
     Fail String
    | NoOp
    | CloseDialog
    | GotUserStatus UserAccountConnection


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
            case msg of
                GotUserStatus acctconn ->
                     case acctconn of
                        NoConnection ->
                            ( { model
                                    | showDialog = True
                                }
                                , Cmd.none, Cmd.none
                                )

                        NoAccountNumber ->
                            ( { model
                                | showDialog = True
                            }
                            , Cmd.none, Cmd.none
                            )

                        NoUserDetails ->
                            ( { model
                                | showDialog = True
                            }
                            , Cmd.none, Cmd.none
                            )

                        Success -> 
                            ( { model
                                | showDialog = False
                            }
                            , Cmd.none, Cmd.none
                            )

                CloseDialog ->
                            ( { model
                                | showDialog = False
                            }
                            , Cmd.none, Cmd.none
                            )

                 
                Fail str ->
                    let
                        _ =
                            Debug.log str
                    in
                    ( model, Cmd.none, Cmd.none )


                NoOp ->
                            ( model, Cmd.none, Cmd.none )
           

-- determineUserStatus: Model -> Msg 
-- determineUserStatus model = 
--     if model.account == Nothing then 
--         GotUserStatus NoAccountNumber
--     else
--         GotUserStatus Success

--TODO: re-factor - repeated code - to MyUtils?
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
        --Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
  

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

            dialogConfig =
                if model.showDialog then
                    Just config

                else
                    Nothing

            descTxt = "Welcome " ++ context.global.username
        in
    
        Element.el [ inFront (Dialog.view dialogConfig)]
            (Ui.hero {  title = "SPORTRANK", description = descTxt, buttons = [ ( "Continue ...", "/rankings" ) ] })

modalBody: Element Msg 
modalBody = 
    Element.text "hi there"
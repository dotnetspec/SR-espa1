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
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { account : Maybe Eth.Types.Address
    , node : Ports.EthNode
    , user : User
    , showDialog : Bool 
    }

type alias User = 
    { 
    account : Maybe Eth.Types.Address
    , name : String
    , description : String
    , contact : String 
    , email : String 
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

init : Params.Top -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
  let
        node =
            --Net.toNetworkId networkId
            Net.toNetworkId 4
                |> Ports.ethNode
    in
    ( { account = Nothing
    , node = node
    , user =  { 
            account = Nothing
            , name = ""
            , description = ""
            , contact = "" 
            , email = "" 
        } 
    , showDialog = False
    }
    , Cmd.none
    , Cmd.none
    )



-- UPDATE


type Msg
    = WalletStatus Eth.Sentry.Wallet.WalletSentry
    | Fail String
    | NoOp
    | CloseDialog


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
            case msg of
                WalletStatus walletSentry_ ->
                            ( { model
                                | account = walletSentry_.account
                                , node = Ports.ethNode walletSentry_.networkId
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
           

--TODO: re-factor - repeated code - to MyUtils?
validateAddress : Maybe Eth.Types.Address -> String
validateAddress addr =
  case addr of
    Nothing ->
      "No address"

    Just a ->
       Eth.Utils.addressToString a

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
        Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
  

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
        in
    --Element.layout
        
        Element.el [ inFront (Dialog.view dialogConfig)] 
        --(Element.text "hello")
       (Ui.hero {  title = "SPORTRANK", description = "Context " ++ context.global.incomingData ++ validateAddress model.account, buttons = [ ( "Continue ...", "/rankings" ) ] })

modalBody: Element Msg 
modalBody = 
    Element.text "hi there"
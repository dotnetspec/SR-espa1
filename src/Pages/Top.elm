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
    , node : Ports.EthNode }


init : Params.Top -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
  let
        node =
            --Net.toNetworkId networkId
            Net.toNetworkId 4
                |> Ports.ethNode
    in
    ( { account = Nothing
    , node = node }
    , Cmd.none
    , Cmd.none
    )



-- UPDATE


type Msg
    = WalletStatus Eth.Sentry.Wallet.WalletSentry
    | Fail String
    | NoOp


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

                 
                Fail str ->
                    let
                        _ =
                            Debug.log str
                    in
                    ( model, Cmd.none, Cmd.none )


                NoOp ->
                            ( model, Cmd.none, Cmd.none )
           

--TODO: re-factor - repeated code - 
validateAddress : Maybe Eth.Types.Address -> String
validateAddress addr =
  case addr of
    Nothing ->
      "No address"

    Just a ->
       Eth.Utils.addressToString a

-- SUBSCRIPTIONS


-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.none

subscriptions : Model -> Sub Msg
subscriptions model =
        Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)


-- VIEW
view : Utils.Spa.PageContext -> Model -> Element Msg
view context model =
    Ui.hero {  title = "SPORTRANK", description = "Context " ++ context.global.incomingData ++ validateAddress model.account, buttons = [ ( "Continue ...", "/rankings" ) ] }
    --context.global.incomingData
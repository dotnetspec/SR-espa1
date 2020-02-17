module Pages.Top exposing (Model, Msg, page)

import Spa.Page
import Element exposing (..)
import Generated.Params as Params
import Global
import Utils.Spa exposing (Page)
import Ui


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
    {}


init : Params.Top -> ( Model, Cmd Msg, Cmd Global.Msg )
init _ =
    ( {}
    , Cmd.none
    , Cmd.none
    )



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    ( model
    , Cmd.none
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
view : Utils.Spa.PageContext -> Model -> Element Msg
view context model =
    Ui.hero {  title = "SPORTRANK", description = "Context " ++ context.global.incomingData, buttons = [ ( "Continue ...", "/rankings" ) ] }
    --context.global.incomingData
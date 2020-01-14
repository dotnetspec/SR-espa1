module Pages.Docs.Dynamic.Dynamic exposing (Model, Msg, page)

import Element exposing (..)
import Generated.Docs.Dynamic.Params as Params
import Spa.Page
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Docs.Dynamic.Dynamic"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- INIT


type alias Model =
    { section : String
    , page : String
    }


init : Params.Dynamic -> ( Model, Cmd Msg )
init { param1, param2 } =
    ( { section = param1
      , page = param2
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Element Msg
view model =
    text (model.section ++ ": " ++ model.page)

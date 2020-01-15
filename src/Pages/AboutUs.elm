module Pages.AboutUs exposing (Model, Msg, page)

import Element exposing (..)
import Generated.Params as Params
import Spa.Page
import Utils.Spa exposing (Page)


type alias Model =
    ()


type alias Msg =
    Never


page : Page Params.AboutUs Model Msg model msg appMsg
page =
    Spa.Page.static
        { title = always "AboutUs"
        , view = always view
        }



-- VIEW


view : Element Msg
view =
    text "AboutUs"

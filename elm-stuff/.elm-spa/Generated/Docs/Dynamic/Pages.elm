module Generated.Docs.Dynamic.Pages exposing
    ( Model
    , Msg
    , page
    , path
    )

import Spa.Page
import Spa.Path exposing (Path, static, dynamic)
import Layouts.Docs.Dynamic as Layout
import Utils.Spa as Spa
import Generated.Docs.Dynamic.Params as Params
import Generated.Docs.Dynamic.Route as Route exposing (Route)
import Pages.Docs.Dynamic.Dynamic




type Model
    = DynamicModel Pages.Docs.Dynamic.Dynamic.Model


type Msg
    = DynamicMsg Pages.Docs.Dynamic.Dynamic.Msg


page : Spa.Page Route Model Msg layoutModel layoutMsg appMsg
page =
    Spa.layout
        { path = path
        , view = Layout.view
        , recipe =
            { init = init
            , update = update
            , bundle = bundle
            }
        }


path : Path
path =
    [ static "docs", dynamic ]


-- RECIPES


type alias Recipe flags model msg appMsg =
    Spa.Recipe flags model msg Model Msg appMsg


type alias Recipes msg =
    { dynamic : Recipe Params.Dynamic Pages.Docs.Dynamic.Dynamic.Model Pages.Docs.Dynamic.Dynamic.Msg msg
    }


recipes : Recipes msg
recipes =
    { dynamic =
        Spa.recipe
            { page = Pages.Docs.Dynamic.Dynamic.page
            , toModel = DynamicModel
            , toMsg = DynamicMsg
            }
    }



-- INIT


init : Route -> Spa.Init Model Msg
init route_ =
    case route_ of
        Route.Dynamic _ params ->
            recipes.dynamic.init params



-- UPDATE


update : Msg -> Model -> Spa.Update Model Msg
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( DynamicMsg msg, DynamicModel model ) ->
            recipes.dynamic.update msg model



-- BUNDLE


bundle : Model -> Spa.Bundle Msg msg
bundle bigModel =
    case bigModel of
        DynamicModel model ->
            recipes.dynamic.bundle model
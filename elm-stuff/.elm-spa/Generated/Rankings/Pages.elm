module Generated.Rankings.Pages exposing
    ( Model
    , Msg
    , page
    , path
    )

import Spa.Page
import Spa.Path exposing (Path, static, dynamic)
import Layouts.Rankings as Layout
import Utils.Spa as Spa
import Generated.Rankings.Params as Params
import Generated.Rankings.Route as Route exposing (Route)
import Pages.Rankings.Dynamic
import Pages.Rankings.Top




type Model
    = DynamicModel Pages.Rankings.Dynamic.Model
    | TopModel Pages.Rankings.Top.Model


type Msg
    = DynamicMsg Pages.Rankings.Dynamic.Msg
    | TopMsg Pages.Rankings.Top.Msg


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
    [ static "rankings" ]


-- RECIPES


type alias Recipe flags model msg appMsg =
    Spa.Recipe flags model msg Model Msg appMsg


type alias Recipes msg =
    { dynamic : Recipe Params.Dynamic Pages.Rankings.Dynamic.Model Pages.Rankings.Dynamic.Msg msg
    , top : Recipe Params.Top Pages.Rankings.Top.Model Pages.Rankings.Top.Msg msg
    }


recipes : Recipes msg
recipes =
    { dynamic =
        Spa.recipe
            { page = Pages.Rankings.Dynamic.page
            , toModel = DynamicModel
            , toMsg = DynamicMsg
            }
    , top =
        Spa.recipe
            { page = Pages.Rankings.Top.page
            , toModel = TopModel
            , toMsg = TopMsg
            }
    }



-- INIT


init : Route -> Spa.Init Model Msg
init route_ =
    case route_ of
        Route.Top params ->
            recipes.top.init params
        
        Route.Dynamic _ params ->
            recipes.dynamic.init params



-- UPDATE


update : Msg -> Model -> Spa.Update Model Msg
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( TopMsg msg, TopModel model ) ->
            recipes.top.update msg model
        
        ( DynamicMsg msg, DynamicModel model ) ->
            recipes.dynamic.update msg model
        _ ->
            Spa.Page.keep bigModel


-- BUNDLE


bundle : Model -> Spa.Bundle Msg msg
bundle bigModel =
    case bigModel of
        TopModel model ->
            recipes.top.bundle model
        
        DynamicModel model ->
            recipes.dynamic.bundle model
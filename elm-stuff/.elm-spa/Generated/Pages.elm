module Generated.Pages exposing
    ( Model
    , Msg
    , page
    , path
    )

import Spa.Page
import Spa.Path exposing (Path, static, dynamic)
import Layout as Layout
import Utils.Spa as Spa
import Generated.Params as Params
import Generated.Route as Route exposing (Route)
import Pages.Guide
import Pages.NotFound
import Pages.Top
import Generated.Docs.Route
import Generated.Docs.Pages


type Model
    = GuideModel Pages.Guide.Model
    | NotFoundModel Pages.NotFound.Model
    | TopModel Pages.Top.Model
    | Docs_Folder_Model Generated.Docs.Pages.Model


type Msg
    = GuideMsg Pages.Guide.Msg
    | NotFoundMsg Pages.NotFound.Msg
    | TopMsg Pages.Top.Msg
    | Docs_Folder_Msg Generated.Docs.Pages.Msg


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
    []


-- RECIPES


type alias Recipe flags model msg appMsg =
    Spa.Recipe flags model msg Model Msg appMsg


type alias Recipes msg =
    { guide : Recipe Params.Guide Pages.Guide.Model Pages.Guide.Msg msg
    , notFound : Recipe Params.NotFound Pages.NotFound.Model Pages.NotFound.Msg msg
    , top : Recipe Params.Top Pages.Top.Model Pages.Top.Msg msg
    , docs_folder : Recipe Generated.Docs.Route.Route Generated.Docs.Pages.Model Generated.Docs.Pages.Msg msg
    }


recipes : Recipes msg
recipes =
    { guide =
        Spa.recipe
            { page = Pages.Guide.page
            , toModel = GuideModel
            , toMsg = GuideMsg
            }
    , notFound =
        Spa.recipe
            { page = Pages.NotFound.page
            , toModel = NotFoundModel
            , toMsg = NotFoundMsg
            }
    , top =
        Spa.recipe
            { page = Pages.Top.page
            , toModel = TopModel
            , toMsg = TopMsg
            }
    , docs_folder =
        Spa.recipe
            { page = Generated.Docs.Pages.page
            , toModel = Docs_Folder_Model
            , toMsg = Docs_Folder_Msg
            }
    }



-- INIT


init : Route -> Spa.Init Model Msg
init route_ =
    case route_ of
        Route.Guide params ->
            recipes.guide.init params
        
        Route.NotFound params ->
            recipes.notFound.init params
        
        Route.Top params ->
            recipes.top.init params
        
        Route.Docs_Folder route ->
            recipes.docs_folder.init route



-- UPDATE


update : Msg -> Model -> Spa.Update Model Msg
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( GuideMsg msg, GuideModel model ) ->
            recipes.guide.update msg model
        
        ( NotFoundMsg msg, NotFoundModel model ) ->
            recipes.notFound.update msg model
        
        ( TopMsg msg, TopModel model ) ->
            recipes.top.update msg model
        
        ( Docs_Folder_Msg msg, Docs_Folder_Model model ) ->
            recipes.docs_folder.update msg model
        _ ->
            Spa.Page.keep bigModel


-- BUNDLE


bundle : Model -> Spa.Bundle Msg msg
bundle bigModel =
    case bigModel of
        GuideModel model ->
            recipes.guide.bundle model
        
        NotFoundModel model ->
            recipes.notFound.bundle model
        
        TopModel model ->
            recipes.top.bundle model
        
        Docs_Folder_Model model ->
            recipes.docs_folder.bundle model
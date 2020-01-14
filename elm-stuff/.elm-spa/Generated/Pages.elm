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
import Pages.AboutUs
import Pages.Guide
import Pages.NotFound
import Pages.Top
import Generated.Docs.Route
import Generated.Rankings.Route
import Generated.Docs.Pages
import Generated.Rankings.Pages


type Model
    = AboutUsModel Pages.AboutUs.Model
    | GuideModel Pages.Guide.Model
    | NotFoundModel Pages.NotFound.Model
    | TopModel Pages.Top.Model
    | Docs_Folder_Model Generated.Docs.Pages.Model
    | Rankings_Folder_Model Generated.Rankings.Pages.Model


type Msg
    = AboutUsMsg Pages.AboutUs.Msg
    | GuideMsg Pages.Guide.Msg
    | NotFoundMsg Pages.NotFound.Msg
    | TopMsg Pages.Top.Msg
    | Docs_Folder_Msg Generated.Docs.Pages.Msg
    | Rankings_Folder_Msg Generated.Rankings.Pages.Msg


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
    { aboutUs : Recipe Params.AboutUs Pages.AboutUs.Model Pages.AboutUs.Msg msg
    , guide : Recipe Params.Guide Pages.Guide.Model Pages.Guide.Msg msg
    , notFound : Recipe Params.NotFound Pages.NotFound.Model Pages.NotFound.Msg msg
    , top : Recipe Params.Top Pages.Top.Model Pages.Top.Msg msg
    , docs_folder : Recipe Generated.Docs.Route.Route Generated.Docs.Pages.Model Generated.Docs.Pages.Msg msg
    , rankings_folder : Recipe Generated.Rankings.Route.Route Generated.Rankings.Pages.Model Generated.Rankings.Pages.Msg msg
    }


recipes : Recipes msg
recipes =
    { aboutUs =
        Spa.recipe
            { page = Pages.AboutUs.page
            , toModel = AboutUsModel
            , toMsg = AboutUsMsg
            }
    , guide =
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
    , rankings_folder =
        Spa.recipe
            { page = Generated.Rankings.Pages.page
            , toModel = Rankings_Folder_Model
            , toMsg = Rankings_Folder_Msg
            }
    }



-- INIT


init : Route -> Spa.Init Model Msg
init route_ =
    case route_ of
        Route.AboutUs params ->
            recipes.aboutUs.init params
        
        Route.Guide params ->
            recipes.guide.init params
        
        Route.NotFound params ->
            recipes.notFound.init params
        
        Route.Top params ->
            recipes.top.init params
        
        Route.Docs_Folder route ->
            recipes.docs_folder.init route
        
        Route.Rankings_Folder route ->
            recipes.rankings_folder.init route



-- UPDATE


update : Msg -> Model -> Spa.Update Model Msg
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( AboutUsMsg msg, AboutUsModel model ) ->
            recipes.aboutUs.update msg model
        
        ( GuideMsg msg, GuideModel model ) ->
            recipes.guide.update msg model
        
        ( NotFoundMsg msg, NotFoundModel model ) ->
            recipes.notFound.update msg model
        
        ( TopMsg msg, TopModel model ) ->
            recipes.top.update msg model
        
        ( Docs_Folder_Msg msg, Docs_Folder_Model model ) ->
            recipes.docs_folder.update msg model
        
        ( Rankings_Folder_Msg msg, Rankings_Folder_Model model ) ->
            recipes.rankings_folder.update msg model
        _ ->
            Spa.Page.keep bigModel


-- BUNDLE


bundle : Model -> Spa.Bundle Msg msg
bundle bigModel =
    case bigModel of
        AboutUsModel model ->
            recipes.aboutUs.bundle model
        
        GuideModel model ->
            recipes.guide.bundle model
        
        NotFoundModel model ->
            recipes.notFound.bundle model
        
        TopModel model ->
            recipes.top.bundle model
        
        Docs_Folder_Model model ->
            recipes.docs_folder.bundle model
        
        Rankings_Folder_Model model ->
            recipes.rankings_folder.bundle model
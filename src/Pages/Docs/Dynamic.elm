module Pages.Docs.Dynamic exposing (Model, Msg, page)

import Element exposing (..)
import Generated.Docs.Params as Params
import Http
import Spa.Page
import Ui
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Docs.Dynamic"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- INIT


type alias Model =
    { content : String }


init : Params.Dynamic -> ( Model, Cmd Msg )
init { param1 } =
    ( { content = "" }
    , Http.get
        { expect = Http.expectString FetchedContent

        --, url = "/content/docs/" ++ param1 ++ ".md"
        --, url = "https://api.jsonbin.io/b/5c36f5422c87fa27306acb52/latest"
        , url = "https://api.jsonbin.io/b/" ++ param1 ++ "/latest"
        }
    )



--Http.get
--   { url = "https://api.jsonbin.io/b/5c36f5422c87fa27306acb52/latest"
--   , expect =
--       rankingsDecoder
--           |> Http.expectJson (RemoteData.fromResult >> RankingsReceived)
--   }
-- UPDATE


type Msg
    = FetchedContent (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedContent (Ok markdown) ->
            ( { model | content = markdown }
            , Cmd.none
            )

        FetchedContent (Err _) ->
            ( { model | content = "there was an error" }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- display whatever is in the model


view : Model -> Element Msg
view model =
    Ui.markdown model.content

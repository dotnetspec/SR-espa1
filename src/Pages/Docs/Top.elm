module Pages.Docs.Top exposing (Model, Msg, page)

import Element exposing (..)
import Generated.Docs.Params as Params
import Http
import Spa.Page
import Ui exposing (colors, markdown)
import Utils.Spa exposing (Page)


page : Page Params.Top Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Docs.Top"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



-- INIT


type alias Model =
    { content : String }



--make an http request as a command on init


init : Params.Top -> ( Model, Cmd Msg )
init _ =
    ( { content = "" }
    , Http.get
        { expect = Http.expectString FetchedContent
        , url = "/content/docs.md"
        }
    )



-- UPDATE
-- the Msg that we call when we get our content


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


view : Model -> Element Msg
view model =
    Ui.markdown model.content

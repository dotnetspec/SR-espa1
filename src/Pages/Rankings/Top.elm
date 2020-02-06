module Pages.Rankings.Top exposing (Model, Msg, page)

import Components.Ranking exposing (Ranking, RankingId(..), rankingDecoder, rankingEncoder, rankingsDecoder)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as E
import Element.Font as Font
import Element.Input as I
import Element.Lazy
import Generated.Rankings.Params as Params
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import RemoteData exposing (RemoteData, WebData)
import Spa.Page
import Ui
import Utils.MyUtils exposing (stringFromBool)
import Utils.Spa exposing (Page)


page : Page Params.Top Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Rankings.Top"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }


type Msg
    = RankingsReceived (WebData (List Ranking))
    | FetchedContent (Result Http.Error String)
    | OpenModal
    | CloseModal


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type ModalState
    = Open
    | Closed


type alias Model =
    { rankings : WebData (List Ranking)
    , fetchedContentNotRankingList : String
    , error : String
    , modalState : ModalState
    }



-- INIT


init : Params.Top -> ( Model, Cmd Msg )
init _ =
    ( { rankings = RemoteData.NotAsked
      , error = ""
      , fetchedContentNotRankingList = ""
      , modalState = Closed
      }
    , Http.get
        { url = "https://api.jsonbin.io/b/5e2a585f593fd741856f4b04/latest"
        , expect =
            rankingsDecoder
                |> expectJson (RemoteData.fromResult >> RankingsReceived)
        }
    )


expectJson : (Result Http.Error a -> msg) -> Decode.Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Decode.errorToString err))



-- UPDATE
-- first update comes from init


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedContent (Ok fetchedContentNotRankingList) ->
            ( { model | fetchedContentNotRankingList = fetchedContentNotRankingList }
            , Cmd.none
            )

        FetchedContent (Err _) ->
            ( { model | error = "there was an error" }
            , Cmd.none
            )

        RankingsReceived rankings ->
            --removes[?] the first record (created on ranking creation with different format)
            ( { model | rankings = rankings }, Cmd.none )

        OpenModal ->
            ( { model | modalState = Open }, Cmd.none )

        CloseModal ->
            ( { model | modalState = Closed }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- display whatever is in the model


view : Model -> Element Msg
view model =
    Element.paragraph []
        [ viewRankingsOrError model
        ]


viewRankingsOrError : Model -> Element Msg
viewRankingsOrError model =
    case model.rankings of
        RemoteData.NotAsked ->
            Element.text ""

        RemoteData.Loading ->
            Element.text "Loading..."

        RemoteData.Success rankings ->
            viewRankings rankings

        RemoteData.Failure httpError ->
            Element.text "Failure"



--viewError (buildErrorMessage httpError)
--you might need this later
-- (stringFromBool ranking.active)


viewRankings : List Ranking -> Element Msg
viewRankings rankings =
    html <|
        Element.layout
            [ Element.padding 25
            , Background.color (rgba 0 0 0 1)
            , Font.color (rgba 1 1 1 1)

            --, Font.italic
            , Font.size 22
            , Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Roboto"
                    , name = "Roboto"
                    }
                , Font.sansSerif
                ]
            ]
        <|
            Element.table
                [ Element.padding 25
                , Background.color Ui.colors.white
                , Border.solid
                , Border.color Ui.colors.black
                , Border.widthXY 1 1
                , Border.rounded 3
                ]
                { data = rankings
                , columns =
                    [ rankingNameCol rankings "Ranking Name"
                    , rankingDescCol rankings "Ranking Desc"
                    ]
                }


rankingNameCol : List Ranking -> String -> Column Ranking msg
rankingNameCol rankings str =
    { header = Element.text str
    , width = fill
    , view =
        \ranking ->
            Element.row
                [ Font.color Ui.colors.lightblue
                , Border.widthXY 2 2
                ]
                [ Element.link
                    [ --Background.color Ui.colors.blue
                      Font.color Ui.colors.lightblue

                    --, padding 5
                    --, Border.widthXY 2 2
                    ]
                    { url = "/rankings/" ++ ranking.id
                    , label = Element.text ranking.name
                    }
                ]
    }


rankingDescCol : List Ranking -> String -> Column Ranking msg
rankingDescCol rankings str =
    { header = Element.text str
    , width = fill
    , view =
        \ranking ->
            Element.row
                [ Border.widthXY 2 2
                , Font.color Ui.colors.green
                ]
                [ Element.text ranking.desc
                ]
    }

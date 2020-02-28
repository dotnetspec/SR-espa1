module Pages.Rankings.Top exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Rankings.Params as Params
import Http
import Json.Decode
import RemoteData
import SR.Decode
import SR.Types
import Spa.Page
import Ui
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


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type Model
    = ListOfAllRankings (RemoteData.WebData (List SR.Types.Ranking)) String
    | FailureOnAllRankings String



-- INIT
-- this accesses COLLECTION RECORDS - GLOBAL - public bin


init : Params.Top -> ( Model, Cmd Msg )
init _ =
    ( ListOfAllRankings RemoteData.Loading ""
    , Http.get
        { url = "https://api.jsonbin.io/b/5e2a585f593fd741856f4b04/latest"
        , expect =
            SR.Decode.rankingsDecoder
                |> expectJson (RemoteData.fromResult >> FetchAllRankings)
        }
    )


expectJson : (Result Http.Error a -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
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
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Json.Decode.errorToString err))



-- Msg is a description of the transition that needs to happen


type Msg
    = FetchAllRankings (RemoteData.WebData (List SR.Types.Ranking))



-- UPDATE
-- Update needs to take two things: a message (which
-- is a description of the transition that needs to happen),
--  and the model (which is the model BEFORE the update is applied),
--  Update will return a new model for View to render


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchAllRankings rmtdata ->
            case rmtdata of
                --removes[?] the first record (created on ranking creation with different format)
                RemoteData.Success a ->
                    ( ListOfAllRankings (RemoteData.Success a) "Successful download", Cmd.none )

                RemoteData.Failure e ->
                    ( ListOfAllRankings (RemoteData.Failure e) "Failure to download", Cmd.none )

                RemoteData.NotAsked ->
                    ( ListOfAllRankings RemoteData.NotAsked "Not Asked", Cmd.none )

                RemoteData.Loading ->
                    ( ListOfAllRankings RemoteData.Loading "Loading", Cmd.none )



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
    case model of
        ListOfAllRankings rmtdata str ->
            case rmtdata of
                RemoteData.NotAsked ->
                    Element.text ""

                RemoteData.Loading ->
                    Element.text "Loading..."

                RemoteData.Success rankings ->
                    viewRankings rankings

                RemoteData.Failure httpError ->
                    Element.text "(Err httpError - real value to fix here)"

        FailureOnAllRankings str ->
            Element.text str



--viewError (buildErrorMessage httpError)
--you might need this later
-- (stringFromBool ranking.active)


viewRankings : List SR.Types.Ranking -> Element Msg
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
                    [ rankingNameCol rankings "SR.Types.Ranking Name"
                    , rankingDescCol rankings "SR.Types.Ranking Desc"
                    ]
                }


rankingNameCol : List SR.Types.Ranking -> String -> Column SR.Types.Ranking msg
rankingNameCol _ str =
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


rankingDescCol : List SR.Types.Ranking -> String -> Column SR.Types.Ranking msg
rankingDescCol _ str =
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

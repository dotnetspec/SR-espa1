module Pages.Rankings.Top exposing (Model, Msg, page)

--import Pages.Rankings.Players exposing (Player, PlayerId(..), emptyPlayer, emptyPlayerId, ladderOfPlayersDecoder, playerDecoder, playerEncoder)
--import Text
--import Element exposing (..)

import Components.Ranking exposing (Ranking, RankingId(..), rankingDecoder, rankingEncoder, rankingsDecoder)
import Element exposing (Element, alignRight, alignTop, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.Rankings.Params as Params
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import RemoteData exposing (RemoteData, WebData)
import Spa.Page
import Ui
import Utils.MyUtils exposing (stringFromBool)
import Utils.Spa exposing (Page)



--main = link "http://www.google.com" <| leftAligned <| Text.color red <| toText "Google"


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


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type alias Model =
    { content : WebData (List Ranking)
    , fetchedContentNotPlayerList : String
    , error : String
    }



-- INIT


init : Params.Top -> ( Model, Cmd Msg )
init _ =
    ( { content = RemoteData.NotAsked
      , error = ""
      , fetchedContentNotPlayerList = ""
      }
    , Http.get
        { url = "https://api.jsonbin.io/b/5c36f5422c87fa27306acb52/latest"
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
        FetchedContent (Ok fetchedContentNotPlayerList) ->
            ( { model | fetchedContentNotPlayerList = fetchedContentNotPlayerList }
            , Cmd.none
            )

        FetchedContent (Err _) ->
            ( { model | error = "there was an error" }
            , Cmd.none
            )

        RankingsReceived post ->
            --remove the first record (created on ranking creation with different format)
            ( { model | content = post }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- display whatever is in the model


view : Model -> Element Msg
view model =
    viewPostsOrError model


extractRanking : Ranking -> { id : String, active : Bool, name : String, desc : String }
extractRanking ranking =
    { id = ranking.id
    , active = ranking.active
    , name = ranking.name
    , desc = ranking.desc
    }


viewPostsOrError : Model -> Element Msg
viewPostsOrError model =
    case model.content of
        RemoteData.NotAsked ->
            Element.text ""

        RemoteData.Loading ->
            Element.text "Loading..."

        RemoteData.Success posts ->
            viewPosts posts

        RemoteData.Failure httpError ->
            Element.text "Failure"



--viewError (buildErrorMessage httpError)
--you might need this later
-- (stringFromBool ranking.active)


viewPosts : List Ranking -> Element Msg
viewPosts posts =
    Element.table []
        { data = posts
        , columns =
            [ { header = Element.text "Ranking Name"
              , width = fill
              , view =
                    \ranking ->
                        Element.link
                            [ Background.color (rgb255 255 255 255)
                            , Font.color (rgb255 0 128 255)
                            , Border.rounded 3
                            , padding 10
                            ]
                            { url = "/rankings/" ++ ranking.id
                            , label = Element.text ranking.name
                            }
              }
            , { header = Element.text "Ranking Desc"
              , width = fill
              , view =
                    \ranking ->
                        Element.text ranking.desc
              }
            ]
        }

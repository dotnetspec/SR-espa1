module Pages.Rankings.Dynamic exposing (Model, Msg, page)

import Components.Players exposing (Player, PlayerId(..), emptyPlayer, emptyPlayerId, ladderOfPlayersDecoder, playerDecoder, playerEncoder)
import Components.Ranking exposing (Ranking, RankingId(..), rankingDecoder, rankingEncoder, rankingsDecoder)
import Element exposing (..)
import Generated.Rankings.Params as Params
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import RemoteData exposing (RemoteData, WebData)
import Spa.Page
import Ui
import Utils.MyUtils exposing (stringFromBool)
import Utils.Spa exposing (Page)


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Rankings.Dynamic"
        , init = always init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }


type Msg
    = RankingsReceived (WebData (List Player))
    | FetchedContent (Result Http.Error String)


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type alias Model =
    { content : WebData (List Player)
    , fetchedContentNotPlayerList : String
    , error : String
    }



-- INIT
-- param1 (can add ,param2 etc. if nec.), is the RankingId


init : Params.Dynamic -> ( Model, Cmd Msg )
init { param1 } =
    ( { content = RemoteData.NotAsked
      , error = ""
      , fetchedContentNotPlayerList = ""
      }
    , fetchPost (RankingId param1)
    )


fetchPost : RankingId -> Cmd Msg
fetchPost (RankingId postId) =
    let
        _ =
            Debug.log "rankingid in fetchPost" postId

        headerKey =
            Http.header
                "secret-key"
                "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"
    in
    --RankingsReceived is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    --all the json is sent to the ladderDecoder (in Ladder(?).elm)
    Http.request
        { body = Http.emptyBody
        , expect =
            ladderOfPlayersDecoder
                |> Http.expectJson (RemoteData.fromResult >> RankingsReceived)
        , headers = [ headerKey ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = "https://api.jsonbin.io/b/" ++ postId ++ "/latest"
        }



--type Msg
--    = RankingsReceived (WebData (List Player))


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
--update the model before it gets passed to view


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
            let
                _ =
                    Debug.log "list of rankings" post
            in
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
    viewPlayersOrError model


viewPlayersOrError : Model -> Element Msg
viewPlayersOrError model =
    case model.content of
        RemoteData.NotAsked ->
            Element.text ""

        RemoteData.Loading ->
            Element.text "Loading..."

        RemoteData.Success players ->
            viewplayers players

        RemoteData.Failure httpError ->
            Element.text "Failure"



--{ header = Element.text "Ranking Name"
--              , width = fill
--              , view =
--                    \ranking ->
--                        Element.link
--                            [ Background.color (rgb255 255 255 255)
--                            , Font.color (rgb255 0 128 255)
--                            , Border.rounded 3
--                            , padding 10
--                            ]
--                            { url = "/rankings/" ++ ranking.id
--                            , label = Element.text ranking.name
--                            }
--              }
--stringFromBool ranking.active
--(String.fromInt ranking.currentchallengerid)
-- (String.fromInt ranking.rank)
-- String.fromInt ranking.datestamp)
--player.address


viewplayers : List Player -> Element Msg
viewplayers players =
    Element.table []
        { data = players
        , columns =
            [ { header = Element.text "Name"
              , width = fill
              , view =
                    \player ->
                        Element.text player.name
              }
            , { header = Element.text "id"
              , width = fill
              , view =
                    \player ->
                        Element.text (String.fromInt player.id)
              }
            , { header = Element.text "Current Challenger"
              , width = fill
              , view =
                    \player ->
                        Element.text player.currentchallengername
              }
            , { header = Element.text "Current Challenger ID"
              , width = fill
              , view =
                    \player ->
                        Element.text (String.fromInt player.currentchallengerid)
              }
            , { header = Element.text "RANK"
              , width = fill
              , view =
                    \player ->
                        Element.text (String.fromInt player.rank)
              }
            , { header = Element.text "CURRENTCHALLENGERADDRESS"
              , width = fill
              , view =
                    \player ->
                        Element.text player.currentchallengeraddress
              }
            ]
        }

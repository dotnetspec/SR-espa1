module Pages.Rankings.Top exposing (Model, Msg, page)

import Element exposing (..)
import Generated.Rankings.Params as Params
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Pages.Rankings.Players exposing (Player, PlayerId(..), emptyPlayer, emptyPlayerId, ladderOfPlayersDecoder, playerDecoder, playerEncoder)
import Pages.Rankings.Ranking exposing (Ranking, RankingId(..), rankingDecoder, rankingEncoder, rankingsDecoder)
import RemoteData exposing (RemoteData, WebData)
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


type Msg
    = RankingsReceived (WebData (List Ranking))



--| FetchedContent (Result Http.Error String)


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type alias Model =
    { content : WebData (List Ranking) }



-- INIT


init : Params.Top -> ( Model, Cmd Msg )
init _ =
    ( { content = RemoteData.NotAsked }
    , --Http.get
      --{ expect = Http.expectString FetchedContent
      --, url = "https://api.jsonbin.io/b/5c36f5422c87fa27306acb52/latest"
      --}
      Http.get
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --FetchedContent (Ok markdown) ->
        --    ( { model | content = markdown }
        --    , Cmd.none
        --    )
        --FetchedContent (Err _) ->
        --    ( { model | content = "there was an error" }
        --    , Cmd.none
        --    )
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
    --Ui.markdown (WebData Ranking model.content)
    let
        --newContent = List { model | rankingid, rankingname }
        _ =
            Debug.log "made it to view" model.content
    in
    Element.text "hello"



--    Element.table []
--    { columns =
--        [ { header = Element.text "Ranking"
--          , width = fill
--          , view =
--                \ranking ->
--                    Element.text "ranking.rankingid"
--          }
--        , { header = Element.text "Ranking Name"
--          , width = fill
--          , view =
--                \ranking ->
--                    Element.text 'name here'
--          }
--        ]
--        ,data = model.content
--}



--{ data = model.content
--, columns =
--    [ { header = Element.text "Ranking"
--      , width = fill
--      , view =
--            \ranking ->
--                Element.text ranking.rankingid
--      }
--    , { header = Element.text "Ranking Name"
--      , width = fill
--      , view =
--            \ranking ->
--                Element.text ranking.rankingname
--      }
--    ]
--}

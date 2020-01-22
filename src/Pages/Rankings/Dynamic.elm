module Pages.Rankings.Dynamic exposing (Model, Msg, page)

import Components.Players exposing (Player, PlayerId(..), emptyPlayer, emptyPlayerId, ladderOfPlayersDecoder, playerDecoder, playerEncoder)
import Components.Ranking exposing (Ranking, RankingId(..), rankingDecoder, rankingEncoder, rankingsDecoder)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Rankings.Params as Params
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import RemoteData exposing (RemoteData, WebData)
import Spa.Page
import Ui exposing (colors, markdown)
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



-- Msg gets passed to update


type Msg
    = PlayersReceived (WebData (List Player))
    | FetchedContent (Result Http.Error String)
    | ModalEnabled Bool Int


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a



--Model is updated in update


type alias Model =
    { players : WebData (List Player)
    , fetchedContentNotPlayerList : String
    , error : String
    , rankingid : String
    , modalStatus : Bool
    , playerid : Int
    , player : Player
    }



-- INIT
-- param1 (can add ,param2 etc. if nec.), is the RankingId


init : Params.Dynamic -> ( Model, Cmd Msg )
init { param1 } =
    ( { players = RemoteData.NotAsked
      , fetchedContentNotPlayerList = ""
      , error = ""
      , rankingid = param1
      , modalStatus = False
      , playerid = 0
      , player =
            { datestamp = 12345
            , active = False
            , currentchallengername = ""
            , currentchallengerid = 0
            , address = ""
            , rank = 0
            , name = ""
            , id = 0
            , currentchallengeraddress = ""
            }
      }
    , fetchRanking (RankingId param1)
    )


fetchRanking : RankingId -> Cmd Msg
fetchRanking (RankingId rankingId) =
    let
        _ =
            Debug.log "rankingid in fetchRanking" rankingId

        headerKey =
            Http.header
                "secret-key"
                "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"
    in
    --PlayersReceived is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    --all the json is sent to the ladderDecoder (in Ladder(?).elm)
    Http.request
        { body = Http.emptyBody
        , expect =
            ladderOfPlayersDecoder
                |> Http.expectJson (RemoteData.fromResult >> PlayersReceived)
        , headers = [ headerKey ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = "https://api.jsonbin.io/b/" ++ rankingId ++ "/latest"
        }



--type Msg
--    = PlayersReceived (WebData (List Player))


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

        PlayersReceived players ->
            let
                _ =
                    Debug.log "list of players" players
            in
            --remove the first record (created on ranking creation with different format)
            ( { model | players = players }, Cmd.none )

        ModalEnabled modalStatus newplayerid ->
            let
                _ =
                    Debug.log "player id" newplayerid
            in
            ( { model | modalStatus = modalStatus, playerid = newplayerid }, Cmd.none )



--{ model | price = new_price, productId = newProductId}
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- display whatever is in the model


view : Model -> Element Msg
view model =
    --viewPlayersOrError model
    controlledView model


controlledView : Model -> Element Msg
controlledView model =
    --Element.el [] (Element.text "Howdy!")
    --Element.layout []
    case model.modalStatus of
        False ->
            viewPlayersOrError model

        True ->
            viewplayer [ retrieveSinglePlayer model.playerid (extractPlayersFromWebData model) ]


retrieveSinglePlayer : Int -> List Player -> Player
retrieveSinglePlayer id players =
    let
        x =
            List.filter (\i -> i.id == id) players
    in
    case List.head x of
        Nothing ->
            { datestamp = 12345
            , active = False
            , currentchallengername = ""
            , currentchallengerid = 0
            , address = ""
            , rank = 0
            , name = ""
            , id = 0
            , currentchallengeraddress = ""
            }

        Just item ->
            item



--Element.row
--    []
--    --[ Element.el
--    --[ --Element.inFront (Element.text "I'm in front!")
--    --  Element.inFront (enabledButton False model.playerid)
--    --]
--    [ Element.text
--        ("PlayerId is : " ++ String.fromInt model.playerid)
--    ]
--]
-- markdown won't take dynmanic value insertions like rankingid unfortunately perhaps
--                markdown
--                """
--### header 3
--This is a paragraph. [Click me](/rankings/)!
--                """


enabledButton : Bool -> Int -> Element Msg
enabledButton hasClicked playerid =
    Input.button
        [ Background.color colors.green
        , Font.color colors.white
        , Element.focused
            [ Background.color colors.blue ]
        , Element.mouseOver
            [ Background.color colors.blue ]
        ]
        { onPress = Just (ModalEnabled hasClicked playerid)
        , label = text (String.fromInt playerid)
        }


viewPlayersOrError : Model -> Element Msg
viewPlayersOrError model =
    case model.players of
        RemoteData.NotAsked ->
            Element.text ""

        RemoteData.Loading ->
            Element.text "Loading..."

        RemoteData.Success players ->
            viewplayers players

        RemoteData.Failure httpError ->
            Element.text "Failure"


extractPlayersFromWebData : Model -> List Player
extractPlayersFromWebData model =
    case model.players of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success players ->
            players

        RemoteData.Failure httpError ->
            []



--possibly useful ref:
--stringFromBool ranking.active
--(String.fromInt ranking.currentchallengerid)
-- (String.fromInt ranking.rank)
-- String.fromInt ranking.datestamp)
--player.address


viewplayers : List Player -> Element Msg
viewplayers players =
    --Element.text "hello"
    Element.table
        []
        { data = players
        , columns =
            [ { header = Element.text "Button"
              , width = fill
              , view =
                    \player ->
                        enabledButton True player.id
              }
            , { header = Element.text "Name"
              , width = fill
              , view =
                    \player ->
                        Element.text player.name
              }
            , { header = Element.text "Current Challenger"
              , width = fill
              , view =
                    \player ->
                        Element.text player.currentchallengername
              }
            , { header = Element.text "RANK"
              , width = fill
              , view =
                    \player ->
                        Element.text (String.fromInt player.rank)
              }
            ]
        }



--the 'List' is just a list of one player (for the table to work)


viewplayer : List Player -> Element Msg
viewplayer player =
    --Element.text "hello"
    Element.table
        []
        { data = player
        , columns =
            [ { header = Element.text "Button"
              , width = fill
              , view =
                    \selectedplayer ->
                        enabledButton True selectedplayer.id
              }
            , { header = Element.text "Name"
              , width = fill
              , view =
                    \selectedplayer ->
                        Element.text selectedplayer.name
              }
            , { header = Element.text "Current Challenger"
              , width = fill
              , view =
                    \selectedplayer ->
                        Element.text selectedplayer.currentchallengername
              }
            , { header = Element.text "RANK"
              , width = fill
              , view =
                    \selectedplayer ->
                        Element.text (String.fromInt selectedplayer.rank)
              }
            ]
        }

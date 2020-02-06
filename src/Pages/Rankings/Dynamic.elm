module Pages.Rankings.Dynamic exposing (Model, Msg, page)

import Components.Players exposing (Player, PlayerId(..), emptyPlayer, emptyPlayerId, ladderOfPlayersDecoder, playerDecoder, playerEncoder)
import Components.Ranking exposing (Ranking, RankingId(..), rankingDecoder, rankingEncoder, rankingsDecoder)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as E
import Element.Font as Font
import Element.Input as Input
import Generated.Rankings.Params as Params
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import RemoteData exposing (RemoteData, WebData)
import Spa.Page
import Ui exposing (colors, markdown)
import Utils.MyUtils exposing (stringFromBool)
import Utils.Spa exposing (Page, PageContext)



--import Utils.Spa exposing (Page)
-- 'always' taken away from init so can access Global.Model
-- 'Model' here is coming from the Model defined in this page
-- not Global
-- Currently is
--Params.Dynamic -> ( Model, Cmd Msg )
-- needs to be
--Spa.Page.PageContext route globalModel
--          -> pageParams
--          -> ( Model, Cmd Msg )
-- these are output types of page function (not input)
--i.e. Spa.Page.element init now expects to output
-- Spa.Page.PageContext route globalModel as an arg to init


page : Page Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Rankings.Dynamic"

        --, init = \pageContext -> init pageContext.global.networkId
        , init = init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a



-- these next 2 are used in the Model


type ModalState
    = Open
    | Closed


type ResultOptions
    = Won
    | Lost
    | Undecided



--Model is updated in update


type alias Model =
    { --browserEnv : BrowserEnv
      --, settings : Maybe SettingsData
      --,
      players : WebData (List Player)
    , fetchedContentNotPlayerList : String
    , error : String
    , rankingid : String
    , modalState : ModalState
    , playerid : Int
    , player : Player
    , selectedRadio : ResultOptions
    }



-- this is where the types matter and are actually set to values ...


init : PageContext -> Params.Dynamic -> ( Model, Cmd Msg )
init pageContext { param1 } =
    ( { --  browserEnv = pageContext.global.browserEnv
        --, settings = Nothing
        --,
        players = RemoteData.NotAsked
      , fetchedContentNotPlayerList = ""
      , error = ""
      , rankingid = param1
      , modalState = Closed
      , playerid = 0
      , player =
            { datestamp = 12345
            , active = False
            , currentchallengername = "Available"
            , currentchallengerid = 0
            , address = ""
            , rank = 0
            , name = "Unidentified"
            , id = 0
            , currentchallengeraddress = ""
            }
      , selectedRadio = Undecided
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



-- Use Msg types to pattern match and trigger different updates in update
-- define according to variant. Pass the variant the nec. type (if nec.)
-- if change here, change update, and change wherever the Msg is called from


type Msg
    = PlayersReceived (WebData (List Player))
    | FetchedContent (Result Http.Error String)
    | OpenModal Int
    | CloseModal
    | SetRadioOption ResultOptions



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

        OpenModal playerid ->
            ( { model | modalState = Open, playerid = playerid }, Cmd.none )

        CloseModal ->
            ( { model | modalState = Closed }, Cmd.none )

        SetRadioOption val ->
            ( { model | selectedRadio = val }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- display whatever is in the model


view : Model -> Element Msg
view model =
    --  add any extra views on a new line eg.
    --, el [ Font.bold ] (text "this is bold")
    Element.row []
        [ viewWithModalReady model
        ]



--modal is being used to pick out a single player and his opponent
--so that a result can be entered


viewWithModalReady : Model -> Element Msg
viewWithModalReady model =
    let
        modalString =
            case model.modalState of
                Open ->
                    "Open"

                Closed ->
                    model.player.name ++ " you are currently ranked " ++ String.fromInt model.player.rank ++ " \nand your challenger is " ++ model.player.currentchallengername
    in
    -- html turns html Msg into Element Msg
    html
        (layout
            [ inFront <| viewModal model ]
         <|
            el
                [ width fill
                , height fill
                , padding 20
                ]
            <|
                column
                    [ width fill
                    , height fill
                    , spacing 50
                    ]
                    [ el [ Font.color (rgb 0.2 0.2 0.2) ] (text modalString)
                    , el [] <| viewPlayersOrError model

                    --below was the original 'Open' button from the ellie example (now handled by Result btn)
                    --, el [ centerX ] <| playeridbtn (rgb 0.25 0.75 0.75) (OpenModal model.player.id) "Opensadfa"
                    ]
        )


viewModal : Model -> Element Msg
viewModal model =
    let
        box =
            el
                [ width (px 300)
                , height (px 300)
                , centerX
                , centerY
                , Background.color (rgb 1 1 1)
                ]
            <|
                el [ centerX, centerY ] <|
                    textColumn []
                        [ row []
                            [ el
                                []
                                (viewplayer model)
                            ]
                        , row []
                            [ el []
                                (closebutton
                                    (rgb 0.95 0.6 0.25)
                                    CloseModal
                                    "Confirm"
                                )
                            ]
                        , row []
                            [ el []
                                (closebutton
                                    (rgb 0.95 0.6 0.25)
                                    CloseModal
                                    "Close"
                                )
                            ]
                        ]
    in
    case model.modalState of
        Open ->
            el
                [ width fill
                , height fill
                , behindContent <|
                    el
                        [ width fill
                        , height fill
                        , Background.color (rgba 0.5 0.5 0.5 0.7)
                        , E.onClick CloseModal
                        ]
                        none
                ]
                box

        Closed ->
            el [] none


closebutton : Color -> Msg -> String -> Element Msg
closebutton color msg label =
    Input.button
        [ padding 20
        , Background.color color
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , Font.center
                , Font.color (rgba 0.2 0.2 0.2 0.9)
                ]
                (text label)
        }


playersResultBtnCol : List Player -> String -> Column Player Msg
playersResultBtnCol players str =
    { header = Element.text str
    , width = fill
    , view =
        \player ->
            Element.row
                [ Font.color Ui.colors.lightblue
                , Border.widthXY 2 2
                ]
                [ playeridbtn colors.blue (OpenModal player.id) "Result"
                ]
    }


playersNameCol : List Player -> String -> Column Player msg
playersNameCol players str =
    { header = Element.text str
    , width = fill
    , view =
        \player ->
            Element.row
                [ Border.widthXY 2 2
                , Font.color Ui.colors.green
                ]
                [ Element.text player.name
                ]
    }


playersRankCol : List Player -> String -> Column Player msg
playersRankCol players str =
    { header = Element.text str
    , width = fill
    , view =
        \player ->
            Element.row
                [ Border.widthXY 2 2
                , Font.color Ui.colors.green
                ]
                [ Element.text (String.fromInt player.rank)
                ]
    }


playersCurrentChallCol : List Player -> String -> Column Player msg
playersCurrentChallCol players str =
    { header = Element.text str
    , width = fill
    , view =
        \player ->
            Element.row
                [ Border.widthXY 2 2
                , Font.color Ui.colors.green
                ]
                [ Element.text player.currentchallengername
                ]
    }


playeridbtn : Color -> Msg -> String -> Element Msg
playeridbtn color selectedplayermsg label =
    Input.button
        [ Background.color color
        , centerX
        , centerY
        ]
        { onPress = Just selectedplayermsg
        , label =
            el
                [ centerX
                , centerY
                , Font.center
                , Font.color (rgba 0.2 0.2 0.2 0.9)
                ]
                (text label)
        }


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
--stringFromBool player.active
--(String.fromInt player.currentchallengerid)
-- (String.fromInt player.rank)
-- String.fromInt player.datestamp)
--player.address


viewplayers : List Player -> Element Msg
viewplayers players =
    html <|
        Element.layout
            [ Element.padding 25
            , Background.color (rgba 0 0 0 1)
            , Font.color (rgba 1 1 1 1)
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
                [ Background.color Ui.colors.white
                , Border.solid
                , Border.color Ui.colors.black
                , Border.widthXY 1 1
                , Border.rounded 3
                ]
                { data = players
                , columns =
                    [ playersResultBtnCol players "Result"
                    , playersNameCol players "Player Name"
                    , playersRankCol players "Rank"
                    , playersCurrentChallCol players "Current Challenger"
                    ]
                }



--the 'List' is just a list of one player (for the table to work)


viewplayer : Model -> Element Msg
viewplayer model =
    let
        playerToView =
            retrieveSinglePlayer model.playerid (extractPlayersFromWebData model)
    in
    Element.paragraph []
        [ Input.radio
            [ spacing 12
            , Background.color Ui.colors.grey
            ]
            { selected = Just model.selectedRadio
            , onChange = SetRadioOption
            , label = Input.labelAbove [ Font.size 22, paddingXY 0 12 ] (text (playerToView.name ++ " would you like to \nenter a result against " ++ playerToView.currentchallengername ++ "?"))
            , options =
                [ Input.option Won (text "Won")
                , Input.option Lost (text "Lost")
                , Input.option Undecided (text "Undecided")
                ]
            }
        ]

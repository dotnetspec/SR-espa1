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



--variants are labels for different situations, with assoc data possibly attached.


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a



--The model represents the state of the application
-- Model is what is going to change via Update (which is changed from places like View, Subs etc.)
-- it will go from 1 state to another
-- functions like view will just reflect
-- current state of model


type Model
    = JsonbinData SR.Types.SRState
    | FailureOnAllRankings String



-- INIT
-- this accesses COLLECTION RECORDS - GLOBAL - public bin


init : Params.Top -> ( Model, Cmd Msg )
init _ =
    ( --JsonbinData RemoteData.Loading ""
      JsonbinData (SR.Types.AllRankings RemoteData.Loading (SR.Types.RankingId "5e2a585f593fd741856f4b04"))
    , -- Http.get
      --     { url = "https://api.jsonbin.io/b/5e2a585f593fd741856f4b04/latest"
      -- , expect =
      --     SR.Decode.rankingsDecoder
      --         |> expectJson (RemoteData.fromResult >> GotJsonbinData)
      -- , expect =
      --     getRankingList (JsonbinData (SR.Types.AllRankings RemoteData.Loading (SR.Types.RankingId "5e2a585f593fd741856f4b04")))
      -- }
      --getRankingList (JsonbinData (SR.Types.AllRankings (SR.Types.RankingId "5e2a585f593fd741856f4b04")))
      --getRankingList (JsonbinData SR.Types.AllRankings RemoteData.fromResult >> GotJsonbinData (SR.Types.RankingId "5e2a585f593fd741856f4b04"))
      getRankingList
    )


getRankingList : Cmd Msg
getRankingList =
    Http.get
        { url = "https://api.jsonbin.io/b/5e2a585f593fd741856f4b04/latest"
        , expect = Http.expectJson (RemoteData.fromResult >> GotJsonbinAllRankings) SR.Decode.rankingsDecoder
        }



-- getSingleRanking : Cmd Msg
-- getSingleRanking =
--     Http.get
--         { url = "https://api.jsonbin.io/b/5d8f5dcabfb1f70f0b11638b/latest"
--         , expect = Http.expectJson (RemoteData.fromResult >> GotJsonbinSingleRanking) SR.Decode.ladderOfPlayersDecoder
--         }
-- case model of
--     JsonbinData r ->
--         case r of
--             SR.Types.AllRankings a glbRankingId ->
--                 Http.get
--                     { url =
--                         "https://api.jsonbin.io/b/5e2a585f593fd741856f4b04/latest"
--                     , expect =
--                         SR.Decode.rankingsDecoder
--                             |> expectJson (RemoteData.fromResult >> GotJsonbinData (SR.Types.AllRankings RemoteData.Loading (SR.Types.RankingId "5e2a585f593fd741856f4b04")))
--                     }
--             SR.Types.SingleRanking a singleRankingId ->
--                 { url =
--                     "https://api.jsonbin.io/b/"
--                         ++ singleRankingId
--                         ++ "/latest"
--                 , expect =
--                     SR.Decode.ladderOfPlayersDecoder
--                         |> expectJson (RemoteData.fromResult >> GotJsonbinData (SR.Types.AllRankings RemoteData.Loading (SR.Types.RankingId "5e2a585f593fd741856f4b04")))
--                 }
--             _ ->
--                 { url =
--                     "https://api.jsonbin.io/b/5e2a585f593fd741856f4b04/latest"
--                 , expect =
--                     SR.Decode.ladderOfPlayersDecoder
--                         |> expectJson (RemoteData.fromResult >> GotJsonbinData (SR.Types.AllRankings RemoteData.Loading (SR.Types.RankingId "5e2a585f593fd741856f4b04")))
--                 }


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
    = GotJsonbinAllRankings (RemoteData.WebData (List SR.Types.RankingInfo))



--| GotJsonbinSingleRanking (RemoteData.WebData (List SR.Types.Player))
-- UPDATE
-- Update needs to take two things: a message (which
-- is a description of the transition that needs to happen),
--  and the model (which is the model BEFORE the update is applied),
--  Update component contains code that should be executed for each message generated by the view
-- or a cmd/sub


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJsonbinAllRankings rmtdata ->
            case rmtdata of
                --removes[?] the first record (created on ranking creation with different format)
                RemoteData.Success a ->
                    -- let
                    --     _ =
                    --         Debug.log "result" a
                    -- in
                    ( JsonbinData (SR.Types.AllRankings (RemoteData.Success a) (SR.Types.RankingId "5e2a585f593fd741856f4b04")), Cmd.none )

                --( JsonbinData (SR.Types.Failure "Failure"), Cmd.none )
                -- let
                --    -- _ =
                --         --Debug.log "result" a
                -- in
                -- case model of
                --     JsonbinData rd ->
                --         let
                --             _ =
                --                 Debug.log "result" rd
                --         in
                --         case rd of
                --             --here just branch so we can send back the same ranking type in a new model,
                --             --but now with new data. e is error. c is the data (Success c)
                --             SR.Types.AllRankings e c ->
                --                 let
                --                     _ =
                --                         Debug.log "result c" c
                --                 in
                --                 ( JsonbinData (SR.Types.AllRankings e c), Cmd.none )
                --             SR.Types.SingleRanking e c ->
                --                 ( JsonbinData (SR.Types.SingleRanking e c), Cmd.none )
                --             SR.Types.EnterResult ->
                --                 ( JsonbinData SR.Types.EnterResult, Cmd.none )
                --             SR.Types.Failure s ->
                --                 ( JsonbinData (SR.Types.Failure s), Cmd.none )
                -- FailureOnAllRankings s ->
                --     ( JsonbinData (SR.Types.Failure "Failure"), Cmd.none )
                RemoteData.Failure e ->
                    case model of
                        JsonbinData rd ->
                            case rd of
                                SR.Types.AllRankings b c ->
                                    ( JsonbinData (SR.Types.AllRankings b c), Cmd.none )

                                SR.Types.SingleRanking b c ->
                                    ( JsonbinData (SR.Types.SingleRanking b c), Cmd.none )

                                SR.Types.EnterResult ->
                                    ( JsonbinData SR.Types.EnterResult, Cmd.none )

                                SR.Types.Failure s ->
                                    ( JsonbinData (SR.Types.Failure s), Cmd.none )

                        FailureOnAllRankings s ->
                            ( JsonbinData (SR.Types.Failure "Failure"), Cmd.none )

                RemoteData.NotAsked ->
                    case model of
                        JsonbinData rd ->
                            case rd of
                                SR.Types.AllRankings b c ->
                                    ( JsonbinData (SR.Types.AllRankings b c), Cmd.none )

                                SR.Types.SingleRanking b c ->
                                    ( JsonbinData (SR.Types.SingleRanking b c), Cmd.none )

                                SR.Types.EnterResult ->
                                    ( JsonbinData SR.Types.EnterResult, Cmd.none )

                                SR.Types.Failure s ->
                                    ( JsonbinData (SR.Types.Failure s), Cmd.none )

                        FailureOnAllRankings s ->
                            ( JsonbinData (SR.Types.Failure "Failure"), Cmd.none )

                RemoteData.Loading ->
                    case model of
                        JsonbinData rd ->
                            let
                                _ =
                                    Debug.log "result" rd
                            in
                            case rd of
                                SR.Types.AllRankings b c ->
                                    let
                                        _ =
                                            Debug.log "result" c
                                    in
                                    ( JsonbinData (SR.Types.AllRankings b c), Cmd.none )

                                SR.Types.SingleRanking b c ->
                                    ( JsonbinData (SR.Types.SingleRanking b c), Cmd.none )

                                SR.Types.EnterResult ->
                                    ( JsonbinData SR.Types.EnterResult, Cmd.none )

                                SR.Types.Failure s ->
                                    ( JsonbinData (SR.Types.Failure s), Cmd.none )

                        -- GotJsonbinSingleRanking remtdata ->
                        --     case remtdata of
                        --     RemoteData.Success a ->
                        --     ( JsonbinData (SR.Types.SingleRanking (RemoteData.Success a) (SR.Types.RankingId "5d8f5dcabfb1f70f0b11638b")), Cmd.none )
                        FailureOnAllRankings s ->
                            ( JsonbinData (SR.Types.Failure "Failure"), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
--The view represents the visual elements of the application and will generate msgs for Update
-- it will also display whatever is currently in model


view : Model -> Element Msg
view model =
    Element.paragraph []
        [ viewRankingsOrError model
        ]


viewRankingsOrError : Model -> Element Msg
viewRankingsOrError model =
    case model of
        JsonbinData srState ->
            case srState of
                SR.Types.AllRankings rmtData c ->
                    case rmtData of
                        RemoteData.NotAsked ->
                            Element.text ""

                        RemoteData.Loading ->
                            Element.text "Loading..."

                        RemoteData.Success rankings ->
                            viewRankings rankings

                        RemoteData.Failure httpError ->
                            Element.text "(Err httpError - real value to fix here)"

                SR.Types.SingleRanking rmtData c ->
                    Element.text "to do : single ranking"

                SR.Types.EnterResult ->
                    Element.text "to do : enter result"

                SR.Types.Failure s ->
                    Element.text "failure"

        FailureOnAllRankings s ->
            Element.text "failure on all"



-- SR.Types.SingleRanking ->
--     Element.text "Err - attempting to load single ranking"
-- SR.Types.EnterResult ->
--     Element.text "Err - attempting to enter result"
--viewError (buildErrorMessage httpError)
--you might need this later
-- (stringFromBool ranking.active)


viewRankings : List SR.Types.RankingInfo -> Element Msg
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


rankingNameCol : List SR.Types.RankingInfo -> String -> Column SR.Types.RankingInfo msg
rankingNameCol _ str =
    { header = Element.text str
    , width = fill
    , view =
        \rankingInfo ->
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
                    { url = "/rankings/" ++ rankingInfo.id
                    , label = Element.text rankingInfo.name
                    }
                ]
    }


rankingDescCol : List SR.Types.RankingInfo -> String -> Column SR.Types.RankingInfo msg
rankingDescCol _ str =
    { header = Element.text str
    , width = fill
    , view =
        \rankingInfo ->
            Element.row
                [ Border.widthXY 2 2
                , Font.color Ui.colors.green
                ]
                [ Element.text rankingInfo.desc
                ]
    }

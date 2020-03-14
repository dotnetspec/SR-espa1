module Pages.Rankings.Top exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Framework
import Framework.Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Group as Group
import Framework.Heading as Heading
import Framework.Input as Input
import Framework.Slider as Slider
import Framework.Tag as Tag
import Generated.Rankings.Params as Params
import Global
import Html
import Http
import Json.Decode
import Json.Encode
import RemoteData
import SR.Constants
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
--nb: each variant added to model has to be handled e.g. do you need 'failure' if it's anyway handled by RemoteData?


type Model
    = AllRankingsJson (RemoteData.WebData (List SR.Types.RankingInfo))
    | NewEmpty (RemoteData.WebData SR.Types.RankingId) (RemoteData.WebData SR.Types.RankingInfo)



--| NewEmpty (RemoteData.WebData SR.Types.Player) (RemoteData.WebData SR.Types.RankingInfo) (RemoteData.WebData SR.Types.RankingId)
-- Msg is a description of the transition that already happened
-- Messages that delivered the response (orign doc says 'will deliver')


type Msg
    = GotJsonbinAllRankings (RemoteData.WebData (List SR.Types.RankingInfo))
      --| NewRankingCreated (RemoteData.WebData (List SR.Types.RankingInfo))
    | SwitchToNewEmpty
    | NewRankingRequestedByBtnClick
    | GotNewRankingResponse (RemoteData.WebData SR.Types.RankingId) (RemoteData.WebData SR.Types.RankingInfo)
    | InputChg String



--| GotNewRankingResponse (RemoteData.WebData SR.Types.Player)
-- | NewRankingInfoConfirmedByBtnClick (RemoteData.WebData SR.Types.Player) (RemoteData.WebData SR.Types.RankingInfo) (RemoteData.WebData SR.Types.RankingId)
-- | SentNewRankingInfoToJsonbin (RemoteData.WebData SR.Types.Player) (RemoteData.WebData SR.Types.RankingInfo) (RemoteData.WebData SR.Types.RankingId)
--NewRankingCreated (Result Http.Error ())
-- INIT
-- this accesses COLLECTION RECORDS - GLOBAL - public bin


init : Params.Top -> ( Model, Cmd Msg )
init _ =
    -- let
    --     uname =
    --         case context.global of
    --             Global.GlobalVariant wSentry uName ->
    --                 case uName of
    --                     SR.Types.NewUser ->
    --                         "Hello New User"
    --                     SR.Types.ExistingUser str ->
    --                         "temp value whilst sort Top.elm"
    --             Global.Failure str ->
    --                 str
    --     _ =
    --         Debug.log "username " ++ uname
    -- in
    ( AllRankingsJson RemoteData.Loading
    , -- nb. getRankingList is an expression not a function
      getRankingList
    )


getRankingList : Cmd Msg
getRankingList =
    let
        secretKey =
            Http.header
                "secret-key"
                "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"

        binName =
            Http.header
                "name"
                "Global"

        containerId =
            Http.header
                "collection-id"
                "5d7deab3371673119fab12a6"
    in
    Http.request
        { body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> GotJsonbinAllRankings) SR.Decode.rankingsDecoder
        , headers = [ secretKey, binName, containerId ]
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingReadLink
        }


createNewRankingList : Cmd Msg
createNewRankingList =
    let
        secretKey =
            Http.header
                "secret-key"
                "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"

        binName =
            Http.header
                "name"
                "Selected"

        containerId =
            Http.header
                "collection-id"
                "5d7deb68371673119fab12d7"

        idJsonObj : Json.Encode.Value
        idJsonObj =
            Json.Encode.list
                Json.Encode.object
                [ [ ( "DATESTAMP", Json.Encode.int 123456 )
                  , ( "ACTIVE", Json.Encode.bool True )
                  , ( "CURRENTCHALLENGERNAME", Json.Encode.string "" )
                  , ( "CURRENTCHALLENGERID", Json.Encode.int 0 )
                  , ( "ADDRESS", Json.Encode.string "" )
                  , ( "RANK", Json.Encode.int 1 )
                  , ( "NAME", Json.Encode.string "" )
                  , ( "PLAYERID", Json.Encode.int 1 )
                  , ( "CURRENTCHALLENGERADDRESS", Json.Encode.string "" )
                  ]
                ]
    in
    --GotNewRankingResponse is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- decoder relates to what comes back from server. Nothing to do with above.
    Http.request
        { body =
            Http.jsonBody <| idJsonObj

        --, expect = Http.expectJson (RemoteData.fromResult >> GotNewRankingResponse RemoteData.NotAsked) SR.Decode.newRankingIdDecoder
        , expect = Http.expectJson (RemoteData.fromResult >> GotNewRankingResponse RemoteData.NotAsked) SR.Decode.newRankingDecoder
        , headers = [ secretKey, binName, containerId ]
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlForCreateNewRankingAndReturnNewId
        }



-- this also has to be done when a new ranking is created.


addNewRankingListEntryInGlobal : Cmd Msg
addNewRankingListEntryInGlobal =
    let
        secretKey =
            Http.header
                "secret-key"
                "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"

        binName =
            Http.header
                "name"
                "Global"

        containerId =
            Http.header
                "collection-id"
                "5d7deab3371673119fab12a6"

        idJsonObj : Json.Encode.Value
        idJsonObj =
            --Json.Encode.list
            Json.Encode.object
                --[
                [ ( "id", Json.Encode.string "" )
                , ( "ACTIVE", Json.Encode.bool True )
                , ( "RANKINGNAME", Json.Encode.string "" )
                , ( "RANKINGDESC", Json.Encode.string "" )
                ]

        --]
    in
    --GotNewRankingResponse is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| idJsonObj

        --, expect = Http.expectJson (RemoteData.fromResult >> GotNewRankingResponse) SR.Decode.newRankingIdDecoder
        , expect = Http.expectJson (RemoteData.fromResult >> GotNewRankingResponse RemoteData.NotAsked) SR.Decode.newRankingDecoder
        , headers = [ secretKey, binName, containerId ]
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlForCreateNewRankingAndReturnNewId
        }



-- updateNewRankingListInfoToJsonbin : SR.Types.RankingId -> Cmd Msg
-- updateNewRankingListInfoToJsonbin rankingIdRemData =
--     let
--         newrankingId =
--             case rankingIdRemData of
--                 RemoteData.Success b ->
--                     b
--                 _ ->
--                     "999"
--         --             { id = "String"
--         --             , active = False
--         --             , name = "String"
--         --             , desc = "String"
--         --             }
--         -- _ =
--         --     Debug.log "rankingid in updateNewRankingListInfoToJsonbin" rankingInfo.id
--         headerKey =
--             Http.header
--                 "secret-key"
--                 "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"
--         -- todo: re-factor?
--         idJsonObj : Json.Encode.Value
--         idJsonObj =
--             Json.Encode.object
--                 [ ( "id", Json.Encode.string newrankingId )
--                 , ( "ACTIVE", Json.Encode.bool True )
--                 , ( "RANKINGNAME", Json.Encode.string "rankingInfo.name" )
--                 , ( "RANKINGDESC", Json.Encode.string "rankingInfo.desc" )
--                 ]
--     in
--     --PlayersReceived is the Msg handled by update whenever a request is made
--     --RemoteData is used throughout the module, including update
--     Http.request
--         { body =
--             Http.jsonBody <| idJsonObj
--         , expect = Http.expectJson (RemoteData.fromResult >> SentNewRankingInfoToJsonbin) SR.Decode.newRankingDecoder
--         , headers = [ headerKey ]
--         , method = "PUT"
--         , timeout = Nothing
--         , tracker = Nothing
--         , url = "https://api.jsonbin.io/b/" ++ newrankingId
--         }
-- this is where the errors etc. are assigned to be matched against later if necessary e.g. to get new ranking id


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



-- UPDATE
-- Update needs to take two things: a message (which
-- is a description of the transition that already happened),
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
                    ( AllRankingsJson (RemoteData.Success a), Cmd.none )

                RemoteData.Failure e ->
                    ( AllRankingsJson (RemoteData.Failure e), Cmd.none )

                RemoteData.NotAsked ->
                    ( AllRankingsJson RemoteData.NotAsked, Cmd.none )

                RemoteData.Loading ->
                    ( AllRankingsJson RemoteData.Loading, Cmd.none )

        SwitchToNewEmpty ->
            ( NewEmpty RemoteData.Loading RemoteData.NotAsked, Cmd.none )

        --this doesn't do much - just fires the createNewRankingList Cmd
        NewRankingRequestedByBtnClick ->
            ( NewEmpty RemoteData.Loading RemoteData.NotAsked, createNewRankingList )

        -- this is the result from createNewRankingList Cmd
        -- it should have the Http.expectStringResponse in it
        GotNewRankingResponse id result ->
            ( NewEmpty id result, Cmd.none )

        InputChg str ->
            ( NewEmpty RemoteData.Loading RemoteData.NotAsked, Cmd.none )



--have to pass both sets of info in as NewEmpty requires it ...
-- NewRankingInfoConfirmedByBtnClick playerinfo rankinfo rankId ->
--     ( NewEmpty playerinfo rankinfo rankId, updateNewRankingListInfoToJsonbin playerinfo rankinfo )
-- SentNewRankingInfoToJsonbin playerinfo rankinfo rankId ->
--     ( NewEmpty playerinfo rankinfo, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
--The view represents the visual elements of the application and
-- will enable user to generate msgs telling Update what was done
-- It will also display whatever is currently in model


view : Model -> Element Msg
view model =
    renderView model



-- Element.column
-- Framework.container
-- [ Element.el Heading.h1 <| Element.text "Elm-Ui Framework"
-- -- , heading
-- -- , tag
-- -- , group
-- -- , color
-- -- , card
-- -- , grid
-- -- , button
-- , input
-- --, slider
-- ]


renderView : Model -> Element Msg
renderView model =
    html <|
        Framework.responsiveLayout [ Element.explain Debug.todo ] <|
            Element.el Framework.container <|
                Element.paragraph [ Element.explain Debug.todo ] <|
                    listOfElementmsgs
                        model



-- <|
--     Element.paragraph [ Element.explain Debug.todo ] <|
--         listOfElementmsgs
--             model
-- add any new html as Element Msg to this list


listOfElementmsgs : Model -> List (Element Msg)
listOfElementmsgs model =
    [ getHeaderGroup model

    --, currentView model
    ]



-- Input.button (Framework.Button.simple ++ Color.primary) <|
--         { onPress = Just NewRankingCreated
--         , label = Element.text "Create New Ranking"
--         }


getHeaderGroup : Model -> Element Msg
getHeaderGroup model =
    case model of
        AllRankingsJson rnkingList ->
            Element.column Grid.section <|
                [ Element.el Heading.h2 <| Element.text "Global Rankings"
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left) <| createnewRankingbutton Color.primary SwitchToNewEmpty "Create New"
                        , Element.el (Card.fill ++ Group.center ++ Color.disabled) <| joinbutton Color.primary NewRankingRequestedByBtnClick "Join"
                        , Element.el (Card.fill ++ Group.right ++ Color.disabled) <| enterResultbutton Color.primary NewRankingRequestedByBtnClick "Enter Result"
                        , Element.el (Card.fill ++ Group.top ++ Color.disabled) <| updateProfilebutton Color.primary NewRankingRequestedByBtnClick "Update Profile"
                        ]
                    ]
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left) <| currentView model
                        ]
                    ]

                -- , [ Element.wrappedRow Grid.simple
                --         [ Element.el (Card.fill ++ Group.left ++ Color.disabled) <| createnewRankingbutton Color.primary NewRankingRequestedByBtnClick "Create New"
                --         , currentView model
                --         --, Element.el (Card.fill ++ Group.bottom) <| listAllbutton Color.primary getRankingList "List All"
                --         ]
                --   ]
                ]

        NewEmpty rankingIdremdata rnkInfo ->
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text <| "Create New Ranking ... new id is " ++ gotNewRankingId model

                --Element.el Heading.h2 <| Element.text <| "Create New Ranking ... new id is " ++ remdata.expect
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left ++ Color.disabled) <| createnewRankingbutton Color.primary NewRankingRequestedByBtnClick "Create New"
                        , Element.el (Card.fill ++ Group.center) <| joinbutton Color.primary NewRankingRequestedByBtnClick "Join"
                        , Element.el (Card.fill ++ Group.right ++ Color.disabled) <| enterResultbutton Color.primary NewRankingRequestedByBtnClick "Enter Result"
                        , Element.el (Card.fill ++ Group.top ++ Color.disabled) <| updateProfilebutton Color.primary NewRankingRequestedByBtnClick "Update Profile"
                        ]
                    ]
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left) <| currentView model
                        ]
                    ]
                ]


gotNewRankingId : Model -> String
gotNewRankingId model =
    case model of
        AllRankingsJson rmtdata ->
            "Not in NewEmpty!"

        NewEmpty rankingIdremdata rknInfo ->
            case rankingIdremdata of
                RemoteData.Success a ->
                    case a of
                        b ->
                            case b of
                                SR.Types.RankingId c ->
                                    c

                --"Success : " ++ SR.Types.RankingId b
                RemoteData.NotAsked ->
                    "Initialising."

                RemoteData.Loading ->
                    "Loading."

                RemoteData.Failure err ->
                    case err of
                        Http.BadUrl s ->
                            "Bad Url"

                        Http.Timeout ->
                            "Timeout"

                        Http.NetworkError ->
                            "Network Err"

                        Http.BadStatus statuscode ->
                            String.fromInt <| statuscode

                        Http.BadBody s ->
                            "BadBody " ++ s


currentView : Model -> Element Msg
currentView model =
    case model of
        AllRankingsJson rmtData ->
            case rmtData of
                RemoteData.NotAsked ->
                    Element.text ""

                RemoteData.Loading ->
                    Element.text "Loading..."

                RemoteData.Success rankings ->
                    viewRankings rankings

                RemoteData.Failure httpError ->
                    Element.text <| buildErrorMessage httpError

        NewEmpty rankingIdremdata rnkInfo ->
            input


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



--you might need this later
-- (stringFromBool ranking.active)


viewRankings : List SR.Types.RankingInfo -> Element Msg
viewRankings rankings =
    html <|
        Element.layout
            [ Element.explain Debug.todo

            --     Element.padding 25
            -- , Background.color (rgba 0 0 0 1)
            -- , Font.color (rgba 1 1 1 1)
            -- --, Font.italic
            -- , Font.size 22
            -- , Font.family
            --     [ Font.external
            --         { url = "https://fonts.googleapis.com/css?family=Roboto"
            --         , name = "Roboto"
            --         }
            --     , Font.sansSerif
            --     ]
            ]
        <|
            Element.table
                [--     Element.padding 25
                 -- , Background.color Ui.colors.white
                 -- , Border.solid
                 -- , Border.color Ui.colors.black
                 -- , Border.widthXY 1 1
                 -- , Border.rounded 3
                ]
                { data = rankings
                , columns =
                    [ rankingNameCol rankings "Ranking Name"
                    , rankingDescCol rankings "Ranking Desc"
                    ]
                }


rankingNameCol : List SR.Types.RankingInfo -> String -> Column SR.Types.RankingInfo msg
rankingNameCol _ str =
    { header = Element.text str
    , width = fill
    , view =
        \rankingInfo ->
            Element.row
                [--     Font.color Ui.colors.lightblue
                 -- , Border.widthXY 2 2
                ]
                [ Element.link
                    [--Background.color Ui.colors.blue
                     --Font.color Ui.colors.lightblue
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
                [--     Border.widthXY 2 2
                 -- , Font.color Ui.colors.green
                ]
                [ Element.text rankingInfo.desc
                ]
    }



--UI


createnewRankingbutton : List (Attribute Msg) -> Msg -> String -> Element.Element Msg
createnewRankingbutton color msg label =
    Input.button (Framework.Button.simple ++ color) <|
        { onPress = Just msg
        , label = Element.text label
        }


enterResultbutton : List (Attribute Msg) -> Msg -> String -> Element.Element Msg
enterResultbutton color msg label =
    Input.button (Framework.Button.simple ++ color) <|
        { onPress = Just msg
        , label = Element.text label
        }


joinbutton : List (Attribute Msg) -> Msg -> String -> Element.Element Msg
joinbutton color msg label =
    Input.button (Framework.Button.simple ++ color) <|
        { onPress = Just msg
        , label = Element.text label
        }


updateProfilebutton : List (Attribute Msg) -> Msg -> String -> Element.Element Msg
updateProfilebutton color msg label =
    Input.button (Framework.Button.simple ++ color) <|
        { onPress = Just msg
        , label = Element.text label
        }


listAllbutton : List (Attribute Msg) -> Msg -> String -> Element.Element Msg
listAllbutton color msg label =
    Input.button (Framework.Button.simple ++ color) <|
        { onPress = Just msg
        , label = Element.text label
        }


input : Element Msg
input =
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Please name and describe your new ranking"
        , Element.wrappedRow (Card.fill ++ Grid.simple)
            [ Element.column Grid.simple
                [ Input.text Input.simple
                    { onChange = InputChg
                    , text = "e.g. Stockton On Pullet Juniors"
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Name"
                    }
                , Input.multiline Input.simple
                    { onChange = InputChg
                    , text = "e.g. For all under 19s"
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Description"
                    , spellcheck = False
                    }
                ]
            ]
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ --     Input.button Framework.Button.simple <|
                  --     { onPress = Nothing
                  --     , label = Element.text "Framework.simple"
                  --     }
                  -- ,
                  Input.button Framework.Button.fill <|
                    { onPress = Nothing
                    , label = Element.text "Create New Ranking"
                    }
                ]
            ]
        , Element.paragraph [] <|
            List.singleton <|
                Element.text "Once created - get your friends to join "

        -- , Element.wrappedRow Grid.simple <|
        --     [ Input.text (Color.danger ++ Input.simple)
        --         { onChange = InputChg
        --         , text = ""
        --         , placeholder = Nothing
        --         , label = Input.labelLeft Input.label <| Element.text "Color.danger ++ Input.simple"
        --         }
        --     ]
        -- , Element.wrappedRow Grid.simple <|
        --     [ Input.text (Color.disabled ++ Input.simple)
        --         { onChange = InputChg
        --         , text = ""
        --         , placeholder = Nothing
        --         , label = Input.labelLeft Input.label <| Element.text "Color.disabled ++ Input.simple"
        --         }
        --     ]
        , Element.paragraph (Card.fill ++ Color.warning) <|
            [ Element.el [ Font.bold ] <| Element.text "Please Note: "
            , Element.paragraph [] <|
                List.singleton <|
                    Element.text "Creating a ranking makes the administration of it entirely your responsibility. \nOnly you can delete it"
            ]
        ]

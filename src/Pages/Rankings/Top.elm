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
-- AllRankingsJson is just the current list of all rankings
-- AddingNewRankingToGlobalList holds a new ranking id, data for a new ranking and the existing global list to add the new data to


type Model
    = AllRankingsJson (RemoteData.WebData (List SR.Types.RankingInfo)) String String SR.Types.UIState
    | AddingNewRankingToGlobalList (RemoteData.WebData SR.Types.RankingId) (RemoteData.WebData SR.Types.RankingInfo) (RemoteData.WebData (List SR.Types.RankingInfo))
    | ModelFailure String



--| AddingNewRankingToGlobalList (RemoteData.WebData SR.Types.Player) (RemoteData.WebData SR.Types.RankingInfo) (RemoteData.WebData SR.Types.RankingId)
-- Msg is a description of the transition that already happened
-- Messages that delivered the response (orign doc says 'will deliver')


type Msg
    = GotJsonbinAllRankings (RemoteData.WebData (List SR.Types.RankingInfo))
      --| NewRankingCreated (RemoteData.WebData (List SR.Types.RankingInfo))
    | ChangedUIStateToCreateNew
      --| SwitchedToNewEmptyAndFilledGlobalList
    | NewRankingRequestedByConfirmBtnClick
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | NameInputChg String
    | DescInputChg String
    | AddedNewRankingToGlobalList (RemoteData.WebData (List SR.Types.RankingInfo))



--| SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.Player)
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
    ( AllRankingsJson RemoteData.Loading "" "" SR.Types.RenderAllRankings
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


createNewPlayerListWithCurrentUser : Cmd Msg
createNewPlayerListWithCurrentUser =
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
    --SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- decoder relates to what comes back from server. Nothing to do with above.
    Http.request
        { body =
            Http.jsonBody <| idJsonObj
        , expect = Http.expectJson (RemoteData.fromResult >> SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId) SR.Decode.newRankingIdDecoder

        -- at this point we don't have the ranking id, it's in the ranking object
        --, expect = Http.expectJson (RemoteData.fromResult >> SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId RemoteData.NotAsked) SR.Decode.newRankingDecoder
        , headers = [ secretKey, binName, containerId ]
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.jsonbinUrlForCreateNewEntryAndRespond
        }



-- this also has to be done when a new ranking is created.


addedNewRankingListEntryInGlobal : RemoteData.WebData SR.Types.RankingId -> RemoteData.WebData (List SR.Types.RankingInfo) -> String -> String -> Cmd Msg
addedNewRankingListEntryInGlobal newrankingid globalList newName newDesc =
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

        justGlobalList =
            gotRankingListFromRemData globalList

        idJsonObj : Json.Encode.Value
        idJsonObj =
            --Json.Encode.list
            Json.Encode.object
                --[
                [ ( "id", Json.Encode.string <| gotNewRankingIdFromWebData newrankingid )
                , ( "ACTIVE", Json.Encode.bool True )
                , ( "RANKINGNAME", Json.Encode.string newName )
                , ( "RANKINGDESC", Json.Encode.string newDesc )
                ]

        newRankingInfo =
            { id = gotNewRankingIdFromWebData newrankingid
            , active = True
            , name = newName
            , desc = newDesc
            }

        globalListWithJsonObjAdded =
            newRankingInfo :: justGlobalList

        _ =
            Debug.log "new added ?" globalListWithJsonObjAdded

        --]
    in
    --SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| idJsonObj

        --Http.jsonBody <| justGlobalList
        --Http.jsonBody <| globalListWithJsonObjAdded
        --, expect = Http.expectJson (RemoteData.fromResult >> SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId) SR.Decode.newRankingIdDecoder
        , expect = Http.expectJson (RemoteData.fromResult >> AddedNewRankingToGlobalList) SR.Decode.rankingsDecoder

        --, expect = Http.expectWhatever
        , headers = [ secretKey, binName, containerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingUpdateLink
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
--  Update component contains code that should be executed for each message generated by the view
-- or a cmd/sub


update : Msg -> Model -> ( Model, Cmd Msg )
update msgOfTransitonThatAlreadyHappened previousmodel =
    case msgOfTransitonThatAlreadyHappened of
        GotJsonbinAllRankings rmtdata ->
            case rmtdata of
                --removes[?] the first record (created on ranking creation with different format)
                RemoteData.Success a ->
                    ( AllRankingsJson (RemoteData.Success a) "" "" SR.Types.RenderAllRankings, Cmd.none )

                RemoteData.Failure e ->
                    ( AllRankingsJson (RemoteData.Failure e) "" "" SR.Types.RenderAllRankings, Cmd.none )

                RemoteData.NotAsked ->
                    ( AllRankingsJson RemoteData.NotAsked "" "" SR.Types.RenderAllRankings, Cmd.none )

                RemoteData.Loading ->
                    ( AllRankingsJson RemoteData.Loading "" "" SR.Types.RenderAllRankings, Cmd.none )

        ChangedUIStateToCreateNew ->
            case previousmodel of
                AllRankingsJson globalList _ _ _ ->
                    --( AddingNewRankingToGlobalList RemoteData.Loading RemoteData.NotAsked globalList, Cmd.none )
                    ( AllRankingsJson globalList "" "" SR.Types.CreateNewLadder, Cmd.none )

                _ ->
                    ( ModelFailure "Error in SwitchedToNewEmptyAndFilledGlobalList", Cmd.none )

        -- SwitchedToNewEmptyAndFilledGlobalList ->
        --     case previousmodel of
        --         AllRankingsJson globalList _ _ _ ->
        --             ( AddingNewRankingToGlobalList RemoteData.Loading RemoteData.NotAsked globalList, Cmd.none )
        --         _ ->
        --             ( ModelFailure "Error in SwitchedToNewEmptyAndFilledGlobalList", Cmd.none )
        -- AddingNewRankingToGlobalList _ _ _ ->
        --this fires the createNewPlayerListWithCurrentUser Cmd
        -- from the button (which only accepts Msg not Cmd.Msg)
        NewRankingRequestedByConfirmBtnClick ->
            --( AddingNewRankingToGlobalList RemoteData.Loading RemoteData.NotAsked RemoteData.NotAsked, createNewPlayerListWithCurrentUser )
            case previousmodel of
                AllRankingsJson globalList newrankingName newRankingDesc _ ->
                    ( AllRankingsJson globalList newrankingName newRankingDesc SR.Types.CreateNewLadder, createNewPlayerListWithCurrentUser )

                _ ->
                    ( ModelFailure "Error in NewRankingRequestedByConfirmBtnClick", Cmd.none )

        -- this is the response from createNewPlayerListWithCurrentUser Cmd
        -- it had the Http.expectStringResponse in it
        -- it's already created the new ranking with current player as the first entry
        -- the result now is the ranking id only at this point which was pulled out by the decoder
        -- the globalList is preserved
        SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder ->
            --( AddingNewRankingToGlobalList idValueFromDecoder RemoteData.NotAsked RemoteData.NotAsked, addedNewRankingListEntryInGlobal idValueFromDecoder )
            case previousmodel of
                AllRankingsJson globalList newrankingName newRankingDesc _ ->
                    ( AllRankingsJson globalList newrankingName newRankingDesc SR.Types.CreateNewLadder, addedNewRankingListEntryInGlobal idValueFromDecoder globalList newrankingName newRankingDesc )

                _ ->
                    ( ModelFailure "Error in SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId", Cmd.none )

        AddedNewRankingToGlobalList updatedListAfterNewEntryAddedToGlobalList ->
            ( AllRankingsJson updatedListAfterNewEntryAddedToGlobalList "" "" SR.Types.RenderAllRankings, Cmd.none )

        NameInputChg namefield ->
            case previousmodel of
                AllRankingsJson list _ desc _ ->
                    ( AllRankingsJson list namefield desc SR.Types.CreateNewLadder, Cmd.none )

                _ ->
                    ( ModelFailure "Error in InputChg", Cmd.none )

        DescInputChg descfield ->
            case previousmodel of
                AllRankingsJson list name _ _ ->
                    ( AllRankingsJson list name descfield SR.Types.CreateNewLadder, Cmd.none )

                _ ->
                    ( ModelFailure "Error in InputChg", Cmd.none )



--have to pass both sets of info in as AddingNewRankingToGlobalList requires it ...
-- NewRankingInfoConfirmedByBtnClick playerinfo rankinfo rankId ->
--     ( AddingNewRankingToGlobalList playerinfo rankinfo rankId, updateNewRankingListInfoToJsonbin playerinfo rankinfo )
-- SentNewRankingInfoToJsonbin playerinfo rankinfo rankId ->
--     ( AddingNewRankingToGlobalList playerinfo rankinfo, Cmd.none )


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
        AllRankingsJson rnkingList _ _ _ ->
            Element.column Grid.section <|
                [ Element.el Heading.h2 <| Element.text "Global Rankings"
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left) <| createnewRankingbutton Color.primary ChangedUIStateToCreateNew "Create New"
                        , Element.el (Card.fill ++ Group.center ++ Color.disabled) <| joinbutton Color.primary NewRankingRequestedByConfirmBtnClick "Join"
                        , Element.el (Card.fill ++ Group.right ++ Color.disabled) <| enterResultbutton Color.primary NewRankingRequestedByConfirmBtnClick "Enter Result"
                        , Element.el (Card.fill ++ Group.top ++ Color.disabled) <| updateProfilebutton Color.primary NewRankingRequestedByConfirmBtnClick "Update Profile"
                        ]
                    ]
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left) <| currentView model
                        ]
                    ]

                -- , [ Element.wrappedRow Grid.simple
                --         [ Element.el (Card.fill ++ Group.left ++ Color.disabled) <| createnewRankingbutton Color.primary NewRankingRequestedByConfirmBtnClick "Create New"
                --         , currentView model
                --         --, Element.el (Card.fill ++ Group.bottom) <| listAllbutton Color.primary getRankingList "List All"
                --         ]
                --   ]
                ]

        AddingNewRankingToGlobalList rankingIdremdata rnkInfo globalList ->
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text <| "Create New Ranking ... new id is " ++ gotNewRankingId model

                --Element.el Heading.h2 <| Element.text <| "Create New Ranking ... new id is " ++ rankingIdremdata
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left ++ Color.disabled) <| createnewRankingbutton Color.primary NewRankingRequestedByConfirmBtnClick "Create New"
                        , Element.el (Card.fill ++ Group.center) <| joinbutton Color.primary NewRankingRequestedByConfirmBtnClick "Join"
                        , Element.el (Card.fill ++ Group.right ++ Color.disabled) <| enterResultbutton Color.primary NewRankingRequestedByConfirmBtnClick "Enter Result"
                        , Element.el (Card.fill ++ Group.top ++ Color.disabled) <| updateProfilebutton Color.primary NewRankingRequestedByConfirmBtnClick "Update Profile"
                        ]
                    ]
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left) <| currentView model
                        ]
                    ]
                ]

        ModelFailure str ->
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text <| "Something wrong  " ++ str

                --Element.el Heading.h2 <| Element.text <| "Create New Ranking ... new id is " ++ rankingIdremdata
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left ++ Color.disabled) <| createnewRankingbutton Color.primary NewRankingRequestedByConfirmBtnClick "Create New"
                        , Element.el (Card.fill ++ Group.center) <| joinbutton Color.primary NewRankingRequestedByConfirmBtnClick "Join"
                        , Element.el (Card.fill ++ Group.right ++ Color.disabled) <| enterResultbutton Color.primary NewRankingRequestedByConfirmBtnClick "Enter Result"
                        , Element.el (Card.fill ++ Group.top ++ Color.disabled) <| updateProfilebutton Color.primary NewRankingRequestedByConfirmBtnClick "Update Profile"
                        ]
                    ]
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left) <| currentView model
                        ]
                    ]
                ]


currentView : Model -> Element Msg
currentView model =
    case model of
        AllRankingsJson rmtData _ _ uiState ->
            case rmtData of
                RemoteData.NotAsked ->
                    Element.text ""

                RemoteData.Loading ->
                    Element.text "Loading..."

                RemoteData.Success rankings ->
                    viewRankings rankings uiState

                RemoteData.Failure httpError ->
                    Element.text <| buildErrorMessage httpError

        AddingNewRankingToGlobalList rankingIdremdata rnkInfo globalList ->
            input

        ModelFailure str ->
            Element.text str



--you might need this later
-- (stringFromBool ranking.active)


viewRankings : List SR.Types.RankingInfo -> SR.Types.UIState -> Element Msg
viewRankings rankings uiState =
    case uiState of
        SR.Types.RenderAllRankings ->
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

        SR.Types.CreateNewLadder ->
            input


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
                    { onChange = NameInputChg
                    , text = "e.g. Stockton On Pullet Juniors"
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Name"
                    }
                , Input.multiline Input.simple
                    { onChange = DescInputChg
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
                    { -- this btn fires createNewPlayerListWithCurrentUser from update
                      onPress = Just NewRankingRequestedByConfirmBtnClick
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



-- Helpers
-- addNewRankingToTopOfGlobalList : Model -> List SR.Types.RankingInfo
-- addNewRankingToTopOfGlobalList model =
--     -- remove the remote data and reveal the data
--     let
--         newRanking =
--             gotNewRanking model
--         -- case model of
--         --     AllRankingsJson rmtdata ->
--         --         gotNewRanking rmtdata
--         --     AddingNewRankingToGlobalList id info globalList ->
--         --         gotNewRanking info
--         newGlobalList =
--             gotRankingListfromModel model
--         -- case model of
--         --     AllRankingsJson rmtdata ->
--         --         RemoteData.NotAsked
--         --     AddingNewRankingToGlobalList id info globalList ->
--         --         globalList
--     in
--     newRanking :: newGlobalList


addNewRankingToTopOfGlobalList : SR.Types.RankingInfo -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
addNewRankingToTopOfGlobalList newRankingInfo existingGlobalList =
    -- remove the remote data and reveal the data
    -- let
    --     newRanking =
    --         gotNewRanking model
    --     -- case model of
    --     --     AllRankingsJson rmtdata ->
    --     --         gotNewRanking rmtdata
    --     --     AddingNewRankingToGlobalList id info globalList ->
    --     --         gotNewRanking info
    --     newGlobalList =
    --         gotRankingListfromModel model
    --     -- case model of
    --     --     AllRankingsJson rmtdata ->
    --     --         RemoteData.NotAsked
    --     --     AddingNewRankingToGlobalList id info globalList ->
    --     --         globalList
    -- in
    newRankingInfo :: existingGlobalList


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



-- gotNewRanking : Model -> SR.Types.RankingInfo
-- gotNewRanking model =
-- case model of
--     AllRankingsJson rmtdata _ _ _ ->
--         { id = "AllRankingsJson"
--         , active = False
--         , name = "AllRankingsJson"
--         , desc = "AllRankingsJson"
--         }
-- AddingNewRankingToGlobalList rankingIdremdata rknInfo globalList ->
--     let
--         _ =
--             Debug.log "here" rknInfo
--     in
-- case rknInfo of
--     RemoteData.Success a ->
--         a
--     -- case a of
--     --     b ->
--     --         case b of
--     --             SR.Types.RankingId c ->
--     --                 c
--     RemoteData.NotAsked ->
--         { id = "Initialising"
--         , active = False
--         , name = ""
--         , desc = ""
--         }
--     RemoteData.Loading ->
--         { id = "Loading"
--         , active = False
--         , name = ""
--         , desc = ""
--         }
--     RemoteData.Failure err ->
--         case err of
--             Http.BadUrl s ->
--                 { id = s
--                 , active = False
--                 , name = ""
--                 , desc = ""
--                 }
--             Http.Timeout ->
--                 { id = "Timeout"
--                 , active = False
--                 , name = ""
--                 , desc = ""
--                 }
--             Http.NetworkError ->
--                 { id = "0"
--                 , active = False
--                 , name = "Network Err"
--                 , desc = ""
--                 }
--             Http.BadStatus statuscode ->
--                 { id = "0"
--                 , active = False
--                 , name = String.fromInt <| statuscode
--                 , desc = ""
--                 }
--             Http.BadBody s ->
--                 { id = "0"
--                 , active = False
--                 , name = s
--                 , desc = ""
--                 }
-- ModelFailure str ->
--     { id = "0"
--     , active = False
--     , name = str
--     , desc = ""
--     }


gotRankingListFromRemData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
gotRankingListFromRemData globalList =
    case globalList of
        RemoteData.Success a ->
            a

        -- case a of
        --     b ->
        --         case b of
        --             SR.Types.RankingId c ->
        --                 c
        RemoteData.NotAsked ->
            [ { id = "Initialising"
              , active = False
              , name = ""
              , desc = ""
              }
            ]

        RemoteData.Loading ->
            [ { id = "Loading"
              , active = False
              , name = ""
              , desc = ""
              }
            ]

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    [ { id = s
                      , active = False
                      , name = ""
                      , desc = ""
                      }
                    ]

                Http.Timeout ->
                    [ { id = "Timeout"
                      , active = False
                      , name = ""
                      , desc = ""
                      }
                    ]

                Http.NetworkError ->
                    [ { id = "0"
                      , active = False
                      , name = "Network Err"
                      , desc = ""
                      }
                    ]

                Http.BadStatus statuscode ->
                    [ { id = "0"
                      , active = False
                      , name = String.fromInt <| statuscode
                      , desc = ""
                      }
                    ]

                Http.BadBody s ->
                    [ { id = "0"
                      , active = False
                      , name = s
                      , desc = ""
                      }
                    ]



--         ModelFailure str ->
--             [ { id = "0"
--               , active = False
--               , name = str
--               , desc = ""
--               }
--             ]
-- gotNewRankingId : Model -> String
-- gotNewRankingId model =
--     case model of
--         AllRankingsJson rmtdata ->
--             "Not in AddingNewRankingToGlobalList!"
--         AddingNewRankingToGlobalList rankingIdremdata rknInfo globalList ->
--             case rknInfo of
--                 RemoteData.Success arknInfo ->
--                     case arknInfo of
--                         b ->
--                             case b of
--                                 SR.Types.RankingId c ->
--                                     c
--                 RemoteData.NotAsked ->
--                     "Initialising."
--                 RemoteData.Loading ->
--                     "Loading."
--                 RemoteData.Failure err ->
--                     case err of
--                         Http.BadUrl s ->
--                             "Bad Url"
--                         Http.Timeout ->
--                             "Timeout"
--                         Http.NetworkError ->
--                             "Network Err"
--                         Http.BadStatus statuscode ->
--                             String.fromInt <| statuscode
--                         Http.BadBody s ->
--                             "BadBody " ++ s


gotNewRankingId : Model -> String
gotNewRankingId model =
    case model of
        AllRankingsJson rmtdata _ _ _ ->
            "Not in AddingNewRankingToGlobalList!"

        AddingNewRankingToGlobalList rankingIdremdata rknInfo globalList ->
            case rankingIdremdata of
                RemoteData.Success a ->
                    case a of
                        b ->
                            case b of
                                SR.Types.RankingId c ->
                                    c

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

        ModelFailure str ->
            str


gotNewRankingIdFromWebData : RemoteData.WebData SR.Types.RankingId -> String
gotNewRankingIdFromWebData rankingIdremdata =
    case rankingIdremdata of
        RemoteData.Success a ->
            case a of
                b ->
                    case b of
                        SR.Types.RankingId c ->
                            c

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

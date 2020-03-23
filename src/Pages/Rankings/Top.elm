module Pages.Rankings.Top exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Eth.Types
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
import Http
import Json.Decode
import Json.Encode
import RemoteData
import SR.Constants
import SR.Decode
import SR.Defaults
import SR.Types
import Spa.Page
import Ui
import Utils.MyUtils
import Utils.Spa exposing (Page)


page : Page Params.Top Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Rankings.Top"
        , init = init
        , update = always update
        , subscriptions = always subscriptions
        , view = always view
        }



--variants are labels for different situations, with assoc data possibly attached.
-- type RemoteData e a
--     = NotAsked
--     | Loading
--     | Failure e
--     | Success a
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
    | ModelFailure String



-- Msg is a description of the transition that already happened
-- Messages that delivered the response (orign doc says 'will deliver')


type Msg
    = GotJsonbinAllRankings (RemoteData.WebData (List SR.Types.RankingInfo))
    | UserChangedUIStateToRenderAll (RemoteData.WebData (List SR.Types.RankingInfo))
    | ChangedUIStateToCreateNew
    | NewRankingRequestedByConfirmBtnClicked
    | SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId (RemoteData.WebData SR.Types.RankingId)
    | NameInputChg String
    | DescInputChg String
    | AddedNewRankingToGlobalList (RemoteData.WebData (List SR.Types.RankingInfo))



-- INIT
-- this accesses COLLECTION RECORDS - GLOBAL - public bin


init : Utils.Spa.PageContext -> Params.Top -> ( Model, Cmd Msg )
init context _ =
    let
        uaddr =
            case context.global of
                Global.GlobalModel userState ->
                    case userState of
                        SR.Types.NewUser ->
                            ""

                        SR.Types.ExistingUser a ->
                            Utils.MyUtils.addressToString <| Just a

                Global.Failure str ->
                    "failed"

        _ =
            Debug.log "useraddress " uaddr
    in
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

        _ =
            Debug.log "new ranking owner address in addedNewRankingListEntryInGlobal: " "it will go here"

        justGlobalList =
            gotRankingListFromRemData globalList

        newRankingInfo =
            { id = gotNewRankingIdFromWebData newrankingid
            , active = True
            , name = newName
            , desc = newDesc
            , rankingowneraddr = "0x847700B781667abdD98E1393420754E503dca5b7"
            }

        globalListWithJsonObjAdded =
            newRankingInfo :: justGlobalList
    in
    --AddedNewRankingToGlobalList is the Msg handled by update whenever a request is made
    --RemoteData is used throughout the module, including update
    -- using Http.jsonBody means json header automatically applied. Adding twice will break functionality
    -- the Decoder decodes what comes back in the response
    Http.request
        { body =
            Http.jsonBody <| jsonEncodeNewGlobalRankingList globalListWithJsonObjAdded
        , expect = Http.expectJson (RemoteData.fromResult >> AddedNewRankingToGlobalList) SR.Decode.decodeNewRankingListServerResponse
        , headers = [ secretKey, binName, containerId ]
        , method = "PUT"
        , timeout = Nothing
        , tracker = Nothing
        , url = SR.Constants.globalJsonbinRankingUpdateLink
        }


jsonEncodeNewGlobalRankingList : List SR.Types.RankingInfo -> Json.Encode.Value
jsonEncodeNewGlobalRankingList globalList =
    let
        encodeAglobalRankingObj : SR.Types.RankingInfo -> Json.Encode.Value
        encodeAglobalRankingObj rankingInfo =
            Json.Encode.object
                [ ( "id", Json.Encode.string rankingInfo.id )
                , ( "active", Json.Encode.bool rankingInfo.active )
                , ( "rankingname", Json.Encode.string rankingInfo.name )
                , ( "rankingdesc", Json.Encode.string rankingInfo.desc )
                , ( "rankingowneraddr", Json.Encode.string rankingInfo.rankingowneraddr )
                ]

        encodedList =
            Json.Encode.list encodeAglobalRankingObj globalList

        _ =
            Debug.log "encode the list: " encodedList
    in
    encodedList



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
                    ( AllRankingsJson globalList "" "" SR.Types.CreateNewLadder, Cmd.none )

                _ ->
                    ( ModelFailure "Error in SwitchedToNewEmptyAndFilledGlobalList", Cmd.none )

        UserChangedUIStateToRenderAll globalList ->
            ( AllRankingsJson globalList "" "" SR.Types.RenderAllRankings, Cmd.none )

        --this fires the createNewPlayerListWithCurrentUser Cmd
        -- from the button (which only accepts Msg not Cmd.Msg)
        NewRankingRequestedByConfirmBtnClicked ->
            case previousmodel of
                AllRankingsJson globalList newrankingName newRankingDesc _ ->
                    ( AllRankingsJson globalList newrankingName newRankingDesc SR.Types.CreateNewLadder, createNewPlayerListWithCurrentUser )

                _ ->
                    ( ModelFailure "Error in NewRankingRequestedByConfirmBtnClicked", Cmd.none )

        -- this is the response from createNewPlayerListWithCurrentUser Cmd
        -- it had the Http.expectStringResponse in it
        -- it's already created the new ranking with current player as the first entry
        -- the result now is the ranking id only at this point which was pulled out by the decoder
        -- the globalList is preserved
        SentCurrentPlayerInfoAndDecodedResponseToJustNewRankingId idValueFromDecoder ->
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


renderView : Model -> Element Msg
renderView model =
    html <|
        Framework.responsiveLayout [ Element.explain Debug.todo ] <|
            -- Element.el Framework.container <|
            --     Element.paragraph [ Element.explain Debug.todo ] <|
            --         listOfElementmsgs
            --             model
            Element.column
                Framework.container
                [ Element.el Heading.h1 <| Element.text "SportRank"

                -- , heading
                -- , tag
                -- , group
                -- , color
                -- , card
                -- , grid
                -- , button
                -- , input
                -- , slider
                , gotGroupView
                    model
                ]


listOfElementmsgs : Model -> List (Element Msg)
listOfElementmsgs model =
    [ gotGroupView model

    --, currentView model
    ]


gotGroupView : Model -> Element Msg
gotGroupView model =
    let
        _ =
            Debug.log "new ranking owner address in gotGroupView: " "it will go here"
    in
    case model of
        AllRankingsJson rnkingList _ _ _ ->
            Element.column Grid.section <|
                [ Element.el Heading.h2 <| Element.text "Username"
                , Element.column Grid.simple <|
                    [ Element.wrappedRow Grid.simple
                        [ Element.column (Card.simple ++ Grid.spaceEvenly) <|
                            [ Element.el Heading.h4 <| Element.text "Manage Your Rankings ..."
                            , Element.row Grid.spacedEvenly <|
                                [ Element.el (Card.fill ++ Group.left) <| createnewRankingbutton Color.primary ChangedUIStateToCreateNew "Create New"
                                , Element.el Card.simple <| joinbutton Color.primary NewRankingRequestedByConfirmBtnClicked "Join"
                                , Element.el Card.simple <| enterResultbutton Color.primary NewRankingRequestedByConfirmBtnClicked "Enter Result"
                                , Element.el Card.simple <| updateProfilebutton Color.primary NewRankingRequestedByConfirmBtnClicked "Update Profile"
                                ]
                            ]
                        , Element.column (Card.fill ++ Grid.simple)
                            [ Element.wrappedRow Grid.simple
                                [ Element.el (Card.fill ++ Group.left) <| currentView model
                                ]
                            ]
                        ]
                    ]
                ]

        ModelFailure str ->
            Element.column Grid.section <|
                [ Element.el Heading.h5 <| Element.text <| "Model failure  " ++ str

                --Element.el Heading.h2 <| Element.text <| "Create New Ranking ... new id is " ++ rankingIdremdata
                , Element.column (Card.fill ++ Grid.simple)
                    [ Element.wrappedRow Grid.simple
                        [ Element.el (Card.fill ++ Group.left ++ Color.disabled) <| createnewRankingbutton Color.primary NewRankingRequestedByConfirmBtnClicked "Create New"
                        , Element.el (Card.fill ++ Group.center) <| joinbutton Color.primary NewRankingRequestedByConfirmBtnClicked "Join"
                        , Element.el (Card.fill ++ Group.right ++ Color.disabled) <| enterResultbutton Color.primary NewRankingRequestedByConfirmBtnClicked "Enter Result"
                        , Element.el (Card.fill ++ Group.top ++ Color.disabled) <| updateProfilebutton Color.primary NewRankingRequestedByConfirmBtnClicked "Update Profile"
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
                    viewRankings model rankings uiState

                RemoteData.Failure httpError ->
                    Element.text <| buildErrorMessage httpError

        ModelFailure str ->
            Element.text str



-- gotRankingsListFromResponse : List SR.Types.RankingInfo -> List SR.Types.RankingInfo
-- gotRankingsListFromResponse jsonResponseWithRankings =
--     jsonResponseWithRankings.data
--Json.Decode.decodeO SR.Decode.rankingsDecoder
--you might need this later
-- (stringFromBool ranking.active)


viewRankings : Model -> List SR.Types.RankingInfo -> SR.Types.UIState -> Element Msg
viewRankings model rankings uiState =
    case uiState of
        SR.Types.RenderAllRankings ->
            Element.column Grid.spacedEvenly <|
                [ Element.wrappedRow Grid.spacedEvenly
                    [ Element.column Card.fill <|
                        [ Element.el Heading.h4 <| Element.text "Click ranking name to select a ranking ..."
                        , Element.row Grid.spacedEvenly <|
                            [ Element.table
                                [--     Element.padding 25
                                 -- , spacing 15
                                 -- , Background.color Ui.colors.white
                                 -- , Border.solid
                                 -- , Border.color Ui.colors.black
                                 -- , Border.widthXY 1 1
                                 -- , Border.rounded 3
                                 --Element.width fill
                                 -- , Font.size
                                 --     30
                                 -- , width
                                 --     fill
                                ]
                                { data = rankings
                                , columns =
                                    [ rankingNameCol rankings "Ranking Name"
                                    , rankingDescCol rankings "Ranking Desc"
                                    ]
                                }
                            ]
                        ]
                    ]
                ]

        --]
        -- SR.Types.RenderAllRankings ->
        --     input (RemoteData.Success rankings)
        SR.Types.CreateNewLadder ->
            input model (RemoteData.Success rankings)



--input RemoteData.NotAsked


rankingNameCol : List SR.Types.RankingInfo -> String -> Column SR.Types.RankingInfo msg
rankingNameCol _ str =
    { header = Element.el [ Font.size 25 ] (Element.text str)
    , width = fill
    , view =
        \rankingInfo ->
            Element.row
                [--     Font.color Ui.colors.lightblue
                 -- , Border.widthXY 2 2
                 --   padding 5
                 -- , spacing 5
                 -- , width fill
                 --Font.size 25
                ]
                [ Element.link
                    [--Background.color Ui.colors.blue
                     --Font.color Ui.colors.lightblue
                     --padding 5
                     --, Border.widthXY 2 2
                    ]
                    { url = "/rankings/" ++ rankingInfo.id
                    , label = Element.text rankingInfo.name
                    }
                ]
    }


rankingDescCol : List SR.Types.RankingInfo -> String -> Column SR.Types.RankingInfo msg
rankingDescCol _ str =
    { header = Element.el [ Font.size 25 ] (Element.text str)
    , width = fill
    , view =
        \rankingInfo ->
            Element.row
                [--     Border.widthXY 2 2
                 -- , Font.color Ui.colors.green
                 -- width fill
                 --Element.padding 25
                 --Font.size 25
                 -- , paddingXY
                 --     5
                 --     10
                 -- , spacing 5
                 --, alignLeft
                ]
                [ Element.text rankingInfo.desc
                ]
    }


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


input : Model -> RemoteData.WebData (List SR.Types.RankingInfo) -> Element Msg
input model rankings =
    let
        updatedname =
            case model of
                AllRankingsJson _ b c _ ->
                    b

                ModelFailure s ->
                    "no name"

        updateddesc =
            case model of
                AllRankingsJson _ b c _ ->
                    c

                ModelFailure s ->
                    "no name"
    in
    Element.column Grid.section <|
        [ Element.el Heading.h2 <| Element.text "Please name and describe your new ranking"
        , Element.wrappedRow (Card.fill ++ Grid.simple)
            [ Element.column Grid.simple
                [ Input.text Input.simple
                    { --onChange = Just NameInputChg
                      onChange = NameInputChg
                    , text = updatedname
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Name"
                    }
                , Input.multiline Input.simple
                    { onChange = DescInputChg
                    , text = updateddesc
                    , placeholder = Nothing
                    , label = Input.labelLeft Input.label <| Element.text "Description"
                    , spellcheck = False
                    }
                ]
            ]
        , Element.column (Card.simple ++ Grid.simple) <|
            [ Element.wrappedRow Grid.simple <|
                [ Input.button Framework.Button.simple <|
                    { onPress = Just (UserChangedUIStateToRenderAll rankings)
                    , label = Element.text "Cancel"
                    }
                , Input.button Framework.Button.fill <|
                    { -- this btn fires createNewPlayerListWithCurrentUser from update
                      onPress = Just NewRankingRequestedByConfirmBtnClicked
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


gotRankingListFromRemData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
gotRankingListFromRemData globalList =
    case globalList of
        RemoteData.Success a ->
            a

        RemoteData.NotAsked ->
            [ SR.Defaults.emptyRankingInfo
            ]

        RemoteData.Loading ->
            [ SR.Defaults.emptyRankingInfo
            ]

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.Timeout ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.NetworkError ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.BadStatus statuscode ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.BadBody s ->
                    [ SR.Defaults.emptyRankingInfo
                    ]


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

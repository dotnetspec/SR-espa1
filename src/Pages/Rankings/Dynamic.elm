--standard elm-spa dynamic file. For this app where most of the functionality is implemented
module Pages.Rankings.Dynamic exposing (Model, Msg, page)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as E
import Element.Font as Font
import Element.Input as Input
import Generated.Rankings.Params
import Http
import RemoteData
import Spa.Page
import Ui
import Utils.Spa
import Eth
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Tx
import Eth.Sentry.Wallet
import Eth.Types
import Eth.Units
import Eth.Utils
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Process
import Task
import Ports
import Internal.Types as Internal
import SR.Types


-- {
--    "DATESTAMP": 1569839363942,
--    "ACTIVE": true,
--    "CURRENTCHALLENGERNAME": "testuser1",
--    "CURRENTCHALLENGERID": 3,
--    "ADDRESS": "0xD99eB29299CEF8726fc688180B30E634827b3078",
--    "RANK": 1,
--    "NAME": "GanacheAcct2",
--    "id": 2,
--    "CURRENTCHALLENGERADDRESS": "0x48DF2ee04DFE67902B83a670281232867e5dC0Ca"
--  },




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
--i.e. Spa.Page.Element init now expects to output
-- Spa.Page.PageContext route globalModel as an arg to init


page : Utils.Spa.Page Generated.Rankings.Params.Dynamic Model Msg model msg appMsg
page =
    Spa.Page.element
        { title = always "Rankings.Dynamic"

        --, init = \pageContext -> init pageContext.global.networkId
        , init = init
        , update = always update
        , subscriptions = always subscriptions
        , view = view
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
      players : RemoteData.WebData (List SR.Types.Player)
    , fetchedContentNotPlayerList : String
    , error : String
    , rankingid : String
    , modalState : ModalState
    , playerid : Int
    , player : SR.Types.Player
    , selectedRadio : ResultOptions
    , tempMsg : String
    , txSentry : Eth.Sentry.Tx.TxSentry Msg
    , account : Maybe Eth.Types.Address
    , node : Ports.EthNode
    , blockNumber : Maybe Int
    , txHash : Maybe Eth.Types.TxHash
    , tx : Maybe Eth.Types.Tx
    , txReceipt : Maybe Eth.Types.TxReceipt
    , blockDepth : Maybe Eth.Sentry.Tx.TxTracker
    , errors : List String
    }


type Msg
    = TxSentryMsg Eth.Sentry.Tx.Msg
    | WalletStatus Eth.Sentry.Wallet.WalletSentry
    | PollBlock (Result Http.Error Int)
    | InitTx
    | WatchTxHash (Result String Eth.Types.TxHash)
    | WatchTx (Result String Eth.Types.Tx)
    | WatchTxReceipt (Result String Eth.Types.TxReceipt)
    | TrackTx Eth.Sentry.Tx.TxTracker
    | Fail String
    | NoOp
    | PlayersReceived (RemoteData.WebData (List SR.Types.Player))
    | FetchedContent (Result Http.Error String)
    | OpenModal Int
    | CloseModal
    | SetRadioOption ResultOptions
    | ChangePlayerRank SR.Types.Player

-- this is where the types matter and are actually set to values ...
--init : Int -> ( Model, Cmd Msg )
init : Utils.Spa.PageContext -> Generated.Rankings.Params.Dynamic -> ( Model, Cmd Msg )
init pageContext { param1 } =
--init networkId =
    let
        node =
            --Net.toNetworkId networkId
            Net.toNetworkId 4
                |> Ports.ethNode
    in
    ( { txSentry = Eth.Sentry.Tx.init ( Ports.txOut, Ports.txIn ) TxSentryMsg node.http
      , account = Nothing
      , node = node
      , blockNumber = Nothing
      , txHash = Nothing
      , tx = Nothing
      , txReceipt = Nothing
      , blockDepth = Nothing
      , errors = []
      , players = RemoteData.NotAsked
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
      , tempMsg = "Not confirmed yet"
      }
    , Cmd.batch [Ports.log "Hello!", fetchRanking (Internal.RankingId param1), Task.attempt PollBlock (Eth.getBlockNumber node.http)]
    )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.walletSentry (Eth.Sentry.Wallet.decodeToMsg Fail WalletStatus)
        , Eth.Sentry.Tx.listen model.txSentry
        ]

--Http and assoc Json en/decoders
fetchRanking : Internal.RankingId -> Cmd Msg
fetchRanking (Internal.RankingId rankingId) =
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


-- this put on hold 16 Feb 2020 until create new player functionality in place
--updateRanking : RemoteData.WebData Ranking -> Cmd Msg
-- updateRanking: Model -> Cmd Msg
-- updateRanking model =
--      let
--         _ =
--             Debug.log "rankingid in model" model.rankingid

--         headerKey =
--             Http.header
--                 "secret-key"
--                 "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"
--     in
--             Http.request
--                 { method = "PATCH"
--                 , headers = [headerKey]
--                 , url = "https://api.jsonbin.io/b/" ++ model.rankingid
--                 , body = Http.jsonBody (postEncoder rankingData)
--                 , expect = Http.expectJson PostSaved postDecoder
--                 , timeout = Nothing
--                 , tracker = Nothing
--                 }

        -- _ ->
        --     Cmd.none

-- Use Msg types to pattern match and trigger different updates in update
-- define according to variant. Pass the variant the nec. type (if nec.)
-- if change here, change update, and change wherever the Msg is called from


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

        ChangePlayerRank newPlayer ->
             ( { model | player = newPlayer }, Cmd.none )

        SetRadioOption val ->
            ( { model | selectedRadio = val }, Cmd.none )

        TxSentryMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Eth.Sentry.Tx.update subMsg model.txSentry
            in
            ( { model | txSentry = subModel }, subCmd )

        WalletStatus walletSentry_ ->
            ( { model
                | account = walletSentry_.account
                , node = Ports.ethNode walletSentry_.networkId
              }
            , Cmd.none
            )

        PollBlock (Ok blockNumber) ->
            ( { model | blockNumber = Just blockNumber }
            , Task.attempt PollBlock <|
                Task.andThen (\_ -> Eth.getBlockNumber model.node.http) (Process.sleep 1000)
            )

        PollBlock (Err error) ->
            ( model, Cmd.none )

        InitTx ->
            let
                _ =
                    Debug.log "list of players" model.players
                txParams =
                    { to = model.account
                    , from = model.account
                    , gas = Nothing
                    , gasPrice = Just <| Eth.Units.gwei 4
                    , value = Just <| Eth.Units.gwei 1
                    , data = Nothing
                    , nonce = Nothing
                    }

                ( newSentry, sentryCmd ) =
                    Eth.Sentry.Tx.customSend
                        model.txSentry
                        { onSign = Just WatchTxHash
                        , onBroadcast = Just WatchTx
                        , onMined = Just ( WatchTxReceipt, Just { confirmations = 3, toMsg = TrackTx } )
                        }
                        txParams
            in
            ( { model | txSentry = newSentry }, sentryCmd )

        WatchTxHash (Ok txHash) ->
            ( { model | txHash = Just txHash }, Cmd.none )

        WatchTxHash (Err err) ->
            ( { model | errors = ("Error Retrieving TxHash: " ++ err) :: model.errors }, Cmd.none )

        WatchTx (Ok tx) ->
            ( { model | tx = Just tx }, Cmd.none )

        WatchTx (Err err) ->
            ( { model | errors = ("Error Retrieving Tx: " ++ err) :: model.errors }, Cmd.none )

        WatchTxReceipt (Ok txReceipt) ->
            ( { model | txReceipt = Just txReceipt }, Cmd.none )

        WatchTxReceipt (Err err) ->
            ( { model | errors = ("Error Retrieving TxReceipt: " ++ err) :: model.errors }, Cmd.none )

        TrackTx blockDepth ->
            ( { model | blockDepth = Just blockDepth }, Cmd.none )

        Fail str ->
            let
                _ =
                    Debug.log str
            in
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

-- VIEW
-- display whatever is in the model


view : Utils.Spa.PageContext -> Model -> Element.Element Msg
view context model =
    --  add any extra views on a new line eg.
    --, element.el [ Font.bold ] (text "this is bold")
    Element.row []
        [ viewWithModalReady context model
        ]



--modal is being used to pick out a single player and his opponent
--so that a result can be entered


viewWithModalReady : Utils.Spa.PageContext -> Model -> Element.Element Msg
viewWithModalReady context model =
    let
        modalString =
            case model.modalState of
                Open ->
                    "Open"

                Closed ->
                    --validateAddress model.account ++ " you are currently ranked " ++ String.fromInt model.player.rank ++ " \nand your challenger is " ++ model.player.currentchallengername
    
                    context.global.username ++ " you are currently ranked " ++ String.fromInt model.player.rank ++ " \nand your challenger is " ++ model.player.currentchallengername
   

    
    in
    -- html turns html Msg into Element Msg
    Element.html
        (Element.layout
            [ Element.inFront <| viewModal model ]
         <|
           Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 20
                ]
            <|
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 50
                    ]
                    [ Element.el [ Font.color (Element.rgb 0.2 0.2 0.2) ] (Element.text modalString)
                    , Element.el [] <| viewPlayersOrError model
                    , Element.el [ Font.color (Element.rgb 0.2 0.2 0.2) ] (Element.text model.tempMsg)

                    --below was the original 'Open' button from the ellie example (now handled by Result btn)
                    --, Element.el [ centerX ] <| playeridbtn (rgb 0.25 0.75 0.75) (OpenModal model.player.id) "Opensadfa"
                    ]
        )


validateAddress : Maybe Eth.Types.Address -> String
validateAddress addr =
  case addr of
    Nothing ->
      "No address"

    Just a ->
       Eth.Utils.addressToString a 


viewModal : Model -> Element.Element Msg
viewModal model =
    let
        box =
            Element.el
                [ Element.width (Element.px 300)
                , Element.height (Element.px 300)
                , Element.centerX
                , Element.centerY
                , Background.color (Element.rgb 1 1 1)
                ]
            <|
                Element.el [ Element.centerX, Element.centerY ] <|
                    Element.textColumn []
                        [ Element.row []
                            [ Element.el
                                []
                                (viewplayer model)
                            ]
                        , Element.row []
                            [ Element.el []
                                (confirmbutton
                                    (Element.rgb 0.95 0.6 0.25)
                                    InitTx
                                    "Confirm"
                                 --div [] [ button [ onClick InitTx ] [ text "Yup Send 0 value Tx to yourself as a test yup" ] ]
                                )
                            ]
                        , Element.row []
                            [ Element.el []
                                (closebutton
                                    (Element.rgb 0.95 0.6 0.25)
                                    CloseModal
                                    "Close"
                                )
                            ]
                        ]
    in
    case model.modalState of
        Open ->
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.behindContent <|
                    Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Background.color (Element.rgba 0.5 0.5 0.5 0.7)
                        , E.onClick CloseModal
                        ]
                        Element.none
                ]
                box

        Closed ->
            Element.el [] Element.none


closebutton : Element.Color -> Msg -> String -> Element.Element Msg
closebutton color msg label =
    Input.button
        [ Element.padding 20
        , Background.color color
        ]
        { onPress = Just msg
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.center
                , Font.color (Element.rgba 0.2 0.2 0.2 0.9)
                ]
                (Element.text label)
        }


confirmbutton : Element.Color -> Msg -> String -> Element.Element Msg
confirmbutton color msg label =
    Input.button
        [ Element.padding 20
        , Background.color color
        ]
        { onPress = Just msg
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.center
                , Font.color (Element.rgba 0.2 0.2 0.2 0.9)
                ]
                (Element.text label)
        }

-- dothisOnPress : String -> Msg -> Msg
-- dothisOnPress  str msg = 
--     -- this is going to run and update with InitTx 
--     msg str


playersResultBtnCol : List SR.Types.Player -> String -> Element.Column SR.Types.Player Msg
playersResultBtnCol players str =
    { header = Element.text str
    , width = Element.fill
    , view =
        \player ->
            Element.row
                [ Font.color Ui.colors.lightblue
                , Border.widthXY 2 2
                ]
                [ 
                    playeridbtn Ui.colors.blue (OpenModal player.id) "Result"
                   -- playeridbtn Ui.colors.blue (ChangePlayerRank (updatedPlayerRank player 5)) "Result"
                    
                ]
    }


playersNameCol : List SR.Types.Player -> String -> Element.Column SR.Types.Player msg
playersNameCol players str =
    { header = Element.text str
    , width = Element.fill
    , view =
        \player ->
            Element.row
                [ Border.widthXY 2 2
                , Font.color Ui.colors.green
                ]
                [ Element.text player.name
                ]
    }


playersRankCol : List SR.Types.Player -> String -> Element.Column SR.Types.Player msg
playersRankCol players str =
    { header = Element.text str
    , width = Element.fill
    , view =
        \player ->
            Element.row
                [ Border.widthXY 2 2
                , Font.color Ui.colors.green
                ]
                [ Element.text (String.fromInt player.rank)
                ]
    }


playersCurrentChallCol : List SR.Types.Player -> String -> Element.Column SR.Types.Player msg
playersCurrentChallCol players str =
    { header = Element.text str
    , width = Element.fill
    , view =
        \player ->
            Element.row
                [ Border.widthXY 2 2
                , Font.color Ui.colors.green
                ]
                [ Element.text player.currentchallengername
                ]
    }


playeridbtn : Element.Color -> Msg -> String -> Element.Element Msg
playeridbtn color selectedplayermsg label =
    Input.button
        [ Background.color color
        , Element.centerX
        , Element.centerY
        ]
        { onPress = Just selectedplayermsg
        , label =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.center
                , Font.color (Element.rgba 0.2 0.2 0.2 0.9)
                ]
                (Element.text label)
        }


retrieveSinglePlayer : Int -> List SR.Types.Player -> SR.Types.Player
retrieveSinglePlayer id players =
    let
        x =
            List.filter (\i -> i.id == id) players
    in
    case List.head x of
        Nothing ->
         emptyPlayer

        Just item ->
            item


viewPlayersOrError : Model -> Element.Element Msg
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


extractPlayersFromWebData : Model -> List SR.Types.Player
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


viewplayers : List SR.Types.Player -> Element.Element Msg
viewplayers players =
    Element.html <|
        Element.layout
            [ Element.padding 25
            , Background.color (Element.rgba 0 0 0 1)
            , Font.color (Element.rgba 1 1 1 1)
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


viewplayer : Model -> Element.Element Msg
viewplayer model =
    let
        playerToView =
            retrieveSinglePlayer model.playerid (extractPlayersFromWebData model)
    in
    Element.paragraph []
        [ Input.radio
            [ Element.spacing 12
            , Background.color Ui.colors.grey
            ]
            { selected = Just model.selectedRadio
            , onChange = SetRadioOption
            , label = Input.labelAbove [ Font.size 22, Element.paddingXY 0 12 ] (Element.text (playerToView.name ++ " would you like to \nenter a result against " ++ playerToView.currentchallengername ++ "?"))
            , options =
                [ Input.option Won (Element.text "Won")
                , Input.option Lost (Element.text "Lost")
                , Input.option Undecided (Element.text "Undecided")
                ]
            }
        ]


-- type alias Player =
--     { datestamp : Int
--     , active : Bool
--     , currentchallengername : String
--     , currentchallengerid : Int
--     , address : String
--     , rank : Int
--     , name : String
--     , id : Int
--     , currentchallengeraddress : String
--     }


ladderOfPlayersDecoder : Json.Decode.Decoder (List SR.Types.Player)
ladderOfPlayersDecoder =
    let
        _ =
            Debug.log "in ladderDecoder" playerDecoder
    in
        Json.Decode.list playerDecoder


playerDecoder : Json.Decode.Decoder SR.Types.Player
playerDecoder =
    Json.Decode.succeed SR.Types.Player
        |> Json.Decode.Pipeline.required "DATESTAMP" Json.Decode.int
        |> Json.Decode.Pipeline.required "ACTIVE" Json.Decode.bool
        |> Json.Decode.Pipeline.required "CURRENTCHALLENGERNAME" Json.Decode.string
        |> Json.Decode.Pipeline.required "CURRENTCHALLENGERID" Json.Decode.int
        |> Json.Decode.Pipeline.required "ADDRESS" Json.Decode.string
        |> Json.Decode.Pipeline.required "RANK" Json.Decode.int
        |> Json.Decode.Pipeline.required "NAME" Json.Decode.string
        |> Json.Decode.Pipeline.required "id" Json.Decode.int
        |> Json.Decode.Pipeline.required "CURRENTCHALLENGERADDRESS" Json.Decode.string



playerEncoder : SR.Types.Player -> Json.Encode.Value
playerEncoder player =
    Json.Encode.object
        [ ( "DATESTAMP", Json.Encode.int player.datestamp )
        , ( "ACTIVE"
          , Json.Encode.bool player.active
          )
        , ( "CURRENTCHALLENGERNAME"
          , Json.Encode.string player.currentchallengername
          )
        , ( "CURRENTCHALLENGERID"
          , Json.Encode.int player.currentchallengerid
          )
        , ( "ADDRESS"
          , Json.Encode.string player.address
          )
        , ( "RANK"
          , Json.Encode.int player.rank
          )
        , ( "NAME"
          , Json.Encode.string player.name
          )
        , ( "id"
          , Json.Encode.int player.id
          )
        , ( "CURRENTCHALLENGERADDRESS"
          , Json.Encode.string player.currentchallengeraddress
          )
        ]


emptyPlayer : SR.Types.Player
emptyPlayer =
    {    datestamp = 12345
    , active = False
    , currentchallengername = "Available"
    , currentchallengerid = 0
    , address = ""
    , rank = 0
    , name = "Unidentified"
    , id = 0
    , currentchallengeraddress = ""
    }

updatedPlayerRank : SR.Types.Player -> Int -> SR.Types.Player
updatedPlayerRank player rank =
    {    datestamp = player.datestamp
    , active = player.active
    , currentchallengername = player.currentchallengername
    , currentchallengerid = player.currentchallengerid
    , address = player.address
    , rank = rank
    , name = player.name
    , id = player.id
    , currentchallengeraddress = player.currentchallengeraddress
    }


emptyPlayerId : SR.Types.PlayerId
emptyPlayerId =
    Internal.PlayerId -1

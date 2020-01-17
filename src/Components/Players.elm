module Components.Players exposing (Player, PlayerId(..), emptyPlayer, emptyPlayerId, ladderOfPlayersDecoder, playerDecoder, playerEncoder)

--import Element exposing (..)
--import Spa.Page

import Generated.Rankings.Params as Params
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)



--import Utils.Spa exposing (Page)
--type alias Model =
--    ()
--type alias Msg =
--    Never
--page : Page Params.Players Model Msg model msg appMsg
--page =
--    Spa.Page.static
--        { title = always "Players"
--        , view = view
--        }
--view : Element Msg
--view =
--    text "AboutUs"


type alias Player =
    { datestamp : Int
    , active : Bool
    , currentchallengername : String
    , currentchallengerid : Int
    , address : String
    , rank : Int
    , name : String
    , id : Int
    , currentchallengeraddress : String
    }


type PlayerId
    = PlayerId Int



-- TODO: make an opaque type?
-- rankingIdToString: RankingId -> String
-- rankingIdToString rankingid =
--   rankingid


ladderOfPlayersDecoder : Decoder (List Player)
ladderOfPlayersDecoder =
    let
        _ =
            Debug.log "in ladderDecoder" playerDecoder
    in
    list playerDecoder


playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> required "DATESTAMP" int
        |> required "ACTIVE" bool
        |> required "CURRENTCHALLENGERNAME" string
        |> required "CURRENTCHALLENGERID" int
        |> required "ADDRESS" string
        |> required "RANK" int
        |> required "NAME" string
        |> required "id" int
        |> required "CURRENTCHALLENGERADDRESS" string



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


playerEncoder : Player -> Encode.Value
playerEncoder player =
    Encode.object
        [ ( "DATESTAMP", Encode.int player.datestamp )
        , ( "ACTIVE"
          , Encode.bool player.active
          )
        , ( "CURRENTCHALLENGERNAME"
          , Encode.string player.currentchallengername
          )
        , ( "CURRENTCHALLENGERID"
          , Encode.int player.currentchallengerid
          )
        , ( "ADDRESS"
          , Encode.string player.address
          )
        , ( "RANK"
          , Encode.int player.rank
          )
        , ( "NAME"
          , Encode.string player.name
          )
        , ( "id"
          , Encode.int player.id
          )
        , ( "CURRENTCHALLENGERADDRESS"
          , Encode.string player.currentchallengeraddress
          )
        ]



--
--
-- newPostEncoder : Ranking -> Encode.Value
-- newPostEncoder ranking =
--     Encode.object
--         [ ( "ACTIVE", Encode.bool ranking.active )
--         , ( "RANKINGNAME", Encode.string ranking.name )
--         , ( "RANKINGDESC", Encode.string ranking.desc )
--         ]
--
--


emptyPlayer : Player
emptyPlayer =
    { datestamp = 1
    , active = False
    , currentchallengername = ""
    , currentchallengerid = 0
    , address = ""
    , rank = 0
    , name = ""
    , id = 0
    , currentchallengeraddress = ""
    }


emptyPlayerId : PlayerId
emptyPlayerId =
    PlayerId -1

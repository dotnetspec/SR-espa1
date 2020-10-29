module PlayerTests exposing (..)

--import Random.Pcg as Random
--import lazy-list

import Element exposing (..)
import Eth.Types
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, intRange)
import Html
import Internal.Types
import Json.Encode
import Main
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas, fillIn, update)
import Random
import Random.Char
import Random.Extra
import Random.String
import SR.Defaults
import Data.Players
import Data.Selected
import SR.Types
import Shrink
import Test exposing (..)
import Testdata.UserPlayerTestData
import Json.Decode as Decode exposing (decodeValue)
import SR.Decode
import Json.Encode as Encode

-- decoderTest : Test
-- decoderTest =
--     only <| 
--      fuzz3 string int string "challengerid defaults to ('')" <|
--      \address rank challengerid ->
--          --\_ ->
--              [ ( "address", Encode.string address )
--              , ( "rank", Encode.int rank )
--              , ( "challengerid", Encode.string challengerid)
--              ]
--                  |> Encode.object
--                  |> decodeValue SR.Decode.playerDecoder
--                  |> Result.map .challengerid
--                  |> Expect.equal (Ok "")


start : ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
start =
    ProgramTest.createElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


userPlayerFuzzer : Fuzzer SR.Types.UserPlayer
userPlayerFuzzer =
    Fuzz.map2 SR.Types.UserPlayer
        playerFuzzer
        userFuzzer


playerFuzzer : Fuzzer SR.Types.Player
playerFuzzer =
    Fuzz.map3
        SR.Types.Player
        Fuzz.string
        --Fuzz.int
        (Fuzz.intRange 1 100)
        Fuzz.string


userFuzzer : Fuzzer SR.Types.User
userFuzzer =
    Fuzz.constant SR.Types.User
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap (Fuzz.list Fuzz.string)


testsortPlayerListByRank : Test
testsortPlayerListByRank =
    let
        player =
            { address = ""
            , rank = 2
            , challengerid = ""
            }

        challenger =
            { address = ""
            , rank = 1
            , challengerid = ""
            }

        listOfUserPlayers =
            [ { player = player
              , user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
              }
            , { player = challenger
              , user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
              }
            ]

        outputplayer =
            { address = ""
            , rank = 1
            , challengerid = ""
            }

        outputchallenger =
            { address = ""
            , rank = 2
            , challengerid = ""
            }

        output =
            [ { player = outputplayer
              , user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
              }
            , { player = outputchallenger
              , user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
              }
            ]
    in
    describe "testsortPlayerListByRank test"
        [ test "outputs correctly ordered list" <|
            \_ ->
                Data.Selected.sortedRank listOfUserPlayers
                    |> Expect.equal output
        ]


userPlayerRankingOrderTest : Test
userPlayerRankingOrderTest =
    let
        player =
            { address = ""
            , rank = 2
            , challengerid = ""
            }

        challenger =
            { address = ""
            , rank = 1
            , challengerid = ""
            }

        listOfUserPlayers =
            [ { player = player
              , user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
              }
            , { player = challenger
              , user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
              }
            ]

        outputplayer =
            { address = ""
            , rank = 1
            , challengerid = ""
            }

        outputchallenger =
            { address = ""
            , rank = 2
            , challengerid = ""
            }

        output =
            [ { player = outputplayer
              , user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
              }
            , { player = outputchallenger
              , user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
              }
            ]
    in
    test "However the ranking order starts out it must always be sorted " <|
        \() ->
            Data.Selected.sortedRank listOfUserPlayers
                |> Expect.equal output


sortPlayerListTest1 : Test
sortPlayerListTest1 =
    fuzz (Fuzz.list userPlayerFuzzer) "a sorted list should have the first rank == 1" <|
        \list ->
            case Data.Selected.sortedRank list of
                [] ->
                    Expect.pass

                a :: _ ->
                    Expect.equal 1 a.player.rank

-- following test needs to be converted for use with sets:
-- assignChallengerAddrTest : Test
-- assignChallengerAddrTest =
--     let
--         singleUser1 =
--             {
--                 active = True,
--                 datestamp = 1569839363942,
--                 description = "t5",
--                 email = "t5@t.com",
--                 ethaddress = "0xf5003cea9657a15123b1cc83c305f87555d190cf",
--                 mobile = "55555555",
--                 userjoinrankings = ["5e96c74b5fa47104cea0c7c6", "5e8e879d8e85c8437012e2a7"],
--                 username = "Test 5"
--             }

--         singleUser2 =
--             {
--                 active = True,
--                 datestamp = 1569839363942,
--                 description = "t4",
--                 email = "t4@t.com",
--                 ethaddress = "0x3bb244dec13253d39e22606850f4704b469a4b93",
--                 mobile = "123456",
--                 userjoinrankings = ["5e96c74b5fa47104cea0c7c6", "5e96c9ed2940c704e1d8685e"],
--                 username = "Test 4"
--             }

--         outputplayer =
--             { address = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
--             , rank = 2
--             , challengerid = "0x3bb244dec13253d39e22606850f4704b469a4b93"
--             }

--         outputchallenger =
--             { address = "0x3bb244dec13253d39e22606850f4704b469a4b93"
--             , rank = 1
--             , challengerid = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
--             }

--         output =
--             [ { player = outputplayer
--               , user = singleUser1
--               }
--             , { player = outputchallenger
--               , user = singleUser2
--               }
--             ]
--     in
--     describe "assignChallengerAddr test"
--         [
--             -- test "Sub func - Challenge successfully updates player's challenger address in list " <|
--             --     \() ->
--             --         Data.Players.assignChallengerAddr Testdata.UserPlayerTestData.userPlayerList 
--             --         Testdata.UserPlayerTestData.singleUserPlayer1 Testdata.UserPlayerTestData.singleUserPlayer2.player.uid
--             --             |> Expect.equal output
            
--             -- ,
--             --only <|
--             test "Whole func - Challenge successfully updates player's challenger address in list " <|
--                 \() ->
                    
--                     Data.Selected.assignChallengerAddr 
--                     --Testdata.UserPlayerTestData.userPlayerList 
--                     (Data.Selected.assignChallengerAddr 
--                     Testdata.UserPlayerTestData.userPlayerList 
--                     Testdata.UserPlayerTestData.singleUserPlayer2 
--                     Testdata.UserPlayerTestData.singleUserPlayer1.player.uid)
--                     Testdata.UserPlayerTestData.singleUserPlayer1 
--                     Testdata.UserPlayerTestData.singleUserPlayer2.player.uid
--                     |> Data.Selected.asList
--                     |> List.take 2 
--                     |> Expect.equal output
--         ]



-- changedRankTest : Test
-- changedRankTest =
--     only <|
--     fuzz3  (Fuzz.list userPlayerFuzzer) userPlayerFuzzer (Fuzz.intRange 1 100) "rank should change" <|
--         \luplayerF uplayerF newRankF ->
--             case Data.Players.changedRank luplayerF uplayerF newRankF  of
--                 [] ->
--                     Expect.pass
--                 a :: _ ->
--                     -- let 
--                     --     _ = Debug.log "a rank " a.player.rank 

--                     --     _ = Debug.log "uPlayer rank " newRankF
--                     -- in
--                     Expect.equal a.player.rank newRankF


extractRankFromPlayer : Maybe SR.Types.Player -> Int
extractRankFromPlayer player =
    case player of
        Just a ->
            a.rank

        Nothing ->
            0

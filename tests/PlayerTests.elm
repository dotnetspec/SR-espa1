module PlayerTests exposing (..)

--import Random.Pcg as Random
--import lazy-list

import Element exposing (..)
import Eth.Types
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html
import Internal.Types
import Json.Encode
import Main
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas, fillIn, update)
import Random
import Random.Char
import Random.Extra
import Random.String
import SR.Decode
import SR.Defaults
import SR.ListOps
import SR.Types
import Shrink
import Test exposing (..)


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
        Fuzz.int
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
            , challengeraddress = ""
            }

        challenger =
            { address = ""
            , rank = 1
            , challengeraddress = ""
            }

        listOfUserPlayers =
            [ { player = player
              , user = SR.Defaults.emptyUser
              }
            , { player = challenger
              , user = SR.Defaults.emptyUser
              }
            ]

        outputplayer =
            { address = ""
            , rank = 1
            , challengeraddress = ""
            }

        outputchallenger =
            { address = ""
            , rank = 2
            , challengeraddress = ""
            }

        output =
            [ { player = outputplayer
              , user = SR.Defaults.emptyUser
              }
            , { player = outputchallenger
              , user = SR.Defaults.emptyUser
              }
            ]
    in
    describe "testsortPlayerListByRank test"
        [ test "outputs correctly ordered list" <|
            \_ ->
                SR.ListOps.sortedPlayerListByRank listOfUserPlayers
                    |> Expect.equal output
        ]


userPlayerRankingOrderTest : Test
userPlayerRankingOrderTest =
    let
        player =
            { address = ""
            , rank = 2
            , challengeraddress = ""
            }

        challenger =
            { address = ""
            , rank = 1
            , challengeraddress = ""
            }

        listOfUserPlayers =
            [ { player = player
              , user = SR.Defaults.emptyUser
              }
            , { player = challenger
              , user = SR.Defaults.emptyUser
              }
            ]

        outputplayer =
            { address = ""
            , rank = 1
            , challengeraddress = ""
            }

        outputchallenger =
            { address = ""
            , rank = 2
            , challengeraddress = ""
            }

        output =
            [ { player = outputplayer
              , user = SR.Defaults.emptyUser
              }
            , { player = outputchallenger
              , user = SR.Defaults.emptyUser
              }
            ]
    in
    test "However the ranking order starts out it must always be sorted " <|
        \() ->
            SR.ListOps.sortedPlayerListByRank listOfUserPlayers
                |> Expect.equal output


sortPlayerListTest1 : Test
sortPlayerListTest1 =
    fuzz (Fuzz.list userPlayerFuzzer) "a sorted list should have the first rank == 1" <|
        \list ->
            case SR.ListOps.sortedPlayerListByRank list of
                [] ->
                    Expect.pass

                a :: _ ->
                    Expect.equal 1 a.player.rank



-- setPlayerInPlayerListWithChallengeResultTest : Test
-- setPlayerInPlayerListWithChallengeResultTest =
--     fuzz (Fuzz.list playerFuzzer) "a challenge result should be reflected in a new player list" <|
--         \list ->
--             case SR.ListOps.sortedPlayerListByRank list of
--                 [] ->
--                     Expect.pass
--                 a :: _ ->
--                     Expect.equal a.rank 1


extractRankFromPlayer : Maybe SR.Types.Player -> Int
extractRankFromPlayer player =
    case player of
        Just a ->
            a.rank

        Nothing ->
            0

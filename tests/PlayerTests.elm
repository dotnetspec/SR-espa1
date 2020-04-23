module PlayerTests exposing (..)

--import Random.Pcg as Random

import Eth.Types
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal.Types
import Json.Encode
import SR.Decode
import SR.ListOps
import SR.PlayerListOps
import SR.Types
import Shrink
import Test exposing (..)


playerFuzzer : Fuzzer SR.Types.Player
playerFuzzer =
    Fuzz.map3
        SR.Types.Player
        Fuzz.string
        Fuzz.int
        Fuzz.string


testsortPlayerListByRank : Test
testsortPlayerListByRank =
    let
        listOfPlayers =
            [ { address = ""
              , rank = 2
              , challengeraddress = ""
              }
            , { address = ""
              , rank = 1
              , challengeraddress = ""
              }
            ]

        output =
            [ { address = ""
              , rank = 1
              , challengeraddress = ""
              }
            , { address = ""
              , rank = 2
              , challengeraddress = ""
              }
            ]
    in
    describe "testsortPlayerListByRank test"
        [ test "outputs correctly ordered list" <|
            \_ ->
                SR.PlayerListOps.sortedPlayerListByRank listOfPlayers
                    |> Expect.equal output
        ]


sortPlayerListTest1 : Test
sortPlayerListTest1 =
    fuzz (Fuzz.list playerFuzzer) "a sorted list should have the first rank == 1" <|
        \list ->
            case SR.PlayerListOps.sortedPlayerListByRank list of
                [] ->
                    Expect.pass

                a :: _ ->
                    Expect.equal 1 a.rank



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

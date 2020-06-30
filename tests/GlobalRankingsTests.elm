module GlobalRankingsTests exposing (..)

--import Random.Pcg as Random

import Eth.Types
import Eth.Utils
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal.Types
import Json.Encode
import SR.Decode
import SR.Defaults

import SR.Types
import Shrink
import Test exposing (..)
import Testdata.GlobalTestData
import Testdata.UserTestData
import Testdata.UserRankingTestData


rankingInfoFuzzer : Fuzzer SR.Types.RankingInfo
rankingInfoFuzzer =
    Fuzz.map5
        SR.Types.RankingInfo
        Fuzz.string
        Fuzz.bool
        Fuzz.string
        Fuzz.string
        Fuzz.string



-- this could be improved to become equal to a valid eth addrs - not just empty string


ownerValidatedRankingListTest : Test
ownerValidatedRankingListTest =
    --skip <|
    fuzz (Fuzz.list rankingInfoFuzzer) "a globalranking list entry must have valid owneraddresses" <|
        \list ->
            case SR.ListOps.ownerValidatedRankingList list of
                [] ->
                    Expect.pass

                --Expect.true "true" True
                globalRankingList ->
                    -- let
                    --     _ =
                    --         Debug.log "globalRankingList" globalRankingList
                    -- in
                    Expect.true "true" <| List.all isValidOwnerAddress <| SR.ListOps.ownerValidatedRankingList globalRankingList



-- this is used here to keep the original private


isValidOwnerAddress : SR.Types.RankingInfo -> Bool
isValidOwnerAddress rankInfo =
    let
        _ =
            Debug.log "isValidOwnerAddress" rankInfo.rankingowneraddr
    in
    if Eth.Utils.isAddress rankInfo.rankingowneraddr then
        True

    else
        False



--Expect.notEqual "" a.rankingowneraddr




createAllUserAsOwnerGlobalRankingListTest : Test
createAllUserAsOwnerGlobalRankingListTest =
    let
                rankingInfo =
                    { id = "5edf2249655d87580c46a830"
                    , active = True
                    , rankingname = "Test 10"
                    , rankingdesc = "t10"
                    , rankingowneraddr = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
                    }
                userOwner =
                    { datestamp = 1569839363942
                    , active = True
                    , username = "Test 10"
                    , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
                    , description = "t10"
                    , email = "t10@t.com"
                    , mobile = "10101000"
                    , userjoinrankings = []
                    }

                output =
                        [
                            { rankingInfo = rankingInfo
                            , userInfo = userOwner
                            }
                        ]
    in
    --only <|
    --skip <|
    describe "correctly assign an owned ranking to the Your Created Rankings list"
        [ test "createAllUserAsOwnerGlobalRankingList" <|
            \_ ->
                SR.ListOps.gotUserOwnedGlobalRankingList Testdata.UserRankingTestData.userRankingList Testdata.UserTestData.singleUser
                    |> Expect.equal output
        ]

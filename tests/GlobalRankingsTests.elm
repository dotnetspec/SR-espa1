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
import SR.ListOps
import SR.Types
import Shrink
import Test exposing (..)


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

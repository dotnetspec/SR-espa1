module ValidationTests exposing (..)

--import Random.Pcg as Random

import Eth.Types
import Eth.Utils
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal.Types
import Json.Encode
import SR.Decode
import SR.Types
import Shrink
import Test exposing (..)
import Testdata.UserTestData
--import Data.Users
--import EverySet exposing (EverySet)
import Utils.Validation.Validate



rankingIdValidationTest : Test
rankingIdValidationTest =
    --only <|
    describe "a rankingid string value must be valid"
        [ fuzz string "isValidRankingId with fuzzy values" <|
            \s ->
                Utils.Validation.Validate.isValidRankingId s
                -- this test is based on the presumption that the fuzzer will never generate
                -- a random valid rankingId - if it fails check this first!
                    |> Expect.equal False
            
            , test "isValidRankingId with a valid value" <|
                \_ ->
                    Utils.Validation.Validate.isValidRankingId "5e8e879d8e85c8437012e2a7"
                        |> Expect.equal True
        ]

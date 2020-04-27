module UserTests exposing (..)

--import Random.Pcg as Random

import Eth.Types
import Eth.Utils
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal.Types
import Json.Encode
import SR.Decode
import SR.GlobalListOps
import SR.ListOps
import SR.PlayerListOps
import SR.Types
import Shrink
import Test exposing (..)


userFuzzer : Fuzzer SR.Types.User
userFuzzer =
    Fuzz.map5
        SR.Types.User
        Fuzz.int
        Fuzz.bool
        Fuzz.string
        Fuzz.string
        Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.string


gotUserFromUserListTest : Test
gotUserFromUserListTest =
    let
        listOfUsers =
            [ { datestamp = 123456
              , active = True
              , username = "John"
              , ethaddress = "0x450dcBeB535029B62f042222D95a009F59408D5d"
              , description = "Tough"
              , email = "j@j.com"
              , mobile = "123456"
              }
            , { datestamp = 123456
              , active = True
              , username = "Alfred"
              , ethaddress = "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F"
              , description = "Fit"
              , email = "j@j.com"
              , mobile = "123456"
              }
            ]

        output =
            [ { datestamp = 123456
              , active = True
              , username = "Alfred"
              , ethaddress = "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F"
              , description = "Fit"
              , email = "j@j.com"
              , mobile = "123456"
              }
            ]
    in
    --only <|
    describe " a single user must be obtained from the userlist"
        [ test "gotUserFromUserList" <|
            \_ ->
                [ SR.ListOps.gotUserFromUserList listOfUsers "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F" ]
                    |> Expect.equal output
        ]



-- currently testing what could be a private function - test the exposed ones instead


validatedUserListTest : Test
validatedUserListTest =
    fuzz (Fuzz.list userFuzzer) "a user list entry must have valid ethaddresses" <|
        \list ->
            case SR.ListOps.validatedUserList list of
                [] ->
                    Expect.pass

                usersList ->
                    Expect.true "true" <| List.all isValidEthAddress <| SR.ListOps.validatedUserList usersList



-- this is used here to keep the original private


isValidEthAddress : SR.Types.User -> Bool
isValidEthAddress user =
    if Eth.Utils.isAddress user.ethaddress then
        True

    else
        False



--Expect.notEqual "" a.rankingowneraddr

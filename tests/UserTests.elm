module UserTests exposing (..)

--import Random.Pcg as Random

import Eth.Types
import Eth.Utils
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Internal.Types
import Json.Encode
import SR.Decode
import SR.ListOps
import SR.Types
import Shrink
import Test exposing (..)
import Testdata.UserTestData



--userFuzzer : Fuzzer SR.Types.User -> Fuzzer (a -> b) -> Fuzzer b


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
        |> Fuzz.andMap (Fuzz.list Fuzz.string)


gotUserFromUserListTest : Test
gotUserFromUserListTest =
    let
        output =
            [ { datestamp = 1569839363942
              , active = True
              , username = "Test 1"
              , ethaddress = "0x4a0a14ba869bee85c490a5e6401d3f740039a01f"
              , description = "t1"
              , email = "t1@t.com"
              , mobile = "11111111"
              , userjoinrankings = []
              }
            ]
    in
    --only <|
    describe " a single user must be obtained from the userlist"
        [ test "gotUserFromUserList" <|
            \_ ->
                [ SR.ListOps.gotUserFromUserList Testdata.UserTestData.standardUserList "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F" ]
                    |> Expect.equal output
        ]


removedDuplicateUserFromUserListTest : Test
removedDuplicateUserFromUserListTest =
    let
        output =
            [ { datestamp = 123456
              , active = True
              , username = "John"
              , ethaddress = "0x450dcBeB535029B62f042222D95a009F59408D5d"
              , description = "Tough"
              , email = "j@j.com"
              , mobile = "123456"
              , userjoinrankings = []
              }
            , { datestamp = 123456
              , active = True
              , username = "Alfred"
              , ethaddress = "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F"
              , description = "Fit"
              , email = "j@j.com"
              , mobile = "123456"
              , userjoinrankings = []
              }
            ]
    in
    --only <|
    describe " there cannot be more than 1 unique address for each user in the userlist"
        [ test "removedDuplicateUserFromUserList" <|
            \_ ->
                SR.ListOps.removedDuplicateUserFromUserList Testdata.UserTestData.usersWithSameAddressInList
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
                    Expect.true "Expect only valid ethaddresses" <| List.all isValidEthAddress <| SR.ListOps.validatedUserList usersList



-- this is used here to keep the original private


isValidEthAddress : SR.Types.User -> Bool
isValidEthAddress user =
    if Eth.Utils.isAddress user.ethaddress then
        True

    else
        False


uniqueUserNameListTest : Test
uniqueUserNameListTest =
    --only <|
    describe " a user list entry must have a unique user name"
        [ test "isUniqueUserName - True" <|
            \_ ->
                SR.ListOps.isUniqueUserName "CTest1" Testdata.UserTestData.standardUserList
                    |> Expect.true "Expected CTest1 to be unique"
        , test "isUniqueUserName - False" <|
            \_ ->
                SR.ListOps.isUniqueUserName "Test 10" Testdata.UserTestData.standardUserList
                    |> Expect.false "Expected Test 10 not to be unique"
        ]

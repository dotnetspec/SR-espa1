module UserTests exposing (..)

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
import Data.Users
import EverySet exposing (EverySet)


--userFuzzer : Fuzzer Data.Users.User -> Fuzzer (a -> b) -> Fuzzer b


userFuzzer : Fuzzer Data.Users.User
userFuzzer =
    Fuzz.map5
        Data.Users.User
        Fuzz.int
        Fuzz.bool
        Fuzz.string
        Fuzz.string
        Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap (Fuzz.list Fuzz.string)


-- Data.Users tests

validSetTest : Test
validSetTest =
    --only <|
     describe "Data.Users tests"
        [ fuzz (Fuzz.list userFuzzer) """a (fuzzy) user set must contain unique values 
        and will therefore always be shorter than or equal to lists with potential duplicate values""" <|
            \list ->
            case Data.Users.asUsers (EverySet.fromList list) of
              usersSet ->
                List.length list
                  |> Expect.atLeast (List.length (Data.Users.asList usersSet))
        
        ,fuzz (Fuzz.list userFuzzer) """a (fuzzy) user set must contain only 1 extra entry if a new user is added""" <|
            \list ->
            case Data.Users.asUsers (EverySet.fromList list) of
              usersSet ->
                let 
                    newUserSet = Data.Users.addUser Testdata.UserTestData.singleUser usersSet
                in
                (List.length (Data.Users.asList newUserSet))
                |> Expect.all
                    [ Expect.lessThan (List.length (Data.Users.asList usersSet) + 2) 
                    , Expect.greaterThan (List.length (Data.Users.asList usersSet))
                    , Expect.notEqual 0
                    , Expect.lessThan 500000
                    ]
        , 
            fuzz (Fuzz.list userFuzzer) """gotUser must retrieve the correct user from the (fuzzy) set""" <|
            \list ->
            case Data.Users.asUsers (EverySet.fromList list) of
              usersSet ->
                let 
                    addedExtraUser = Data.Users.addUser Testdata.UserTestData.singleUser usersSet
                    newUser = Data.Users.gotUser addedExtraUser "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
                in
                    Expect.equal Testdata.UserTestData.singleUser.ethaddress newUser.ethaddress
        ]
    
-- for now this test works with a list. Perhaps this will later be updated to work with a set ...
addedNewJoinedRankingIdToUserTest : Test
addedNewJoinedRankingIdToUserTest = 
    --only <|
        describe "correctly add a rankingId to a User's userjoinedrankings list"
            [
            --fuzzWith { runs = 1 } (Fuzz.list userFuzzer) """addedNewJoinedRankingId must only add a valid rnkId to a User in the (fuzzy) set""" <|
            fuzz (Fuzz.list userFuzzer) """addedNewJoinedRankingId must only add a valid rnkId to a User in the (fuzzy) set""" <|
                \list ->
                -- case Data.Users.asUsers (EverySet.fromList list) of
                --     usersSet ->
                --         case Data.Users.asList usersSet of
                            --Set.isempty usersSet ->
                    case list of
                            [] ->
                                let 
                                    --addedANewRankingIdToUser = Data.Users.addedNewJoinedRankingId "1234567890" Testdata.UserTestData.singleUser usersSet
                                    -- if there's nothing in the list we can add a valid rankingId to the test data:
                                    addedANewRankingIdToUserInList = Data.Users.addedNewJoinedRankingId "5e8e879d8e85c8437012e2a7" Testdata.UserTestData.singleUser list

                                in 
                                    case addedANewRankingIdToUserInList of 
                                        [] ->
                                            Expect.pass
                                        -- x is a User:
                                        x :: xs ->
                                            x.userjoinedrankings
                                            |>
                                            Expect.all
                                                [ 
                                                --Expect.lessThan (List.length list + 2) 
                                                --, Expect.greaterThan (List.length list)
                                                  Expect.notEqual []
                                                 , Expect.equal ["5e8e879d8e85c8437012e2a7"]
                                                ]
                            -- user:
                            x :: xs ->
                                    case x.userjoinedrankings of 
                                        [] ->
                                            Expect.pass
                                        --single rankingid:
                                        y :: ys ->
                                            let 
                                                --addedANewRankingIdToUserInList = Data.Users.addedNewJoinedRankingId y Testdata.UserTestData.singleUser usersSet
                                                -- y and x are both fuzzy but related - y is the ranking id being added to this user (x)
                                                addedANewRankingIdToUserInList = Data.Users.addedNewJoinedRankingId y x list
                                            in
                                            case addedANewRankingIdToUserInList of 
                                                [] ->
                                                    Expect.pass
                                                --user:
                                                z :: zs ->
                                                    -- let 
                                                    --     _ = Debug.log " z" z.userjoinedrankings
                                                    --     _ = Debug.log " x" x.userjoinedrankings
                                                    -- in 
                                                    -- following seeds provide valid data that won't result in an empty list for z:
                                                    -- 151412038146962
                                                    z.userjoinedrankings
                                                    |>
                                                    Expect.all
                                                        [ 
                                                            Expect.notEqual x.userjoinedrankings 
                                                            --, Expect.notEqual []
                                                        ]
                   
            ]

removedRankingIdFromAllUsersTest : Test 
removedRankingIdFromAllUsersTest =
    --only <|
        describe "correctly remove a rankingId from each User's userjoinedrankings list"
            [
            fuzz (Fuzz.list userFuzzer)"""removeRankingIdFromAllUsers must remove the correct rnkId from every User in the (fuzzy) set"""  <|
            --fuzz (Fuzz.list userFuzzer) """removeRankingIdFromAllUsers must remove the correct rnkId from every User in the (fuzzy) set"""  <|
                \list ->
                -- case Data.Users.asUsers (EverySet.fromList list) of
                --     usersSet ->
                --         case Data.Users.asList usersSet of
                            --Set.isempty usersSet ->
                    case list of
                            [] ->
                                let 
                                    --addedANewRankingIdToUser = Data.Users.addedNewJoinedRankingId "1234567890" Testdata.UserTestData.singleUser usersSet
                                    -- if there's nothing in the list we can add and remove valid rankingIds to/from the test data:
                                    addedANewRankingIdToUserInList = Data.Users.addedNewJoinedRankingId "5e8e879d8e85c8437012e2a7" Testdata.UserTestData.singleUser list
                                    addedANewRankingIdToUserInList2 = Data.Users.addedNewJoinedRankingId "5e96c74b5fa47104cea0c7c6" Testdata.UserTestData.singleUser addedANewRankingIdToUserInList
                                    removedRankingIdFromList = Data.Users.removedRankingIdFromAll (Data.Users.asUsers (EverySet.fromList addedANewRankingIdToUserInList2)) "5e8e879d8e85c8437012e2a7"
                                in 
                                    case Data.Users.asList removedRankingIdFromList of 
                                        [] ->
                                            Expect.pass
                                        -- x is a User:
                                        x :: xs ->
                                            x.userjoinedrankings
                                            |>
                                            Expect.all
                                                [ 
                                                --Expect.lessThan (List.length list + 2) 
                                                --, Expect.greaterThan (List.length list)
                                                  Expect.equal ["5e96c74b5fa47104cea0c7c6"]
                                                 , Expect.notEqual ["5e8e879d8e85c8437012e2a7"] 
                                                ]
                            -- fuzz has generated a list
                            -- take the first user:
                            x :: xs ->
                                    case x.userjoinedrankings of 
                                        [] ->
                                            Expect.pass
                                        --take a single rankingid:
                                        y :: ys ->
                                            let
                                                --addedANewRankingIdToUserInList = Data.Users.addedNewJoinedRankingId y Testdata.UserTestData.singleUser usersSet
                                                -- y and x are both fuzzy but related - y is the ranking id being added to this user (x)
                                                addedANewRankingIdToUserInList = Data.Users.addedNewJoinedRankingId y x list
                                                addedANewRankingIdToUserInList2 = Data.Users.addedNewJoinedRankingId (y ++ "123") x list
                                                removedRankingIdFromFuzzyList = Data.Users.removedRankingIdFromAll (Data.Users.asUsers (EverySet.fromList addedANewRankingIdToUserInList2))  (y ++ "123")
                                            in
                                            case Data.Users.asList removedRankingIdFromFuzzyList of 
                                                [] ->
                                                    Expect.pass
                                                --user:
                                                z :: zs ->
                                                    -- following seeds provide valid data that won't result in an empty list for z:
                                                    -- 151412038146962
                                                    z.userjoinedrankings
                                                    |>
                                                    Expect.all
                                                        [ 
                                                            -- it's only fair to test against a validated fuzz user (x)
                                                            Expect.equal (List.filterMap Data.Users.removedInvalidRankingId x.userjoinedrankings)
                                                            -- if fuzz data is invalid, lists may equal [] cos first 'rankingid' won't be added
                                                            --, Expect.notEqual []
                                                        ]
                   
            ]
                    

-- ListOp tests
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
              , userjoinedrankings = ["5e96c74b5fa47104cea0c7c6"
          , "5e8e879d8e85c8437012e2a7"]
              }
            ]
    in
    --only <|
    describe " a single user must be obtained from the userlist"
        [ test "gotUserFromUserList" <|
            \_ ->
                [ Data.Users.gotUserFromUserList Testdata.UserTestData.standardUserList "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F" ]
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
              , userjoinedrankings = []
              }
            , { datestamp = 123456
              , active = True
              , username = "Alfred"
              , ethaddress = "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F"
              , description = "Fit"
              , email = "j@j.com"
              , mobile = "123456"
              , userjoinedrankings = []
              }
            ]
    in
    --only <|
    describe " there cannot be more than 1 unique address for each user in the userlist"
        [ test "removedDuplicateUserFromUserList" <|
            \_ ->
                Data.Users.removedDuplicateUserFromUserList Testdata.UserTestData.usersWithSameAddressInList
                    |> Expect.equal output
        ]



-- currently testing what could be a private function - test the exposed ones instead


validatedUserListTest : Test
validatedUserListTest =
    fuzz (Fuzz.list userFuzzer) "a user list entry must have valid ethaddresses" <|
        \list ->
            case Data.Users.validatedUserList list of
                [] ->
                    Expect.pass

                usersList ->
                    Expect.true "Expect only valid ethaddresses" <| List.all isValidEthAddress <| Data.Users.validatedUserList usersList



-- this is used here to keep the original private


isValidEthAddress : Data.Users.User -> Bool
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
                Data.Users.isUniqueUserName "CTest1" Testdata.UserTestData.standardUserList
                    |> Expect.true "Expected CTest1 to be unique"
        , test "isUniqueUserName - False" <|
            \_ ->
                Data.Users.isUniqueUserName "Test 10" Testdata.UserTestData.standardUserList
                    |> Expect.false "Expected Test 10 not to be unique"
        ]


removeDuplicateUserListTest : Test
removeDuplicateUserListTest =
    --only <|
    describe " each entry in the user list must be unique"
        [ test "removedDuplicateUserFromUserList" <|
            \_ ->
                Data.Users.removedDuplicateUserFromUserList Testdata.UserTestData.duplicateUsers
                    |> Expect.equal Testdata.UserTestData.duplicateUsersRemoved
        ]

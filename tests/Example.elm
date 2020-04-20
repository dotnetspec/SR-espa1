module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]



-- wipSuite : Test
-- wipSuite =
--     describe "skip, only, and todo"
--         [ --only <|
--           describe "Marking this test as `only` means no other tests will be run!"
--             [ test "This test will be run" <|
--                 \_ -> 1 + 1 |> Expect.equal 2
--             , skip <|
--                 test "This test will be skipped, even though it's in an `only`!" <|
--                     \_ -> 2 + 3 |> Expect.equal 4
--             ]
--         , skip <|
--             test "This test will be skipped because it has no `only`" <|
--                 \_ -> "left" |> Expect.equal "right"
--         , todo "Make sure all splines are reticulated"
--         ]
-- testisUserInList : Test
-- testisUserInList =
--     let
--         listOfUsers =
--             [ { datestamp = 123456
--               , active = True
--               , username = "Phil"
--               , ethaddress = "0xF5003ceA9657a15123b1cc83C305f87555d190Cf"
--               , description = "Fast"
--               , email = "kjlj@ljlk.com"
--               , mobile = "123456"
--               }
--             ]
--         output =
--             True
--     in
--     describe "testisUserInList tests"
--         [ skip <|
--             test "outputs true, because the user is in the list" <|
--                 \_ ->
--                     SR.ListOps.isUserInList listOfUsers (Internal.Types.Address "0xF5003ceA9657a15123b1cc83C305f87555d190Cf")
--                         |> Expect.equal output
--         ]

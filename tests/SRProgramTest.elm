module SRProgramTest exposing (..)

import Main as Main
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas, fillIn, update)
import Random
import Test exposing (..)
import Test.Html.Selector exposing (text)


start : ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
start =
    ProgramTest.createElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "voter registration frontend"
        [ test "happy path: successful registration" <|
            \() ->
                start
                    -- |> fillIn "name" "Name" "Bailey Sheppard"
                    -- |> fillIn "street-address" "Street Address" "14 North Moore Street"
                    -- |> fillIn "postcode" "Postal Code" "60606"
                    -- |> clickButton "Register"
                    -- |> ProgramTest.update (RegistrationResponse (Ok "Aug 12"))
                    |> expectViewHas
                        [ text "SportRank"

                        --, text "Next election date is: Aug 12"
                        ]

        -- , test "invalid postal code shows a validation error" <|
        --     \() ->
        --         start
        --             -- |> fillIn "name" "Name" "Bailey Sheppard"
        --             -- |> fillIn "street-address" "Street Address" "14 North Moore Street"
        --             -- |> fillIn "postcode" "Postal Code" "0000"
        --             -- |> clickButton "Register"
        --             |> expectViewHas
        --                 [ text "You must enter a valid postal code"
        --                 ]
        ]

module SRProgramTest exposing (..)

import Json.Encode as Encode exposing (Value)
import Main as Main exposing (Msg, main)
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas, fillIn, update)
import Random
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query
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
    describe "SportsRank frontend"
        [ test "happy path: successful creation of a new ladder" <|
            \() ->
                start
                    -- |> fillIn "name" "Name" "Bailey Sheppard"
                    -- |> fillIn "street-address" "Street Address" "14 North Moore Street"
                    -- |> fillIn "postcode" "Postal Code" "60606"
                    -- |> clickButton "Register"
                    -- |> ProgramTest.update (RegistrationResponse (Ok "Aug 12"))
                    |> expectViewHas
                        [ text "SportRank"

                        --, text "Click to continue"
                        , text "Initializing ..."
                        ]
        , skip <|
            test "clicking create new btn brings up correct UI" <|
                \() ->
                    let
                        simulatedEventObject : Value
                        simulatedEventObject =
                            Encode.object
                                [ ( "target"
                                  , Encode.object [ ( "value", Encode.string "cats" ) ]
                                  )
                                ]
                    in
                    start
                        -- |> fillIn "name" "Name" "Bailey Sheppard"
                        -- |> fillIn "street-address" "Street Address" "14 North Moore Street"
                        -- |> fillIn "postcode" "Postal Code" "0000"
                        --|> clickButton "Create New"
                        |> ProgramTest.advanceTime 1
                        |> clickButton "Create New"
                        -- |> Event.simulate (Event.custom "input" simulatedEventObject)
                        -- |> Test.Html.Query.find [ createnewladderbtn ]
                        -- |> ProgramTest.simulateDomEvent
                        --|> ProgramTest.update Main.ChangedUIStateToCreateNew
                        |> expectViewHas
                            [ text "Create New" --Ladder Ranking"
                            ]
        ]



-- start
--                 |> ProgramTest.update Main.Msg.ChangedUIStateToCreateNew
--                 |> expectViewHas
--                     [ text "Create New" --Ladder Ranking"
--                     ]
-- A function to find the HTML element that responds to the event
--(typically this will be a call to Test.Html.Query.find [ createnewladderbtn ])
-- The event to simulate (see Test.Html.Event "Event Builders")
--     simulateDomEvent :
-- (Single msg -> Single msg)
-- -> ( String, Value )
-- -> ProgramTest model msg effect
-- -> ProgramTest model msg effect

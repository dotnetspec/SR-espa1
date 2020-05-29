module SRProgramTest exposing (..)

import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Wallet
import Internal.Types
import Json.Encode as Encode exposing (Value)
import Main as Main exposing (Model, Msg, main)
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas, fillIn, update)
import Random
import SR.Defaults
import SR.Types
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query
import Test.Html.Selector exposing (text)
import Testdata.GlobalTestData
import Testdata.TestDefaults
import Testdata.UserTestData


start : ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
start =
    ProgramTest.createElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


globalListTest : Test
globalListTest =
    describe "SportsRank frontend"
        [ --skip <|
          test "happy path: successful creation of home page - DataT1" <|
            \() ->
                start
                    |> ProgramTest.update (Main.WalletStatus Testdata.TestDefaults.simWalletSentryDataT1)
                    |> ProgramTest.update (Main.UsersReceived Testdata.UserTestData.remoteDataUsers)
                    |> ProgramTest.update (Main.GlobalRankingsReceived Testdata.GlobalTestData.globalRankingsJson)
                    |> expectViewHas
                        [ text "SportRank - Test 1"

                        --, text "Click to continue"
                        , text "Test 4"
                        ]
        ]



-- create new tests


existingUserCreateNewLadderTest : Test
existingUserCreateNewLadderTest =
    --skip <|
    test "SRProgramTest - existing user - create new ladder brings up correct header" <|
        \() ->
            start
                |> ProgramTest.update (Main.WalletStatus Testdata.TestDefaults.simWalletSentryDataT1)
                |> ProgramTest.update (Main.UsersReceived Testdata.UserTestData.remoteDataUsers)
                |> ProgramTest.update (Main.GlobalRankingsReceived Testdata.GlobalTestData.globalRankingsJson)
                |> ProgramTest.update Main.ClickedCreateNewLadder
                |> expectViewHas
                    [ text "Create New Ladder Ranking" ]


unregisteredUserCreateNewLadderTest : Test
unregisteredUserCreateNewLadderTest =
    --skip <|
    test "SRProgramTest - unregistered user - create new ladder brings up correct header" <|
        \() ->
            start
                |> ProgramTest.update (Main.WalletStatus Testdata.TestDefaults.simWalletSentryDataT1)
                |> ProgramTest.update (Main.GlobalRankingsReceived Testdata.GlobalTestData.globalRankingsJson)
                |> ProgramTest.update Main.ClickedCreateNewLadder
                |> expectViewHas
                    [ text "Create New Ladder Ranking" ]

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



-- globalRankingsJson : RemoteData.WebData (List SR.Types.RankingInfo)
-- globalRankingsJson =
--     RemoteData.Success
--         [ { id = "5e940251b08d064dc025e8b0"
--           , active = True
--           , rankingname = "Test 3"
--           , rankingdesc = "t3"
--           , rankingowneraddr = "0xac5491bb066c98fec13046928a78761c0b1e5603"
--           }
--         , { id = "5e8e879d8e85c8437012e2a7"
--           , active = True
--           , rankingname = "Test 4"
--           , rankingdesc = "t4"
--           , rankingowneraddr = "0x3bb244dec13253d39e22606850f4704b469a4b93"
--           }
--         ]


start : ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
start =
    ProgramTest.createElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


simWalletSentryData : Eth.Sentry.Wallet.WalletSentry
simWalletSentryData =
    { account = Just (Internal.Types.Address "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F"), networkId = Net.toNetworkId 4 }


all : Test
all =
    describe "SportsRank frontend"
        [ --only <|
          test "happy path: successful creation of a new ladder" <|
            \() ->
                start
                    -- |> fillIn "street-address" "Street Address" "14 North Moore Street"
                    -- |> clickButton "Register"
                    -- |> ProgramTest.update (RegistrationResponse (Ok "Aug 12"))
                    |> ProgramTest.update (Main.WalletStatus simWalletSentryData)
                    --|> ProgramTest.update (Main.PollBlock (Ok 123))
                    |> ProgramTest.update (Main.GotGlobalRankingsJson Testdata.GlobalTestData.globalRankingsJson)
                    |> expectViewHas
                        [ text "SportRank"
                        , text "Click to continue"

                        --, text "Initializing ..."
                        ]
        , --skip <|
          test "ChangedUIStateToCreateNewLadder brings up correct UI header" <|
            \() ->
                start
                    -- |> Event.simulate (Event.custom "input" simulatedEventObject)
                    -- |> Test.Html.Query.find [ createnewladderbtn ]
                    |> ProgramTest.update (Main.WalletStatus simWalletSentryData)
                    |> ProgramTest.update Main.ChangedUIStateToCreateNewLadder
                    |> expectViewHas
                        [ text "Create New Ladder Ranking" ]
        , --skip <|
          test "ChangedUIStateToCreateNewLadder brings up correct ranking data" <|
            \() ->
                start
                    |> ProgramTest.update (Main.WalletStatus simWalletSentryData)
                    |> ProgramTest.update (Main.GotGlobalRankingsJson Testdata.GlobalTestData.globalRankingsJson)
                    |> expectViewHas
                        [ text "Test 4" ]

        --end of all tests
        ]

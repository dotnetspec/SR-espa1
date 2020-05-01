module Testdata.GlobalTestData exposing (..)

import RemoteData
import SR.Types


globalRankingsJson : RemoteData.WebData (List SR.Types.RankingInfo)
globalRankingsJson =
    RemoteData.Success
        [ { id = "5e940251b08d064dc025e8b0"
          , active = True
          , rankingname = "Test 3"
          , rankingdesc = "t3"
          , rankingowneraddr = "0xac5491bb066c98fec13046928a78761c0b1e5603"
          }
        , { id = "5e8e879d8e85c8437012e2a7"
          , active = True
          , rankingname = "Test 4"
          , rankingdesc = "t4"
          , rankingowneraddr = "0x3bb244dec13253d39e22606850f4704b469a4b93"
          }
        ]

module Testdata.GlobalTestData exposing (..)

import RemoteData
import SR.Types


globalRankingsJson : RemoteData.WebData (List SR.Types.Ranking)
globalRankingsJson =
    RemoteData.Success
        [ { id = "5e9d8a00435f5604bb44c3bf"
          , active = True
          , rankingname = "Test 5.1"
          , rankingdesc = "t5.1"
          , rankingowneraddr = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
          }
        , { id = "5e9a96572940c704e1da6f9f"
          , active = True
          , rankingname = "Test 5"
          , rankingdesc = "t5"
          , rankingowneraddr = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
          }
        , { id = "5e96c9ed2940c704e1d8685e"
          , active = True
          , rankingname = "Test 3"
          , rankingdesc = "t3"
          , rankingowneraddr = "0xac5491bb066c98fec13046928a78761c0b1e5603"
          }
        , { id = "5e96c74b5fa47104cea0c7c6"
          , active = True
          , rankingname = "Test 7"
          , rankingdesc = "t7"
          , rankingowneraddr = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
          }
        , { id = "5e96baff2940c704e1d86316"
          , active = True
          , rankingname = "Test 8"
          , rankingdesc = "t8"
          , rankingowneraddr = "0xf9bd658a9f3e23ed77192a14a9da6a5c37566218"
          }
        , { id = "5e8e879d8e85c8437012e2a7"
          , active = True
          , rankingname = "Test 4"
          , rankingdesc = "t4"
          , rankingowneraddr = "0x3bb244dec13253d39e22606850f4704b469a4b93"
          }
          ,
          { id = "5edf2249655d87580c46a830"
          , active = True
          , rankingname = "Test 10"
          , rankingdesc = "t10"
          , rankingowneraddr = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
          }
        
        ]

nonRemoteGlobalRankingsJson : List SR.Types.Ranking
nonRemoteGlobalRankingsJson =
    
        [ { id = "5e9d8a00435f5604bb44c3bf"
          , active = True
          , rankingname = "Test 5.1"
          , rankingdesc = "t5.1"
          , rankingowneraddr = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
          }
        , { id = "5e9a96572940c704e1da6f9f"
          , active = True
          , rankingname = "Test 5"
          , rankingdesc = "t5"
          , rankingowneraddr = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
          }
        , { id = "5e96c9ed2940c704e1d8685e"
          , active = True
          , rankingname = "Test 3"
          , rankingdesc = "t3"
          , rankingowneraddr = "0xac5491bb066c98fec13046928a78761c0b1e5603"
          }
        , { id = "5e96c74b5fa47104cea0c7c6"
          , active = True
          , rankingname = "Test 7"
          , rankingdesc = "t7"
          , rankingowneraddr = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
          }
        , { id = "5e96baff2940c704e1d86316"
          , active = True
          , rankingname = "Test 8"
          , rankingdesc = "t8"
          , rankingowneraddr = "0xf9bd658a9f3e23ed77192a14a9da6a5c37566218"
          }
        , { id = "5e8e879d8e85c8437012e2a7"
          , active = True
          , rankingname = "Test 4"
          , rankingdesc = "t4"
          , rankingowneraddr = "0x3bb244dec13253d39e22606850f4704b469a4b93"
          }
          ,
          { id = "5edf2249655d87580c46a830"
          , active = True
          , rankingname = "Test 10"
          , rankingdesc = "t10"
          , rankingowneraddr = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
          }
        
        ]
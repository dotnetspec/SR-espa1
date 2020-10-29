module Testdata.SelectedTestData exposing (..)

import RemoteData
import SR.Types


playersJson : RemoteData.WebData (List SR.Types.Player)
playersJson =
    RemoteData.Success
        [ { address = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
          , rank = 1
          , challengerid = "0x4a0a14ba869bee85c490a5e6401d3f740039a01f"
          }
        , { address = "0x4a0a14ba869bee85c490a5e6401d3f740039a01f"
          , rank = 2
          , challengerid = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
          }
        , { address = ""
          , rank = 3
          , challengerid = ""
          }
        , { address = "0xac5491bb066c98fec13046928a78761c0b1e5603"
          , rank = 4
          , challengerid = "0x3bb244dec13253d39e22606850f4704b469a4b93"
          }
        , { address = "0x3bb244dec13253d39e22606850f4704b469a4b93"
          , rank = 5
          , challengerid = "0xac5491bb066c98fec13046928a78761c0b1e5603"
          }
        , { address = "0xeb6a4b79ded304aaea7344fd58ce5f8ebc8424b9"
          , rank = 6
          , challengerid = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
          }
        , { address = "0x9a82d050ccb98c31f7817b6263980c21167196c4"
          , rank = 7
          , challengerid = ""
          }
        , { address = "0xf9bd658a9f3e23ed77192a14a9da6a5c37566218"
          , rank = 8
          , challengerid = ""
          }
        , { address = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
          , rank = 9
          , challengerid = "0xeb6a4b79ded304aaea7344fd58ce5f8ebc8424b9"
          }
        ]

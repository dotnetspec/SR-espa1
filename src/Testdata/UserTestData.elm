module Testdata.UserTestData exposing (..)

import RemoteData
import SR.Types


usersJson : RemoteData.WebData (List SR.Types.User)
usersJson =
    RemoteData.Success
        [ { datestamp = 1569839363942
          , active = True
          , username = "Test 10"
          , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
          , description = "t10"
          , email = "t10@t.com"
          , mobile = "10101000"
          , userjoinrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 8"
          , ethaddress = "0xf9bd658a9f3e23ed77192a14a9da6a5c37566218"
          , description = "t8"
          , email = "t8@t.com"
          , mobile = "8888888"
          , userjoinrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 7"
          , ethaddress = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
          , description = "t7"
          , email = "t7@t.com"
          , mobile = "7777777"
          , userjoinrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 2"
          , ethaddress = "0xeb6a4b79ded304aaea7344fd58ce5f8ebc8424b9"
          , description = "t2"
          , email = "t2@t.com"
          , mobile = "222222222"
          , userjoinrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 1"
          , ethaddress = "0x4a0a14ba869bee85c490a5e6401d3f740039a01f"
          , description = "t1"
          , email = "t1@t.com"
          , mobile = "11111111"
          , userjoinrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 5"
          , ethaddress = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
          , description = "t5"
          , email = "t5@t.com"
          , mobile = "55555555"
          , userjoinrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 6"
          , ethaddress = "0x9a82d050ccb98c31f7817b6263980c21167196c4"
          , description = "t6"
          , email = "t6@t.com"
          , mobile = "123456"
          , userjoinrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 4"
          , ethaddress = "0x3bb244dec13253d39e22606850f4704b469a4b93"
          , description = "t4"
          , email = "t4@t.com"
          , mobile = "123456"
          , userjoinrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 3"
          , ethaddress = "0xac5491bb066c98fec13046928a78761c0b1e5603"
          , description = "t3"
          , email = "test3@t3.com"
          , mobile = "777777777777"
          , userjoinrankings = []
          }
        ]

module Testdata.UserPlayerTestData exposing (..)

import RemoteData
import SR.Types
                    

singleUserPlayer1 =
     {
        player = {
          address = "0xf5003cea9657a15123b1cc83c305f87555d190cf",
          challengerid = "",
          rank = 2
        },
        user = {
          active = True,
          datestamp = 1569839363942,
          description = "t5",
          email = "t5@t.com",
          ethaddress = "0xf5003cea9657a15123b1cc83c305f87555d190cf",
          mobile = "55555555",
          userjoinedrankings = ["5e96c74b5fa47104cea0c7c6", "5e8e879d8e85c8437012e2a7"],
          username = "Test 5"
        }
     }

singleUserPlayer2 =
    {
      player = {
        address = "0x3bb244dec13253d39e22606850f4704b469a4b93",
        challengerid = "",
        rank = 1
      },
      user = {
        active = True,
        datestamp = 1569839363942,
        description = "t4",
        email = "t4@t.com",
        ethaddress = "0x3bb244dec13253d39e22606850f4704b469a4b93",
        mobile = "123456",
        userjoinedrankings = ["5e96c74b5fa47104cea0c7c6", "5e96c9ed2940c704e1d8685e"],
        username = "Test 4"
      }
    }

userPlayerList =
  [{
      player = {
        address = "0x3bb244dec13253d39e22606850f4704b469a4b93",
        challengerid = "",
        rank = 1
      },
      user = {
        active = True,
        datestamp = 1569839363942,
        description = "t4",
        email = "t4@t.com",
        ethaddress = "0x3bb244dec13253d39e22606850f4704b469a4b93",
        mobile = "123456",
        userjoinedrankings = ["5e96c74b5fa47104cea0c7c6", "5e96c9ed2940c704e1d8685e"],
        username = "Test 4"
      }
    }, {
      player = {
        address = "0xf5003cea9657a15123b1cc83c305f87555d190cf",
        challengerid = "",
        rank = 2
      },
      user = {
        active = True,
        datestamp = 1569839363942,
        description = "t5",
        email = "t5@t.com",
        ethaddress = "0xf5003cea9657a15123b1cc83c305f87555d190cf",
        mobile = "55555555",
        userjoinedrankings = ["5e96c74b5fa47104cea0c7c6", "5e8e879d8e85c8437012e2a7"],
        username = "Test 5"
      }
    }, {
      player = {
        address = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2",
        challengerid = "0xac5491bb066c98fec13046928a78761c0b1e5603",
        rank = 3
      },
      user = {
        active = True,
        datestamp = 1569839363942,
        description = "t7",
        email = "t7@t.com",
        ethaddress = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2",
        mobile = "7777777",
        userjoinedrankings = ["5e96c74b5fa47104cea0c7c6"],
        username = "Test 7"
      }
    }, {
      player = {
        address = "0xac5491bb066c98fec13046928a78761c0b1e5603",
        challengerid = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2",
        rank = 4
      },
      user = {
        active = True,
        datestamp = 1569839363942,
        description = "t3",
        email = "test3@t3.com",
        ethaddress = "0xac5491bb066c98fec13046928a78761c0b1e5603",
        mobile = "777777777777",
        userjoinedrankings = ["5e96c9ed2940c704e1d8685e"],
        username = "Test 3"
      }
  }]

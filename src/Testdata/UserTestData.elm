module Testdata.UserTestData exposing (..)

import RemoteData
import SR.Types


singleUser =
     { datestamp = 1569839363942
      , active = True
      , username = "Test 10"
      , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
      , description = "t10"
      , email = "t10@t.com"
      , mobile = "10101000"
      , userjoinedrankings = []
      }

singleUserWithuserjoinrankings =
     { datestamp = 1569839363942
      , active = True
      , username = "Test 10"
      , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
      , description = "t10"
      , email = "t10@t.com"
      , mobile = "10101000"
      , userjoinedrankings = ["5e96c74b5fa47104cea0c7c6"
          , "5e8e879d8e85c8437012e2a7"
          , "5e96baff2940c704e1d86316"]
      }
    

singleUserList =
    [ { datestamp = 1569839363942
      , active = True
      , username = "Test 10"
      , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
      , description = "t10"
      , email = "t10@t.com"
      , mobile = "10101000"
      , userjoinedrankings = []
      }
    ]


standardUserList =
    [ { datestamp = 1569839363942
      , active = True
      , username = "Test 10"
      , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
      , description = "t10"
      , email = "t10@t.com"
      , mobile = "10101000"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 8"
      , ethaddress = "0xf9bd658a9f3e23ed77192a14a9da6a5c37566218"
      , description = "t8"
      , email = "t8@t.com"
      , mobile = "8888888"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 7"
      , ethaddress = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
      , description = "t7"
      , email = "t7@t.com"
      , mobile = "7777777"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 2"
      , ethaddress = "0xeb6a4b79ded304aaea7344fd58ce5f8ebc8424b9"
      , description = "t2"
      , email = "t2@t.com"
      , mobile = "222222222"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 1"
      , ethaddress = "0x4a0a14ba869bee85c490a5e6401d3f740039a01f"
      , description = "t1"
      , email = "t1@t.com"
      , mobile = "11111111"
      , userjoinedrankings = 
        ["5e96c74b5fa47104cea0c7c6"
          , "5e8e879d8e85c8437012e2a7"]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 5"
      , ethaddress = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
      , description = "t5"
      , email = "t5@t.com"
      , mobile = "55555555"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 6"
      , ethaddress = "0x9a82d050ccb98c31f7817b6263980c21167196c4"
      , description = "t6"
      , email = "t6@t.com"
      , mobile = "123456"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 4"
      , ethaddress = "0x3bb244dec13253d39e22606850f4704b469a4b93"
      , description = "t4"
      , email = "t4@t.com"
      , mobile = "123456"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 3"
      , ethaddress = "0xac5491bb066c98fec13046928a78761c0b1e5603"
      , description = "t3"
      , email = "test3@t3.com"
      , mobile = "777777777777"
      , userjoinedrankings = []
      }
    ]


remoteDataUsers : RemoteData.WebData (List Data.Users.User)
remoteDataUsers =
    RemoteData.Success
        [ { datestamp = 1569839363942
          , active = True
          , username = "Test 10"
          , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
          , description = "t10"
          , email = "t10@t.com"
          , mobile = "10101000"
          , userjoinedrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 8"
          , ethaddress = "0xf9bd658a9f3e23ed77192a14a9da6a5c37566218"
          , description = "t8"
          , email = "t8@t.com"
          , mobile = "8888888"
          , userjoinedrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 7"
          , ethaddress = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
          , description = "t7"
          , email = "t7@t.com"
          , mobile = "7777777"
          , userjoinedrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 2"
          , ethaddress = "0xeb6a4b79ded304aaea7344fd58ce5f8ebc8424b9"
          , description = "t2"
          , email = "t2@t.com"
          , mobile = "222222222"
          , userjoinedrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 1"
          , ethaddress = "0x4a0a14ba869bee85c490a5e6401d3f740039a01f"
          , description = "t1"
          , email = "t1@t.com"
          , mobile = "11111111"
          , userjoinedrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 5"
          , ethaddress = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
          , description = "t5"
          , email = "t5@t.com"
          , mobile = "55555555"
          , userjoinedrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 6"
          , ethaddress = "0x9a82d050ccb98c31f7817b6263980c21167196c4"
          , description = "t6"
          , email = "t6@t.com"
          , mobile = "123456"
          , userjoinedrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 4"
          , ethaddress = "0x3bb244dec13253d39e22606850f4704b469a4b93"
          , description = "t4"
          , email = "t4@t.com"
          , mobile = "123456"
          , userjoinedrankings = []
          }
        , { datestamp = 1569839363942
          , active = True
          , username = "Test 3"
          , ethaddress = "0xac5491bb066c98fec13046928a78761c0b1e5603"
          , description = "t3"
          , email = "test3@t3.com"
          , mobile = "777777777777"
          , userjoinedrankings = []
          }
        ]


usersWithSameAddressInList =
    [ { datestamp = 123456
      , active = True
      , username = "John"
      , ethaddress = "0x450dcBeB535029B62f042222D95a009F59408D5d"
      , description = "Tough"
      , email = "j@j.com"
      , mobile = "123456"
      , userjoinedrankings = []
      }
    , { datestamp = 123456
      , active = True
      , username = "Alfred"
      , ethaddress = "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F"
      , description = "Fit"
      , email = "j@j.com"
      , mobile = "123456"
      , userjoinedrankings = []
      }
    , { datestamp = 123456
      , active = True
      , username = "Alfred"
      , ethaddress = "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F"
      , description = "Fit"
      , email = "j@j.com"
      , mobile = "123456"
      , userjoinedrankings = []
      }
    ]


duplicateUsers =
    [ { datestamp = 1569839363942
      , active = True
      , username = "CTest1"
      , ethaddress = "0x4aa17a859f24328a2295a62770c73333f0ee31bd"
      , description = "First Cypress Test Acct"
      , email = "j@2.com"
      , mobile = "1234567890"
      , userjoinedrankings =
            [ "5e9a96572940c704e1da6f9f"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "CTest1"
      , ethaddress = "0x4aa17a859f24328a2295a62770c73333f0ee31bd"
      , description = "First Cypress Test Acct"
      , email = "j@2.com"
      , mobile = "1234567890"
      , userjoinedrankings =
            [ "5e9d8a00435f5604bb44c3bf"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "CTest2"
      , ethaddress = "0xb10a7ba0f933a406860bd06a2520e94c71a630b0"
      , description = "ctest 2"
      , email = "c@t.com"
      , mobile = "999999"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "CTest1"
      , ethaddress = "0x4aa17a859f24328a2295a62770c73333f0ee31bd"
      , description = "First Cypress Test Acct"
      , email = "j@2.com"
      , mobile = "1234567890"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 10"
      , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
      , description = "t10"
      , email = "t10@t.com"
      , mobile = "10101000"
      , userjoinedrankings =
            [ "5e8e879d8e85c8437012e2a7"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 8"
      , ethaddress = "0xf9bd658a9f3e23ed77192a14a9da6a5c37566218"
      , description = "t8"
      , email = "t8@t.com"
      , mobile = "8888888"
      , userjoinedrankings =
            [ "5e96baff2940c704e1d86316"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 7"
      , ethaddress = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
      , description = "t7"
      , email = "t7@t.com"
      , mobile = "7777777"
      , userjoinedrankings =
            [ "5e96c74b5fa47104cea0c7c6"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 2"
      , ethaddress = "0xeb6a4b79ded304aaea7344fd58ce5f8ebc8424b9"
      , description = "t2"
      , email = "t2@t.com"
      , mobile = "222222222"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 1"
      , ethaddress = "0x4a0a14ba869bee85c490a5e6401d3f740039a01f"
      , description = "t1"
      , email = "t1@t.com"
      , mobile = "11111111"
      , userjoinedrankings =
            [ "5e8e879d8e85c8437012e2a7"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 5"
      , ethaddress = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
      , description = "t5"
      , email = "t5@t.com"
      , mobile = "55555555"
      , userjoinedrankings =
            [ "5e96c74b5fa47104cea0c7c6"
            , "5e8e879d8e85c8437012e2a7"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 6"
      , ethaddress = "0x9a82d050ccb98c31f7817b6263980c21167196c4"
      , description = "t6"
      , email = "t6@t.com"
      , mobile = "123456"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 4"
      , ethaddress = "0x3bb244dec13253d39e22606850f4704b469a4b93"
      , description = "t4"
      , email = "t4@t.com"
      , mobile = "123456"
      , userjoinedrankings =
            [ "5e96c74b5fa47104cea0c7c6"
            , "5e96c9ed2940c704e1d8685e"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 3"
      , ethaddress = "0xac5491bb066c98fec13046928a78761c0b1e5603"
      , description = "t3"
      , email = "test3@t3.com"
      , mobile = "777777777777"
      , userjoinedrankings =
            [ "5e96c9ed2940c704e1d8685e"
            ]
      }
    ]


duplicateUsersRemoved =
    [ { datestamp = 1569839363942
      , active = True
      , username = "CTest2"
      , ethaddress = "0xb10a7ba0f933a406860bd06a2520e94c71a630b0"
      , description = "ctest 2"
      , email = "c@t.com"
      , mobile = "999999"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "CTest1"
      , ethaddress = "0x4aa17a859f24328a2295a62770c73333f0ee31bd"
      , description = "First Cypress Test Acct"
      , email = "j@2.com"
      , mobile = "1234567890"
      , userjoinedrankings = [ "5e9a96572940c704e1da6f9f" ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 10"
      , ethaddress = "0xce987a7e670655f30e582fbde1573b5be8ffb9a8"
      , description = "t10"
      , email = "t10@t.com"
      , mobile = "10101000"
      , userjoinedrankings =
            [ "5e8e879d8e85c8437012e2a7"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 8"
      , ethaddress = "0xf9bd658a9f3e23ed77192a14a9da6a5c37566218"
      , description = "t8"
      , email = "t8@t.com"
      , mobile = "8888888"
      , userjoinedrankings =
            [ "5e96baff2940c704e1d86316"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 7"
      , ethaddress = "0x2b5fa24358f7bda9517c66f9f44aa906070fc5a2"
      , description = "t7"
      , email = "t7@t.com"
      , mobile = "7777777"
      , userjoinedrankings =
            [ "5e96c74b5fa47104cea0c7c6"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 2"
      , ethaddress = "0xeb6a4b79ded304aaea7344fd58ce5f8ebc8424b9"
      , description = "t2"
      , email = "t2@t.com"
      , mobile = "222222222"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 1"
      , ethaddress = "0x4a0a14ba869bee85c490a5e6401d3f740039a01f"
      , description = "t1"
      , email = "t1@t.com"
      , mobile = "11111111"
      , userjoinedrankings =
            [ "5e8e879d8e85c8437012e2a7"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 5"
      , ethaddress = "0xf5003cea9657a15123b1cc83c305f87555d190cf"
      , description = "t5"
      , email = "t5@t.com"
      , mobile = "55555555"
      , userjoinedrankings =
            [ "5e96c74b5fa47104cea0c7c6"
            , "5e8e879d8e85c8437012e2a7"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 6"
      , ethaddress = "0x9a82d050ccb98c31f7817b6263980c21167196c4"
      , description = "t6"
      , email = "t6@t.com"
      , mobile = "123456"
      , userjoinedrankings = []
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 4"
      , ethaddress = "0x3bb244dec13253d39e22606850f4704b469a4b93"
      , description = "t4"
      , email = "t4@t.com"
      , mobile = "123456"
      , userjoinedrankings =
            [ "5e96c74b5fa47104cea0c7c6"
            , "5e96c9ed2940c704e1d8685e"
            ]
      }
    , { datestamp = 1569839363942
      , active = True
      , username = "Test 3"
      , ethaddress = "0xac5491bb066c98fec13046928a78761c0b1e5603"
      , description = "t3"
      , email = "test3@t3.com"
      , mobile = "777777777777"
      , userjoinedrankings =
            [ "5e96c9ed2940c704e1d8685e"
            ]
      }
    ]

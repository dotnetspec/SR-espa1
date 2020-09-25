--module SR.Encode exposing (address, bigInt, blockHash, blockId, hex, hexInt, listOfMaybesToVal, logFilter, topicsList, txCall, txHash)


module SR.Encode exposing (encodeUserList)

{-|

@docs address, bigInt, blockHash, blockId, hex, hexInt, listOfMaybesToVal, logFilter, topicsList, txCall, txHash

-}

-- import BigInt exposing (BigInt)
-- import Eth.Types exposing (..)
-- import Eth.Utils exposing (..)
--import Hex

import Json.Encode as Encode exposing (Value, int, list, null, object, string)
import SR.Types
import Eth.Utils



-- Simple


{-| e.g. not used
-}


encodeUserList : List SR.Types.User -> Encode.Value
encodeUserList lusers =
    let
        encodedList =
            Encode.list encodeUserObj lusers
    in
    encodedList

encodeUserObj : SR.Types.User -> Encode.Value
encodeUserObj user =
    case user.m_ethaddress of
        Nothing ->
            Encode.object
                [ ( "datestamp", Encode.int user.datestamp )
                , ( "active", Encode.bool user.active )
                , ( "username", Encode.string user.username )
                , ( "m_ethaddress", Encode.string "" )
                , ( "description", Encode.string user.description )
                , ( "email", Encode.string user.email )
                , ( "mobile", Encode.string user.mobile )
                , ( "userjoinrankings", encodeUserJoinRankingsList Encode.string user.userjoinrankings )
                ]
        Just m_ethaddress ->
            Encode.object
                [ ( "datestamp", Encode.int user.datestamp )
                , ( "active", Encode.bool user.active )
                , ( "username", Encode.string user.username )
                , ( "m_ethaddress", Encode.string (String.toLower (Eth.Utils.addressToString m_ethaddress)) )
                , ( "description", Encode.string user.description )
                , ( "email", Encode.string user.email )
                , ( "mobile", Encode.string user.mobile )
                , ( "userjoinrankings", encodeUserJoinRankingsList Encode.string user.userjoinrankings )
                ]


encodeUserJoinRankingsList : (a -> Value) -> List a -> Value
encodeUserJoinRankingsList fencodestr luserjoinrankings =
    Encode.list fencodestr luserjoinrankings

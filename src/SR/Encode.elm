--module SR.Encode exposing (address, bigInt, blockHash, blockId, hex, hexInt, listOfMaybesToVal, logFilter, topicsList, txCall, txHash)
module SR.Encode exposing (playerEncoder)

{-|

@docs address, bigInt, blockHash, blockId, hex, hexInt, listOfMaybesToVal, logFilter, topicsList, txCall, txHash

-}

import BigInt exposing (BigInt)
import Eth.Types exposing (..)
import Eth.Utils exposing (..)
import Hex
import Json.Encode as Encode exposing (Value, int, list, null, object, string)



-- Simple


{-| e.g. not used-}
address : Address -> Value
address =
    addressToString >> string


-- Complex


playerEncoder : SR.Types.Player -> Json.Encode.Value
playerEncoder player =
    Json.Encode.object
        [ ( "DATESTAMP", Json.Encode.int player.datestamp )
        , ( "ACTIVE"
          , Json.Encode.bool player.active
          )
        , ( "CURRENTCHALLENGERNAME"
          , Json.Encode.string player.currentchallengername
          )
        , ( "CURRENTCHALLENGERID"
          , Json.Encode.int player.currentchallengerid
          )
        , ( "ADDRESS"
          , Json.Encode.string player.address
          )
        , ( "RANK"
          , Json.Encode.int player.rank
          )
        , ( "NAME"
          , Json.Encode.string player.name
          )
        , ( "id"
          , Json.Encode.int player.id
          )
        , ( "CURRENTCHALLENGERADDRESS"
          , Json.Encode.string player.currentchallengeraddress
          )
        ]


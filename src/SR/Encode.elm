--module SR.Encode exposing (address, bigInt, blockHash, blockId, hex, hexInt, listOfMaybesToVal, logFilter, topicsList, txCall, txHash)


module SR.Encode exposing (playerEncoder)

{-|

@docs address, bigInt, blockHash, blockId, hex, hexInt, listOfMaybesToVal, logFilter, topicsList, txCall, txHash

-}

-- import BigInt exposing (BigInt)
-- import Eth.Types exposing (..)
-- import Eth.Utils exposing (..)
--import Hex

import Json.Encode as Encode exposing (Value, int, list, null, object, string)
import SR.Types



-- Simple


{-| e.g. not used
-}



-- address : Address -> Value
-- address =
--     addressToString >> string
-- Complex


playerEncoder : SR.Types.Player -> Encode.Value
playerEncoder player =
    Encode.object
        [ ( "DATESTAMP", Encode.int player.datestamp )
        , ( "ACTIVE"
          , Encode.bool player.active
          )
        , ( "CURRENTCHALLENGERNAME"
          , Encode.string player.currentchallengername
          )
        , ( "CURRENTCHALLENGERID"
          , Encode.int player.currentchallengerid
          )
        , ( "ADDRESS"
          , Encode.string player.address
          )
        , ( "RANK"
          , Encode.int player.rank
          )
        , ( "NAME"
          , Encode.string player.name
          )
        , ( "id"
          , Encode.int player.id
          )
        , ( "CURRENTCHALLENGERADDRESS"
          , Encode.string player.currentchallengeraddress
          )
        ]

module SR.Types exposing
    ( PlayerId, RankingId, Player
    --, HttpProvider, WebsocketProvider, FilterId
    )

{-| Types


# Simple

@docs PlayerId, RankingId


# Complex

@docs Player


# Misc

@docs examples from original = HttpProvider, WebsocketProvider, FilterId

-}

--import BigInt exposing (BigInt)
--import Http
import Internal.Types as Internal
--import Json.Decode exposing (Decoder)
--import Time exposing (Posix)


-- type Error
--     = Http Http.Error -- Standard HTTP Errors
--     | Encoding String -- Most likely an overflow of int/uint
--       -- Call returns 0x, could mean:
--       -- Contract doesn't exist
--       -- Contract function doesn't exist
--       -- Other things (look at the talk by Augur team at Devcon4 on mainstage)
--     | ZeroX String
--       -- TxSentry Errors:
--     | UserRejected -- User dissapproved of tx in Wallet
--     | Web3Undefined -- Web3 object, or provider not found.



-- Simple

type alias PlayerId =
    Internal.PlayerId


type alias RankingId =
    Internal.RankingId


-- Complex

{-| -}
type alias Player =
    { datestamp : Int
    , active : Bool
    , currentchallengername : String
    , currentchallengerid : Int
    , address : String
    , rank : Int
    , name : String
    , id : Int
    , currentchallengeraddress : String
    }

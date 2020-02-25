module SR.Defaults exposing (emptyPlayer)

{-| Default values.
For those withDefault shenanigans.

@docs emptyPlayer

-}

import SR.Types exposing (..)
import Internal.Types as Internal

emptyPlayer : SR.Types.Player
emptyPlayer =
    {    datestamp = 12345
    , active = False
    , currentchallengername = "Available"
    , currentchallengerid = 0
    , address = ""
    , rank = 0
    , name = "Unidentified"
    , id = 0
    , currentchallengeraddress = ""
    }

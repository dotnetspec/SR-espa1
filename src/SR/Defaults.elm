module SR.Defaults exposing
    ( emptyPlayer
    , emptyRankingInfo, emptyUser, secretKey
    )

{-| Default values.
For those withDefault shenanigans.

@docs emptyPlayer

-}

import Http
import Internal.Types as Internal
import SR.Types exposing (..)


emptyPlayer : SR.Types.Player
emptyPlayer =
    { datestamp = 12345
    , active = False
    , currentchallengername = "Available"
    , currentchallengerid = 0
    , address = ""
    , rank = 0
    , name = "Unidentified"
    , id = 0
    , currentchallengeraddress = ""
    }


emptyUser : SR.Types.User
emptyUser =
    { datestamp = 123456
    , active = False
    , username = ""
    , ethaddress = ""
    , description = ""
    , email = ""
    , mobile = ""
    }


emptyRankingInfo =
    { id = "0"
    , active = False
    , rankingname = ""
    , rankingdesc = ""
    , rankingowneraddr = ""
    }


secretKey =
    Http.header
        "secret-key"
        "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"

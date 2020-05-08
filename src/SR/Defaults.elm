module SR.Defaults exposing
    ( emptyPlayer
    , emptyActiveUser, emptyAllLists, emptyAppInfo, emptyFormValidations, emptyOwnedRanking, emptyRankingInfo, emptyUser, emptyUserPlayer, emptyUserRanking, globalBinName, globalContainerId, secretKey, selectedBinName, selectedContainerId, userBinName, userContainerId
    )

{-| Default values.
For those withDefault shenanigans.

@docs emptyPlayer

-}

import Eth.Types exposing (..)
import Http
import Internal.Types as Internal
import SR.Types exposing (..)


emptyPlayer : SR.Types.Player
emptyPlayer =
    { address = ""
    , rank = 0
    , challengeraddress = ""
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
    , userjoinrankings = []
    }


emptyFormValidations : SR.Types.FormValidations
emptyFormValidations =
    { username = ""
    , laddername = ""
    , userdesc = ""
    , ladderdesc = ""
    , email = ""
    , mobile = ""
    }


emptyActiveUser : SR.Types.User
emptyActiveUser =
    { datestamp = 123456
    , active = True
    , username = ""
    , ethaddress = ""
    , description = ""
    , email = ""
    , mobile = ""
    , userjoinrankings = []
    }


emptyUserRanking =
    { rankingInfo = emptyRankingInfo
    , userInfo = emptyUser
    }


emptyUserPlayer =
    { player = emptyPlayer
    , user = emptyUser
    }


emptyRankingInfo =
    { id = "0"
    , active = False
    , rankingname = ""
    , rankingdesc = "emptyRankingInfo"
    , rankingowneraddr = ""
    }


emptyAllLists =
    { userRankings = []
    , players = []
    , users = []
    }


emptyAppInfo =
    { selectedRanking = emptyRankingInfo
    , player = emptyUserPlayer
    , user = emptyUser
    , challenger = emptyUserPlayer
    }


emptyOwnedRanking =
    { rankingInfo = emptyRankingInfo
    , userInfo = emptyUser
    }


secretKey =
    Http.header
        "secret-key"
        "$2a$10$HIPT9LxAWxYFTW.aaMUoEeIo2N903ebCEbVqB3/HEOwiBsxY3fk2i"


selectedContainerId =
    Http.header
        "collection-id"
        "5d7deb68371673119fab12d7"


selectedBinName =
    Http.header
        "name"
        "Selected"


globalBinName =
    Http.header
        "name"
        "Global"


globalContainerId =
    Http.header
        "collection-id"
        "5d7deab3371673119fab12a6"


userBinName =
    Http.header
        "name"
        "Users"


userContainerId =
    Http.header
        "collection-id"
        "5e4cf4ba4d073155b0dca8b8"

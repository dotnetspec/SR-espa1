module SR.Defaults exposing
    ( 
    emptyFormValidations
    , emptyUserRanking, globalBinName
    , globalContainerId, secretKey, selectedBinName
    , selectedContainerId, userBinName, userContainerId
    )


{-| Default values.
For those withDefault shenanigans.



-}

import Eth.Types exposing (..)
import Http
import Internal.Types as Internal
import SR.Types exposing (..)
import EverySet exposing (EverySet)
import Data.Players
import Data.Users
import Data.Rankings



emptyFormValidations : SR.Types.FormValidations
emptyFormValidations =
    { username = ""
    , laddername = ""
    , userdesc = ""
    , ladderdesc = ""
    , email = ""
    , mobile = ""
    }


emptyUserRanking =
    { rankingInfo = (Data.Rankings.Ranking "" True "" Nothing "")
    , userInfo = Data.Users.empty
    }


-- emptyUserPlayer =
--     { player = SR.Types.Player "" "" 0 ""
--     --, user = Data.Users.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing
--     , user = SR.Types.Spectator
--     }


-- emptyAppInfo =
--     { selectedRanking = (Data.Rankings.Ranking "" True "" Nothing "")
--     , player = {player = Data.Players.empty, user = Data.Users.empty}
--     , user = Data.Users.Spectator
--     , challenger = {player = Data.Players.empty, user = Data.Users.empty}
--     , appState = AppStateGeneral
--     }



-- emptyOwnedRanking =
--     { rankingInfo = (Data.Rankings.Ranking "" True "" Nothing "")
--     --, userInfo = Data.Users.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing
--     -- todo: below not possible for an owned ranking (temp solution)
--     , userInfo = SR.Types.Spectator
--     }


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

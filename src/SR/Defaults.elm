module SR.Defaults exposing
    ( 
    emptyAppInfo, emptyFormValidations, emptyOwnedRanking
    , emptyUserPlayer, emptyUserRanking, globalBinName
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
    { rankingInfo = (SR.Types.Ranking "" True "" Nothing "")
    , userInfo = SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing
    }


emptyUserPlayer =
    { player = SR.Types.Player "" "" 0 ""
    , user = SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing
    }


emptyAppInfo =
    { selectedRanking = (SR.Types.Ranking "" True "" Nothing "")
    , player = emptyUserPlayer
    , m_user = Nothing
    , challenger = emptyUserPlayer
    , appState = AppStateGeneral
    }


emptyOwnedRanking =
    { rankingInfo = (SR.Types.Ranking "" True "" Nothing "")
    , userInfo = SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing
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

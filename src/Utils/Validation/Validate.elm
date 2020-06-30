module Utils.Validation.Validate exposing (

    isRankingNameValidated, isUserNameValidated, validatedMaxTextLength)

import SR.ListOps
import SR.Types
import Data.Users
import Data.Global
import Data.Rankings


validatedMaxTextLength : String -> Int -> String
validatedMaxTextLength str maxLength =
    if String.length str > maxLength then
        String.dropRight 1 str

    else
        str


isUserNameValidated : SR.Types.User -> List SR.Types.User -> Bool
isUserNameValidated user luser =
    if String.length user.username > 3 && String.length user.username < 9 && Data.Users.isUniqueUserName user.username luser then
        True

    else
        False

isRankingNameValidated : SR.Types.RankingInfo -> List SR.Types.UserRanking -> Bool
isRankingNameValidated rankingInfo luranking =
    if String.length rankingInfo.rankingname > 3 && String.length rankingInfo.rankingname < 9 && Data.Rankings.isUniqueRankingName rankingInfo.rankingname luranking then
        True

    else
        False

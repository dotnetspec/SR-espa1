module SR.GlobalListOps exposing
    ( createAllUserAsOwnerGlobalRankingList
    , createduserRankingPlayerList
    , extractRankingList
    , filterSelectedRankingOutOfGlobalList
    , gotOthersGlobalRankingList
    , gotRankingFromRankingList
    , gotUserIsPlayerGlobalRankingList
    , gotUserIsPlayerNonUserRankingList
    , gotUserOwnedGlobalRankingList
    , ownerValidatedRankingList
    )

import Eth.Types
import Eth.Utils
import Http
import Internal.Types
import Maybe.Extra
import RemoteData
import SR.Defaults
import SR.ListOps
import SR.Types
import Utils.MyUtils



-- external


extractRankingList : List SR.Types.UserRanking -> List SR.Types.RankingInfo
extractRankingList luserranking =
    List.map extractRanking luserranking


extractRanking : SR.Types.UserRanking -> SR.Types.RankingInfo
extractRanking uranking =
    uranking.rankingInfo


ownerValidatedRankingList : List SR.Types.RankingInfo -> List SR.Types.RankingInfo
ownerValidatedRankingList lrankinginfo =
    List.filter isValidOwnerAddress lrankinginfo


isValidOwnerAddress : SR.Types.RankingInfo -> Bool
isValidOwnerAddress rankInfo =
    if Eth.Utils.isAddress rankInfo.rankingowneraddr then
        True

    else
        False


gotUserIsPlayerNonUserRankingList : SR.Types.User -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
gotUserIsPlayerNonUserRankingList user lrankinginfo =
    let
        gotNewListRankingInfo =
            List.map gotSingleRankingInfo user.userjoinrankings

        gotSingleRankingInfo rnkId =
            Utils.MyUtils.extractRankinigInfoFromMaybe (List.head (List.filter (isRnkIdMatch rnkId) lrankinginfo))
    in
    gotNewListRankingInfo


isRnkIdMatch : String -> SR.Types.RankingInfo -> Bool
isRnkIdMatch rankingid rnk =
    if rnk.id == rankingid then
        True

    else
        False


isRankingIdInListForPlayerRnkList : String -> SR.Types.RankingInfo -> Maybe SR.Types.RankingInfo
isRankingIdInListForPlayerRnkList rankingid rnk =
    let
        _ =
            Debug.log "rnk" rnk

        _ =
            Debug.log "rnkid" rankingid
    in
    if rnk.id == rankingid then
        Just rnk

    else
        Nothing


canRankingBeInList : Maybe SR.Types.RankingInfo -> Bool
canRankingBeInList ranking =
    case ranking of
        Nothing ->
            False

        Just a ->
            True


gotRankingFromRankingList : List SR.Types.RankingInfo -> Internal.Types.RankingId -> SR.Types.RankingInfo
gotRankingFromRankingList rankingList (Internal.Types.RankingId rnkid) =
    let
        existingRanking =
            List.head <|
                List.filter (\r -> r.id == String.toLower rnkid)
                    rankingList
    in
    case existingRanking of
        Nothing ->
            SR.Defaults.emptyRankingInfo

        Just a ->
            a


filterSelectedRankingOutOfGlobalList : String -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
filterSelectedRankingOutOfGlobalList rankingid lrankinginfo =
    List.filterMap
        (doesCurrentRankingIdNOTMatchId
            rankingid
        )
        lrankinginfo



--internal
-- types
-- type alias SR.Types.UserRanking =
--     { rankingInfo : SR.Types.RankingInfo
--     , userInfo : SR.Types.User
--     }


gotUserOwnedGlobalRankingList : List SR.Types.UserRanking -> SR.Types.User -> List SR.Types.UserRanking
gotUserOwnedGlobalRankingList lownedrankings user =
    List.filterMap
        (isGlobalRankingOwnedByUser
            user
        )
        lownedrankings


isGlobalRankingOwnedByUser : SR.Types.User -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isGlobalRankingOwnedByUser user ownedrnk =
    if ownedrnk.userInfo.ethaddress == user.ethaddress then
        Just ownedrnk

    else
        Nothing


createAllUserAsOwnerGlobalRankingList : List SR.Types.RankingInfo -> List SR.Types.User -> List SR.Types.UserRanking
createAllUserAsOwnerGlobalRankingList lrankinfo luser =
    List.map (createNewOwnedRanking luser) lrankinfo


createNewOwnedRanking : List SR.Types.User -> SR.Types.RankingInfo -> SR.Types.UserRanking
createNewOwnedRanking luser rankingInfo =
    let
        userOwner =
            SR.ListOps.gotUserFromUserList luser rankingInfo.rankingowneraddr

        newOwnedRanking =
            { rankingInfo = rankingInfo
            , userInfo = userOwner
            }
    in
    newOwnedRanking



-- current


gotUserIsPlayerGlobalRankingList : List SR.Types.UserRanking -> SR.Types.User -> List SR.Types.UserRanking
gotUserIsPlayerGlobalRankingList lownedrankings user =
    List.filterMap
        (isUserPlayerInGlobalRankings
            user
        )
        lownedrankings


isUserPlayerInGlobalRankings : SR.Types.User -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isUserPlayerInGlobalRankings user ownedrnk =
    if ownedrnk.userInfo.ethaddress == user.ethaddress then
        Just ownedrnk

    else
        Nothing


createduserRankingPlayerList : List SR.Types.RankingInfo -> List SR.Types.User -> List SR.Types.UserRanking
createduserRankingPlayerList lrankinfo luser =
    List.map (createdUserRankingPlayerRanking luser) lrankinfo


createdUserRankingPlayerRanking : List SR.Types.User -> SR.Types.RankingInfo -> SR.Types.UserRanking
createdUserRankingPlayerRanking luser rankingInfo =
    let
        userOwner =
            SR.ListOps.gotUserFromUserList luser rankingInfo.rankingowneraddr

        newOwnedRanking =
            { rankingInfo = rankingInfo
            , userInfo = userOwner
            }
    in
    newOwnedRanking


gotOthersGlobalRankingList : String -> String
gotOthersGlobalRankingList str =
    str


doesCurrentRankingIdNOTMatchId : String -> SR.Types.RankingInfo -> Maybe SR.Types.RankingInfo
doesCurrentRankingIdNOTMatchId rankingid rankingInfo =
    if rankingInfo.id /= rankingid then
        Just rankingInfo

    else
        Nothing


findSelectedRankingInGlobalList : String -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
findSelectedRankingInGlobalList rankingid lrankinginfo =
    List.filterMap
        (isRankingIdInList
            rankingid
        )
        lrankinginfo


isRankingIdInList : String -> SR.Types.RankingInfo -> Maybe SR.Types.RankingInfo
isRankingIdInList rankingid rnk =
    if rnk.id == rankingid then
        Just rnk

    else
        Nothing


gotRankingListFromRemData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
gotRankingListFromRemData globalList =
    case globalList of
        RemoteData.Success a ->
            a

        RemoteData.NotAsked ->
            [ SR.Defaults.emptyRankingInfo
            ]

        RemoteData.Loading ->
            [ SR.Defaults.emptyRankingInfo
            ]

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.Timeout ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.NetworkError ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.BadStatus statuscode ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.BadBody s ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

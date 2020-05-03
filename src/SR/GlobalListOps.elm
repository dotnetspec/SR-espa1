module SR.GlobalListOps exposing
    ( createOwnedRankingList
    , filterSelectedRankingOutOfGlobalList
    , gotRankingFromRankingList
    , ownerValidatedRankingList
    )

import Eth.Types
import Eth.Utils
import Http
import Internal.Types
import RemoteData
import SR.Defaults
import SR.ListOps
import SR.Types
import Utils.MyUtils



-- external


ownerValidatedRankingList : List SR.Types.RankingInfo -> List SR.Types.RankingInfo
ownerValidatedRankingList lrankinginfo =
    List.filter isValidOwnerAddress lrankinginfo


isValidOwnerAddress : SR.Types.RankingInfo -> Bool
isValidOwnerAddress rankInfo =
    let
        _ =
            Debug.log "rankInfo.rankingowneraddr" rankInfo.rankingowneraddr

        _ =
            Debug.log "isValid addr" (Eth.Utils.isAddress rankInfo.rankingowneraddr)
    in
    if Eth.Utils.isAddress rankInfo.rankingowneraddr then
        True

    else
        False



-- current


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


type alias OwnedRanking =
    { rankingInfo : SR.Types.RankingInfo
    , userInfo : SR.Types.User
    }


createOwnedRankingList : List SR.Types.RankingInfo -> List SR.Types.User -> List OwnedRanking
createOwnedRankingList lrankinfo luser =
    let
        newList =
            List.map (createNewOwnedRanking luser) lrankinfo

        _ =
            Debug.log "newlist of owned" newList
    in
    newList


createNewOwnedRanking : List SR.Types.User -> SR.Types.RankingInfo -> OwnedRanking
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

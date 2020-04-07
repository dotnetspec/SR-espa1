module SR.ListOps exposing (filterSelectedRankingOutOfGlobalList, gotRankingFromRankingList, gotUserFromUserList, isUserSelectedOwner, isUserSelectedOwnerOfRanking, singleUserInList)

import Eth.Types
import Eth.Utils
import Internal.Types
import RemoteData
import SR.Defaults
import SR.Types
import Utils.MyUtils



--external


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


gotUserFromUserList : List SR.Types.User -> Eth.Types.Address -> SR.Types.User
gotUserFromUserList userList uaddr =
    let
        existingUser =
            --List.head <| List.filter (\r -> r.ethaddress == (Eth.Utils.addressToString uaddr |> Debug.log "uaddr argument: ")) userList
            List.head <|
                List.filter (\r -> r.ethaddress == (String.toLower <| Eth.Utils.addressToString <| uaddr))
                    userList
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUser

        Just a ->
            a


singleUserInList : RemoteData.WebData (List SR.Types.User) -> Eth.Types.Address -> SR.Types.User
singleUserInList userlist uaddr =
    gotUserFromUserList (Utils.MyUtils.extractUsersFromWebData <| userlist) uaddr


isUserSelectedOwnerOfRanking : Internal.Types.RankingId -> List SR.Types.RankingInfo -> SR.Types.User -> Bool
isUserSelectedOwnerOfRanking rnkid lrnkInfo user =
    if List.length (extractedSingleRankingFromRankingListWithRankingId rnkid lrnkInfo user) > 0 then
        True

    else
        False


isUserSelectedOwner : List SR.Types.Player -> SR.Types.User -> Bool
isUserSelectedOwner lplayers user =
    let
        _ =
            Debug.log "usr " user.ethaddress
    in
    let
        existingUser =
            List.head <|
                List.filter (\r -> r.address == (String.toLower <| user.ethaddress))
                    lplayers
    in
    case existingUser of
        Nothing ->
            False

        Just a ->
            True



--internal


extractedSingleRankingFromRankingListWithRankingId : Internal.Types.RankingId -> List SR.Types.RankingInfo -> SR.Types.User -> List SR.Types.RankingInfo
extractedSingleRankingFromRankingListWithRankingId rnkid lrnkInfo user =
    [ SR.Defaults.emptyRankingInfo ]



--[]


filterSelectedRankingOutOfGlobalList : String -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
filterSelectedRankingOutOfGlobalList rankingid lrankinginfo =
    List.filterMap
        (isRankingIdInList
            rankingid
        )
        lrankinginfo


isRankingIdInList : String -> SR.Types.RankingInfo -> Maybe SR.Types.RankingInfo
isRankingIdInList rankingid rankingInfo =
    if rankingInfo.id /= rankingid then
        Just rankingInfo

    else
        Nothing

module SR.ListOps exposing (filterSelectedRankingOutOfGlobalList, gotCurrentUserAsPlayerFromPlayerList, gotRankingFromRankingList, gotUserFromUserList, gotUserFromUserListStrAddress, gotUserListFromRemData, isUserInList, isUserMemberOfSelectedRanking, isUserSelectedOwnerOfRanking, singleUserInList, sortPlayerListByRank)

import Eth.Types
import Eth.Utils
import Http
import Internal.Types
import RemoteData
import SR.Defaults
import SR.Types
import Utils.MyUtils



--external
-- gotCurrentUserAsPlayerFromPlayerList : List SR.Types.Player -> SR.Types.User -> SR.Types.Player
-- gotCurrentUserAsPlayerFromPlayerList lplayer user =
--     SR.Defaults.emptyPlayer


isUserInList : List SR.Types.User -> Eth.Types.Address -> Bool
isUserInList userlist uaddr =
    let
        gotSingleUserFromList =
            singleUserInList userlist uaddr
    in
    if gotSingleUserFromList.ethaddress == "" then
        False

    else
        True


sortPlayerListByRank : List SR.Types.Player -> List SR.Types.Player
sortPlayerListByRank lplayer =
    List.sortBy .rank lplayer


isUserMemberOfSelectedRanking : List SR.Types.Player -> SR.Types.User -> Bool
isUserMemberOfSelectedRanking lplayer user =
    let
        filteredList =
            findPlayerInList user lplayer

        filteredRec =
            List.head filteredList
    in
    case filteredRec of
        Nothing ->
            False

        Just a ->
            if a.address == user.ethaddress then
                True

            else
                False


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


gotCurrentUserAsPlayerFromPlayerList : List SR.Types.Player -> SR.Types.User -> SR.Types.Player
gotCurrentUserAsPlayerFromPlayerList lPlayer userRec =
    let
        existingPlayer =
            List.head <|
                List.filter (\r -> r.address == (String.toLower <| userRec.ethaddress))
                    lPlayer
    in
    case existingPlayer of
        Nothing ->
            SR.Defaults.emptyPlayer

        Just a ->
            a


gotUserFromUserListStrAddress : List SR.Types.User -> String -> SR.Types.User
gotUserFromUserListStrAddress userList uaddr =
    let
        existingUser =
            --List.head <| List.filter (\r -> r.ethaddress == (Eth.Utils.addressToString uaddr |> Debug.log "uaddr argument: ")) userList
            List.head <|
                List.filter (\r -> r.ethaddress == (String.toLower <| uaddr))
                    --List.filter (\r -> r.ethaddress == uaddr)
                    userList
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUser

        Just a ->
            a


gotUserFromUserList : List SR.Types.User -> Eth.Types.Address -> SR.Types.User
gotUserFromUserList userList uaddr =
    let
        existingUser =
            --List.head <| List.filter (\r -> r.ethaddress == (Eth.Utils.addressToString uaddr |> Debug.log "uaddr argument: ")) userList
            List.head <|
                List.filter (\r -> r.ethaddress == (String.toLower <| Eth.Utils.addressToString <| uaddr))
                    --List.filter (\r -> r.ethaddress == uaddr)
                    userList
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUser

        Just a ->
            a


singleUserInList : List SR.Types.User -> Eth.Types.Address -> SR.Types.User
singleUserInList userlist uaddr =
    gotUserFromUserList userlist uaddr


isUserSelectedOwnerOfRanking : SR.Types.RankingInfo -> List SR.Types.RankingInfo -> SR.Types.User -> Bool
isUserSelectedOwnerOfRanking rnkInfo lrnkInfo user =
    let
        filteredList =
            findSelectedRankingInGlobalList rnkInfo.id lrnkInfo

        filteredRec =
            List.head filteredList
    in
    case filteredRec of
        Nothing ->
            False

        Just a ->
            if a.rankingowneraddr == user.ethaddress then
                True

            else
                False



--internal


filterSelectedRankingOutOfGlobalList : String -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
filterSelectedRankingOutOfGlobalList rankingid lrankinginfo =
    List.filterMap
        (doesCurrentRankingIdNOTMatchId
            rankingid
        )
        lrankinginfo


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


findPlayerInList : SR.Types.User -> List SR.Types.Player -> List SR.Types.Player
findPlayerInList user lPlayer =
    List.filterMap
        (isPlayerInList
            user.ethaddress
        )
        lPlayer


isPlayerInList : String -> SR.Types.Player -> Maybe SR.Types.Player
isPlayerInList playerAddr player =
    if player.address == playerAddr then
        Just player

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


gotUserListFromRemData : RemoteData.WebData (List SR.Types.User) -> List SR.Types.User
gotUserListFromRemData userList =
    case userList of
        RemoteData.Success a ->
            a

        RemoteData.NotAsked ->
            [ SR.Defaults.emptyUser
            ]

        RemoteData.Loading ->
            [ SR.Defaults.emptyUser
            ]

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.Timeout ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.NetworkError ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.BadStatus statuscode ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.BadBody s ->
                    [ SR.Defaults.emptyUser
                    ]

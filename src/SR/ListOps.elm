module SR.ListOps exposing
    ( addedNewJoinedRankingIdToUser
    , filterSelectedRankingOutOfGlobalList
    , gotUserFromUserList
    ,  gotUserListFromRemData
       --, isUserInList

    , isUserInListStrAddr
    , isUserMemberOfSelectedRanking
    , isUserSelectedOwnerOfRanking
    , ownerValidatedRankingList
    , singleUserInListStrAddr
    , validatedUserList
    )

import Eth.Types
import Eth.Utils
import Http
import Internal.Types
import RemoteData
import SR.Defaults
import SR.PlayerListOps
import SR.Types
import Utils.MyUtils



-- external


addedNewJoinedRankingIdToUser : String -> SR.Types.User -> List SR.Types.User -> List SR.Types.User
addedNewJoinedRankingIdToUser rankingId user lUser =
    let
        currentUser =
            gotUserFromUserList lUser user.username

        userJoinRankings =
            currentUser.userjoinrankings

        newUserJoinRankings =
            rankingId :: userJoinRankings

        newUser =
            { user | userjoinrankings = newUserJoinRankings }

        newUserList =
            newUser :: lUser
    in
    newUserList


ownerValidatedRankingList : List SR.Types.RankingInfo -> List SR.Types.RankingInfo
ownerValidatedRankingList lrankinginfo =
    lrankinginfo


isUserInListStrAddr : List SR.Types.User -> String -> Bool
isUserInListStrAddr userlist uaddr =
    let
        gotSingleUserFromList =
            singleUserInListStrAddr userlist uaddr
    in
    if gotSingleUserFromList.ethaddress == "" then
        False

    else
        True


isUserMemberOfSelectedRanking : List SR.Types.Player -> SR.Types.User -> Bool
isUserMemberOfSelectedRanking lplayer user =
    let
        filteredList =
            SR.PlayerListOps.findPlayerInList user lplayer

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


gotUserFromUserList : List SR.Types.User -> String -> SR.Types.User
gotUserFromUserList userList uaddr =
    let
        existingUser =
            List.head <|
                List.filter (\r -> (String.toLower <| r.ethaddress) == (String.toLower <| uaddr))
                    (validatedUserList userList)
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUser

        Just a ->
            a



-- gotUserFromUserList : List SR.Types.User -> Eth.Types.Address -> SR.Types.User
-- gotUserFromUserList userList uaddr =
--     let
--         existingUser =
--             List.head <|
--                 List.filter (\r -> r.ethaddress == (String.toLower <| Eth.Utils.addressToString <| uaddr))
--                     userList
--     in
--     case existingUser of
--         Nothing ->
--             SR.Defaults.emptyUser
--         Just a ->
--             a
-- singleUserInList : List SR.Types.User -> Eth.Types.Address -> SR.Types.User
-- singleUserInList userlist uaddr =
--     gotUserFromUserList userlist uaddr


singleUserInListStrAddr : List SR.Types.User -> String -> SR.Types.User
singleUserInListStrAddr userlist uaddr =
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


filterSelectedRankingOutOfGlobalList : String -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
filterSelectedRankingOutOfGlobalList rankingid lrankinginfo =
    List.filterMap
        (doesCurrentRankingIdNOTMatchId
            rankingid
        )
        lrankinginfo



--internal


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


validatedUserList : List SR.Types.User -> List SR.Types.User
validatedUserList luser =
    List.filterMap
        isValidUserAddrInList
        luser


isValidUserAddrInList : SR.Types.User -> Maybe SR.Types.User
isValidUserAddrInList user =
    if Eth.Utils.isAddress user.ethaddress then
        Just user

    else
        Nothing

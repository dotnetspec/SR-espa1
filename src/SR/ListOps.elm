module SR.ListOps exposing
    ( addedNewJoinedRankingIdToUser
    , convertListOfMaybeToList
    , convertMaybeUserRankingListToList
    , convertPlayersToUserPlayers
    , convertUserPlayersToPlayers
    , createAllUserAsOwnerGlobalRankingList
    , createduserRankingPlayerList
    , doesCurrentRankingIdNOTMatchId
    , extractAndSortPlayerList
    , extractPlayersFromWebData
    , extractRankingInfoListFromMaybeList
    , extractRankingList
    , extractRankingsFromWebData
    , extractUsersFromWebData
    , filterSelectedRankingOutOfGlobalList
    , findPlayerInList
    , findSelectedRankingInGlobalList
    , gotCurrentUserAsPlayerFromPlayerList
    , gotOthersGlobalRankingList
    , gotRankingFromRankingList
    , gotRankingListFromUserRankingList
    , gotRankingOwnerAsUserPlayer
    , gotUserFromUserList
    , gotUserIsPlayerGlobalRankingList
    , gotUserIsPlayerNonUserRankingList
    , gotUserListFromRemData
    , gotUserOwnedGlobalRankingList
    , gotUserPlayerFromPlayerListStrAddress
    , isRegistered
    , isUniqueUserName
    , isUniqueRankingName
    , isUserInListStrAddr
    , isUserMemberOfSelectedRanking
    , isUserOwnerOfSelectedUserRanking
    , isUserSelectedOwnerOfRanking
    , ownerValidatedRankingList
    , removeCurrentUserEntryFromUserList
    ,  removedDuplicateUserFromUserList
       -- to be privatized

    , setPlayerInPlayerListWithChallengeResult
    , setPlayerInPlayerListWithNewChallengerAddr
    , sortedPlayerListByRank
    , updatePlayerRankWithWonResult
    , validatedUserList
    )

import Eth.Utils
import Http
import Internal.Types
import List.Unique
import RemoteData
import SR.Defaults
import SR.Types
import Utils.MyUtils


isUniqueUserName : String -> List SR.Types.User -> Bool
isUniqueUserName str luser =
    let
        newList =
            List.filter (\r -> (String.toLower <| r.username) == (String.toLower <| str))
                (validatedUserList luser)
    in
    if List.isEmpty newList then
        True

    else
        False

isUniqueRankingName : String -> List SR.Types.UserRanking -> Bool
isUniqueRankingName str luranking =
    let

        lranking = gotRankingListFromUserRankingList luranking
        newList =
            List.filter (\r -> (String.toLower <| r.rankingname) == (String.toLower <| str))
                ( lranking)
    in
    if List.isEmpty newList then
        True

    else
        False


removedDuplicateUserFromUserList : List SR.Types.User -> List SR.Types.User
removedDuplicateUserFromUserList userList =
    let
        laddresses =
            List.map gotAddressesFromUserList userList

        lremovedDuplicateAddresses =
            List.Unique.filterDuplicates laddresses

        lusersWithDuplicatesRemoved =
            List.map (gotUserFromUserList userList) lremovedDuplicateAddresses
    in
    lusersWithDuplicatesRemoved


gotAddressesFromUserList : SR.Types.User -> String
gotAddressesFromUserList user =
    user.ethaddress


removeCurrentUserEntryFromUserList : List SR.Types.User -> String -> List SR.Types.User
removeCurrentUserEntryFromUserList userList uaddr =
    List.filter (\r -> (String.toLower <| r.ethaddress) /= (String.toLower <| uaddr))
        (validatedUserList userList)


isRegistered : List SR.Types.User -> SR.Types.User -> Bool
isRegistered luser user =
    let
        newUser =
            gotUserFromUserList luser user.ethaddress
    in
    if newUser.username == "" then
        False

    else
        True


extractAndSortPlayerList : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.User -> List SR.Types.UserPlayer
extractAndSortPlayerList rdlPlayer luser =
    let
        lplayer =
            extractPlayersFromWebData rdlPlayer

        convertedPlayerListToUserPlayerList =
            convertPlayersToUserPlayers
                lplayer
                luser
    in
    sortedPlayerListByRank <| convertedPlayerListToUserPlayerList


convertMaybeUserRankingListToList : Maybe (List SR.Types.UserRanking) -> List SR.Types.UserRanking
convertMaybeUserRankingListToList luRanking =
    case luRanking of
        Nothing ->
            []

        Just a ->
            a


extractPlayersFromWebData : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.Player
extractPlayersFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success players ->
            players

        RemoteData.Failure httpError ->
            []


extractRankingsFromWebData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
extractRankingsFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success rankings ->
            rankings

        RemoteData.Failure httpError ->
            []


extractUsersFromWebData : RemoteData.WebData (List SR.Types.User) -> List SR.Types.User
extractUsersFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            let
                _ =
                    Debug.log "http err" "not asked"
            in
            []

        RemoteData.Loading ->
            let
                _ =
                    Debug.log "http err" "loading"
            in
            []

        RemoteData.Success users ->
            users

        RemoteData.Failure httpError ->
            let
                _ =
                    Debug.log "http err" Utils.MyUtils.gotHttpErr <| httpError
            in
            []


convertUserPlayersToPlayers : List SR.Types.UserPlayer -> List SR.Types.Player
convertUserPlayersToPlayers luplayers =
    List.map Utils.MyUtils.refEachPlayer luplayers


convertPlayersToUserPlayers : List SR.Types.Player -> List SR.Types.User -> List SR.Types.UserPlayer
convertPlayersToUserPlayers lplayer luser =
    List.map (convertEachPlayerToUserPlayer luser) lplayer


extractRankingInfoListFromMaybeList : Maybe (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
extractRankingInfoListFromMaybeList lranking =
    case lranking of
        Just a ->
            a

        Nothing ->
            []


convertListOfMaybeToList : List (Maybe a) -> List a
convertListOfMaybeToList hasAnything =
    let
        onlyHasRealValues =
            List.filterMap (\x -> x) hasAnything
    in
    onlyHasRealValues


convertEachPlayerToUserPlayer : List SR.Types.User -> SR.Types.Player -> SR.Types.UserPlayer
convertEachPlayerToUserPlayer luser player =
    { player = player, user = gotUserFromUserList luser player.address }


gotRankingOwnerAsUserPlayer : SR.Types.RankingInfo -> List SR.Types.UserRanking -> List SR.Types.UserPlayer -> SR.Types.UserPlayer
gotRankingOwnerAsUserPlayer selectedRanking luranking luplayer =
    let
        rankingOwnerAsUser =
            (gotUserRankingFromUserRankingList luranking (Internal.Types.RankingId selectedRanking.id)).userInfo

        rankingOwnerAsPlayer =
            gotRankingOwnerAPlayer rankingOwnerAsUser.ethaddress luplayer
    in
    { player = rankingOwnerAsPlayer
    , user = rankingOwnerAsUser
    }


gotRankingOwnerAPlayer : String -> List SR.Types.UserPlayer -> SR.Types.Player
gotRankingOwnerAPlayer selectedRanking luplayer =
    (gotUserPlayerFromPlayerListStrAddress luplayer selectedRanking).player


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


isUserMemberOfSelectedRanking : List SR.Types.UserPlayer -> SR.Types.User -> Bool
isUserMemberOfSelectedRanking luplayer user =
    let
        filteredList =
            findPlayerInList user luplayer

        filteredRec =
            List.head filteredList
    in
    case filteredRec of
        Nothing ->
            False

        Just a ->
            if a.player.address == user.ethaddress then
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


gotUserRankingFromUserRankingList : List SR.Types.UserRanking -> Internal.Types.RankingId -> SR.Types.UserRanking
gotUserRankingFromUserRankingList urankingList (Internal.Types.RankingId rnkid) =
    let
        existingRanking =
            List.head <|
                List.filter (\r -> r.rankingInfo.id == String.toLower rnkid)
                    urankingList
    in
    case existingRanking of
        Nothing ->
            SR.Defaults.emptyUserRanking

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
-- singleUserInListStrAddr : List SR.Types.User -> String -> SR.Types.User
-- singleUserInListStrAddr userlist uaddr =
--     gotUserFromUserList userlist uaddr


isUserSelectedOwnerOfRanking : SR.Types.RankingInfo -> List SR.Types.RankingInfo -> SR.Types.User -> Bool
isUserSelectedOwnerOfRanking rnkInfo lrnkInfo user =
    let
        filteredList =
            findSelectedRankingInGlobalList lrnkInfo rnkInfo.id

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


isUserOwnerOfSelectedUserRanking : SR.Types.RankingInfo -> List SR.Types.UserRanking -> SR.Types.User -> Bool
isUserOwnerOfSelectedUserRanking rnkInfo lurnkInfo user =
    let
        filteredRec =
            extractSelectedUserRankingFromGlobalList lurnkInfo rnkInfo.id

        _ =
            Debug.log "filteredRec" filteredRec

        -- filteredRec =
        --     List.head filteredList
    in
    case filteredRec of
        Nothing ->
            False

        Just a ->
            if a.rankingInfo.rankingowneraddr == user.ethaddress then
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


findSelectedRankingInGlobalList : List SR.Types.RankingInfo -> String -> List SR.Types.RankingInfo
findSelectedRankingInGlobalList lrankinginfo rankingid =
    List.filterMap
        (isRankingIdInList
            rankingid
        )
        lrankinginfo


extractSelectedUserRankingFromGlobalList : List SR.Types.UserRanking -> String -> Maybe SR.Types.UserRanking
extractSelectedUserRankingFromGlobalList luranking rankingid =
    List.filterMap
        (isUserRankingIdInList
            rankingid
        )
        luranking
        |> List.head


isUserRankingIdInList : String -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isUserRankingIdInList rankingid urnk =
    if urnk.rankingInfo.id == rankingid then
        Just urnk

    else
        Nothing


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



-- ListOps
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


gotRankingListFromUserRankingList : List SR.Types.UserRanking -> List SR.Types.RankingInfo
gotRankingListFromUserRankingList luranking =
    List.map gotRankingInfo luranking


gotRankingInfo : SR.Types.UserRanking -> SR.Types.RankingInfo
gotRankingInfo uranking =
    uranking.rankingInfo


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



--internal


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
            gotUserFromUserList luser rankingInfo.rankingowneraddr

        newOwnedRanking =
            { rankingInfo = rankingInfo
            , userInfo = userOwner
            }
    in
    newOwnedRanking


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
            gotUserFromUserList luser rankingInfo.rankingowneraddr

        newOwnedRanking =
            { rankingInfo = rankingInfo
            , userInfo = userOwner
            }
    in
    newOwnedRanking



-- current


gotOthersGlobalRankingList : List SR.Types.UserRanking -> List SR.Types.UserRanking -> List SR.Types.UserRanking
gotOthersGlobalRankingList luserOwnerPlayers lalluserrankings =
    let
        lOwnerPlayerRankingIds =
            List.map gotAllRankindIds luserOwnerPlayers

        listWithfirstValFromOwnerPlayersFilteredOut =
            List.filter (\x -> not (List.member x.rankingInfo.id lOwnerPlayerRankingIds)) lalluserrankings
    in
    listWithfirstValFromOwnerPlayersFilteredOut


gotAllRankindIds : SR.Types.UserRanking -> String
gotAllRankindIds userRanking =
    userRanking.rankingInfo.id



-- ListOps
-- external


setPlayerInPlayerListWithChallengeResult : List SR.Types.UserPlayer -> SR.Types.UserPlayer -> Int -> List SR.Types.UserPlayer
setPlayerInPlayerListWithChallengeResult luPlayer uplayer rank =
    let
        playerToUserPlayer =
            uplayer.player

        filteredPlayerList =
            filterPlayerOutOfPlayerList playerToUserPlayer.address luPlayer

        updatedPlayer =
            { playerToUserPlayer | challengeraddress = "", rank = rank }

        newUserPlayer =
            { uplayer | player = updatedPlayer }

        newPlayerList =
            newUserPlayer :: filteredPlayerList

        newPlayerListSorted =
            sortedPlayerListByRank newPlayerList
    in
    newPlayerListSorted


setPlayerInPlayerListWithNewChallengerAddr : List SR.Types.UserPlayer -> SR.Types.UserPlayer -> String -> List SR.Types.UserPlayer
setPlayerInPlayerListWithNewChallengerAddr lPlayer uplayer challengeraddress =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList uplayer.player.address lPlayer

        newUserPlayerPlayerField =
            uplayer.player

        updatedPlayer =
            { newUserPlayerPlayerField | challengeraddress = uplayer.player.challengeraddress }

        newUserPlayer =
            { uplayer | player = updatedPlayer }

        newPlayerList =
            newUserPlayer :: filteredPlayerList
    in
    newPlayerList


updatePlayerRankWithWonResult : List SR.Types.UserPlayer -> SR.Types.UserPlayer -> List SR.Types.UserPlayer
updatePlayerRankWithWonResult luPlayer uplayer =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList uplayer.player.address luPlayer

        opponentAsPlayer =
            gotUserPlayerFromPlayerListStrAddress luPlayer uplayer.player.challengeraddress

        -- this needs more ?:
        newUserPlayerPlayerField =
            uplayer.player

        updatedPlayer =
            { newUserPlayerPlayerField | rank = opponentAsPlayer.player.rank }

        newUserPlayer =
            { uplayer | player = updatedPlayer }

        newPlayerList =
            newUserPlayer :: filteredPlayerList
    in
    newPlayerList


sortedPlayerListByRank : List SR.Types.UserPlayer -> List SR.Types.UserPlayer
sortedPlayerListByRank luplayer =
    let
        validatedMaybePlayerLst =
            List.map Utils.MyUtils.splitPlayerFieldsToCreateMaybePlayer luplayer

        filteredValidatedMaybePlayerLst =
            List.filter canPlayerBeInList validatedMaybePlayerLst

        convertedValidatedPlayerList =
            List.map Utils.MyUtils.convertMaybePlayerToPlayer filteredValidatedMaybePlayerLst

        reorderedConvertedValidatedPlayerList =
            reorderPlayerListToStartAtOne
                convertedValidatedPlayerList
    in
    List.sortBy extractRank reorderedConvertedValidatedPlayerList


extractRank : SR.Types.UserPlayer -> Int
extractRank uplayer =
    uplayer.player.rank


reorderPlayerListToStartAtOne : List SR.Types.UserPlayer -> List SR.Types.UserPlayer
reorderPlayerListToStartAtOne luplayer =
    let
        newPlayerListAllRankIsOne =
            List.map resetPlayerRankToOne luplayer

        newListLength =
            List.length luplayer

        newAscendingList =
            List.range 1 newListLength

        listscombined =
            List.map2 resetPlayerRankingList newAscendingList newPlayerListAllRankIsOne
    in
    listscombined


resetPlayerRankingList : Int -> SR.Types.UserPlayer -> SR.Types.UserPlayer
resetPlayerRankingList newRank uplayer =
    let
        newuserplayerplayer =
            uplayer.player

        newPlayer =
            { newuserplayerplayer
                | address = uplayer.player.address
                , rank = newRank
                , challengeraddress = uplayer.player.challengeraddress
            }

        newUserPlayer =
            { uplayer | player = newPlayer }
    in
    newUserPlayer


resetPlayerRankToOne : SR.Types.UserPlayer -> SR.Types.UserPlayer
resetPlayerRankToOne uplayer =
    let
        newuserplayerplayer =
            uplayer.player

        newPlayer =
            { newuserplayerplayer
                | address = uplayer.player.address
                , rank = 1
                , challengeraddress = uplayer.player.challengeraddress
            }

        newUserPlayer =
            { uplayer | player = newPlayer }
    in
    newUserPlayer


canPlayerBeInList : Maybe SR.Types.UserPlayer -> Bool
canPlayerBeInList uplayer =
    case uplayer of
        Nothing ->
            False

        Just a ->
            True


gotCurrentUserAsPlayerFromPlayerList : List SR.Types.UserPlayer -> SR.Types.User -> SR.Types.UserPlayer
gotCurrentUserAsPlayerFromPlayerList luPlayer userRec =
    let
        existingPlayer =
            List.head <|
                List.filter (\r -> r.player.address == (String.toLower <| userRec.ethaddress))
                    luPlayer
    in
    case existingPlayer of
        Nothing ->
            SR.Defaults.emptyUserPlayer

        Just a ->
            a


gotUserPlayerFromPlayerListStrAddress : List SR.Types.UserPlayer -> String -> SR.Types.UserPlayer
gotUserPlayerFromPlayerListStrAddress luplayer addr =
    let
        existingUser =
            List.head <|
                List.filter (\r -> r.player.address == (String.toLower <| addr))
                    luplayer
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUserPlayer

        Just a ->
            a


filterPlayerOutOfPlayerList : String -> List SR.Types.UserPlayer -> List SR.Types.UserPlayer
filterPlayerOutOfPlayerList addr lplayer =
    List.filterMap
        (doesPlayerAddrNOTMatchAddr
            addr
        )
        lplayer



--internal


doesPlayerAddrNOTMatchAddr : String -> SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
doesPlayerAddrNOTMatchAddr addr player =
    if player.player.address /= addr then
        Just player

    else
        Nothing


findPlayerInList : SR.Types.User -> List SR.Types.UserPlayer -> List SR.Types.UserPlayer
findPlayerInList user luPlayer =
    List.filterMap
        (isThisPlayerAddr
            user.ethaddress
        )
        luPlayer


isThisPlayerAddr : String -> SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
isThisPlayerAddr playerAddr uplayer =
    if uplayer.player.address == playerAddr then
        Just uplayer

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



-- private


isUserInListStrAddr : List SR.Types.User -> String -> Bool
isUserInListStrAddr userlist uaddr =
    let
        gotSingleUserFromList =
            gotUserFromUserList userlist uaddr
    in
    if gotSingleUserFromList.ethaddress == "" then
        False

    else
        True

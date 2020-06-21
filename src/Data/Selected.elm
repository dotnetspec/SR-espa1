module Data.Selected exposing (Selected, isUserOwnerOfSelectedUserRanking, addUserPlayer, removeUserPlayer, asList, changeRank, descendingRanking, isCurrentUserPlayerLowerRanked, isUserPlayerMemberOfSelectedRanking)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils


type Selected = Selected (EverySet SR.Types.UserPlayer) Internal.Types.RankingId

descendingRanking : EverySet SR.Types.UserPlayer -> Internal.Types.RankingId -> Selected 
descendingRanking esUserPlayer rnkId = 
    Selected esUserPlayer rnkId

addUserPlayer : SR.Types.UserPlayer -> Selected -> Selected
addUserPlayer uplayer srank = 
    case srank of 
        Selected rankedUserPlayers rnkId ->
                rnkId 
                |> descendingRanking (EverySet.insert (addNewUserPlayerJoinRanking uplayer rnkId) rankedUserPlayers) 


addNewUserPlayerJoinRanking : SR.Types.UserPlayer -> Internal.Types.RankingId -> SR.Types.UserPlayer
addNewUserPlayerJoinRanking uplayer rnkId = 
    let 
        newUser = uplayer.user 
        updatedUserJoinRankings = {newUser | userjoinrankings = Utils.MyUtils.stringFromRankingId rnkId :: uplayer.user.userjoinrankings}
        newUserPlayer =  { uplayer | player = uplayer.player, user = updatedUserJoinRankings }
        _ = Debug.log "newUserPlayer" newUserPlayer
    in 
        newUserPlayer



-- addedNewJoinedRankingIdToUser : String -> SR.Types.User -> List SR.Types.User -> List SR.Types.User
-- addedNewJoinedRankingIdToUser rankingId user lUser =
--     let
--         currentUser =
--             gotUserFromUserList lUser user.username

--         userJoinRankings =
--             currentUser.userjoinrankings

--         newUserJoinRankings =
--             rankingId :: userJoinRankings

--         newUser =
--             { user | userjoinrankings = newUserJoinRankings }

--         newUserList =
--             newUser :: lUser
--     in
--     newUserList

removeUserPlayer : SR.Types.UserPlayer -> Selected -> Selected
removeUserPlayer uplayer srank = 
    case srank of 
        Selected rankedUserPlayers rnkId->
           rnkId 
           |> descendingRanking (EverySet.remove uplayer rankedUserPlayers) 

changeRank : SR.Types.UserPlayer -> Int -> Selected -> Selected
changeRank uplayer newRank srank = 
    let 
        newPlayer = uplayer.player
        updatedPlayer = { newPlayer | rank = newRank, challengeraddress = ""}
        updatedUserPlayer = { uplayer | player = updatedPlayer}
    in
    
    case srank of 
        Selected rankedUserPlayers rnkId ->
            rnkId
            |> descendingRanking rankedUserPlayers
            |> removeUserPlayer uplayer  
            |> addUserPlayer updatedUserPlayer


isCurrentUserPlayerLowerRanked : SR.Types.UserPlayer -> SR.Types.UserPlayer -> Bool 
isCurrentUserPlayerLowerRanked uplayer challenger = 
    --n.b. for ranks lower int is higher rank!
    if uplayer.player.rank > challenger.player.rank then
        True 
        else False

isUserPlayerMemberOfSelectedRanking : List SR.Types.UserPlayer -> SR.Types.User -> Bool
isUserPlayerMemberOfSelectedRanking luplayer user =
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
            if (String.toLower a.player.address) == (String.toLower user.ethaddress) then
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

findPlayerInList : SR.Types.User -> List SR.Types.UserPlayer -> List SR.Types.UserPlayer
findPlayerInList user luPlayer =
    List.filterMap
        (isThisPlayerAddr
            (String.toLower user.ethaddress)
        )
        luPlayer

isThisPlayerAddr : String -> SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
isThisPlayerAddr playerAddr uplayer =
    if (String.toLower uplayer.player.address) == (String.toLower playerAddr) then
        Just uplayer

    else
        Nothing

asList : Selected -> List SR.Types.UserPlayer 
asList srank = 
    case srank of 
        Selected rankedUserPlayers rnkId ->
            rankedUserPlayers
           |> EverySet.toList
           |> List.sortBy extractRank

extractRank : SR.Types.UserPlayer -> Int
extractRank uplayer =
    uplayer.player.rank
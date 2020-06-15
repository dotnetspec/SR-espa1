module Data.SortedSelected exposing (SortedSelected, isUserOwnerOfSelectedUserRanking, addUserPlayer, removeUserPlayer, asList, changeRank, descendingRanking, isCurrentUserLowerRanked, isUserMemberOfSelectedRanking)


import SR.Types
import EverySet exposing (EverySet)


type SortedSelected = SortedSelected (EverySet SR.Types.UserPlayer)

descendingRanking : EverySet SR.Types.UserPlayer -> SortedSelected 
descendingRanking esUserPlayer = 
    SortedSelected esUserPlayer

addUserPlayer : SR.Types.UserPlayer -> SortedSelected -> SortedSelected
addUserPlayer uplayer srank = 
    case srank of 
        SortedSelected rankedUserPlayers ->
           descendingRanking <| (EverySet.insert uplayer rankedUserPlayers)

removeUserPlayer : SR.Types.UserPlayer -> SortedSelected -> SortedSelected
removeUserPlayer uplayer srank = 
    case srank of 
        SortedSelected rankedUserPlayers ->
           descendingRanking <| (EverySet.remove uplayer rankedUserPlayers)

changeRank : SR.Types.UserPlayer -> Int -> SortedSelected -> SortedSelected
changeRank uplayer newRank srank = 
    let 
        newPlayer = uplayer.player
        updatedPlayer = { newPlayer | rank = newRank, challengeraddress = ""}
        updatedUserPlayer = { uplayer | player = updatedPlayer}
    in
    
    case srank of 
        SortedSelected rankedUserPlayers ->
            descendingRanking rankedUserPlayers
            |> removeUserPlayer uplayer  
            |> addUserPlayer updatedUserPlayer


isCurrentUserLowerRanked : SR.Types.UserPlayer -> SR.Types.UserPlayer -> Bool 
isCurrentUserLowerRanked uplayer challenger = 
    --n.b. for ranks lower int is higher rank!
    if uplayer.player.rank > challenger.player.rank then
        True 
        else False

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

asList : SortedSelected -> List SR.Types.UserPlayer 
asList srank = 
    case srank of 
        SortedSelected rankedUserPlayers ->
            rankedUserPlayers
           |> EverySet.toList
           |> List.sortBy extractRank

extractRank : SR.Types.UserPlayer -> Int
extractRank uplayer =
    uplayer.player.rank
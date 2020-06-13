module Data.SortedSelected exposing (SortedSelected, addUserPlayer, removeUserPlayer, asList, changeRank, descendingRanking)


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
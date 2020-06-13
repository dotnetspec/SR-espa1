module Data.SortedRank exposing (SortedRank, addUserPlayer, removeUserPlayer, asList, changeRank)


import SR.Types
import EverySet exposing (EverySet)


type SortedRank = SortedRank (EverySet SR.Types.UserPlayer)

descendingRanking : EverySet SR.Types.UserPlayer -> SortedRank 
descendingRanking esUserPlayer = 
    SortedRank esUserPlayer

addUserPlayer : SR.Types.UserPlayer -> SortedRank -> SortedRank
addUserPlayer uplayer srank = 
    case srank of 
        SortedRank rankedUserPlayers ->
           descendingRanking <| (EverySet.insert uplayer rankedUserPlayers)

removeUserPlayer : SR.Types.UserPlayer -> SortedRank -> SortedRank
removeUserPlayer uplayer srank = 
    case srank of 
        SortedRank rankedUserPlayers ->
           descendingRanking <| (EverySet.remove uplayer rankedUserPlayers)

changeRank : SR.Types.UserPlayer -> Int -> SortedRank -> SortedRank
changeRank uplayer newRank srank = 
    let 
        newPlayer = uplayer.player
        updatedPlayer = { newPlayer | rank = newRank}
        updatedUserPlayer = { uplayer | player = updatedPlayer}
    in
    
    case srank of 
        SortedRank rankedUserPlayers ->
            descendingRanking rankedUserPlayers
            |> removeUserPlayer uplayer  
            |> addUserPlayer updatedUserPlayer

asList : SortedRank -> List SR.Types.UserPlayer 
asList srank = 
    case srank of 
        SortedRank rankedUserPlayers ->
            rankedUserPlayers
           |> EverySet.toList
           |> List.sortBy extractRank

extractRank : SR.Types.UserPlayer -> Int
extractRank uplayer =
    uplayer.player.rank
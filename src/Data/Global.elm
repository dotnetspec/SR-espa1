module Data.Global exposing (Global, ownerValidatedRankingList, extractRankingsFromWebData, emptyGlobal, asGlobal, othersUserRanking, memberUserRanking, isUserOwnerOfGlobalUserRanking, addUserRanking, removeUserRanking, asList, descendingRanking, isUserRankingMemberOfGlobalRanking, ownedUserRanking)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import SR.Defaults
import Eth.Utils
import RemoteData

-- Global came from Selected - there are many functions etc. not relevant to Global in here currently (even if renamed)

type Global = Global (EverySet SR.Types.UserRanking)

emptyGlobal : Global 
emptyGlobal = 
    Global (EverySet.empty)

asGlobal : EverySet SR.Types.UserRanking -> Global 
asGlobal esGlobal  = 
    Global esGlobal 

descendingRanking : EverySet SR.Types.UserRanking -> Global 
descendingRanking esUserRanking = 
    Global esUserRanking


-- these following 3 are not currently distinguishing the rankings:
ownedUserRanking : Global -> SR.Types.User -> Global 
ownedUserRanking global user = 
     global

memberUserRanking : Global -> SR.Types.User -> Global 
memberUserRanking global user = 
     global

othersUserRanking : Global -> SR.Types.User -> Global 
othersUserRanking global user = 
     global

-- just using a default for now
addUserRanking : SR.Types.UserRanking -> Global -> Global
addUserRanking uplayer srank = 
    case srank of 
        Global rankedUserRankings ->
                descendingRanking (EverySet.insert SR.Defaults.emptyUserRanking rankedUserRankings)


removeUserRanking : SR.Types.UserRanking -> Global -> Global
removeUserRanking uplayer srank = 
    case srank of 
        Global rankedUserRankings->
         descendingRanking (EverySet.remove uplayer rankedUserRankings) 


isUserRankingMemberOfGlobalRanking : List SR.Types.UserRanking -> SR.Types.User -> Bool
isUserRankingMemberOfGlobalRanking luplayer user =
    let
        filteredList =
            --findPlayerInList user luplayer
            [SR.Defaults.emptyUserPlayer]

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


isUserOwnerOfGlobalUserRanking : SR.Types.RankingInfo -> List SR.Types.UserRanking -> SR.Types.User -> Bool
isUserOwnerOfGlobalUserRanking rnkInfo lurnkInfo user =
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


asList : Global -> List SR.Types.UserRanking 
asList srank = 
    case srank of 
        Global rankedUserRankings ->
            rankedUserRankings
           |> EverySet.toList


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

ownerValidatedRankingList : List SR.Types.RankingInfo -> List SR.Types.RankingInfo
ownerValidatedRankingList lrankinginfo =
    List.filter isValidOwnerAddress lrankinginfo


isValidOwnerAddress : SR.Types.RankingInfo -> Bool
isValidOwnerAddress rankInfo =
    if Eth.Utils.isAddress rankInfo.rankingowneraddr then
        True

    else
        False


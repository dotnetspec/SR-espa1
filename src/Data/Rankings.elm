-- Rankings will be mainly used to communicate externally to the jsonbin server

module Data.Rankings exposing (Rankings, gotRankingListFromRemData, extractRankingList, gotRankingFromRankingList, isUniqueRankingName, gotRankingInfo, extractRankingsFromWebData, emptyRankings, updateAddr, addRanking, removeRanking, asList, asRankings, getRanking, gotRanking, rankingsetLength)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import SR.Defaults
import RemoteData
import Http



type Rankings = Rankings (EverySet SR.Types.RankingInfo)

emptyRankings : Rankings 
emptyRankings = 
    Rankings (EverySet.empty)

asRankings : EverySet SR.Types.RankingInfo -> Rankings 
asRankings esRanking  = 
    Rankings esRanking 

addRanking : SR.Types.RankingInfo -> Rankings -> Rankings
addRanking ranking sRankings = 
    case sRankings of 
        Rankings setOfRankings  ->
                asRankings (EverySet.insert ranking setOfRankings)



extractRankingList : List SR.Types.UserRanking -> List SR.Types.RankingInfo
extractRankingList luserranking =
    List.map extractRanking luserranking


extractRanking : SR.Types.UserRanking -> SR.Types.RankingInfo
extractRanking uranking =
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



rankingsetLength : Rankings -> Int 
rankingsetLength (Rankings sRankings) = 
    EverySet.size sRankings


gotRanking : Rankings  -> String -> SR.Types.RankingInfo
gotRanking (Rankings sRankings) uaddr =
    let
        existingRanking =
            List.head <|
                 EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.rankingowneraddr) == (String.toLower <| uaddr))
                    sRankings)
    in
    
    case existingRanking of
        Nothing ->
            SR.Defaults.emptyRankingInfo

        Just a ->
            a


-- probably should return a set, not a list:
-- addedNewJoinedRankingIdToRanking : String -> SR.Types.RankingInfo -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
-- addedNewJoinedRankingIdToRanking rankingId Ranking lRanking =
--     let
--         currentRanking =
--             gotRankingFromRankingList lRanking Ranking.Rankingname

--         RankingJoinRankings =
--             currentRanking.Rankingjoinrankings

--         newRankingJoinRankings =
--             rankingId :: RankingJoinRankings

--         newRanking =
--             { Ranking | Rankingjoinrankings = newRankingJoinRankings }

--         newRankingList =
--             newRanking :: lRanking
--     in
--     newRankingList

removeRanking : SR.Types.RankingInfo -> Rankings -> Rankings
removeRanking ranking sRankings = 
    case sRankings of 
        Rankings setOfRankings->
        --    rnkId 
        --    |> 
           asRankings (EverySet.remove ranking setOfRankings) 


getRanking : List SR.Types.RankingInfo -> String -> Maybe SR.Types.RankingInfo
getRanking luranking rankingid =
    List.filterMap
        (isRankingRankingIdInList
            rankingid
        )
        luranking
        |> List.head

isRankingRankingIdInList : String -> SR.Types.RankingInfo -> Maybe SR.Types.RankingInfo
isRankingRankingIdInList rankingid urnk =
    if urnk.id == rankingid then
        Just urnk

    else
        Nothing

asList : Rankings -> List SR.Types.RankingInfo
asList sRankings = 
    case sRankings of 
        Rankings setOfRankings ->
            setOfRankings
           |> EverySet.toList



updateAddr : Rankings -> String -> Rankings
updateAddr sRankings addr =
            let 
                ranking = gotRanking sRankings addr
                rankingRemoved = removeRanking ranking sRankings
                updatedRankingAddr =
                        { ranking | rankingowneraddr = addr }
            in 
                addRanking updatedRankingAddr rankingRemoved


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

gotRankingInfo : SR.Types.UserRanking -> SR.Types.RankingInfo
gotRankingInfo uranking =
    uranking.rankingInfo


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

gotRankingListFromUserRankingList : List SR.Types.UserRanking -> List SR.Types.RankingInfo
gotRankingListFromUserRankingList luranking =
    List.map gotRankingInfo luranking



extractRankingInfoListFromMaybeList : Maybe (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
extractRankingInfoListFromMaybeList lranking =
    case lranking of


        Just a ->
            a

        Nothing ->
            []
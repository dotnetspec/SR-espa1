-- Rankings will be mainly used to communicate externally to the jsonbin server

module Data.Rankings exposing (Rankings
    , gotRankingListFromRemData
    , extractRankingList
    , gotRankingFromRankingList
    , isUniqueRankingName
    , gotRankingInfo
    , extractRankingsFromWebData
    , emptyRankings
    , updateAddr
    , addRanking
    , removeRanking
    , asList, asRankings, getRanking, gotRanking, rankingsetLength
    , isRankingNameValidated)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import SR.Defaults
import RemoteData
import Http



type Rankings = Rankings (EverySet SR.Types.Ranking)

emptyRankings : Rankings 
emptyRankings = 
    Rankings (EverySet.empty)

asRankings : EverySet SR.Types.Ranking -> Rankings 
asRankings esRanking  = 
    Rankings esRanking 

addRanking : SR.Types.Ranking -> Rankings -> Rankings
addRanking ranking sRankings = 
    case sRankings of 
        Rankings setOfRankings  ->
                asRankings (EverySet.insert ranking setOfRankings)

isRankingNameValidated : SR.Types.Ranking -> List SR.Types.UserRanking -> Bool
isRankingNameValidated rankingInfo luranking =
    if String.length rankingInfo.rankingname > 3 && String.length rankingInfo.rankingname < 9 && isUniqueRankingName rankingInfo.rankingname luranking then
        True

    else
        False

extractRankingList : List SR.Types.UserRanking -> List SR.Types.Ranking
extractRankingList luserranking =
    List.map extractRanking luserranking


extractRanking : SR.Types.UserRanking -> SR.Types.Ranking
extractRanking uranking =
    uranking.rankingInfo

gotRankingFromRankingList : List SR.Types.Ranking -> Internal.Types.RankingId -> SR.Types.Ranking
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


gotRanking : Rankings  -> String -> SR.Types.Ranking
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
-- addedNewJoinedRankingIdToRanking : String -> SR.Types.Ranking -> List SR.Types.Ranking -> List SR.Types.Ranking
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

removeRanking : SR.Types.Ranking -> Rankings -> Rankings
removeRanking ranking sRankings = 
    case sRankings of 
        Rankings setOfRankings->
        --    rnkId 
        --    |> 
           asRankings (EverySet.remove ranking setOfRankings) 


getRanking : List SR.Types.Ranking -> String -> Maybe SR.Types.Ranking
getRanking luranking rankingid =
    List.filterMap
        (isRankingRankingIdInList
            rankingid
        )
        luranking
        |> List.head

isRankingRankingIdInList : String -> SR.Types.Ranking -> Maybe SR.Types.Ranking
isRankingRankingIdInList rankingid urnk =
    if urnk.id == rankingid then
        Just urnk

    else
        Nothing

asList : Rankings -> List SR.Types.Ranking
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


extractRankingsFromWebData : RemoteData.WebData (List SR.Types.Ranking) -> List SR.Types.Ranking
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

gotRankingListFromRemData : RemoteData.WebData (List SR.Types.Ranking) -> List SR.Types.Ranking
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

gotRankingInfo : SR.Types.UserRanking -> SR.Types.Ranking
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

gotRankingListFromUserRankingList : List SR.Types.UserRanking -> List SR.Types.Ranking
gotRankingListFromUserRankingList luranking =
    List.map gotRankingInfo luranking



extractRankingInfoListFromMaybeList : Maybe (List SR.Types.Ranking) -> List SR.Types.Ranking
extractRankingInfoListFromMaybeList lranking =
    case lranking of


        Just a ->
            a

        Nothing ->
            []

-- ownerValidatedRankingList : List SR.Types.Ranking -> List SR.Types.Ranking
-- ownerValidatedRankingList lrankinginfo =
--     List.filter isValidOwnerAddress lrankinginfo


-- isValidOwnerAddress : SR.Types.Ranking -> Bool
-- isValidOwnerAddress rankInfo =
--     if Eth.Utils.isAddress rankInfo.rankingowneraddr then
--         True

--     else
--         False
-- Rankings will be mainly used to communicate externally to the jsonbin server

module Data.Rankings exposing (Rankings
    --, gotRankingListFromRemData
    , extractRankingList
    , gotRankingFromRankingList
    , isUniqueRankingName
    , isIdInSet
    , isIdInList
    , isEmpty
    , gotRankingInfo
    , extractRankingsFromWebData
    , empty
    , updateAddr
    , addRanking
    , remove
    , removedById
    , handleServerDeletedRanking
    , removedDeletedRankingsFromUserJoined
    , asList, asRankings, getRanking, gotRanking, rankingsetLength
    , isRankingNameValidated)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import RemoteData
import Http
import Utils.MyUtils
import Utils.Validation.Validate
import Css exposing (ex)



type Rankings = Rankings (EverySet SR.Types.Ranking)

empty : Rankings 
empty = 
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


isEmpty : Rankings -> Bool
-- 'Rankings' is a tag containing a box (of EverySet)
-- using the tag here you can open the box
isEmpty (Rankings sRankings) =
    EverySet.isEmpty sRankings

extractRankingList : List SR.Types.UserRanking -> List SR.Types.Ranking
extractRankingList luserranking =
    List.map extractRanking luserranking


extractRanking : SR.Types.UserRanking -> SR.Types.Ranking
extractRanking uranking =
    uranking.rankingInfo


isIdInList : List SR.Types.Ranking -> Internal.Types.RankingId -> Bool
isIdInList lranking rnkId = 
    isIdInSet (asRankings (EverySet.fromList lranking)) rnkId


isIdInSet : Rankings -> Internal.Types.RankingId -> Bool 
isIdInSet sRankings rnkId = 
    let
        ranking = gotRankingFromRankingList (asList sRankings) rnkId
    in
    case ranking of
        Nothing ->
            False 
        Just rnking ->
            if Utils.Validation.Validate.isValidRankingId rnking.id_ then
                True 
            else 
                False

removedDeletedRankingsFromUserJoined : SR.Types.User -> Rankings -> SR.Types.User 
removedDeletedRankingsFromUserJoined user sRankings = 
    case user of
        SR.Types.Guest ->
            SR.Types.Guest
        (SR.Types.Registered userId token userInfo) ->
            SR.Types.Registered userId token (handleDeletionFromUserJoined userInfo sRankings)
        (SR.Types.NoWallet userId token userInfo) ->
            SR.Types.NoWallet userId token <| handleDeletionFromUserJoined userInfo sRankings
        (SR.Types.NoCredit addr userId token userInfo) ->
            SR.Types.NoCredit addr userId token <| handleDeletionFromUserJoined userInfo sRankings
        (SR.Types.Credited addr userId token userInfo) ->
            SR.Types.Credited addr userId token <| handleDeletionFromUserJoined userInfo sRankings


handleDeletionFromUserJoined : SR.Types.UserInfo -> Rankings -> SR.Types.UserInfo
handleDeletionFromUserJoined userInfo sRankings = 
    let
        lwithDeletedRankingIdsRemoved = List.filter (isIdInSet sRankings) (Utils.MyUtils.stringListToRankingIdList userInfo.userjoinrankings)
        newUserInfo = {userInfo | userjoinrankings = Utils.MyUtils.rankingIdListToStringList lwithDeletedRankingIdsRemoved} 
    in
        newUserInfo


gotRankingFromRankingList : List SR.Types.Ranking -> Internal.Types.RankingId -> Maybe SR.Types.Ranking
gotRankingFromRankingList rankingList (Internal.Types.RankingId rnkid) =
    let
        existingRanking =
            List.head <|
                List.filter (\r -> r.id_ == String.toLower rnkid)
                    rankingList
    in
        existingRanking
    -- case existingRanking of
    --     Nothing ->
    --         (SR.Types.Ranking "" True "" Nothing "")

    --     Just a ->
    --         a



rankingsetLength : Rankings -> Int 
rankingsetLength (Rankings sRankings) = 
    EverySet.size sRankings


gotRanking : Rankings  -> String -> Maybe SR.Types.Ranking
gotRanking (Rankings sRankings) uaddr =
    let
        existingRanking =
            List.head <|
                 EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.rankingownerid) == (String.toLower <| uaddr))
                    sRankings)
    in
        existingRanking
 

gotRankingById : Rankings  -> String -> Maybe SR.Types.Ranking
gotRankingById (Rankings sRankings) rnkId =
    let
        existingRanking =
            List.head <|
                 EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.id_) == (String.toLower <| rnkId))
                    sRankings)
    in
        existingRanking
    


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

remove : SR.Types.Ranking -> Rankings -> Rankings
remove ranking sRankings = 
    case sRankings of 
        Rankings everySetOfRankings->
        --    rnkId 
        --    |> 
           asRankings (EverySet.remove ranking everySetOfRankings)

removedById : Internal.Types.RankingId -> Rankings -> Rankings
removedById rnkId sRankings = 
    let
        rankingToRemove = gotRankingById sRankings (Utils.MyUtils.stringFromRankingId rnkId)
    in
    case rankingToRemove of
        Nothing ->
            sRankings
        Just rnkToRemove ->
            case sRankings of 
                Rankings everySetOfRankings ->
                    asRankings (EverySet.remove rnkToRemove everySetOfRankings) 


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
    if urnk.id_ == rankingid then
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
                m_ranking = gotRanking sRankings addr
            in 
                case m_ranking of
                    Nothing ->
                        sRankings
                    Just ranking ->
                        let
                            rankingRemoved = remove ranking sRankings
                            updatedRankingAddr = { ranking | rankingownerid = addr }
                        in
                            addRanking updatedRankingAddr rankingRemoved



handleServerDeletedRanking : RemoteData.WebData (SR.Types.UpdateGlobalBinResponse) -> (Rankings, String)
handleServerDeletedRanking rdupdateglobalbinresponse =
    case rdupdateglobalbinresponse of
        RemoteData.Success a ->
            (asRankings (EverySet.fromList a.data), "Success")

        RemoteData.NotAsked ->
            (empty, "Not Asked")

        RemoteData.Loading ->
            (empty, "Loading")

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    (empty, s)

                Http.Timeout ->
                    (empty, "TimeOut")

                Http.NetworkError ->
                    (empty, "Network Err")

                Http.BadStatus statuscode ->
                    (empty, (String.fromInt statuscode))

                Http.BadBody s ->
                    (empty, s)



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

-- gotRankingListFromRemData : RemoteData.WebData (List SR.Types.Ranking) -> List SR.Types.Ranking
-- gotRankingListFromRemData globalList =
--     case globalList of
--         RemoteData.Success a ->
--             a

--         RemoteData.NotAsked ->
--             [ (SR.Types.Ranking "" True "" Nothing "")
--             ]

--         RemoteData.Loading ->
--             [ (SR.Types.Ranking "" True "" Nothing "")
--             ]

--         RemoteData.Failure err ->
--             case err of
--                 Http.BadUrl s ->
--                     [ (SR.Types.Ranking "" True "" Nothing "")
--                     ]

--                 Http.Timeout ->
--                     [ (SR.Types.Ranking "" True "" Nothing "")
--                     ]

--                 Http.NetworkError ->
--                     [ (SR.Types.Ranking "" True "" Nothing "")
--                     ]

--                 Http.BadStatus statuscode ->
--                     [ (SR.Types.Ranking "" True "" Nothing "")
--                     ]

--                 Http.BadBody s ->
--                     [ (SR.Types.Ranking "" True "" Nothing "")
--                     ]

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
--     if Eth.Utils.isAddress rankInfo.rankingownerid then
--         True

--     else
--         False
-- Rankings is an opaque type - create and expose functions to work with it

module Data.Rankings exposing (Rankings
    , Ranking
    , FRanking
    , convertFRankingToRanking
    , gotRankingFromRankingList
    , stringListToRankingIdList
    , rankingIdListToStringList
    --, isUniqueRankingName
    , isIdInSet
    , isIdInList
    , isEmpty
    , extractRankingsFromWebData
    , empty
    , emptyRanking
    , updateAddr
    , addRanking
    , remove
    , removedById
    , handleServerDeletedRanking
    , asList, asRankings, getRanking, gotRanking, rankingsetLength
    , emptyFRanking
    , isRankingNameValid
    , testRankings
    , isOwner
    , gotRankingId
    )


--import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import RemoteData
import Http
--import Utils.MyUtils
import Utils.Validation.Validate
import Css exposing (ex)
import SRdb.Scalar exposing (Id(..))
import SRdb.ScalarCodecs
import Set
import Regex

type Rankings = Rankings (EverySet Ranking)

type alias Ranking =
    { 
        id_ : String
     --id_ : SRdb.ScalarCodecs.Id
    , active : Bool
    , rankingname : String
    , rankingdesc : Maybe String
    , rankingownerid : String
    }

emptyRanking : Ranking 
emptyRanking =
    Ranking "" False "" Nothing ""

--nb. currently if the rankingownerid isn't found among the users
-- a ranking won't appear, even if in this list:
testRankings : List Ranking
testRankings =
    [Ranking "282953512300577285" False "Test1Rnk" Nothing "283120055707763201"
    , Ranking "283673261521240581" False "Test4Rnk" Nothing "283075902275846657"
    , Ranking "283311136082756097" False "Test3Rnk" Nothing "283075902275846657"
    , Ranking "283315189344895493" False "Test2Rnk" Nothing "283120055707763201"
    ]

convertFRankingToRanking : FRanking -> Ranking 
convertFRankingToRanking franking = 
    Ranking (fromScalarCodecId franking.id_) True franking.rankingname franking.rankingdesc franking.player

type alias FRanking =

    { id_ : SRdb.ScalarCodecs.Id
    , active : Bool
    , rankingname : String
    , rankingdesc : Maybe String
    , player : String
    }

emptyFRanking : FRanking
emptyFRanking = {
    id_ =  SRdb.Scalar.Id ""
    , active = False
    , rankingname = ""
    , rankingdesc = Nothing
    --, ts_ = SRdb.Scalar.Long ""
    , player = ""
    }


fromScalarCodecId : SRdb.ScalarCodecs.Id -> String
fromScalarCodecId (Id id) =
    id

empty : Rankings 
empty = 
    Rankings (EverySet.empty)

asRankings : EverySet Ranking -> Rankings 
asRankings esRanking  = 
    Rankings esRanking 

addRanking : Ranking -> Rankings -> Rankings
addRanking ranking sRankings = 
    case sRankings of 
        Rankings setOfRankings  ->
                asRankings (EverySet.insert ranking setOfRankings)

gotRankingId : Ranking -> String 
gotRankingId ranking = 
    ranking.id_

-- isRankingNameValidated : Ranking -> Rankings -> Bool
-- isRankingNameValidated rankingInfo sRanking =
--     -- let 
--     --     _ = Debug.log "sRanking " sRanking
--     --     _ = Debug.log "rankingInfo " rankingInfo
--     --     torF = String.length rankingInfo.rankingname > 3 && String.length rankingInfo.rankingname < 9 
--     --             --&& isUniqueRankingName rankingInfo.rankingname sRanking
--     --     _ = Debug.log "TorF " torF
--     --in
--     if String.length rankingInfo.rankingname > 3 && String.length rankingInfo.rankingname < 9 
--     && isUniqueRankingName rankingInfo.rankingname sRanking then
--         True
--     else
--         False

isOwner : String -> Ranking -> Bool 
isOwner uid ranking = 
    if uid  == ranking.rankingownerid then
        True 
    else
        False


isEmpty : Rankings -> Bool
-- 'Rankings' is a tag containing a box (of EverySet)
-- using the tag here you can open the box
isEmpty (Rankings sRankings) =
    EverySet.isEmpty sRankings

stringToRankingId : String -> Internal.Types.RankingId
stringToRankingId rnkId =
    Internal.Types.RankingId rnkId

stringListToRankingIdList : List String -> List Internal.Types.RankingId
stringListToRankingIdList lrnkId =
    List.map stringToRankingId lrnkId


rankingIdListToStringList : List Internal.Types.RankingId -> List String
rankingIdListToStringList lrnkId =
    List.map stringFromRankingId lrnkId


stringFromRankingId : Internal.Types.RankingId -> String
stringFromRankingId (Internal.Types.RankingId rnkId) =
    rnkId


isIdInList : List Ranking -> Internal.Types.RankingId -> Bool
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


gotRankingFromRankingList : List Ranking -> Internal.Types.RankingId -> Maybe Ranking
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
    --         (Ranking "" True "" Nothing "")

    --     Just a ->
    --         a



rankingsetLength : Rankings -> Int 
rankingsetLength (Rankings sRankings) = 
    EverySet.size sRankings


gotRanking : Rankings  -> String -> Maybe Ranking
gotRanking (Rankings sRankings) uaddr =
    let
        existingRanking =
            List.head <|
                 EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.rankingownerid) == (String.toLower <| uaddr))
                    sRankings)
    in
        existingRanking
 

gotRankingById : Rankings  -> String -> Maybe Ranking
gotRankingById (Rankings sRankings) rnkId =
    let
        existingRanking =
            List.head <|
                 EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.id_) == (String.toLower <| rnkId))
                    sRankings)
    in
        existingRanking
    


-- probably should return a set, not a list:
-- addedNewJoinedRankingIdToRanking : String -> Ranking -> List Ranking -> List Ranking
-- addedNewJoinedRankingIdToRanking rankingId Ranking lRanking =
--     let
--         currentRanking =
--             gotRankingFromRankingList lRanking Ranking.Rankingname

--         RankingJoinRankings =
--             currentRanking.Rankingjoinrankings

--         newRankingJoinRankings =
--             rankingId :: RankingJoinRankings

--         convertFRankingToRanking =
--             { Ranking | Rankingjoinrankings = newRankingJoinRankings }

--         newRankingList =
--             convertFRankingToRanking :: lRanking
--     in
--     newRankingList

remove : Ranking -> Rankings -> Rankings
remove ranking sRankings = 
    case sRankings of 
        Rankings everySetOfRankings->
        --    rnkId 
        --    |> 
           asRankings (EverySet.remove ranking everySetOfRankings)

removedById : Internal.Types.RankingId -> Rankings -> Rankings
removedById rnkId sRankings = 
    let
        rankingToRemove = gotRankingById sRankings (stringFromRankingId rnkId)
    in
    case rankingToRemove of
        Nothing ->
            sRankings
        Just rnkToRemove ->
            case sRankings of 
                Rankings everySetOfRankings ->
                    asRankings (EverySet.remove rnkToRemove everySetOfRankings)




getRanking : List Ranking -> String -> Maybe Ranking
getRanking luranking rankingid =
    List.filterMap
        (isRankingRankingIdInList
            rankingid
        )
        luranking
        |> List.head

isRankingRankingIdInList : String -> Ranking -> Maybe Ranking
isRankingRankingIdInList rankingid urnk =
    if urnk.id_ == rankingid then
        Just urnk

    else
        Nothing

asList : Rankings -> List Ranking
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


-- not sure we will use WebData here:
--handleServerDeletedRanking : RemoteData.WebData (SR.Types.UpdateGlobalBinResponse) -> (Rankings, String)
handleServerDeletedRanking : RemoteData.WebData (Ranking) -> (Rankings, String)
handleServerDeletedRanking rdupdateglobalbinresponse =
    -- todo: fix
        (empty, "")
    -- case rdupdateglobalbinresponse of
    --     RemoteData.Success a ->
    --         (asRankings (EverySet.fromList a.data), "Success")

    --     RemoteData.NotAsked ->
    --         (empty, "Not Asked")

    --     RemoteData.Loading ->
    --         (empty, "Loading")

    --     RemoteData.Failure err ->
    --         case err of
    --             Http.BadUrl s ->
    --                 (empty, s)

    --             Http.Timeout ->
    --                 (empty, "TimeOut")

    --             Http.NetworkError ->
    --                 (empty, "Network Err")

    --             Http.BadStatus statuscode ->
    --                 (empty, (String.fromInt statuscode))

    --             Http.BadBody s ->
    --                 (empty, s)



extractRankingsFromWebData : RemoteData.WebData (List Ranking) -> List Ranking
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


-- isUniqueRankingName : String -> Rankings -> Bool
-- isUniqueRankingName str sRanking =
--     let 
--             nameSet = (Set.fromList <|  List.map  (\r -> r.rankingname) (asList sRanking))
--             _ = Debug.log "nameset " nameSet
--             _ = Debug.log "str " str
--     in 
--     --False
--     --currently only know how to do this by converting to List first
--     not <| Set.member str (Set.fromList <|  List.map  (\r -> r.rankingname) (asList sRanking))
--     --Set.member str <| Set.map  (\r -> r.rankingname) sRanking

isRankingNameValid : String -> Rankings -> Bool 
isRankingNameValid newName (Rankings esRanking) =
    if EverySet.member newName <| EverySet.map (\r -> r.rankingname) esRanking then
        False 
    else 
        Regex.contains (Maybe.withDefault Regex.never (Regex.fromString "(?!.*[\\.\\-\\_]{2,})^[a-zA-Z0-9\\.\\-\\_]{4,8}$")) newName



extractRankingInfoListFromMaybeList : Maybe (List Ranking) -> List Ranking
extractRankingInfoListFromMaybeList lranking =
    case lranking of


        Just a ->
            a

        Nothing ->
            []

-- ownerValidatedRankingList : List Ranking -> List Ranking
-- ownerValidatedRankingList lrankinginfo =
--     List.filter isValidOwnerAddress lrankinginfo


-- isValidOwnerAddress : Ranking -> Bool
-- isValidOwnerAddress rankInfo =
--     if Eth.Utils.isAddress rankInfo.rankingownerid then
--         True

--     else
--         False
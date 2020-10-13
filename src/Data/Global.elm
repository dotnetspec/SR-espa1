-- Global will be mainly used to handle internal data of the global rankings listing as it relates to the current user
-- Global currently uses the UserRankings type
module Data.Global exposing (Global, gotOthers
    , gotUserRanking
    , gotOwned
    , filteredSelected
    , createdPlayers
    , gotRankingOwner
    , rankingsAsList
    , usersAsList
    , asRankings
    , newJsonEncodedList
    , created
    , gotUserRankingByRankingId
    , empty
    , asGlobal
    , gotMember, addUserRanking, removeUserRanking, asList
    , removedUserRankingByRankingId
    , removedDeletedRankingsFromUserJoined
    , gotUpdatedFromWebData
    , gotNewRankingIdFromWebData)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import SR.Defaults
import Eth.Utils
import RemoteData
import Data.Users
import Data.Rankings
import Http
import Json.Encode
import EverySet
import List
import SR.Defaults
--import GlobalRankingsTests exposing (userOwner)
-- Global came from Selected - there are many functions etc. not relevant to Global in here currently (even if renamed)

type Global = Global (EverySet SR.Types.UserRanking)

empty : Global 
empty = 
    Global (EverySet.empty)

asGlobal : EverySet SR.Types.UserRanking -> Global 
asGlobal esGlobal  = 
    Global esGlobal 


asEverySet : Global -> EverySet SR.Types.UserRanking
asEverySet (Global esGlobal)  = 
     esGlobal


created : Data.Rankings.Rankings -> Data.Users.Users -> Global
created sRankings sUser =
    let
        luser = Data.Users.asList sUser
        esUserRanking = List.map (createdUserRanking luser) (Data.Rankings.asList sRankings)
                        |> Utils.MyUtils.removeNothingFromList
                        |> EverySet.fromList 
    in
        asGlobal esUserRanking
    

createdUserRanking : List SR.Types.User -> SR.Types.Ranking -> Maybe SR.Types.UserRanking
createdUserRanking luser ranking =
    let
        userOwner =
            Data.Users.gotUserFromUserList luser ranking.rankingowneraddr
    in
        case userOwner of
            Nothing ->
                Nothing 
            Just uOwner ->  
                let
                     newOwnedRanking =
                        { rankingInfo = ranking
                        , userInfo = uOwner
                        }
                in
                    Just newOwnedRanking

    


filteredSelected : String -> List SR.Types.Ranking -> List SR.Types.Ranking
filteredSelected rankingid lrankinginfo =
    List.filterMap
        (doesCurrentRankingIdNOTMatchId
            rankingid
        )
        lrankinginfo

doesCurrentRankingIdNOTMatchId : String -> SR.Types.Ranking -> Maybe SR.Types.Ranking
doesCurrentRankingIdNOTMatchId rankingid rankingInfo =
    if rankingInfo.id_ /= rankingid then
        Just rankingInfo

    else
        Nothing


isRnkIdMatch : String -> SR.Types.Ranking -> Bool
isRnkIdMatch rankingid rnk =
    if rnk.id_ == rankingid then
        True

    else
        False

gotOwned : Global -> SR.Types.User -> Global 
gotOwned global user = 
    asGlobal (
        EverySet.fromList (List.filterMap
        (isOwned
            user
        )
        (asList global)))
        
isOwned : SR.Types.User -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isOwned user ownedrnk =
    if ownedrnk.userInfo.m_ethaddress == user.m_ethaddress then
        Just ownedrnk

    else
        Nothing


gotMember : Global -> SR.Types.User -> List SR.Types.UserRanking
gotMember sGlobal user  = 
    let
        lmemberRankingIds = user.userjoinrankings
    in
        --List.map (gotUserRankingByRankingId sGlobal) lmemberRankingIds
        List.filterMap (gotUserRankingByRankingId sGlobal) lmemberRankingIds

gotOthers : Global -> SR.Types.User -> Global
gotOthers global user = 
    let
        esOfAll = asEverySet global
        esOfOwned = asEverySet (gotOwned global user)

        esWithOwnedRemoved = EverySet.filter (isNotMember esOfOwned ) esOfAll

        esOfMember = EverySet.fromList (gotMember global user)

        esWithMemberRemoved = EverySet.filter (isNotMember esOfMember) esWithOwnedRemoved

    in
        esWithMemberRemoved
        |> asGlobal


isMember : EverySet SR.Types.UserRanking -> SR.Types.UserRanking -> Bool
isMember esURanking uranking = 
    EverySet.member uranking esURanking


isNotMember : EverySet SR.Types.UserRanking -> SR.Types.UserRanking -> Bool
isNotMember esURanking uranking = 
    if EverySet.member uranking esURanking then
        False 
    else True

removeUserRanking :  Global -> SR.Types.UserRanking -> Global
removeUserRanking  sGlobal uRanking = 
    case sGlobal of 
        Global rankedUserRankings->
         asGlobal (EverySet.remove uRanking rankedUserRankings)



removedUserRankingByRankingId : Global -> Internal.Types.RankingId -> Global 
removedUserRankingByRankingId sGlobal rnkId = 
    created (Data.Rankings.removedById rnkId (rankingsAsSet sGlobal) ) (usersAsSet sGlobal)


addEmptyUser : SR.Types.User -> SR.Types.Ranking -> SR.Types.UserRanking 
addEmptyUser user ranking = 
    let
        newUserRanking = {rankingInfo = ranking, userInfo = user}
    in
        newUserRanking


gotUsersFromUserRankings : List SR.Types.UserRanking -> List SR.Types.User 
gotUsersFromUserRankings luRankings = 
    List.map toUser luRankings

toUser : SR.Types.UserRanking -> SR.Types.User 
toUser uRanking = 
    uRanking.userInfo



isGlobalRankingOwnedByUser : SR.Types.User -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isGlobalRankingOwnedByUser user ownedrnk =
    if ownedrnk.userInfo.m_ethaddress == user.m_ethaddress then
        Just ownedrnk

    else
        Nothing


gotUserRanking : List SR.Types.UserRanking -> SR.Types.User -> List SR.Types.UserRanking
gotUserRanking lownedrankings user =
    List.filterMap
        (isUserInGlobalRankings
            user
        )
        lownedrankings

--nb. the app is currently using the Data.Rankings version of removedDeletedRankingsFromUserJoined
-- but the test was created using this version (a mistake, but little difference)
removedDeletedRankingsFromUserJoined : SR.Types.User -> Global -> SR.Types.User 
removedDeletedRankingsFromUserJoined user sGlobal = 
    let
        lwithDeletedRankingIdsRemoved = List.filter (Data.Rankings.isIdInSet (asRankings sGlobal)) (Utils.MyUtils.stringListToRankingIdList user.userjoinrankings)
        newUser = {user | userjoinrankings = Utils.MyUtils.rankingIdListToStringList lwithDeletedRankingIdsRemoved}

    in
        newUser
    


isUserInGlobalRankings : SR.Types.User -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isUserInGlobalRankings user ownedrnk =
    if ownedrnk.userInfo.m_ethaddress == user.m_ethaddress then
        Just ownedrnk

    else
        Nothing


gotAllRankindIds : SR.Types.UserRanking -> String
gotAllRankindIds userRanking =
    userRanking.rankingInfo.id_



gotUserRankingByRankingId : Global -> String -> Maybe SR.Types.UserRanking 
gotUserRankingByRankingId sGlobal rnkId = 
    case sGlobal of 
        Global userRankings ->
            let 
                lranking = List.head (EverySet.toList (EverySet.filter (isUserRankingIdInList rnkId) userRankings))
            in
                lranking
           
    



-- just using a default for now
--addUserRanking : Global -> RemoteData.WebData SR.Types.RankingId -> SR.Types.Ranking -> SR.Types.User -> Global
addUserRanking : Global -> String -> SR.Types.Ranking -> SR.Types.User -> Global
addUserRanking sGlobal newrnkId rnkInfo user = 
    -- todo: fix
        empty
    -- let
    --     newRankingInfo =
    --         { id =  newrnkId
    --         , active = True
    --         , rankingname = rnkInfo.rankingname
    --         , rankingdesc = rnkInfo.rankingdesc
    --         , rankingowneraddr = user.m_ethaddress
    --         }

    --     newUserRanking =
    --         { rankingInfo = newRankingInfo
    --         , userInfo = user
    --         }

    -- in
    --     case sGlobal of 
    --         Global rankedUserRankings->
    --             asGlobal (EverySet.insert newUserRanking rankedUserRankings)


isUserRankingIdInList : String -> SR.Types.UserRanking -> Bool
isUserRankingIdInList rankingid urnk =
    if urnk.rankingInfo.id_ == rankingid then
        True

    else
        False


asList : Global -> List SR.Types.UserRanking 
asList srank = 
    case srank of 
        Global rankedUserRankings ->
            rankedUserRankings
           |> EverySet.toList

rankingsAsList : Global -> List SR.Types.Ranking
rankingsAsList sGlobal = 
    case sGlobal of 
        Global rankedUserRankings ->
            EverySet.map removeUser rankedUserRankings
            |> EverySet.toList

rankingsAsSet : Global -> Data.Rankings.Rankings
rankingsAsSet sGlobal = 
    case sGlobal of 
        Global rankedUserRankings ->
            Data.Rankings.asRankings (EverySet.map removeUser rankedUserRankings)

removeUser : SR.Types.UserRanking -> SR.Types.Ranking
removeUser uranking = 
    uranking.rankingInfo

usersAsList : Global -> List SR.Types.User
usersAsList sGlobal = 
    case sGlobal of 
        Global rankedUserRankings ->
            EverySet.map removeRanking rankedUserRankings
            |> EverySet.toList

usersAsSet : Global -> Data.Users.Users
usersAsSet sGlobal = 
    case sGlobal of 
        Global rankedUserRankings ->
            Data.Users.asUsers (EverySet.map removeRanking rankedUserRankings)

removeRanking : SR.Types.UserRanking -> SR.Types.User
removeRanking uranking = 
    uranking.userInfo


asRankings : Global -> Data.Rankings.Rankings
asRankings sGlobal = 
    case sGlobal of 
        Global rankedUserRankings ->
            Data.Rankings.asRankings (EverySet.map removeUser rankedUserRankings)









gotNewRankingIdFromWebData : RemoteData.WebData SR.Types.RankingId -> String
gotNewRankingIdFromWebData rankingIdremdata =
    case rankingIdremdata of
        RemoteData.Success a ->
            case a of
                b ->
                    case b of
                        SR.Types.RankingId c ->
                            c

        RemoteData.NotAsked ->
            "Initialising."

        RemoteData.Loading ->
            "Loading."

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    "Bad Url"

                Http.Timeout ->
                    "Timeout"

                Http.NetworkError ->
                    "Network Err"

                Http.BadStatus statuscode ->
                    String.fromInt <| statuscode

                Http.BadBody s ->
                    "BadBody " ++ s


gotUpdatedFromWebData : RemoteData.WebData SR.Types.UpdateGlobalBinResponse -> String
gotUpdatedFromWebData  rdugbinresponse =
    case rdugbinresponse of
        RemoteData.Success a ->
            let
                _ = Debug.log "in success " a
            in
            "success in gotUpdatedFromWebData"

        RemoteData.NotAsked ->
            "Initialising."

        RemoteData.Loading ->
            "Loading."

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    "Bad Url"

                Http.Timeout ->
                    "Timeout"

                Http.NetworkError ->
                    "Network Err"

                Http.BadStatus statuscode ->
                    String.fromInt <| statuscode

                Http.BadBody s ->
                    "BadBody " ++ s





-- we have a function within a function - this may be simplified ?
newJsonEncodedList : List SR.Types.Ranking -> Json.Encode.Value
newJsonEncodedList lotherrankingInfo =
    let
        --newRankingInfoList =
            --SR.ListOps.extractRankingList lotherrankingInfo

        encodeAglobalRankingObj : SR.Types.Ranking -> Json.Encode.Value
        encodeAglobalRankingObj rankingInfo =
            Json.Encode.object
                [ ( "id", Json.Encode.string rankingInfo.id_ )
                , ( "active", Json.Encode.bool rankingInfo.active )
                , ( "rankingname", Json.Encode.string rankingInfo.rankingname )
                --, ( "rankingdesc", Json.Encode.string rankingInfo.rankingdesc )
                , ( "rankingdesc", Json.Encode.string "" )
                , ( "rankingowneraddr", Json.Encode.string rankingInfo.rankingowneraddr )
                ]

        encodedList =
            Json.Encode.list encodeAglobalRankingObj lotherrankingInfo
    in
    encodedList


gotRankingOwner : SR.Types.Ranking -> List SR.Types.UserRanking -> List SR.Types.UserPlayer -> SR.Types.UserPlayer
gotRankingOwner selectedRanking luranking luplayer =
    -- todo: fix
        SR.Defaults.emptyUserPlayer
    -- let
    --     rankingOwnerAsUser =
    --         (gotUserRankingFromUserRankingList luranking (Internal.Types.RankingId selectedRanking.id)).userInfo

    --     rankingOwnerAsPlayer =
    --         gotRankingOwnerAsPlayer rankingOwnerAsUser.m_ethaddress luplayer
    -- in
    -- { player = rankingOwnerAsPlayer
    -- , user = rankingOwnerAsUser
    -- }

gotUserRankingFromUserRankingList : List SR.Types.UserRanking -> Internal.Types.RankingId -> SR.Types.UserRanking
gotUserRankingFromUserRankingList urankingList (Internal.Types.RankingId rnkid) =
    -- todo: fix
        SR.Defaults.emptyUserRanking
    -- let
    --     existingRanking =
    --         List.head <|
    --             List.filter (\r -> r.rankingInfo.id == String.toLower rnkid)
    --                 urankingList
    -- in
    -- case existingRanking of
    --     Nothing ->
    --         SR.Defaults.emptyUserRanking

    --     Just a ->
    --         a

gotRankingOwnerAsPlayer : String -> List SR.Types.UserPlayer -> SR.Types.Player
gotRankingOwnerAsPlayer selectedRanking luplayer =
    (gotUserPlayerFromPlayerListStrAddress luplayer selectedRanking).player


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

createdPlayers : List SR.Types.Ranking -> List SR.Types.User -> List SR.Types.UserRanking
createdPlayers lrankinfo luser =
    List.map (createdUserRankingPlayerRanking luser) lrankinfo

createdUserRankingPlayerRanking : List SR.Types.User -> SR.Types.Ranking -> SR.Types.UserRanking
createdUserRankingPlayerRanking luser rankingInfo =
    -- let
    --     userOwner =
    --         Data.Users.gotUserFromUserList luser rankingInfo.rankingowneraddr

    --     newOwnedRanking =
    --         { rankingInfo = rankingInfo
    --         , userInfo = userOwner
    --         }
    -- in
    -- newOwnedRanking
    --todo: fix
    SR.Defaults.emptyUserRanking

convertMaybeUserRankingListToList : Maybe (List SR.Types.UserRanking) -> List SR.Types.UserRanking
convertMaybeUserRankingListToList luRanking =
    case luRanking of
        Nothing ->
            []

        Just a ->
            a

findSelectedRankingInGlobalList : List SR.Types.Ranking -> String -> List SR.Types.Ranking
findSelectedRankingInGlobalList lrankinginfo rankingid =
    List.filterMap
        (isRankingIdInList
            rankingid
        )
        lrankinginfo



isRankingIdInList : String -> SR.Types.Ranking -> Maybe SR.Types.Ranking
isRankingIdInList rankingid rnk =
    if rnk.id_ == rankingid then
        Just rnk

    else
        Nothing
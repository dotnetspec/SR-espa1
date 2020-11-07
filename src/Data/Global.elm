-- Global will be mainly used to handle internal data of the global rankings listing as it relates to the current user
-- Global currently uses the UserRankings type
module Data.Global exposing (Global(..)
    , UserRanking
    , GlobalState(..)
    , listUserRankingsToGlobal
    , gotOthers
    , gotOwned
    , filteredSelected
    , createdPlayers
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
    , gotNewRankingIdFromWebData)


import SR.Types
import EverySet exposing (EverySet)
import Set exposing (Set)
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
import Data.Selected
--import GlobalRankingsTests exposing (userOwner)
-- Global came from Selected - there are many functions etc. not relevant to Global in here currently (even if renamed)

type Global = Global (EverySet UserRanking) GlobalState

--UserRanking.userInfo will always be Registered only
type alias UserRanking =
    { rankingInfo : Data.Rankings.Ranking
    , userInfo : Data.Users.User
    }

type GlobalState =
    DisplayGlobal
    | CreatingNewLadder Data.Users.User
    | CreatedNewLadder Data.Users.User Internal.Types.RankingId

-- newUserRanking ranking user =
--     UserRanking ranking user

empty : Global 
empty = 
    Global (EverySet.empty) DisplayGlobal

emptyUserRanking : UserRanking
emptyUserRanking =
    {
        rankingInfo = Data.Rankings.emptyRanking
        ,userInfo = Data.Users.Guest Data.Users.General
    }


asGlobal : EverySet UserRanking -> GlobalState -> Global 
asGlobal esUserRanking  gState = 
    Global esUserRanking gState


asEverySet : Global -> EverySet UserRanking
asEverySet (Global esGlobal globalState)  = 
     esGlobal


listUserRankingsToGlobal : List UserRanking -> GlobalState -> Global 
listUserRankingsToGlobal lUR gState =
    Global (EverySet.fromList lUR) gState


--although this refers to 'selected' the data types all relate to Global - so use here
isUserOwnerOfSelectedUserRanking : Data.Rankings.Ranking -> List UserRanking -> Data.Users.User -> Bool
isUserOwnerOfSelectedUserRanking rnkInfo lurnkInfo user =
    let
        filteredRec =
            extractSelectedUserRankingFromGlobalList lurnkInfo rnkInfo.id_
    in
    case filteredRec of
        Nothing ->
            False

        Just a ->
            case user of
                Data.Users.Guest _ ->
                    False

                (Data.Users.Registered userId token userInfo userState) ->
                    if a.rankingInfo.rankingownerid == userId then
                        True

                    else
                        False

                (Data.Users.NoWallet userId token userInfo userState) ->
                    if a.rankingInfo.rankingownerid == userId then
                        True

                    else
                        False

                (Data.Users.NoCredit addr userId token userInfo userState) ->
                    if a.rankingInfo.rankingownerid == userId then
                        True

                    else
                        False

                (Data.Users.Credited addr userId token userInfo userState) ->
                    if a.rankingInfo.rankingownerid == userId then
                        True

                    else
                        False

extractSelectedUserRankingFromGlobalList : List UserRanking -> String -> Maybe UserRanking
extractSelectedUserRankingFromGlobalList luranking rnkId =
    List.head (EverySet.toList (EverySet.filter (isUserRankingIdInList rnkId) (EverySet.fromList luranking)))


gotRanking : UserRanking -> Data.Rankings.Ranking
gotRanking uranking =
    uranking.rankingInfo


created : Data.Rankings.Rankings -> Data.Users.Users -> Global
created sRankings sUser =
    let
        esUserRanking = List.map (createdUserRanking sUser) (Data.Rankings.asList sRankings)
                        |> Utils.MyUtils.removeNothingFromList
                        |> EverySet.fromList 
    in
        asGlobal esUserRanking DisplayGlobal
    

createdUserRanking : Data.Users.Users -> Data.Rankings.Ranking -> Maybe UserRanking
createdUserRanking sUser ranking =
    let
        luserId = List.map (\x -> Data.Users.gotUserIdFromUser x ) <| Data.Users.asList sUser
        userOwnerId = List.filter (\x -> x == ranking.rankingownerid) <| luserId
    in
        case userOwnerId of
            id :: nothingHere ->
                let
                    m_user = Data.Users.gotUser sUser (Data.Users.convertedStrToUserId id)
                in
                    case m_user of
                        Nothing ->
                            Nothing 
                        Just user -> 
                            let
                                newOwnedRanking =
                                    { rankingInfo = ranking
                                    , userInfo = user
                                    }
                            in 
                                Just newOwnedRanking
            [] ->
                Nothing

filteredSelected : String -> List Data.Rankings.Ranking -> List Data.Rankings.Ranking
filteredSelected rankingid lrankinginfo =
    List.filterMap
        (doesCurrentRankingIdNOTMatchId
            rankingid
        )
        lrankinginfo

doesCurrentRankingIdNOTMatchId : String -> Data.Rankings.Ranking -> Maybe Data.Rankings.Ranking
doesCurrentRankingIdNOTMatchId rankingid rankingInfo =
    if rankingInfo.id_ /= rankingid then
        Just rankingInfo

    else
        Nothing


isRnkIdMatch : String -> Data.Rankings.Ranking -> Bool
isRnkIdMatch rankingid rnk =
    if rnk.id_ == rankingid then
        True

    else
        False

gotOwned : Global -> Data.Users.User -> Global 
gotOwned (Global esUP gState ) user = 
    asGlobal (EverySet.filter (isOwned user) esUP) gState

        
isOwned : Data.Users.User -> UserRanking -> Bool
isOwned user ownedrnk =
    case user of
        Data.Users.Guest _ ->
            False
        --UserRanking.userInfo will always be Registered only
        Data.Users.Registered userId _ _ _->
            case ownedrnk.userInfo of 
                Data.Users.Registered owneruserId _ _ _ ->
                    if owneruserId == userId then
                        True
                    else
                        False

                _ ->
                    False 

        (Data.Users.NoWallet userId _ _ _) ->
            case ownedrnk.userInfo of 
                Data.Users.Registered owneruserId _ _ _ ->
                    if owneruserId == userId then
                        True
                    else
                        False

                _ ->
                    False 

        (Data.Users.NoCredit addr userId _ _ _) ->
            case ownedrnk.userInfo of 
                Data.Users.Registered owneruserId _ _ _ ->
                    if owneruserId == userId then
                        True
                    else
                        False

                _ ->
                    False

        (Data.Users.Credited addr userId _ _ _) ->
            case ownedrnk.userInfo of 
                Data.Users.Registered owneruserId _ _ _ ->
                    if owneruserId == userId then
                        True
                    else
                        False

                _ ->
                    False 


gotMember : Global -> Data.Users.User -> List UserRanking
gotMember sGlobal user = 
    case user of
        Data.Users.Guest _ ->
            []
        (Data.Users.Registered _ _ userInfo _) ->
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinrankings
        (Data.Users.NoWallet userId token userInfo userState) ->
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinrankings
        (Data.Users.NoCredit addr userId token userInfo userState) ->
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinrankings
        (Data.Users.Credited addr userId token userInfo userState) ->
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinrankings

gotOthers : Global -> Data.Users.User -> Global
gotOthers (Global esUP gState) user = 
    let
        --esOfAll = asEverySet global
        esOfOwned = asEverySet (gotOwned (asGlobal esUP gState) user)

        esWithOwnedRemoved = EverySet.filter (isNotMember esOfOwned ) esUP

        esOfMember = EverySet.fromList (gotMember (asGlobal esUP gState) user)

        esWithMemberRemoved = EverySet.filter (isNotMember esOfMember) esWithOwnedRemoved

    in
        asGlobal esWithMemberRemoved gState


isMember : EverySet UserRanking -> UserRanking -> Bool
isMember esURanking uranking = 
    EverySet.member uranking esURanking


isNotMember : EverySet UserRanking -> UserRanking -> Bool
isNotMember esURanking uranking = 
    if EverySet.member uranking esURanking then
        False 
    else True

removeUserRanking :  Global -> UserRanking -> GlobalState -> Global
removeUserRanking  sGlobal uRanking gState = 
    case sGlobal of 
        Global rankedUserRankings _ ->
         asGlobal (EverySet.remove uRanking rankedUserRankings) gState

removedUserRankingByRankingId : Global -> Internal.Types.RankingId -> Global 
removedUserRankingByRankingId sGlobal rnkId = 
    created (Data.Rankings.removedById rnkId (rankingsAsSet sGlobal) ) (usersAsSet sGlobal)

addEmptyUser : Data.Users.User -> Data.Rankings.Ranking -> UserRanking 
addEmptyUser user ranking =
    {rankingInfo = ranking, userInfo = user}
        
gotUsersFromUserRankings : List UserRanking -> List Data.Users.User 
gotUsersFromUserRankings luRankings = 
    List.map toUser luRankings

toUser : UserRanking -> Data.Users.User 
toUser uRanking = 
    uRanking.userInfo


--nb. the app is currently using the Data.Rankings version of removedDeletedRankingsFromUserJoined
-- but the test was created using this version (a mistake, but little difference)
removedDeletedRankingsFromUserJoined : Data.Users.User -> Global -> Data.Users.User 
removedDeletedRankingsFromUserJoined user sGlobal = 
        case user of 
            Data.Users.Guest _ ->
                Data.Users.Guest Data.Users.General

            (Data.Users.Registered userId token userInfo userState) ->
                let
                    lwithDeletedRankingIdsRemoved = List.filter (Data.Rankings.isIdInSet (asRankings sGlobal)) (Data.Rankings.stringListToRankingIdList userInfo.userjoinrankings)

                    newUserInfo = {userInfo | userjoinrankings = Data.Rankings.rankingIdListToStringList lwithDeletedRankingIdsRemoved}
                in
                    Data.Users.Registered userId token newUserInfo Data.Users.General

            --todo: as above for the others or refactor
            (Data.Users.NoWallet userId token userInfo userState) ->
                Data.Users.NoWallet userId token userInfo userState
            (Data.Users.NoCredit addr userId token userInfo userState) ->
                Data.Users.NoCredit addr userId token userInfo userState
            (Data.Users.Credited addr userId token userInfo userState) ->
                Data.Users.Credited addr userId token userInfo userState
        --newUser
    



gotAllRankindIds : UserRanking -> String
gotAllRankindIds userRanking =
    userRanking.rankingInfo.id_



gotUserRankingByRankingId : Global -> String -> Maybe UserRanking 
gotUserRankingByRankingId sGlobal rnkId = 
    case sGlobal of 
        Global userRankings _ ->
            List.head (EverySet.toList (EverySet.filter (isUserRankingIdInList rnkId) userRankings))
            
           
    



-- just using a default for now
--addUserRanking : Global -> RemoteData.WebData SR.Types.RankingId -> Data.Rankings.Ranking -> Data.Users.User -> Global
addUserRanking : Global -> String -> Data.Rankings.Ranking -> Data.Users.User -> Global
addUserRanking sGlobal newrnkId rnkInfo user = 
    -- todo: fix
        empty
    -- let
    --     newRankingInfo =
    --         { id =  newrnkId
    --         , active = True
    --         , rankingname = rnkInfo.rankingname
    --         , rankingdesc = rnkInfo.rankingdesc
    --         , rankingownerid = user.m_ethaddress
    --         }

    --     newUserRanking =
    --         { rankingInfo = newRankingInfo
    --         , userInfo = user
    --         }

    -- in
    --     case sGlobal of 
    --         Global rankedUserRankings _ ->
    --             asGlobal (EverySet.insert newUserRanking rankedUserRankings)


isUserRankingIdInList : String -> UserRanking -> Bool
isUserRankingIdInList rankingid urnk =
    if urnk.rankingInfo.id_ == rankingid then
        True

    else
        False


asList : Global -> List UserRanking 
asList (Global rankedUserRankings _) =
            rankedUserRankings
           |> EverySet.toList

rankingsAsList : Global -> List Data.Rankings.Ranking
rankingsAsList sGlobal = 
    case sGlobal of 
        Global rankedUserRankings _ ->
            EverySet.map removeUser rankedUserRankings
            |> EverySet.toList

rankingsAsSet : Global -> Data.Rankings.Rankings
rankingsAsSet sGlobal = 
    case sGlobal of 
        Global rankedUserRankings _ ->
            Data.Rankings.asRankings (EverySet.map removeUser rankedUserRankings)

removeUser : UserRanking -> Data.Rankings.Ranking
removeUser uranking = 
    uranking.rankingInfo

usersAsList : Global -> List Data.Users.User
usersAsList sGlobal = 
    case sGlobal of 
        Global rankedUserRankings _ ->
            EverySet.map removeRanking rankedUserRankings
            |> EverySet.toList

usersAsSet : Global -> Data.Users.Users
usersAsSet sGlobal = 
    case sGlobal of 
        Global rankedUserRankings _ ->
            Data.Users.asUsers (EverySet.map removeRanking rankedUserRankings)

removeRanking : UserRanking -> Data.Users.User
removeRanking uranking = 
    uranking.userInfo


asRankings : Global -> Data.Rankings.Rankings
asRankings sGlobal = 
    case sGlobal of 
        Global rankedUserRankings _ ->
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



-- we have a function within a function - this may be simplified ?
newJsonEncodedList : List Data.Rankings.Ranking -> Json.Encode.Value
newJsonEncodedList lotherrankingInfo =
    let
        --newRankingInfoList =
            --SR.ListOps.extractRankingList lotherrankingInfo

        encodeAglobalRankingObj : Data.Rankings.Ranking -> Json.Encode.Value
        encodeAglobalRankingObj rankingInfo =
            Json.Encode.object
                [ ( "id", Json.Encode.string rankingInfo.id_ )
                , ( "active", Json.Encode.bool rankingInfo.active )
                , ( "rankingname", Json.Encode.string rankingInfo.rankingname )
                --, ( "rankingdesc", Json.Encode.string rankingInfo.rankingdesc )
                , ( "rankingdesc", Json.Encode.string "" )
                , ( "rankingownerid", Json.Encode.string rankingInfo.rankingownerid )
                ]

        encodedList =
            Json.Encode.list encodeAglobalRankingObj lotherrankingInfo
    in
    encodedList

-- todo: find another way to get a ranking owner ...
-- gotRankingOwner : Data.Rankings.Ranking -> List UserRanking -> List UserPlayer -> UserPlayer
-- gotRankingOwner selectedRanking luranking luplayer =
--     -- todo: fix
--         
    -- let
    --     rankingOwnerAsUser =
    --         (gotUserRankingFromUserRankingList luranking (Internal.Types.RankingId selectedRanking.id)).userInfo

    --     rankingOwnerAsPlayer =
    --         gotRankingOwnerAsPlayer rankingOwnerAsUser.m_ethaddress luplayer
    -- in
    -- { player = rankingOwnerAsPlayer
    -- , user = rankingOwnerAsUser
    -- }

gotUserRankingFromUserRankingList : List UserRanking -> Internal.Types.RankingId -> UserRanking
gotUserRankingFromUserRankingList urankingList (Internal.Types.RankingId rnkid) =
    -- todo: fix
        emptyUserRanking
    -- let
    --     existingRanking =
    --         List.head <|
    --             List.filter (\r -> r.rankingInfo.id == String.toLower rnkid)
    --                 urankingList
    -- in
    -- case existingRanking of
    --     Nothing ->
    --         emptyUserRanking

    --     Just a ->
    --         a




createdPlayers : List Data.Rankings.Ranking -> List Data.Users.User -> List UserRanking
createdPlayers lrankinfo luser =
    List.map (createdUserRankingPlayerRanking luser) lrankinfo

createdUserRankingPlayerRanking : List Data.Users.User -> Data.Rankings.Ranking -> UserRanking
createdUserRankingPlayerRanking luser rankingInfo =
    -- let
    --     userOwner =
    --         Data.Users.gotUserFromUserList luser rankingInfo.rankingownerid

    --     newOwnedRanking =
    --         { rankingInfo = rankingInfo
    --         , userInfo = userOwner
    --         }
    -- in
    -- newOwnedRanking
    --todo: fix
    emptyUserRanking

convertMaybeUserRankingListToList : Maybe (List UserRanking) -> List UserRanking
convertMaybeUserRankingListToList luRanking =
    case luRanking of
        Nothing ->
            []

        Just a ->
            a

findSelectedRankingInGlobalList : List Data.Rankings.Ranking -> String -> List Data.Rankings.Ranking
findSelectedRankingInGlobalList lrankinginfo rankingid =
    List.filterMap
        (isRankingIdInList
            rankingid
        )
        lrankinginfo



isRankingIdInList : String -> Data.Rankings.Ranking -> Maybe Data.Rankings.Ranking
isRankingIdInList rankingid rnk =
    if rnk.id_ == rankingid then
        Just rnk

    else
        Nothing
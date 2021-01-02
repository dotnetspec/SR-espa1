-- Global will be mainly used to handle internal data of the global rankings listing as it relates to the current user
-- Global currently uses the UserRankings type
module Data.Global exposing 
    (Global
    , UserRanking
    --, GlobalState(..)
    --, RankingType(..)
    , setToOwnedUR
    -- , memberUR
    -- , otherUR
    , fetchedOwned
    , fetchedMember
    , fetchedOther
    , gotOwned
    , gotOther
    , gotMember
    --, listUserRankingsToGlobal
    , isOwned
    , filteredSelected
    , createdPlayers
    , rankingsAsList
    , usersAsList
    , asRankings
    , newJsonEncodedList
    , created
    , createdNewUR
    , gotUserRankingByRankingId
    , updateRankingName
    , updateRankingDesc
    , empty
    , addUserRanking, removeUserRanking, asList
    , removedUserRankingByRankingId
    , removedDeletedRankingsFromUserJoined
    , gotNewRankingIdFromWebData
    , isEmpty
    )


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
-- nb. in main.elm 'Global' is a DataKind
type Global = Global (EverySet UserRanking) UserRanking

type RankingType = 
    Owned
    | Member
    | Other

isEmpty : Global -> Bool 
isEmpty (Global esUR uR) = 
    EverySet.isEmpty esUR

setToOwnedUR : Global -> Global
setToOwnedUR (Global esUR gstate) =
    Global (EverySet.map (\x -> setRankingType x Owned ) esUR) gstate
    -- { rankingInfo = esUR.rankingInfo
    -- , userInfo = esUR.userInfo
    -- , rankingtype = Owned
    -- }

createdNewUR : Global -> Data.Users.User -> Global
createdNewUR (Global esUR _ ) user = 
    Global esUR <| emptyUserRanking user


updateRankingName : Global -> String -> Global 
updateRankingName (Global esUR uR) name = 
    let
        rankingInfo = uR.rankingInfo
        newRankingInfo = { rankingInfo | rankingname = name}
        newUser = uR.user
    in
        Global esUR { rankingInfo = newRankingInfo, user = newUser, rankingtype = Owned } 

updateRankingDesc : Global -> String -> Global 
updateRankingDesc (Global esUR uR) desc = 
    let
        rankingInfo = uR.rankingInfo
        newRankingInfo = { rankingInfo | rankingdesc = Just desc}
        newUser = uR.user
    in
        Global esUR { rankingInfo = newRankingInfo, user = newUser, rankingtype = Owned } 



-- memberUR : Global -> Global
-- memberUR (Global esUR gstate) =
--     { rankingInfo = esUR.rankingInfo
--     , userInfo = esUR.userInfo
--     , rankingtype = Member
--     }

-- otherUR : Global -> Global
-- otherUR (Global esUR gstate) =
--     { rankingInfo = esUR.rankingInfo
--     , userInfo = esUR.userInfo
--     , rankingtype = Other
--     }

setRankingType : UserRanking -> RankingType -> UserRanking
setRankingType ur rt = 
    { rankingInfo = ur.rankingInfo
    , user = ur.user
    , rankingtype = rt
    }

--UserRanking.userInfo will always be Registered only
type alias UserRanking =
    { rankingInfo : Data.Rankings.Ranking
    , user : Data.Users.User
    , rankingtype : RankingType
    }

-- displayGlobalLogin : Global -> Global
-- displayGlobalLogin (Global esUR gstate) =
--     (Global esUR DisplayGlobalLogin)




empty : Global 
empty = 
    Global EverySet.empty <| emptyUserRanking Data.Users.emptyUser

emptyUserRanking : Data.Users.User -> UserRanking
emptyUserRanking user =
    {
        rankingInfo = Data.Rankings.emptyRanking
        , user = user
        , rankingtype = Other
    }


-- asGlobalRankings : EverySet UserRanking -> GlobalState -> Global 
-- asGlobalRankings esUserRanking  gState = 
--     Global (All esUserRanking) gState


-- asEverySet : Global -> EverySet UserRanking
-- asEverySet sGlobal  = 
--     case sGlobal of 
--         Global (All esUR) gState ->
--             esUR
        


-- listUserRankingsToGlobal : List UserRanking -> GlobalState -> Global 
-- listUserRankingsToGlobal lUR gState =
--     Global a b (EverySet.fromList lUR)


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
                Data.Users.Spectator _ _ ->
                    False

                (Data.Users.Registered userInfo userState) ->
                    if a.rankingInfo.rankingownerid == userInfo.id then
                        True

                    else
                        False


extractSelectedUserRankingFromGlobalList : List UserRanking -> String -> Maybe UserRanking
extractSelectedUserRankingFromGlobalList luranking rnkId =
    List.head (EverySet.toList (EverySet.filter (isUserRankingIdInList rnkId) (EverySet.fromList luranking)))


gotRanking : UserRanking -> Data.Rankings.Ranking
gotRanking uranking =
    uranking.rankingInfo


created : Data.Rankings.Rankings -> Data.Users.Users -> Data.Users.User -> Global
created sRankings sUser user =
    let
        esUserRanking = List.map (createdUserRanking sUser user) (Data.Rankings.asList sRankings)
                        |> Utils.MyUtils.removeNothingFromList
                        |> EverySet.fromList 
    in
        case user of 
            Data.Users.Spectator _ _ ->
                Global esUserRanking <| emptyUserRanking user
            _ ->
                Global esUserRanking <| emptyUserRanking user
    

createdUserRanking : Data.Users.Users -> Data.Users.User -> Data.Rankings.Ranking -> Maybe UserRanking
createdUserRanking sUser user ranking =
    case user of
        Data.Users.Spectator _ _ ->
            case Data.Users.gotUser sUser ranking.rankingownerid of 
                Nothing ->
                    Nothing
                Just userVal ->
                    Just { rankingInfo = ranking
                    , user = userVal
                    , rankingtype = Other
                    }

        Data.Users.Registered userInfo _->
            if (ranking.rankingownerid) == userInfo.id then
                Just
                    { rankingInfo = ranking
                    , user = user
                    , rankingtype = Owned
                    } 
            
            else if (List.member ranking.id_ userInfo.userjoinedrankings) then
                Just
                    { rankingInfo = ranking
                    , user = user
                    , rankingtype = Member
                    }
            else 
                case Data.Users.gotUser sUser (ranking.rankingownerid) of 
                    Nothing ->
                        Nothing
                    Just userVal ->
                        if (ranking.rankingownerid) == userInfo.id then
                            Nothing 
                        else 
                            Just { rankingInfo = ranking
                            , user = userVal
                            , rankingtype = Other
                            }

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

fetchedOwned : Global -> Global
fetchedOwned (Global a b) = 
    Global (EverySet.filter (\x -> x.rankingtype == Owned) a) b

fetchedMember : Global -> Global
fetchedMember (Global a b) = 
    Global (EverySet.filter (\x -> x.rankingtype == Member) a) b

fetchedOther : Global -> Global
fetchedOther (Global a b) = 
    Global (EverySet.filter (\x -> x.rankingtype == Other) a) b

gotOwned : Global -> Data.Users.User -> Global 
gotOwned sGlobal user = 
    case sGlobal of 
        Global a b  ->
            Global (EverySet.filter (isOwned user) a) b

        
isOwned : Data.Users.User -> UserRanking -> Bool
isOwned user ownedrnk =
    case user of
        Data.Users.Spectator _ _ ->
            False
        --UserRanking.userInfo will always be Registered only
        Data.Users.Registered userInfo _->
            case ownedrnk.user of 
                Data.Users.Registered owneruserInfo _ ->
                    if owneruserInfo.id == userInfo.id then
                        True
                    else
                        False

                _ ->
                    False


gotMember : Global -> Data.Users.User -> List UserRanking
gotMember sGlobal user = 
    case user of
        Data.Users.Spectator _ _ ->
            []
        (Data.Users.Registered userInfo _) ->
            let
                _ = Debug.log "in gotMember" user
            in
            
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinedrankings

--Global RankingType GlobalState
-- this is now more like createOthers ...
-- currently this is not doing anything
gotOther : Global -> Data.Users.User -> Global
gotOther sGlobal user =  
    case sGlobal of 
        Global a _ ->
            -- let
            --     --esOfOwned = asEverySet (gotOwned (asGlobalRankings a) user)

            --     esWithOwnedRemoved = EverySet.filter (isNotMember esOfOwned ) esUP

            --     esOfMember = EverySet.fromList (gotMember (asGlobalRankings esUP) user)

            --     esWithMemberRemoved = EverySet.filter (isNotMember esOfMember) esWithOwnedRemoved

            -- in
                --Global a b esWithMemberRemoved
                Global a <| emptyUserRanking Data.Users.emptyUser

        -- _ ->
        --     sGlobal


isMember : EverySet UserRanking -> UserRanking -> Bool
isMember esURanking uranking = 
    EverySet.member uranking esURanking


isNotMember : EverySet UserRanking -> UserRanking -> Bool
isNotMember esURanking uranking = 
    if EverySet.member uranking esURanking then
        False 
    else True

removeUserRanking :  Global -> UserRanking -> Global
removeUserRanking  sGlobal uRanking = 
    case sGlobal of
        Global a b ->
            Global (EverySet.remove uRanking a) b
               

removedUserRankingByRankingId : Global -> Internal.Types.RankingId -> Global 
removedUserRankingByRankingId sGlobal rnkId = 
--todo: fix
    --created (Data.Rankings.removedById rnkId (rankingsAsSet sGlobal) ) (usersAsSet sGlobal)
    sGlobal

addEmptyUser : Data.Users.User -> Data.Rankings.Ranking -> UserRanking 
addEmptyUser user ranking =
    {rankingInfo = ranking, user = user, rankingtype = Other}
        
gotUsersFromUserRankings : List UserRanking -> List Data.Users.User 
gotUsersFromUserRankings luRankings = 
    List.map toUser luRankings

toUser : UserRanking -> Data.Users.User 
toUser uRanking = 
    uRanking.user


--nb. the app is currently using the Data.Rankings version of removedDeletedRankingsFromUserJoined
-- but the test was created using this version (a mistake, but little difference)
removedDeletedRankingsFromUserJoined : Data.Users.User -> Global -> Data.Users.User 
removedDeletedRankingsFromUserJoined user sGlobal = 
        case user of 
            Data.Users.Spectator _ _ ->
                Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General

            (Data.Users.Registered userInfo userState) ->
                let
                    lwithDeletedRankingIdsRemoved = List.filter (Data.Rankings.isIdInSet (asRankings sGlobal)) (Data.Rankings.stringListToRankingIdList userInfo.userjoinedrankings)

                    newUserInfo = {userInfo | userjoinedrankings = Data.Rankings.rankingIdListToStringList lwithDeletedRankingIdsRemoved}
                in
                    Data.Users.Registered newUserInfo Data.Users.General


gotAllRankindIds : UserRanking -> String
gotAllRankindIds userRanking =
    userRanking.rankingInfo.id_



gotUserRankingByRankingId : Global -> String -> Maybe UserRanking 
gotUserRankingByRankingId sGlobal rnkId = 
    case sGlobal of
        Global esUP _ ->
            List.head (EverySet.toList (EverySet.filter (isUserRankingIdInList rnkId) esUP))
           
     
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
    --         Global (All rankedUserRankings) _ ->
    --             asGlobalRankings (EverySet.insert newUserRanking rankedUserRankings)


isUserRankingIdInList : String -> UserRanking -> Bool
isUserRankingIdInList rankingid urnk =
    let
        _ = Debug.log "rankingid" rankingid 
        _ = Debug.log "urnk" urnk
    in
    if urnk.rankingInfo.id_ == rankingid then
        True

    else
        False


asList : Global -> List UserRanking 
asList sGlobal =
    case sGlobal of
        Global a _ ->
            a
            |> EverySet.toList
               

rankingsAsList : Global -> List Data.Rankings.Ranking
rankingsAsList sGlobal = 
    case sGlobal of      
        Global a _ ->
           EverySet.map removeUser a
                |> EverySet.toList
                

rankingsAsSet : Global -> Data.Rankings.Rankings
rankingsAsSet sGlobal = 
    case sGlobal of
        Global a _ ->
            Data.Rankings.asRankings (EverySet.map removeUser a)
                

removeUser : UserRanking -> Data.Rankings.Ranking
removeUser uranking = 
    uranking.rankingInfo

usersAsList : Global -> List Data.Users.User
usersAsList sGlobal = 
   case sGlobal of 
        Global a _ ->
            EverySet.map removeRanking a
                |> EverySet.toList
     

usersAsSet : Global -> Data.Users.Users
usersAsSet sGlobal = 
    case sGlobal of 
        Global a _ ->
            Data.Users.asUsers (EverySet.map removeRanking a)
                

removeRanking : UserRanking -> Data.Users.User
removeRanking uranking = 
    uranking.user


asRankings : Global -> Data.Rankings.Rankings
asRankings sGlobal = 
    case sGlobal of 
        Global a _ ->
            Data.Rankings.asRankings (EverySet.map removeUser a)
                

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
-- do something like (gotOwnerAsUP selected).player <as you do in Selected> instead
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
        emptyUserRanking Data.Users.emptyUser
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
    emptyUserRanking Data.Users.emptyUser

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
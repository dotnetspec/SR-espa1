-- Global will be mainly used to handle internal data of the global rankings listing as it relates to the current user
-- Global currently uses the UserRankings type
module Data.Global exposing (Global, gotOthers
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

type Global = Global (EverySet UserRanking)

--UserRanking.userInfo will always be Registered only
type alias UserRanking =
    { rankingInfo : Ranking
    , userInfo : User
    }

-- newUserRanking ranking user =
--     UserRanking ranking user

empty : Global 
empty = 
    Global (EverySet.empty)

asGlobal : EverySet UserRanking -> Global 
asGlobal esGlobal  = 
    Global esGlobal 


asEverySet : Global -> EverySet UserRanking
asEverySet (Global esGlobal)  = 
     esGlobal


gotRanking : SR.Types.UserRanking -> Ranking
gotRanking uranking =
    uranking.rankingInfo


created : Data.Rankings.Rankings -> Data.Users.Users -> Global
created sRankings sUser =
    let
        luser = Data.Users.asList sUser
        esUserRanking = List.map (createdUserRanking luser) (Data.Rankings.asList sRankings)
                        |> Utils.MyUtils.removeNothingFromList
                        |> EverySet.fromList 
    in
        asGlobal esUserRanking
    

createdUserRanking : List SR.Types.User -> SR.Types.Ranking -> Maybe UserRanking
createdUserRanking luser ranking =
    let
        --todo: fix
        userOwner = Nothing
            --Data.Users.gotUserFromUserList luser ranking.rankingownerid
            
            
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
        
isOwned : SR.Types.User -> UserRanking -> Maybe UserRanking
isOwned user ownedrnk =
    case user of
        SR.Types.Guest ->
            Nothing
        --UserRanking.userInfo will always be Registered only
        SR.Types.Registered userId _ _ ->
            case ownedrnk.userInfo of 
                SR.Types.Registered owneruserId _ _ ->
                    if owneruserId == userId then
                        Just ownedrnk
                    else
                        Nothing

                _ ->
                    Nothing 

        (SR.Types.NoWallet userId _ _) ->
            case ownedrnk.userInfo of 
                SR.Types.Registered owneruserId _ _ ->
                    if owneruserId == userId then
                        Just ownedrnk
                    else
                        Nothing

                _ ->
                    Nothing 

        (SR.Types.NoCredit addr userId _ _) ->
            case ownedrnk.userInfo of 
                SR.Types.Registered owneruserId _ _ ->
                    if owneruserId == userId then
                        Just ownedrnk
                    else
                        Nothing

                _ ->
                    Nothing

        (SR.Types.Credited addr userId _ _) ->
            case ownedrnk.userInfo of 
                SR.Types.Registered owneruserId _ _ ->
                    if owneruserId == userId then
                        Just ownedrnk
                    else
                        Nothing

                _ ->
                    Nothing 


gotMember : Global -> SR.Types.User -> List UserRanking
gotMember sGlobal user = 
    case user of
        SR.Types.Guest ->
            []
        (SR.Types.Registered _ _ userInfo) ->
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinrankings
        (SR.Types.NoWallet userId token userInfo) ->
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinrankings
        (SR.Types.NoCredit addr userId token userInfo) ->
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinrankings
        (SR.Types.Credited addr userId token userInfo) ->
            List.filterMap (gotUserRankingByRankingId sGlobal) userInfo.userjoinrankings

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
        Global rankedUserRankings->
         asGlobal (EverySet.remove uRanking rankedUserRankings)

removedUserRankingByRankingId : Global -> Internal.Types.RankingId -> Global 
removedUserRankingByRankingId sGlobal rnkId = 
    created (Data.Rankings.removedById rnkId (rankingsAsSet sGlobal) ) (usersAsSet sGlobal)

addEmptyUser : SR.Types.User -> SR.Types.Ranking -> UserRanking 
addEmptyUser user ranking =
    {rankingInfo = ranking, userInfo = user}
        
gotUsersFromUserRankings : List UserRanking -> List SR.Types.User 
gotUsersFromUserRankings luRankings = 
    List.map toUser luRankings

toUser : UserRanking -> SR.Types.User 
toUser uRanking = 
    uRanking.userInfo


--nb. the app is currently using the Data.Rankings version of removedDeletedRankingsFromUserJoined
-- but the test was created using this version (a mistake, but little difference)
removedDeletedRankingsFromUserJoined : SR.Types.User -> Global -> SR.Types.User 
removedDeletedRankingsFromUserJoined user sGlobal = 
        case user of 
            SR.Types.Guest ->
                SR.Types.Guest

            (SR.Types.Registered userId token userInfo) ->
                let
                    lwithDeletedRankingIdsRemoved = List.filter (Data.Rankings.isIdInSet (asRankings sGlobal)) (Utils.MyUtils.stringListToRankingIdList userInfo.userjoinrankings)

                    newUserInfo = {userInfo | userjoinrankings = Utils.MyUtils.rankingIdListToStringList lwithDeletedRankingIdsRemoved}
                in
                    SR.Types.Registered userId token newUserInfo

            --todo: as above for the others or refactor
            (SR.Types.NoWallet userId token userInfo) ->
                SR.Types.NoWallet userId token userInfo
            (SR.Types.NoCredit addr userId token userInfo) ->
                SR.Types.NoCredit addr userId token userInfo
            (SR.Types.Credited addr userId token userInfo) ->
                SR.Types.Credited addr userId token userInfo
        --newUser
    



gotAllRankindIds : UserRanking -> String
gotAllRankindIds userRanking =
    userRanking.rankingInfo.id_



gotUserRankingByRankingId : Global -> String -> Maybe UserRanking 
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
    --         , rankingownerid = user.m_ethaddress
    --         }

    --     newUserRanking =
    --         { rankingInfo = newRankingInfo
    --         , userInfo = user
    --         }

    -- in
    --     case sGlobal of 
    --         Global rankedUserRankings->
    --             asGlobal (EverySet.insert newUserRanking rankedUserRankings)


isUserRankingIdInList : String -> UserRanking -> Bool
isUserRankingIdInList rankingid urnk =
    if urnk.rankingInfo.id_ == rankingid then
        True

    else
        False


asList : Global -> List UserRanking 
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

removeUser : UserRanking -> SR.Types.Ranking
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

removeRanking : UserRanking -> SR.Types.User
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
                , ( "rankingownerid", Json.Encode.string rankingInfo.rankingownerid )
                ]

        encodedList =
            Json.Encode.list encodeAglobalRankingObj lotherrankingInfo
    in
    encodedList


gotRankingOwner : SR.Types.Ranking -> List UserRanking -> List SR.Types.UserPlayer -> SR.Types.UserPlayer
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

gotUserRankingFromUserRankingList : List UserRanking -> Internal.Types.RankingId -> UserRanking
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
                List.filter (\r -> r.player.uid == (String.toLower <| addr))
                    luplayer
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUserPlayer

        Just a ->
            a

createdPlayers : List SR.Types.Ranking -> List SR.Types.User -> List UserRanking
createdPlayers lrankinfo luser =
    List.map (createdUserRankingPlayerRanking luser) lrankinfo

createdUserRankingPlayerRanking : List SR.Types.User -> SR.Types.Ranking -> UserRanking
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
    SR.Defaults.emptyUserRanking

convertMaybeUserRankingListToList : Maybe (List UserRanking) -> List UserRanking
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
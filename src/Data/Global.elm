-- Global will be mainly used to handle internal data of the global rankings listing as it relates to the current user
-- Global currently uses the UserRankings type
module Data.Global exposing (Global, gotOthers
    , gotUserIsPlayer
    , gotOwned
    , filteredSelected
    , createdPlayers
    , gotRankingOwner
    , rankingsAsList
    , newJsonEncodedList
    , createdGlobal
    , gotUserRanking
    , emptyGlobal
    , asGlobal
    , gotMember, addUserRanking, removeUserRanking, asList, gotNewRankingIdFromWebData)


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
-- Global came from Selected - there are many functions etc. not relevant to Global in here currently (even if renamed)

type Global = Global (EverySet SR.Types.UserRanking) 



emptyGlobal : Global 
emptyGlobal = 
    Global (EverySet.empty)

asGlobal : EverySet SR.Types.UserRanking -> Global 
asGlobal esGlobal  = 
    Global esGlobal 


asEverySet : Global -> EverySet SR.Types.UserRanking
asEverySet (Global esGlobal)  = 
     esGlobal


filteredSelected : String -> List SR.Types.Ranking -> List SR.Types.Ranking
filteredSelected rankingid lrankinginfo =
    List.filterMap
        (doesCurrentRankingIdNOTMatchId
            rankingid
        )
        lrankinginfo

doesCurrentRankingIdNOTMatchId : String -> SR.Types.Ranking -> Maybe SR.Types.Ranking
doesCurrentRankingIdNOTMatchId rankingid rankingInfo =
    if rankingInfo.id /= rankingid then
        Just rankingInfo

    else
        Nothing


isRnkIdMatch : String -> SR.Types.Ranking -> Bool
isRnkIdMatch rankingid rnk =
    if rnk.id == rankingid then
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
    if ownedrnk.userInfo.ethaddress == user.ethaddress then
        Just ownedrnk

    else
        Nothing


gotMember : Global -> SR.Types.User -> List SR.Types.UserRanking
gotMember sGlobal user  = 
    let
        lmemberRankingIds = user.userjoinrankings
    in
        List.map (gotUserRanking sGlobal) lmemberRankingIds

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
    if ownedrnk.userInfo.ethaddress == user.ethaddress then
        Just ownedrnk

    else
        Nothing


gotUserIsPlayer : List SR.Types.UserRanking -> SR.Types.User -> List SR.Types.UserRanking
gotUserIsPlayer lownedrankings user =
    List.filterMap
        (isUserPlayerInGlobalRankings
            user
        )
        lownedrankings


isUserPlayerInGlobalRankings : SR.Types.User -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isUserPlayerInGlobalRankings user ownedrnk =
    if ownedrnk.userInfo.ethaddress == user.ethaddress then
        Just ownedrnk

    else
        Nothing


gotAllRankindIds : SR.Types.UserRanking -> String
gotAllRankindIds userRanking =
    userRanking.rankingInfo.id



gotUserRanking : Global -> String -> SR.Types.UserRanking 
gotUserRanking sGlobal rnkId = 
    case sGlobal of 
        Global userRankings ->
            let 
                lranking = List.head (EverySet.toList (EverySet.filter (isUserRankingIdInList rnkId) userRankings))
            in
            case lranking of
                Nothing ->
                    SR.Defaults.emptyUserRanking
                Just ranking ->
                    ranking
    



-- just using a default for now
--addUserRanking : Global -> RemoteData.WebData SR.Types.RankingId -> SR.Types.Ranking -> SR.Types.User -> Global
addUserRanking : Global -> String -> SR.Types.Ranking -> SR.Types.User -> Global
addUserRanking sGlobal newrnkId rnkInfo user = 
    let
        newRankingInfo =
            { id =  newrnkId
            , active = True
            , rankingname = rnkInfo.rankingname
            , rankingdesc = rnkInfo.rankingdesc
            , rankingowneraddr = user.ethaddress
            }

        newUserRanking =
            { rankingInfo = newRankingInfo
            , userInfo = user
            }

    in
        case sGlobal of 
            Global rankedUserRankings->
                asGlobal (EverySet.insert newUserRanking rankedUserRankings)


isUserRankingIdInList : String -> SR.Types.UserRanking -> Bool
isUserRankingIdInList rankingid urnk =
    if urnk.rankingInfo.id == rankingid then
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


removeUser : SR.Types.UserRanking -> SR.Types.Ranking
removeUser uranking = 
    uranking.rankingInfo



createdGlobal : List SR.Types.Ranking -> List SR.Types.User -> List SR.Types.UserRanking
createdGlobal lrankinfo luser =
    List.map (createNewOwnedRanking luser) lrankinfo

createNewOwnedRanking : List SR.Types.User -> SR.Types.Ranking -> SR.Types.UserRanking
createNewOwnedRanking luser rankingInfo =
    let
        userOwner =
            Data.Users.gotUserFromUserList luser rankingInfo.rankingowneraddr

        newOwnedRanking =
            { rankingInfo = rankingInfo
            , userInfo = userOwner
            }
    in
    newOwnedRanking


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
newJsonEncodedList : List SR.Types.Ranking -> Json.Encode.Value
newJsonEncodedList lotherrankingInfo =
    let
        --newRankingInfoList =
            --SR.ListOps.extractRankingList lotherrankingInfo

        encodeAglobalRankingObj : SR.Types.Ranking -> Json.Encode.Value
        encodeAglobalRankingObj rankingInfo =
            Json.Encode.object
                [ ( "id", Json.Encode.string rankingInfo.id )
                , ( "active", Json.Encode.bool rankingInfo.active )
                , ( "rankingname", Json.Encode.string rankingInfo.rankingname )
                , ( "rankingdesc", Json.Encode.string rankingInfo.rankingdesc )
                , ( "rankingowneraddr", Json.Encode.string rankingInfo.rankingowneraddr )
                ]

        encodedList =
            Json.Encode.list encodeAglobalRankingObj lotherrankingInfo
    in
    encodedList


gotRankingOwner : SR.Types.Ranking -> List SR.Types.UserRanking -> List SR.Types.UserPlayer -> SR.Types.UserPlayer
gotRankingOwner selectedRanking luranking luplayer =
    let
        rankingOwnerAsUser =
            (gotUserRankingFromUserRankingList luranking (Internal.Types.RankingId selectedRanking.id)).userInfo

        rankingOwnerAsPlayer =
            gotRankingOwnerAsPlayer rankingOwnerAsUser.ethaddress luplayer
    in
    { player = rankingOwnerAsPlayer
    , user = rankingOwnerAsUser
    }

gotUserRankingFromUserRankingList : List SR.Types.UserRanking -> Internal.Types.RankingId -> SR.Types.UserRanking
gotUserRankingFromUserRankingList urankingList (Internal.Types.RankingId rnkid) =
    let
        existingRanking =
            List.head <|
                List.filter (\r -> r.rankingInfo.id == String.toLower rnkid)
                    urankingList
    in
    case existingRanking of
        Nothing ->
            SR.Defaults.emptyUserRanking

        Just a ->
            a

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
    let
        userOwner =
            Data.Users.gotUserFromUserList luser rankingInfo.rankingowneraddr

        newOwnedRanking =
            { rankingInfo = rankingInfo
            , userInfo = userOwner
            }
    in
    newOwnedRanking

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
    if rnk.id == rankingid then
        Just rnk

    else
        Nothing
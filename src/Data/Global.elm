-- Global will be mainly used to handle internal data of the global rankings listing as it relates to the current user
module Data.Global exposing (Global, gotRankingOwnerAsUserPlayer, rankingsAsList, jsonEncodeNewGlobalRankingList, createdUserRankings, gotUserRanking, emptyGlobal, asGlobal, othersUserRanking, memberUserRanking, addUserRanking, removeUserRanking, asList, asSelected, isUserRankingMemberOfGlobalRanking, ownedUserRanking)


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

asSelected : EverySet SR.Types.UserRanking -> Global 
asSelected esUserRanking = 
    Global esUserRanking


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
    

-- these following 3 are not currently distinguishing the rankings:
ownedUserRanking : Global -> SR.Types.User -> Global 
ownedUserRanking global user = 
    asGlobal (
        EverySet.fromList (List.filterMap
        (isOwned
            user
        )
        (asList global)))
        



memberUserRanking : Global -> SR.Types.User -> Global 
memberUserRanking global user = 
     global

othersUserRanking : Global -> SR.Types.User -> Global 
othersUserRanking global user = 
     global

-- just using a default for now
addUserRanking : Global -> RemoteData.WebData SR.Types.RankingId -> SR.Types.RankingInfo -> SR.Types.User -> Global
addUserRanking sGlobal newrnkId rnkInfo user = 
    let
        newRankingInfo =
            { id = gotNewRankingIdFromWebData newrnkId
            , active = True
            , rankingname = rnkInfo.rankingname
            , rankingdesc = rnkInfo.rankingdesc
            , rankingowneraddr = user.ethaddress
            }

        newUserRanking =
            { rankingInfo = newRankingInfo
            , userInfo = user
            }

        -- globalListWithJsonObjAdded =
        --     newOtherRankingInfo :: lrankingInfo
    in
        case sGlobal of 
            Global rankedUserRankings->
                asSelected (EverySet.insert newUserRanking rankedUserRankings)


removeUserRanking : SR.Types.UserRanking -> Global -> Global
removeUserRanking uplayer srank = 
    case srank of 
        Global rankedUserRankings->
         asSelected (EverySet.remove uplayer rankedUserRankings) 


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


-- isUserOwnerOfGlobalUserRanking : SR.Types.RankingInfo -> List SR.Types.UserRanking -> SR.Types.User -> Bool
-- isUserOwnerOfGlobalUserRanking rnkInfo lurnkInfo user =
--     let
--         filteredRec =
--             extractSelectedUserRankingFromGlobalList lurnkInfo rnkInfo.id
            

--         _ =
--             Debug.log "filteredRec" filteredRec

--         -- filteredRec =
--         --     List.head filteredList
--     in
--     case filteredRec of
--         Nothing ->
--             False

--         Just a ->
--             if a.rankingInfo.rankingowneraddr == user.ethaddress then
--                 True

--             else
--                 False

-- extractSelectedUserRankingFromGlobalList : List SR.Types.UserRanking -> String -> Maybe SR.Types.UserRanking
-- extractSelectedUserRankingFromGlobalList luranking rankingid =
--     List.filterMap
--         (isUserRankingIdInList
--             rankingid
--         )
--         luranking
--         |> List.head


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

rankingsAsList : Global -> List SR.Types.RankingInfo
rankingsAsList sGlobal = 
    case sGlobal of 
        Global rankedUserRankings ->
            EverySet.map removeUser rankedUserRankings
            |> EverySet.toList


removeUser : SR.Types.UserRanking -> SR.Types.RankingInfo
--removeUser uranking sGlobal = 
removeUser uranking = 
    uranking.rankingInfo
    -- case sGlobal of 
    --     Global rankedUserRankings ->
    --         EverySet.remove uranking rankedUserRankings





-- extractRankingsFromWebData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
-- extractRankingsFromWebData remData =
--     case remData of
--         RemoteData.NotAsked ->
--             []

--         RemoteData.Loading ->
--             []

--         RemoteData.Success rankings ->
--             rankings

--         RemoteData.Failure httpError ->
--             []

-- ownerValidatedRankingList : List SR.Types.RankingInfo -> List SR.Types.RankingInfo
-- ownerValidatedRankingList lrankinginfo =
--     List.filter isValidOwnerAddress lrankinginfo


-- isValidOwnerAddress : SR.Types.RankingInfo -> Bool
-- isValidOwnerAddress rankInfo =
--     if Eth.Utils.isAddress rankInfo.rankingowneraddr then
--         True

--     else
--         False

-- gotUserOwnedGlobalRankingList : List SR.Types.UserRanking -> SR.Types.User -> List SR.Types.UserRanking
-- gotUserOwnedGlobalRankingList lownedrankings user =
--     List.filterMap
--         (isOwned
--             user
--         )
--         lownedrankings


isOwned : SR.Types.User -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isOwned user ownedrnk =
    if ownedrnk.userInfo.ethaddress == user.ethaddress then
        Just ownedrnk

    else
        Nothing


createdUserRankings : List SR.Types.RankingInfo -> List SR.Types.User -> List SR.Types.UserRanking
createdUserRankings lrankinfo luser =
    List.map (createNewOwnedRanking luser) lrankinfo

createNewOwnedRanking : List SR.Types.User -> SR.Types.RankingInfo -> SR.Types.UserRanking
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
jsonEncodeNewGlobalRankingList : List SR.Types.RankingInfo -> Json.Encode.Value
jsonEncodeNewGlobalRankingList lotherrankingInfo =
    let
        --newRankingInfoList =
            --SR.ListOps.extractRankingList lotherrankingInfo


        encodeAglobalRankingObj : SR.Types.RankingInfo -> Json.Encode.Value
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


gotRankingOwnerAsUserPlayer : SR.Types.RankingInfo -> List SR.Types.UserRanking -> List SR.Types.UserPlayer -> SR.Types.UserPlayer
gotRankingOwnerAsUserPlayer selectedRanking luranking luplayer =
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


-- gotRankingOwnerAsUserPlayer : SR.Types.RankingInfo -> List SR.Types.UserRanking -> List SR.Types.UserPlayer -> SR.Types.UserPlayer
-- gotRankingOwnerAsUserPlayer selectedRanking luranking luplayer =
--     let
--         rankingOwnerAsUser =
--             (gotUserRankingFromUserRankingList luranking (Internal.Types.RankingId selectedRanking.id)).userInfo

--         rankingOwnerAsPlayer =
--             gotRankingOwnerAPlayer rankingOwnerAsUser.ethaddress luplayer
--     in
--     { player = rankingOwnerAsPlayer
--     , user = rankingOwnerAsUser
--     }

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


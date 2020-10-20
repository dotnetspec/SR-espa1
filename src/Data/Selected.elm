-- Selected will be mainly used to handle internal data of the selected ranking listing as it relates to the current user
module Data.Selected exposing (Selected
    , asEverySet
    , gotCurrentUserAsPlayerFromPlayerList
    , gotUserPlayerFromPlayerListStrAddress
    , gotUserAsPlayer
    , gotOpponent
    , empty
    , assignChallengerAddrsForBOTHPlayers
    , updateSelectedRankingOnChallenge
    , jsonEncodeNewSelectedRankingPlayerList
    , gotRankingId
    , handleWon
    , handleLost
    , handleUndecided
    , convertPlayersToUserPlayers
    , extractAndSortPlayerList
    , convertUserPlayersToPlayers
    , isPlayerCurrentUser
    , printChallengerNameOrAvailable
    --, userAdded
    , isChallenged
    , assignChallengerAddr
    , isUserOwnerOfSelectedUserRanking
    , addUserPlayer
    , removeUserPlayer
    , asList
    , asUsers
    , changedRank
    , asSelected
    , isCurrentUserPlayerLowerRanked
    , isUserPlayerMemberOfSelectedRanking
    , created
    , resultView
    , sortedRank
    , gotStatus
    , gotPlayers
    )


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import Data.Users
import Element exposing (Element)
import RemoteData
import Data.AppState
import Data.Players
import Json.Encode
import EverySet
import Eth.Utils
import Eth.Types
import SR.Defaults
import SR.Defaults
import SRdb.Object.User exposing (ethaddress)



-- the relevant Users are picked out with the Create function and for the UserPlayer EverySet (no need for Users assoc data)
type Selected = Selected (EverySet SR.Types.UserPlayer) Internal.Types.RankingId SR.Types.SelectedStatus Data.Players.Players

asSelected : EverySet SR.Types.UserPlayer -> Internal.Types.RankingId -> SR.Types.SelectedStatus -> Data.Players.Players -> Selected 
asSelected esUserPlayer rnkId status sPlayers = 
    Selected esUserPlayer rnkId status sPlayers

asEverySet : Selected -> EverySet SR.Types.UserPlayer
asEverySet (Selected esSelected rnkId status sPlayers)  = 
     esSelected

created : List SR.Types.Player -> Data.Users.Users -> Internal.Types.RankingId -> Selected
created lplayer sUser rnkId =
    let
        --lplayer = Data.Players.extractPlayersFromWebData lplayer
        luser = Data.Users.asList sUser
        esUserPlayers = 
            List.map (createdUserPlayer luser) lplayer
            |> EverySet.fromList
    in
        asSelected esUserPlayers rnkId SR.Types.UserIsOwner (Data.Players.asPlayers (EverySet.fromList lplayer))

gotPlayers : Selected -> Data.Players.Players 
gotPlayers  (Selected sSelected _ _ sPlayers ) = 
    sPlayers

gotStatus : Selected -> SR.Types.SelectedStatus
gotStatus selected = 
    case selected of 
        Selected sSelected _ status _ ->
            status

gotRankingId : Selected -> Internal.Types.RankingId 
gotRankingId selected = 
    case selected of 
        Selected sSelected rnkId status sPlayers ->
            rnkId


empty : Selected
empty = Selected (EverySet.empty) (Internal.Types.RankingId "") SR.Types.UserIsNeitherOwnerNorMember Data.Players.empty


resultView : SR.Types.SelectedStatus -> SR.Types.UIState
resultView  status = 
    case status of
            SR.Types.UserIsOwner -> 
                SR.Types.UISelectedRankingUserIsOwner

            SR.Types.UserIsMember -> 
                SR.Types.UISelectedRankingUserIsPlayer

            SR.Types.UserIsNeitherOwnerNorMember -> 
                SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer
                

createdUserPlayer : List SR.Types.User -> SR.Types.Player -> SR.Types.UserPlayer
createdUserPlayer luser player =
    let
        m_user = Data.Users.gotUserFromUserList luser player.address
    in
        case m_user of
            Nothing ->
                SR.Defaults.emptyUserPlayer
            Just user ->
                let
                      newUserPlayer =
                        { player = player
                        , user = user
                        }
                in
                    newUserPlayer

    

gotUserAsPlayer : Selected -> Eth.Types.Address -> Maybe SR.Types.UserPlayer 
gotUserAsPlayer sSelected uaddr = 
    --todo: fix
    --SR.Defaults.emptyUserPlayer
    case sSelected of 
        Selected esUserPlayer rnkId status sPlayers -> 
            gotUserPlayerFromPlayerListStrAddress (EverySet.toList esUserPlayer) (Eth.Utils.addressToString uaddr)

addUserPlayer : SR.Types.UserPlayer -> Selected -> Selected
addUserPlayer uplayer sSelected = 
    case sSelected of 
        Selected rankedUserPlayers rnkId status sPlayers ->
                asSelected (EverySet.insert (addNewUserPlayerJoinRanking uplayer rnkId)  rankedUserPlayers) rnkId status sPlayers


addNewUserPlayerJoinRanking : SR.Types.UserPlayer -> Internal.Types.RankingId -> SR.Types.UserPlayer
addNewUserPlayerJoinRanking uplayer rnkId = 
    let 
        newUser = uplayer.user 
        updatedUserJoinRankings = {newUser | userjoinrankings = Utils.MyUtils.stringFromRankingId rnkId :: uplayer.user.userjoinrankings}
        newUserPlayer =  { uplayer | player = uplayer.player, user = updatedUserJoinRankings }
        _ = Debug.log "newUserPlayer" newUserPlayer
    in 
        newUserPlayer

removeUserPlayer : SR.Types.UserPlayer -> Selected -> Selected
removeUserPlayer uplayer sSelected = 
    --(Data.Players.asPlayers (EverySet.fromList lplayer))
        asSelected (EverySet.remove uplayer (asEverySet sSelected)) (gotRankingId sSelected) SR.Types.UserIsOwner Data.Players.empty

changedRank : SR.Types.UserPlayer -> Int -> Selected -> Selected
changedRank uplayer newRank sSelected = 
    let 
        newPlayer = uplayer.player
        updatedPlayer = { newPlayer | rank = newRank, challengeraddress = ""}
        updatedUserPlayer = { uplayer | player = updatedPlayer}
    in   
        removeUserPlayer uplayer sSelected
        |> addUserPlayer updatedUserPlayer


isCurrentUserPlayerLowerRanked : SR.Types.UserPlayer -> SR.Types.UserPlayer -> Bool 
isCurrentUserPlayerLowerRanked uplayer challenger = 
    --n.b. for ranks lower int is higher rank!
    if uplayer.player.rank > challenger.player.rank then
        True 
        else False

isUserPlayerMemberOfSelectedRanking : List SR.Types.UserPlayer -> SR.Types.User -> Bool
isUserPlayerMemberOfSelectedRanking luplayer user =
    let
        filteredList =
            findPlayerInList user luplayer

        filteredRec =
            List.head filteredList
    in
    case filteredRec of
        Nothing ->
            False

        Just a ->
            case user.m_ethaddress of 
                Nothing ->
                    False 
                Just addr ->
                    if (String.toLower a.player.address) == (String.toLower (Eth.Utils.addressToString addr)) then
                        True

                    else
                        False


isUserOwnerOfSelectedUserRanking : SR.Types.Ranking -> List SR.Types.UserRanking -> SR.Types.User -> Bool
isUserOwnerOfSelectedUserRanking rnkInfo lurnkInfo user =
    let
        filteredRec =
            extractSelectedUserRankingFromGlobalList lurnkInfo rnkInfo.id_
    in
    case filteredRec of
        Nothing ->
            False

        Just a ->
            case user.m_ethaddress of 
                Nothing ->
                    False 
                Just addr ->
                    if a.rankingInfo.rankingowneraddr == (Eth.Utils.addressToString addr) then
                        True

                    else
                        False

extractSelectedUserRankingFromGlobalList : List SR.Types.UserRanking -> String -> Maybe SR.Types.UserRanking
extractSelectedUserRankingFromGlobalList luranking rankingid =
    List.filterMap
        (isUserRankingIdInList
            rankingid
        )
        luranking
        |> List.head

isUserRankingIdInList : String -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isUserRankingIdInList rankingid urnk =
    if urnk.rankingInfo.id_ == rankingid then
        Just urnk

    else
        Nothing

findPlayerInList : SR.Types.User -> List SR.Types.UserPlayer -> List SR.Types.UserPlayer
findPlayerInList user luPlayer =
    -- todo: fix
    --[SR.Defaults.emptyUserPlayer]
    case user.m_ethaddress of 
        Nothing ->
            luPlayer 
        Just ethaddress ->
            List.filterMap
                (isThisPlayerAddr
                    (String.toLower (Eth.Utils.addressToString ethaddress))
                )
                luPlayer

isThisPlayerAddr : String -> SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
isThisPlayerAddr playerAddr uplayer =
    if (String.toLower uplayer.player.address) == (String.toLower playerAddr) then
        Just uplayer

    else
        Nothing

asList : Selected -> List SR.Types.UserPlayer 
asList srank = 
    case srank of 
        Selected rankedUserPlayers rnkId status sPlayers ->
            rankedUserPlayers
           |> EverySet.toList
           |> List.sortBy extractRank

extractRank : SR.Types.UserPlayer -> Int
extractRank uplayer =
    uplayer.player.rank

assignChallengerAddr : Selected -> SR.Types.UserPlayer -> String -> Selected
assignChallengerAddr sSelected uplayer challengeraddress =
    let 
        newSSelected = removeUserPlayer uplayer sSelected
        newPlayer = uplayer.player
        updatedPlayer = {newPlayer | challengeraddress = challengeraddress}
        newUPlayer = {uplayer | player = updatedPlayer}
    in
        addUserPlayer newUPlayer newSSelected

isChallenged : Selected -> Data.Users.Users -> SR.Types.UserPlayer -> Bool
isChallenged (Selected sSelected rnkId status sPlayers ) sUsers uplayer = 
    let
        m_challenger = Data.Users.gotUser sUsers uplayer.player.challengeraddress
    in
        case m_challenger of 
            Nothing ->
                False 
            Just challenger ->
                if challenger.username /= "" then
                    True

                else
                    False

--todo: should this be here or just in User?
-- userAdded : String -> List SR.Types.UserPlayer -> SR.Types.User -> List SR.Types.UserPlayer
-- userAdded strrankingId luPlayer userRec =
--         case userRec.m_ethaddress of
--             Nothing ->
--                 --there should be a player addr
--                 luPlayer
--             Just ethaddress ->
--                 let
--                     newUserPlayer =
--                         { player =
--                             { rankingid =  strrankingId
--                             , address = Eth.Utils.addressToString ethaddress
--                             , rank = List.length luPlayer + 1
--                             , challengeraddress = ""
--                             }
--                         , user = userRec
--                         }
--                 in
--                     Utils.MyUtils.stringToRankingId strrankingId 
--                     |> asSelected (EverySet.fromList luPlayer) 
--                     |> addUserPlayer newUserPlayer
--                     |> asList
    
isPlayerCurrentUser : SR.Types.User -> SR.Types.UserPlayer -> Bool
isPlayerCurrentUser user uplayer = 
    case user.m_ethaddress of 
        Nothing ->
            False

        Just addr ->
            if (String.toLower uplayer.player.address) == (String.toLower (Eth.Utils.addressToString addr)) then
                True

            else
                False

printChallengerNameOrAvailable : Selected -> Data.Users.Users -> SR.Types.UserPlayer -> String 
printChallengerNameOrAvailable sSelected sUsers uplayer = 
    let
        m_opponent =  (gotOpponent sSelected uplayer)
    in
        case m_opponent of
            Nothing ->
                "Available"
            Just opponent ->
                if isChallenged sSelected sUsers uplayer then
                    opponent.user.username
                else
                    "Available"

gotOpponent : Selected -> SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
gotOpponent sSelected uplayer = 
    gotUserPlayerFromPlayerListStrAddress (asList sSelected) uplayer.player.challengeraddress


updatePlayerRankWithWonResult : List SR.Types.UserPlayer -> SR.Types.UserPlayer -> List SR.Types.UserPlayer
updatePlayerRankWithWonResult luPlayer uplayer =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList uplayer.player.address luPlayer

        m_opponentAsPlayer =
            gotUserPlayerFromPlayerListStrAddress luPlayer uplayer.player.challengeraddress
    in
        case m_opponentAsPlayer of
            Nothing ->
                luPlayer 
            Just opponentAsPlayer ->
                let
                    -- this needs more ?:
                    newUserPlayerPlayerField =
                        uplayer.player

                    updatedPlayer =
                        { newUserPlayerPlayerField | rank = opponentAsPlayer.player.rank }

                    newUserPlayer =
                        { uplayer | player = updatedPlayer }

                    newPlayerList =
                        newUserPlayer :: filteredPlayerList
                in
                    newPlayerList


sortedRank : List SR.Types.UserPlayer -> List SR.Types.UserPlayer
sortedRank luplayer =
    let
        validatedMaybePlayerLst =
            List.map Utils.MyUtils.splitPlayerFieldsToCreateMaybePlayer luplayer

        filteredValidatedMaybePlayerLst =
            List.filter canPlayerBeInList validatedMaybePlayerLst

        convertedValidatedPlayerList =
            List.map Utils.MyUtils.convertMaybePlayerToPlayer filteredValidatedMaybePlayerLst

        reorderedConvertedValidatedPlayerList =
            reorderPlayerListToStartAtOne
                convertedValidatedPlayerList
    in
    List.sortBy extractRank reorderedConvertedValidatedPlayerList



reorderPlayerListToStartAtOne : List SR.Types.UserPlayer -> List SR.Types.UserPlayer
reorderPlayerListToStartAtOne luplayer =
    let
        newPlayerListAllRankIsOne =
            List.map resetPlayerRankToOne luplayer

        newListLength =
            List.length luplayer

        newAscendingList =
            List.range 1 newListLength

        listscombined =
            List.map2 resetPlayerRankingList newAscendingList newPlayerListAllRankIsOne
    in
    listscombined


resetPlayerRankingList : Int -> SR.Types.UserPlayer -> SR.Types.UserPlayer
resetPlayerRankingList newRank uplayer =
    let
        newuserplayerplayer =
            uplayer.player

        newPlayer =
            { newuserplayerplayer
                | address = uplayer.player.address
                , rank = newRank
                , challengeraddress = uplayer.player.challengeraddress
            }

        newUserPlayer =
            { uplayer | player = newPlayer }
    in
    newUserPlayer


resetPlayerRankToOne : SR.Types.UserPlayer -> SR.Types.UserPlayer
resetPlayerRankToOne uplayer =
    let
        newuserplayerplayer =
            uplayer.player

        newPlayer =
            { newuserplayerplayer
                | address = uplayer.player.address
                , rank = 1
                , challengeraddress = uplayer.player.challengeraddress
            }

        newUserPlayer =
            { uplayer | player = newPlayer }
    in
    newUserPlayer


canPlayerBeInList : Maybe SR.Types.UserPlayer -> Bool
canPlayerBeInList uplayer =
    case uplayer of
        Nothing ->
            False

        Just a ->
            True


gotCurrentUserAsPlayerFromPlayerList : List SR.Types.UserPlayer -> SR.Types.User -> Maybe SR.Types.UserPlayer
gotCurrentUserAsPlayerFromPlayerList luPlayer userRec =
    --todo: fix
    --SR.Defaults.emptyUserPlayer
    case userRec.m_ethaddress of
        Nothing ->
            Nothing

        Just ethaddress ->
            let
                existingPlayer =
                    List.head <|
                        List.filter (\r -> r.player.address == (String.toLower <| (Eth.Utils.addressToString ethaddress)))
                            luPlayer
            in
                existingPlayer




gotRankingOwnerAsPlayer : String -> List SR.Types.UserPlayer -> SR.Types.Player
gotRankingOwnerAsPlayer selectedRanking luplayer =
    let
        m_userPlayer = (gotUserPlayerFromPlayerListStrAddress luplayer selectedRanking)
    in
        case m_userPlayer of
            Nothing ->
                SR.Types.Player "" "" 0 ""
            Just userPlayer ->
                userPlayer.player

gotUserPlayerFromPlayerListStrAddress : List SR.Types.UserPlayer -> String -> Maybe SR.Types.UserPlayer
gotUserPlayerFromPlayerListStrAddress luplayer addr =
    let
        existingUser =
            List.head <|
                List.filter (\r -> r.player.address == (String.toLower <| addr))
                    luplayer
    in
        existingUser
    -- case existingUser of
    --     Nothing ->
    --         SR.Defaults.emptyUserPlayer

    --     Just a ->
    --         a

filterPlayerOutOfPlayerList : String -> List SR.Types.UserPlayer -> List SR.Types.UserPlayer
filterPlayerOutOfPlayerList addr lplayer =
    List.filterMap
        (doesPlayerAddrNOTMatchAddr
            addr
        )
        lplayer

doesPlayerAddrNOTMatchAddr : String -> SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
doesPlayerAddrNOTMatchAddr addr player =
    if player.player.address /= addr then
        Just player

    else
        Nothing


-- extractPlayersFromWebData : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.Player
-- extractPlayersFromWebData remData =
--     case remData of
--         RemoteData.NotAsked ->
--             []

--         RemoteData.Loading ->
--             []

--         RemoteData.Success players ->
--             players

--         RemoteData.Failure httpError ->
--             []

convertUserPlayersToPlayers : List SR.Types.UserPlayer -> List SR.Types.Player
convertUserPlayersToPlayers luplayers =
    List.map Utils.MyUtils.refEachPlayer luplayers

asUsers : Selected -> Data.Users.Users 
asUsers sSelected =
    (EverySet.fromList (List.map Utils.MyUtils.refEachUser (asList sSelected)))
    |> Data.Users.asUsers



convertPlayersToUserPlayers : List SR.Types.Player -> List SR.Types.User -> List SR.Types.UserPlayer
convertPlayersToUserPlayers lplayer luser =
    List.map (convertEachPlayerToUserPlayer luser) lplayer


convertEachPlayerToUserPlayer : List SR.Types.User -> SR.Types.Player -> SR.Types.UserPlayer
convertEachPlayerToUserPlayer luser player =
    let
        m_user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) player.address
    in
        case m_user of 
            Nothing ->
                { player = player, user = (SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing) }
            Just userVal ->
                { player = player, user = userVal }
                
    --{ player = player, user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) player.address }


handleWon : Selected -> SR.Types.AppInfo -> Data.Users.Users -> (Selected, SR.Types.AppInfo)
handleWon (Selected esUPlayer rnkId status sPlayers ) appInfo sUsers =
            let
                whoHigher =
                    isOpponentHigherRank appInfo.player appInfo.challenger
            in
            case whoHigher of
                SR.Types.OpponentRankHigher ->
                    
                    let
                        
                        supdatedPlayer =  
                            changedRank appInfo.player appInfo.challenger.player.rank (asSelected esUPlayer rnkId status sPlayers)
                           

                        supdatedPlayerAndChallenger =
                            supdatedPlayer
                            |> changedRank appInfo.challenger (appInfo.challenger.player.rank + 1)
                            
                        -- handling with AppState then handing back to AppInfo for now ...
                        
                            
                        updatedUserPlayer = Data.AppState.releasePlayerForUI (Data.AppState.updateAppState appInfo.m_user 
                            appInfo.player appInfo.challenger (Utils.MyUtils.stringToRankingId appInfo.selectedRanking.id_))

                        newAppInfo =
                           
                            { appInfo | player = updatedUserPlayer, challenger = SR.Defaults.emptyUserPlayer }
           
                    in
                    --nb. higher rank is a lower number and vice versa!
                        (supdatedPlayerAndChallenger, newAppInfo)
                        

                SR.Types.OpponentRankLower ->

                    let
                        --no ranking change - just update the player list for both players challenger to emptyPlayer, no rank change
                        supdatedPlayer =
                            changedRank appInfo.player appInfo.player.player.rank (asSelected esUPlayer rnkId status sPlayers)

                        supdatedPlayerAndChallenger =
                            changedRank  appInfo.challenger appInfo.challenger.player.rank  supdatedPlayer

                        --update current player now
                        newUserPlayerPlayer =
                            appInfo.player.player

                        newUserPlayerPlayerUpdated =
                            { newUserPlayerPlayer | address = "" }

                        newAppInfoPlayer =
                            appInfo.player

                        newAppInfoUserPlayer =
                            { newAppInfoPlayer | player = newUserPlayerPlayerUpdated }

                        newAppInfo =
                            { appInfo | player = newAppInfoUserPlayer, challenger = SR.Defaults.emptyUserPlayer }

                    in
                    --nb. higher rank is a lower number and vice versa!
                        (supdatedPlayerAndChallenger, newAppInfo)


handleLost : Selected -> SR.Types.AppInfo -> (Selected, SR.Types.AppInfo)
handleLost (Selected esUPlayer rnkId status sPlayers ) appInfo =
    let
        whoHigher =
            isOpponentHigherRank appInfo.player appInfo.challenger
    in
    case whoHigher of
        SR.Types.OpponentRankHigher ->
            let
                supdatedPlayer =
                    changedRank appInfo.player appInfo.player.player.rank (asSelected esUPlayer rnkId status sPlayers)

                supdatedPlayerAndChallenger =
                    changedRank  appInfo.challenger appInfo.challenger.player.rank  supdatedPlayer

                --update current player now
                newUserPlayer =
                    appInfo.player

                newUserPlayerPlayer =
                    appInfo.player.player

                newPlayerUpdated =
                    { newUserPlayerPlayer | address = "" }

                newUserPlayerUpdated =
                    { newUserPlayer | player = newPlayerUpdated }

                newAppInfo =
                    { appInfo | player = newUserPlayerUpdated, challenger = SR.Defaults.emptyUserPlayer }

            in
            --nb. higher rank is a lower number and vice versa!
            (supdatedPlayerAndChallenger, newAppInfo)
            

        SR.Types.OpponentRankLower ->
            --nb. higher rank is a lower number and vice versa!
            let
                supdatedPlayer =  
                    changedRank appInfo.player appInfo.challenger.player.rank (asSelected esUPlayer rnkId status sPlayers)
                    

                supdatedPlayerAndChallenger =
                    supdatedPlayer
                    |> changedRank appInfo.challenger (appInfo.challenger.player.rank + 1)

                --update current player now
                newUserPlayer =
                    appInfo.player

                newUserPlayerPlayer =
                    appInfo.player.player

                newUserPlayerPlayerUpdated =
                    { newUserPlayerPlayer | address = "" }

                newUserPlayerUpdated =
                    { newUserPlayer | player = newUserPlayerPlayerUpdated }

                newAppInfo =
                    { appInfo | player = newUserPlayerUpdated, challenger = SR.Defaults.emptyUserPlayer }

            in
            (supdatedPlayerAndChallenger, newAppInfo)


handleUndecided : Selected -> SR.Types.AppInfo -> (Selected, SR.Types.AppInfo)
handleUndecided (Selected esUPlayer rnkId status sPlayers ) appInfo =
            let
                supdatedPlayer =  
                            changedRank appInfo.player appInfo.player.player.rank (asSelected esUPlayer rnkId status sPlayers)
                           
                supdatedPlayerAndChallenger =
                    supdatedPlayer
                    |> changedRank appInfo.challenger (appInfo.challenger.player.rank)


                --update current player now
                newUserPlayer =
                    appInfo.player

                newUserPlayerPlayer =
                    appInfo.player.player

                newPlayerPlayerUpdated =
                    { newUserPlayerPlayer | address = "" }

                newUserPlayerUpdated =
                    { newUserPlayer | player = newPlayerPlayerUpdated }

                newAppInfo =
                    { appInfo | player = newUserPlayerUpdated, challenger = SR.Defaults.emptyUserPlayer }

            in
            (supdatedPlayerAndChallenger, newAppInfo)


isOpponentHigherRank : SR.Types.UserPlayer -> SR.Types.Opponent -> SR.Types.OpponentRelativeRank
isOpponentHigherRank uplayer opponent =
    -- nb. if player rank is 'higher' than opponent his rank integer will actually be 'less than' opponent
    -- we go by the integer ...
    if uplayer.player.rank > opponent.player.rank then
        SR.Types.OpponentRankHigher

    else
        SR.Types.OpponentRankLower

jsonEncodeNewSelectedRankingPlayerList : List SR.Types.UserPlayer -> Json.Encode.Value
jsonEncodeNewSelectedRankingPlayerList luplayers =
    let
        lplayers =
            convertUserPlayersToPlayers luplayers

        encodePlayerObj : SR.Types.Player -> Json.Encode.Value
        encodePlayerObj player =
            Json.Encode.object
                [ ( "address", Json.Encode.string (String.toLower player.address |> Debug.log "player.address: ") )
                , ( "rank", Json.Encode.int player.rank )
                , ( "challengeraddress", Json.Encode.string (player.challengeraddress |> Debug.log "challenger.address: "))
                ]

        encodedList =
            Json.Encode.list encodePlayerObj lplayers
    in
    encodedList

updateSelectedRankingOnChallenge : Selected -> SR.Types.AppInfo -> Selected
updateSelectedRankingOnChallenge allSets appInfo =
    allSets

assignChallengerAddrsForBOTHPlayers : Selected -> SR.Types.AppInfo -> Selected
assignChallengerAddrsForBOTHPlayers sSelected appInfo =
    case sSelected of 
        Selected sselected rnkId status sPlayers  ->
            let
                sUserUpdated = assignChallengerAddr (asSelected sselected rnkId status sPlayers) appInfo.player appInfo.challenger.player.address
                sChallengerUpdated = assignChallengerAddr sUserUpdated appInfo.challenger appInfo.player.player.address --sUsers rnkId
            in 
                sChallengerUpdated

extractAndSortPlayerList : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.User -> List SR.Types.UserPlayer
extractAndSortPlayerList rdlPlayer luser =
    let
        _ = Debug.log "luser in extractAndSortPlayerList" luser

        lplayer =
            Data.Players.extractPlayersFromWebData rdlPlayer

        convertedPlayerListToUserPlayerList =
            convertPlayersToUserPlayers
                lplayer
                luser
    in
    sortedRank <| convertedPlayerListToUserPlayerList


--useful?:
-- convertUserPlayersToPlayers : List SR.Types.UserPlayer -> List SR.Types.Player
-- convertUserPlayersToPlayers luplayers =
--     List.map Utils.MyUtils.refEachPlayer luplayers

--useful?
-- convertPlayersToUserPlayers : List SR.Types.Player -> List SR.Types.User -> List SR.Types.UserPlayer
-- convertPlayersToUserPlayers lplayer luser =
--     List.map (convertEachPlayerToUserPlayer luser) lplayer


-- convertEachPlayerToUserPlayer : List SR.Types.User -> SR.Types.Player -> SR.Types.UserPlayer
-- convertEachPlayerToUserPlayer luser player =
--     { player = player, user = gotUserFromUserList luser player.address }


-- gotRankingOwner : SR.Types.Ranking -> List SR.Types.UserRanking -> List SR.Types.UserPlayer -> SR.Types.UserPlayer
-- gotRankingOwner selectedRanking luranking luplayer =
--     let
--         rankingOwnerAsUser =
--             (gotUserRankingFromUserRankingList luranking (Internal.Types.RankingId selectedRanking.id)).userInfo

--         rankingOwnerAsPlayer =
--             gotRankingOwnerAPlayer rankingOwnerAsUser.m_ethaddress luplayer
--     in
--     { player = rankingOwnerAsPlayer
--     , user = rankingOwnerAsUser
--     }


-- gotRankingOwnerAPlayer : String -> List SR.Types.UserPlayer -> SR.Types.Player
-- gotRankingOwnerAPlayer selectedRanking luplayer =
--     (gotUserPlayerFromPlayerListStrAddress luplayer selectedRanking).player



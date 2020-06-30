-- Selected will be mainly used to handle internal data of the selected ranking listing as it relates to the current user
module Data.Selected exposing (Selected, assignChallengerAddrsForBOTHPlayers, updateSelectedRankingOnChallenge, jsonEncodeNewSelectedRankingPlayerList, getRankingId, handleWon, handleLost, handleUndecided, convertPlayersToUserPlayers, extractPlayersFromWebData, extractAndSortPlayerList, gotCurrentUserAsPlayerFromPlayerList, gotUserPlayerFromPlayerListStrAddress, convertUserPlayersToPlayers, isPlayerCurrentUser, printChallengerNameOrAvailable, userAdded, isChallenged, assignChallengerAddr, isUserOwnerOfSelectedUserRanking, addUserPlayer, removeUserPlayer, asList, changedRank, asSelected, isCurrentUserPlayerLowerRanked, isUserPlayerMemberOfSelectedRanking)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import Data.Users
import Element exposing (Element)
import SR.Defaults
import RemoteData
import Data.AppState
import Json.Encode


type Selected = Selected (EverySet SR.Types.UserPlayer) Data.Users.Users Internal.Types.RankingId

asSelected : EverySet SR.Types.UserPlayer -> Data.Users.Users -> Internal.Types.RankingId -> Selected 
asSelected esUserPlayer susers rnkId = 
    Selected esUserPlayer susers rnkId

getRankingId : Selected -> Internal.Types.RankingId 
getRankingId selected = 
    case selected of 
        Selected sSelected sUsers rnkId ->
            rnkId


addUserPlayer : SR.Types.UserPlayer -> Selected -> Selected
addUserPlayer uplayer srank = 
    case srank of 
        Selected rankedUserPlayers susers rnkId ->
                rnkId 
                |> asSelected (EverySet.insert (addNewUserPlayerJoinRanking uplayer rnkId) rankedUserPlayers)  susers 


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
removeUserPlayer uplayer srank = 
    case srank of 
        Selected rankedUserPlayers susers rnkId->
           rnkId 
           |> asSelected (EverySet.remove uplayer rankedUserPlayers) susers

changedRank : SR.Types.UserPlayer -> Int -> Selected -> Selected
changedRank uplayer newRank srank = 
    let 
        newPlayer = uplayer.player
        updatedPlayer = { newPlayer | rank = newRank, challengeraddress = ""}
        updatedUserPlayer = { uplayer | player = updatedPlayer}
    in
    
    case srank of 
        Selected rankedUserPlayers susers rnkId ->
            rnkId
            |> asSelected rankedUserPlayers susers
            |> removeUserPlayer uplayer  
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
            if (String.toLower a.player.address) == (String.toLower user.ethaddress) then
                True

            else
                False


isUserOwnerOfSelectedUserRanking : SR.Types.RankingInfo -> List SR.Types.UserRanking -> SR.Types.User -> Bool
isUserOwnerOfSelectedUserRanking rnkInfo lurnkInfo user =
    let
        filteredRec =
            extractSelectedUserRankingFromGlobalList lurnkInfo rnkInfo.id
    in
    case filteredRec of
        Nothing ->
            False

        Just a ->
            if a.rankingInfo.rankingowneraddr == user.ethaddress then
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
    if urnk.rankingInfo.id == rankingid then
        Just urnk

    else
        Nothing

findPlayerInList : SR.Types.User -> List SR.Types.UserPlayer -> List SR.Types.UserPlayer
findPlayerInList user luPlayer =
    List.filterMap
        (isThisPlayerAddr
            (String.toLower user.ethaddress)
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
        Selected rankedUserPlayers susers rnkId ->
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

isChallenged : Selected -> SR.Types.UserPlayer -> Bool
isChallenged (Selected sSelected sUsers rnkId) uplayer = 
    let
        challenger = Data.Users.gotUser sUsers uplayer.player.challengeraddress
    in
    
                    if challenger.username /= "" then
                        True

                    else
                        False


userAdded : Data.Users.Users -> String -> List SR.Types.UserPlayer -> SR.Types.User -> List SR.Types.UserPlayer
userAdded sUsers strrankingId luPlayer userRec =
    let
        newUserPlayer =
            { player =
                { address = userRec.ethaddress
                , rank = List.length luPlayer + 1
                , challengeraddress = ""
                }
            , user = userRec
            }
    in
            Utils.MyUtils.stringToRankingId strrankingId 
            |> asSelected (EverySet.fromList luPlayer) sUsers
            |> addUserPlayer newUserPlayer
            |> asList
    
isPlayerCurrentUser : SR.Types.User -> SR.Types.UserPlayer -> Bool
isPlayerCurrentUser user uplayer = 
     if (String.toLower uplayer.player.address) == (String.toLower user.ethaddress) then
                                True

                            else
                                False

printChallengerNameOrAvailable : Selected -> SR.Types.UserPlayer -> String 
printChallengerNameOrAvailable sSelected challengerAsUPlayer = 
    if isChallenged sSelected challengerAsUPlayer then
                                challengerAsUPlayer.user.username

                            else
                                "Available"

updatePlayerRankWithWonResult : List SR.Types.UserPlayer -> SR.Types.UserPlayer -> List SR.Types.UserPlayer
updatePlayerRankWithWonResult luPlayer uplayer =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList uplayer.player.address luPlayer

        opponentAsPlayer =
            gotUserPlayerFromPlayerListStrAddress luPlayer uplayer.player.challengeraddress

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


gotCurrentUserAsPlayerFromPlayerList : List SR.Types.UserPlayer -> SR.Types.User -> SR.Types.UserPlayer
gotCurrentUserAsPlayerFromPlayerList luPlayer userRec =
    let
        existingPlayer =
            List.head <|
                List.filter (\r -> r.player.address == (String.toLower <| userRec.ethaddress))
                    luPlayer
    in
    case existingPlayer of
        Nothing ->
            SR.Defaults.emptyUserPlayer

        Just a ->
            a

extractAndSortPlayerList : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.User -> List SR.Types.UserPlayer
extractAndSortPlayerList rdlPlayer luser =
    let
        lplayer =
            extractPlayersFromWebData rdlPlayer

        convertedPlayerListToUserPlayerList =
            convertPlayersToUserPlayers
                lplayer
                luser
    in
    sortedRank <| convertedPlayerListToUserPlayerList


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


extractPlayersFromWebData : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.Player
extractPlayersFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success players ->
            players

        RemoteData.Failure httpError ->
            []

convertUserPlayersToPlayers : List SR.Types.UserPlayer -> List SR.Types.Player
convertUserPlayersToPlayers luplayers =
    List.map Utils.MyUtils.refEachPlayer luplayers



convertPlayersToUserPlayers : List SR.Types.Player -> List SR.Types.User -> List SR.Types.UserPlayer
convertPlayersToUserPlayers lplayer luser =
    List.map (convertEachPlayerToUserPlayer luser) lplayer


convertEachPlayerToUserPlayer : List SR.Types.User -> SR.Types.Player -> SR.Types.UserPlayer
convertEachPlayerToUserPlayer luser player =
    { player = player, user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) player.address }


handleWon : Selected -> SR.Types.AppInfo -> (Selected, SR.Types.AppInfo)
handleWon (Selected esUPlayer sUsers rnkId) appInfo =
            let
                whoHigher =
                    isOpponentHigherRank appInfo.player appInfo.challenger
            in
            case whoHigher of
                SR.Types.OpponentRankHigher ->
                    let
                        
                        supdatedPlayer =  
                            changedRank appInfo.player appInfo.challenger.player.rank (asSelected esUPlayer sUsers rnkId)
                           

                        supdatedPlayerAndChallenger =
                            supdatedPlayer
                            |> changedRank appInfo.challenger (appInfo.challenger.player.rank + 1)
                            
                        -- handling with AppState then handing back to AppInfo for now ...
                        updatedUserPlayer = Data.AppState.releasePlayerForUI (Data.AppState.updateAppState appInfo.user 
                            appInfo.player appInfo.challenger (Utils.MyUtils.stringToRankingId appInfo.selectedRanking.id))

                        newAppInfo =
                           
                            { appInfo | player = updatedUserPlayer, challenger = SR.Defaults.emptyUserPlayer }
           
                    in
                    --nb. higher rank is a lower number and vice versa!
                        (supdatedPlayerAndChallenger, newAppInfo)
                        

                SR.Types.OpponentRankLower ->

                    let
                        --no ranking change - just update the player list for both players challenger to emptyPlayer, no rank change
                        supdatedPlayer =
                            changedRank appInfo.player appInfo.player.player.rank (asSelected esUPlayer sUsers rnkId)

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
handleLost (Selected esUPlayer sUsers rnkId) appInfo =
            let
                whoHigher =
                    isOpponentHigherRank appInfo.player appInfo.challenger
            in
            case whoHigher of
                SR.Types.OpponentRankHigher ->
                    let

                        supdatedPlayer =
                            changedRank appInfo.player appInfo.player.player.rank (asSelected esUPlayer sUsers rnkId)

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
                            changedRank appInfo.player appInfo.challenger.player.rank (asSelected esUPlayer sUsers rnkId)
                           

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
handleUndecided (Selected esUPlayer sUsers rnkId) appInfo =
            let
                supdatedPlayer =  
                            changedRank appInfo.player appInfo.player.player.rank (asSelected esUPlayer sUsers rnkId)
                           
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
                    Selected sselected sUsers rnkId ->
                        let
                            sUserUpdated = assignChallengerAddr (asSelected sselected sUsers rnkId) appInfo.player appInfo.challenger.player.address
                            sChallengerUpdated = assignChallengerAddr sUserUpdated appInfo.challenger appInfo.player.player.address --sUsers rnkId
                        in 
                            sChallengerUpdated

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


-- gotRankingOwnerAPlayer : String -> List SR.Types.UserPlayer -> SR.Types.Player
-- gotRankingOwnerAPlayer selectedRanking luplayer =
--     (gotUserPlayerFromPlayerListStrAddress luplayer selectedRanking).player

-- Selected will be mainly used to handle internal data of the selected ranking listing as it relates to the current user
module Data.Selected exposing (Selected
    , UserPlayer
    , ResultOfMatch(..)
    , PlayerStatus(..)
    --, SelectedState(..)
    , releasePlayerForUI
    , releaseChallengerForUI
    , asEverySet
    , gotCurrentUserAsPlayerFromPlayerList
    , gotUP
    , gotUserPlayerByUserId
    , empty
    , emptyUserPlayer
    --, assignChallengerUID
    , assignedChallengerUIDForBOTHPlayers
    --, updateSelectedRankingOnChallenge
    , gotRankingId
    , handleWon
    , handleLost
    , handleUndecided
    --, convertPlayersToUserPlayers
    --, extractAndSortPlayerList
    , convertUserPlayersToPlayers
    , isRegisteredPlayerCurrentUser
    , challorAvail
    , isChallenged
    , addUserPlayer
    , removeUserPlayer
    , asList
    , asUsers
    , changedRank
    , isCurrentUserPlayerLowerRanked
    , created
    , sortedRank
    , gotStatus
    , gotPlayers
    , gotPlayer
    , gotOwnerAsUP
    --, updatedUPinSet
    , isMember
    , gotRanking
    , gotUserName
    )

import EverySet exposing (EverySet)
import Internal.Types
import Data.Users
import Element exposing (Element)
import RemoteData
import Data.Players
import Json.Encode
import EverySet
import Eth.Utils
import Eth.Types
import Data.Rankings



-- the relevant Users are picked out with the Create function and for the UserPlayer EverySet (no need for Users assoc data)
-- nb. in main.elm 'Selected' is a DataKind
type Selected = 
    -- todo: we may be able to extract the rankingname (String) differently,
    -- but for now:
    Selected (EverySet UserPlayer) UserPlayer Data.Rankings.Ranking

type alias UserPlayer =
    { player : Data.Players.Player
    , user : Data.Users.User
    , status : PlayerStatus
    }

type ResultOfMatch =
    NoResult
    | Won UserPlayer Opponent
    | Lost UserPlayer Opponent
    | Undecided UserPlayer Opponent

type alias Opponent =
    UserPlayer


type PlayerStatus
 = Owner
 | Member
 | Other

type Options
    = MatchChallenge
    | Result


type OpponentRelativeRank
    = OpponentRankHigher
    | OpponentRankLower


-- asSelected : EverySet UserPlayer -> Internal.Types.RankingId -> PlayerStatus -> Data.Players.Players -> SelectedState -> String -> Selected 
-- asSelected esUserPlayer uP ranking = 
--     Selected esUserPlayer uP ranking

asEverySet : Selected -> EverySet UserPlayer
asEverySet (Selected esSelected uP ranking)  = 
     esSelected


created : List Data.Players.Player -> Data.Users.Users -> Data.Users.User -> Data.Rankings.Ranking -> PlayerStatus -> Selected
created lplayer sUser user ranking playerStatus =
    let
        luser = Data.Users.asList sUser
        esUserPlayers = 
            List.map (createdUserPlayer luser) lplayer
            |> EverySet.fromList
    in
        Selected esUserPlayers {player = Data.Players.emptyIndividualPlayer, user = user, status = playerStatus } ranking

gotUserName : Selected -> String 
gotUserName (Selected _ uP _) = 
    case uP.user of 
        Data.Users.Spectator userInfo _ ->
            userInfo.username
        Data.Users.Registered userInfo _ ->
            userInfo.username



gotRanking : Selected -> Data.Rankings.Ranking 
gotRanking s = 
    case s of 
        Selected esUp uP ranking ->
            ranking
           
isOwner : Selected -> Bool 
isOwner s = 
    case s of 
        Selected esUP uP ranking ->
            if Data.Rankings.isOwner (Data.Users.gotId uP.user) ranking then
                True 
            else 
                False
        
isMember : UserPlayer -> Data.Rankings.Ranking -> Bool 
isMember uP ranking = 
    case uP.user of 
        Data.Users.Registered userInfo userState ->
            List.isEmpty (List.filter (\x -> x == ranking.id_) userInfo.userjoinedrankings)
        _ ->
            False

updatedUPInSet : Selected -> UserPlayer -> Selected
updatedUPInSet sSelected updatedUP =
    addUserPlayer updatedUP <| removeUserPlayer updatedUP sSelected
        

gotPlayers : Selected -> Data.Players.Players 
gotPlayers  (Selected esUP uP ranking) = 
    Data.Players.asPlayers (EverySet.map (\x -> x.player) esUP)

gotStatus : Selected -> PlayerStatus
gotStatus selected = 
    case selected of 
        Selected esSelected uP ranking->
            uP.status
                

gotUP : Selected -> UserPlayer
gotUP s = 
    case s of 
        Selected esSelected uP ranking->
            uP

gotUserPlayerByUserId : Selected -> String -> Maybe UserPlayer
gotUserPlayerByUserId (Selected esSelected uP ranking) uid =
    List.head <|
        List.filter (\r -> (gotPlayerUID r) == (String.toLower <| uid))
            <| EverySet.toList esSelected

-- now can get eithe user or player with e.g.  (gotOwnerAbsUP selected).player
-- NB. I don't think below works to get the Owner
-- it's just matching a players.uid against all the users
-- maybe a gotOwner function instead of got Id?
gotOwnerAsUP : Selected -> Maybe UserPlayer
gotOwnerAsUP (Selected esSelected uP ranking) =
    List.head <|
    List.filter (\r -> (gotPlayerUID r) == Data.Users.gotId r.user)
    <| EverySet.toList esSelected

gotPlayerUID : UserPlayer -> String 
gotPlayerUID uplayer = 
    case uplayer.player of 
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            playerInfo.uid

gotRankingId : Selected -> Internal.Types.RankingId 
gotRankingId selected = 
    case selected of 
        Selected sSelected uP ranking ->
            ( Internal.Types.RankingId  ranking.id_ )


empty : Selected
empty =
    Selected (EverySet.empty) emptyUserPlayer Data.Rankings.emptyRanking

emptyUserPlayer : UserPlayer 
emptyUserPlayer =
    {player = Data.Players.IndividualPlayer (Data.Players.PlayerInfo "" "" 0 ) Data.Players.Available
    , user = Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General
    , status = Other}

createdUserPlayer : List Data.Users.User -> Data.Players.Player -> UserPlayer
createdUserPlayer luser player =
    case player of 
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            let
                m_user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) playerInfo.uid
            in
            case m_user of
                Nothing ->
                    emptyUserPlayer
                    
                Just user ->
                    let
                        newUserPlayer =
                            { player = player
                            , user = user
                            --ok to just assign as 'Other'?
                            , status = Other
                            }
                    in
                        newUserPlayer

addUserPlayer : UserPlayer -> Selected -> Selected
addUserPlayer uplayer sSelected = 
    case sSelected of 
        Selected rankedUserPlayers uP ranking ->
            Selected (EverySet.insert (addNewUserPlayerJoinRanking uplayer (Internal.Types.RankingId ranking.id_))  rankedUserPlayers) uP ranking



addNewUserPlayerJoinRanking : UserPlayer -> Internal.Types.RankingId -> UserPlayer
-- I think (Internal.Types.RankingId rnkId) pattern matching here gives us the rnkId as a String)
addNewUserPlayerJoinRanking uplayer (Internal.Types.RankingId rnkId) = 
        case uplayer.user of
            Data.Users.Spectator _ _ ->
                uplayer

            (Data.Users.Registered userInfo sStatus) ->
                 let 
                    updatedUserJoinRankings = {userInfo | userjoinedrankings = rnkId :: userInfo.userjoinedrankings}
                    newUser = Data.Users.Registered updatedUserJoinRankings
                    newUserPlayer =  { uplayer | player = uplayer.player, user = newUser Data.Users.General}
                    
                in
                    newUserPlayer


removeUserPlayer : UserPlayer -> Selected -> Selected
removeUserPlayer uplayer sSelected = 
    case sSelected of 
        Selected esUP uP ranking ->
            Selected (EverySet.remove uplayer esUP) uP ranking

changedRank : UserPlayer -> Int -> Selected -> Selected
changedRank uplayer newRank sSelected = 
    case uplayer.player of
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            let
                updatedPlayer = Data.Players.IndividualPlayer 
                    (Data.Players.PlayerInfo playerInfo.rankingid playerInfo.uid newRank )
                    playerStatus
                updatedUserPlayer = { uplayer | player = updatedPlayer}
            in
                removeUserPlayer uplayer sSelected
                |> addUserPlayer updatedUserPlayer


isCurrentUserPlayerLowerRanked : UserPlayer -> UserPlayer -> Bool 
isCurrentUserPlayerLowerRanked uplayer challenger = 
    case uplayer.player of
        Data.Players.IndividualPlayer playerInfo playerStatus  -> 
            --n.b. for ranks lower int is higher rank!
            case challenger.player of 
                Data.Players.IndividualPlayer challengerInfo challengerStatus  ->
                    if playerInfo.rank > challengerInfo.rank then
                        True 
                    else False       

gotPlayer : Data.Users.User -> Selected -> Selected
gotPlayer user (Selected esUP uP ranking) =
    -- todo: fix
    --[emptyUserPlayer]
    case user of
                Data.Users.Spectator _ _ ->
                    empty
                    
                (Data.Users.Registered userInfo sStatus) ->
                --todo: fix 
                    empty
                    -- asSelected <| 
                    --     (EverySet.filter
                    --         (isThisPlayerId
                    --             (userInfo.id)
                    --         )
                    --         sSelected)
                    --         rnkId sStatus players

                     
               


    -- case user.m_ethaddress of 
    --     Nothing ->
    --         luPlayer 
    --     Just ethaddress ->
    --         List.filterMap
    --             (isThisPlayerId
    --                 (String.toLower (Eth.Utils.addressToString ethaddress))
    --             )
    --             luPlayer

isThisPlayerId : String -> UserPlayer -> Maybe UserPlayer
isThisPlayerId playerId uplayer =
    case uplayer.player of 
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            if playerInfo.uid == playerId then
                Just uplayer
            else
                Nothing
    

asList : Selected -> List UserPlayer 
asList srank = 
    case srank of 
        Selected rankedUserPlayers uP ranking ->
            rankedUserPlayers
           |> EverySet.toList
           |> List.sortBy extractRank

extractRank : UserPlayer -> Int
extractRank uplayer =
    case uplayer.player of 
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            playerInfo.rank

isChallenged : UserPlayer -> Bool
isChallenged uplayer = 
    case uplayer.player of 
        Data.Players.IndividualPlayer playerInfo playerStatus  ->
            case playerStatus of 
                Data.Players.Available ->
                    False 
                Data.Players.Challenged _ ->
                    True

--todo: should this be here or just in User?
-- userAdded : String -> List UserPlayer -> Data.Users.User -> List UserPlayer
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
--                             , challengerid = ""
--                             }
--                         , user = userRec
--                         }
--                 in
--                     Utils.MyUtils.stringToRankingId strrankingId 
--                     |> asSelected (EverySet.fromList luPlayer) 
--                     |> addUserPlayer newUserPlayer
--                     |> asList
    
isRegisteredPlayerCurrentUser : Data.Users.User -> UserPlayer -> Bool
isRegisteredPlayerCurrentUser user uplayer = 
    --player display only deals with Registered players
    case user of
        (Data.Users.Registered userInfo sStatus) ->
            case uplayer.player of
                Data.Players.IndividualPlayer playerInfo playerStatus  -> 
                    if playerInfo.uid == userInfo.id then
                        True
                    else
                        False
        _ ->
            False
       

challorAvail : Selected -> UserPlayer -> String 
challorAvail s uplayer = 
    case uplayer.player of 
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            case playerStatus of 
                Data.Players.Available ->
                    "Available"
                Data.Players.Challenged challengerid -> 
                    --case (Data.Users.gotUser sUsers challengerid) of 
                    case gotUserPlayerByUserId s challengerid of
                    --case 
                        Nothing ->
                            "Available"
                        Just uP ->
                        -- can't use gotUserName here cos it's the challenger 
                        -- we're referring to
                            case uP.user of 
                                Data.Users.Spectator userInfo _ ->
                                    userInfo.username
                                Data.Users.Registered userInfo _ ->
                                    userInfo.username 


updatePlayerRankWithWonResult : List UserPlayer -> UserPlayer -> List UserPlayer
updatePlayerRankWithWonResult luPlayer uplayer =
    -- todo: fix
        []
    -- case uplayer.player of 
    --     Data.Players.IndividualPlayer playerInfo playerStatus ->
    --         let
    --             filteredPlayerList =
    --                 filterPlayerOutOfPlayerList playerInfo.uid luPlayer
    --             -- todo: fix get a real not empty selected
    --             --m_opponentAsPlayer = gotOpponent empty uplayer
    --             m_opponentAsPlayer = (gotUserPlayerByUserId empty)
    --                 --gotUserPlayerFromPlayerListStrAddress luPlayer uplayer.player.challengerid
    --         in
                -- case m_opponentAsPlayer of
                --     Nothing ->
                --         luPlayer 
                --     Just opponentAsPlayer ->
                --         case opponentAsPlayer.player of 
                --             Data.Players.IndividualPlayer playerInfoVal playerStatusVal ->
                --                 let
                --                     -- this needs more ?:
                --                     -- newUserPlayerPlayerField =
                --                     --     uplayer.player
                --                     -- I've given the oppenent here the user's player's rank (playerInfo) - not sure if that's correct (?)
                --                     updatedPlayer = Data.Players.IndividualPlayer 
                --                         (Data.Players.PlayerInfo playerInfoVal.rankingid playerInfoVal.uid playerInfo.rank playerInfoVal.challengerid)
                --                         playerStatusVal
                --                         --{ newUserPlayerPlayerField | rank = playerInfoVal.rank }

                --                     newUserPlayer =
                --                         { uplayer | player = updatedPlayer }

                --                     newPlayerList =
                --                         newUserPlayer :: filteredPlayerList
                --                 in
                --                     newPlayerList


sortedRank : List UserPlayer -> List UserPlayer
sortedRank luplayer =
    let
        validatedMaybePlayerLst =
            List.map splitPlayerFieldsToCreateMaybePlayer luplayer

        filteredValidatedMaybePlayerLst =
            --List.filter canPlayerBeInList validatedMaybePlayerLst
            -- todo: fix
            [Just <|  Data.Players.empty]

        convertedValidatedPlayerList =
            List.map (\x -> (Maybe.withDefault Data.Players.empty x)) filteredValidatedMaybePlayerLst

        reorderedConvertedValidatedPlayerList =
            -- todo: fix - empty list 
            reorderPlayerListToStartAtOne []
                --convertedValidatedPlayerList
    in
    List.sortBy extractRank reorderedConvertedValidatedPlayerList


splitPlayerFieldsToCreateMaybePlayer : UserPlayer -> Maybe UserPlayer
splitPlayerFieldsToCreateMaybePlayer uplayer =
    case uplayer.player of 
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            if playerInfo.rank > 0 && playerInfo.rank < 50000 then
                Just uplayer

            else
                Nothing


reorderPlayerListToStartAtOne : List UserPlayer -> List UserPlayer
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


resetPlayerRankingList : Int -> UserPlayer -> UserPlayer
resetPlayerRankingList newRank uplayer =
    case uplayer.player of
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            let
                newPlayer = Data.Players.IndividualPlayer 
                    (Data.Players.PlayerInfo playerInfo.rankingid playerInfo.uid newRank)
                    playerStatus

                newUserPlayer =
                    { uplayer | player = newPlayer }
            in
                newUserPlayer


resetPlayerRankToOne : UserPlayer -> UserPlayer
resetPlayerRankToOne uplayer =
    case uplayer.player of
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            let
                newPlayer = Data.Players.IndividualPlayer 
                    (Data.Players.PlayerInfo playerInfo.rankingid playerInfo.uid 1)
                    playerStatus

                newUserPlayer =
                    { uplayer | player = newPlayer }
            in
            newUserPlayer


canPlayerBeInList : Maybe UserPlayer -> Bool
canPlayerBeInList uplayer =
    case uplayer of
        Nothing ->
            False

        Just a ->
            True


gotCurrentUserAsPlayerFromPlayerList : List UserPlayer -> Data.Users.User -> Maybe UserPlayer
gotCurrentUserAsPlayerFromPlayerList luPlayer userRec =

    --todo: fix
    Nothing
    -- case userRec.m_ethaddress of
    --     Nothing ->
    --         Nothing

    --     Just ethaddress ->
    --         let
    --             existingPlayer =
    --                 List.head <|
    --                     List.filter (\r -> r.player.uid == (String.toLower <| (Eth.Utils.addressToString ethaddress)))
    --                         luPlayer
    --         in
    --             existingPlayer

-- gotRankingOwnerAsPlayer : Selected -> List UserPlayer -> Data.Players.Player
-- gotRankingOwnerAsPlayer selected luplayer =
    -- let 
    --     -- up = Maybe.withDefault (emptyUserPlayer) (gotUserPlayerFromPlayerListStrAddress luplayer selectedRanking)
    --     up = Maybe.withDefault (emptyUserPlayer) (gotUserPlayerByUserId luplayer selectedRanking)
    --     gotOwnerAsUP selected

    -- in
    --     up.player
       

filterPlayerOutOfPlayerList : String -> List UserPlayer -> List UserPlayer
filterPlayerOutOfPlayerList addr lplayer =
    List.filterMap
        (doesPlayerIdNOTMatchId
            addr
        )
        lplayer

doesPlayerIdNOTMatchId : String -> UserPlayer -> Maybe UserPlayer
doesPlayerIdNOTMatchId uid uplayer =
    case uplayer.player of 
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            if playerInfo.uid /= uid then
                Just uplayer
            else
                Nothing


convertUserPlayersToPlayers : List UserPlayer -> List Data.Players.Player
convertUserPlayersToPlayers luplayers =
    List.map refEachPlayer luplayers


asUsers : Selected -> Data.Users.Users 
asUsers sSelected =
    (EverySet.fromList (List.map refEachUser (asList sSelected)))
    |> Data.Users.asUsers

refEachUser : UserPlayer -> Data.Users.User
refEachUser uplayer =
    uplayer.user

refEachPlayer : UserPlayer -> Data.Players.Player
refEachPlayer uplayer =
    uplayer.player

-- convertPlayersToUserPlayers : List Data.Players.Player -> List Data.Users.User -> List UserPlayer
-- convertPlayersToUserPlayers lplayer luser =
--     List.map (convertEachPlayerToUserPlayer luser) lplayer


-- convertEachPlayerToUserPlayer : List Data.Users.User -> Data.Players.Player -> UserPlayer
-- convertEachPlayerToUserPlayer luser player =
--     case player of 
--         Data.Players.IndividualPlayer playerInfo playerStatus ->
--             let
--                 user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) playerInfo.uid
--             in
--                 case user of 
--                     Nothing ->
--                         { player = player, user = (Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General), status = Other }
--                     Just userVal ->
--                         { player = player, user = userVal , status = gotStatus s}
                
    --{ player = player, user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) player.uid }


handleWon : Selected -> UserPlayer -> UserPlayer -> Data.Users.Users -> Selected
handleWon (Selected esUPlayer uP ranking)  uplayer upChallenger sUsers =
    -- todo: fix
    empty
            -- let
            --     whoHigher =
            --         isOpponentHigherRank appInfo.player appInfo.challenger
            -- in
            -- case whoHigher of
            --     OpponentRankHigher ->
                    
            --         let
                        
            --             supdatedPlayer =  
            --                 changedRank appInfo.player appInfo.challenger.player.rank (asSelected esUPlayer uP ranking)
                           

            --             supdatedPlayerAndChallenger =
            --                 supdatedPlayer
            --                 |> changedRank appInfo.challenger (appInfo.challenger.player.rank + 1)
                            
            --             -- tried 'handling with AppState then handing back to AppInfo for now ...'
            --             -- but Selected cannot import AppState without creating a loop (find another way)
            --             -- todo: fix - not sure what this should be currently:
            --             updatedUserPlayer = emptyUserPlayer
            --             --Data.AppState.releasePlayerForUI (Data.AppState.updateAppState (Just appInfo.user) 
            --                 --appInfo.player appInfo.challenger (Data.Rankings.stringToRankingId appInfo.selectedRanking.id_)
            --                 --appInfo.player appInfo.challenger appInfo.selectedRanking.id_

            --             newAppInfo =
                           
            --                 { appInfo | player = updatedUserPlayer, challenger = emptyUserPlayer }
           
            --         in
            --         --nb. higher rank is a lower number and vice versa!
            --             (supdatedPlayerAndChallenger, newAppInfo)
                        

            --     OpponentRankLower ->

            --         let
            --             --no ranking change - just update the player list for both players challenger to emptyPlayer, no rank change
            --             supdatedPlayer =
            --                 changedRank appInfo.player appInfo.player.player.rank (asSelected esUPlayer uP ranking)

            --             supdatedPlayerAndChallenger =
            --                 changedRank  appInfo.challenger appInfo.challenger.player.rank  supdatedPlayer

            --             --update current player now
            --             newUserPlayerPlayer =
            --                 appInfo.player.player

            --             newUserPlayerPlayerUpdated =
            --                 { newUserPlayerPlayer | uid = "" }

            --             newAppInfoPlayer =
            --                 appInfo.player

            --             newAppInfoUserPlayer =
            --                 { newAppInfoPlayer | player = newUserPlayerPlayerUpdated }

            --             newAppInfo =
            --                 { appInfo | player = newAppInfoUserPlayer, challenger = emptyUserPlayer }

            --         in
            --         --nb. higher rank is a lower number and vice versa!
            --             (supdatedPlayerAndChallenger, newAppInfo)


--handleLost : Selected -> SR.Types.AppInfo -> (Selected, SR.Types.AppInfo)
--handleLost (Selected esUPlayer uP ranking) appInfo =
handleLost : Selected -> UserPlayer -> UserPlayer -> Data.Users.Users -> Selected
handleLost (Selected esUPlayer uP ranking)  uplayer upChallenger sUsers =
--todo: fix
    empty
    -- let
    --     whoHigher =
    --         isOpponentHigherRank appInfo.player appInfo.challenger
    -- in
    -- case whoHigher of
    --     OpponentRankHigher ->
    --         let
    --             supdatedPlayer =
    --                 changedRank appInfo.player appInfo.player.player.rank (asSelected esUPlayer uP ranking)

    --             supdatedPlayerAndChallenger =
    --                 changedRank  appInfo.challenger appInfo.challenger.player.rank  supdatedPlayer

    --             --update current player now
    --             newUserPlayer =
    --                 appInfo.player

    --             newUserPlayerPlayer =
    --                 appInfo.player.player

    --             newPlayerUpdated =
    --                 { newUserPlayerPlayer | uid = "" }

    --             newUserPlayerUpdated =
    --                 { newUserPlayer | player = newPlayerUpdated }

    --             newAppInfo =
    --                 { appInfo | player = newUserPlayerUpdated, challenger = emptyUserPlayer }

    --         in
    --         --nb. higher rank is a lower number and vice versa!
    --         (supdatedPlayerAndChallenger, newAppInfo)
            

    --     OpponentRankLower ->
    --         --nb. higher rank is a lower number and vice versa!
    --         let
    --             supdatedPlayer =  
    --                 changedRank appInfo.player appInfo.challenger.player.rank (asSelected esUPlayer uP ranking)
                    

    --             supdatedPlayerAndChallenger =
    --                 supdatedPlayer
    --                 |> changedRank appInfo.challenger (appInfo.challenger.player.rank + 1)

    --             --update current player now
    --             newUserPlayer =
    --                 appInfo.player

    --             newUserPlayerPlayer =
    --                 appInfo.player.player

    --             newUserPlayerPlayerUpdated =
    --                 { newUserPlayerPlayer | uid = "" }

    --             newUserPlayerUpdated =
    --                 { newUserPlayer | player = newUserPlayerPlayerUpdated }

    --             newAppInfo =
    --                 { appInfo | player = newUserPlayerUpdated, challenger = emptyUserPlayer }

    --         in
    --         (supdatedPlayerAndChallenger, newAppInfo)


handleUndecided : Selected -> UserPlayer -> UserPlayer -> Selected
handleUndecided (Selected esUPlayer uP ranking) playerUP challengerUP =
    case playerUP.player of
        Data.Players.IndividualPlayer playerInfo playerStatus ->
            let
                originalSelectedSet = (Selected esUPlayer uP ranking)    

                supdatedPlayer = 
                    changedRank playerUP playerInfo.rank (Selected esUPlayer uP ranking)
                        
                newPlayerPlayerUpdated = (Data.Players.IndividualPlayer 
                    (Data.Players.PlayerInfo playerInfo.rankingid playerInfo.uid playerInfo.rank) Data.Players.Available)

                newUserPlayerUpdated =
                    { playerUP | player = newPlayerPlayerUpdated }

                updatedSet = removeUserPlayer playerUP originalSelectedSet
                    |> addUserPlayer newUserPlayerUpdated
                    |> asEverySet
                    
            in
                Selected updatedSet uP ranking


--isOpponentHigherRank : UserPlayer -> SR.Types.Opponent -> SR.Types.OpponentRelativeRank
isOpponentHigherRank uplayer opponent =
    -- nb. if player rank is 'higher' than opponent his rank integer will actually be 'less than' opponent
    -- we go by the integer ...
    if uplayer.player.rank > opponent.player.rank then
        OpponentRankHigher

    else
        OpponentRankLower

-- jsonEncodeNewSelectedRankingPlayerList : List UserPlayer -> Json.Encode.Value
-- jsonEncodeNewSelectedRankingPlayerList luplayers =
--     let
--         lplayers =
--             convertUserPlayersToPlayers luplayers

--         encodePlayerObj : Data.Players.Player -> Json.Encode.Value
--         encodePlayerObj player =
--             Json.Encode.object
--                 [ ( "uid", Json.Encode.string (String.toLower player.uid |> Debug.log "player.uid: ") )
--                 , ( "rank", Json.Encode.int player.rank )
--                 , ( "challengerid", Json.Encode.string (player.challengerid |> Debug.log "challenger id: "))
--                 ]

--         encodedList =
--             Json.Encode.list encodePlayerObj lplayers
--     in
--     encodedList

--updateSelectedRankingOnChallenge : Selected -> SR.Types.AppInfo -> Selected
--updateSelectedRankingOnChallenge allSets appInfo =
    --allSets

-- it's easier to do them together
assignedChallengerUIDForBOTHPlayers : Selected -> UserPlayer -> UserPlayer -> Selected
assignedChallengerUIDForBOTHPlayers sSelected user challenger =
    case sSelected of 
        (Selected esUP uP ranking) -> 
            case (user.player, challenger.player) of 
                (Data.Players.IndividualPlayer playerInfo playerStatus, Data.Players.IndividualPlayer challengerInfo challengerStatus) ->
                    case (user.user, challenger.user) of 
                        (Data.Users.Registered userInfo userStatus, Data.Users.Registered cuserInfo cuserStatus) ->
                            let
                                updatedUserAsPlayer = Data.Players.assignChallengerUID user.player cuserInfo.id
                                updatedChallengerAsPlayer = Data.Players.assignChallengerUID challenger.player userInfo.id

                                newSelectedWithUser = updatedUPInSet sSelected {player = updatedUserAsPlayer, user = user.user, status = user.status} 
                                newSelectedWithChallenger = updatedUPInSet newSelectedWithUser {player = updatedChallengerAsPlayer, user = challenger.user, status = challenger.status}
                                newSelected = Selected (asEverySet newSelectedWithChallenger) uP ranking
                            in 
                                newSelected

                        (_) ->
                            sSelected


-- extractAndSortPlayerList : RemoteData.WebData (List Data.Players.Player) -> List Data.Users.User -> List UserPlayer
-- extractAndSortPlayerList rdlPlayer luser =
--     let
--         _ = Debug.log "luser in extractAndSortPlayerList" luser

--         lplayer =
--             Data.Players.extractPlayersFromWebData rdlPlayer

--         convertedPlayerListToUserPlayerList =
--             convertPlayersToUserPlayers
--                 lplayer
--                 luser
--     in
--     sortedRank <| convertedPlayerListToUserPlayerList


--useful?:
-- convertUserPlayersToPlayers : List UserPlayer -> List Data.Players.Player
-- convertUserPlayersToPlayers luplayers =
--     List.map Utils.MyUtils.refEachPlayer luplayers

--useful?
-- convertPlayersToUserPlayers : List Data.Players.Player -> List Data.Users.User -> List UserPlayer
-- convertPlayersToUserPlayers lplayer luser =
--     List.map (convertEachPlayerToUserPlayer luser) lplayer


-- convertEachPlayerToUserPlayer : List Data.Users.User -> Data.Players.Player -> UserPlayer
-- convertEachPlayerToUserPlayer luser player =
--     { player = player, user = gotUserFromUserList luser player.uid }


-- gotRankingOwner : Data.Rankings.Ranking -> List UserRanking -> List UserPlayer -> UserPlayer
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


-- gotRankingOwnerAPlayer : String -> List UserPlayer -> Data.Players.Player
-- gotRankingOwnerAPlayer selectedRanking luplayer =
--     (gotUserPlayerFromPlayerListStrAddress luplayer selectedRanking).player


releasePlayerForUI : UserPlayer
releasePlayerForUI  =
    {player = Data.Players.IndividualPlayer (Data.Players.PlayerInfo "" "" 0 ) Data.Players.Available
    , user = Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General
    , status = Other}

releaseChallengerForUI : UserPlayer
releaseChallengerForUI =
    {player = Data.Players.IndividualPlayer (Data.Players.PlayerInfo "" "" 0 ) Data.Players.Available
    , user = Data.Users.Spectator Data.Users.emptyUserInfo Data.Users.General
    , status = Other}
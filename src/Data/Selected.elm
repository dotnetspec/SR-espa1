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


--import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
--import Utils.MyUtils
import Data.Users
import Element exposing (Element)
import RemoteData
--import Data.AppState
import Data.Players
import Json.Encode
import EverySet
import Eth.Utils
import Eth.Types
--import Data.Global
--import SRdb.Object.User exposing (ethaddress)



-- the relevant Users are picked out with the Create function and for the UserPlayer EverySet (no need for Users assoc data)
type Selected = Selected (EverySet UserPlayer) Internal.Types.RankingId SelectedStatus Data.Players.Players

type alias UserPlayer =
    { player : Data.Players.Player
    , user : Data.Users.User
    }

type alias Opponent =
    UserPlayer

type SelectedStatus
 = UserIsOwner
 | UserIsMember
 | UserIsNeitherOwnerNorMember

asSelected : EverySet UserPlayer -> Internal.Types.RankingId -> SelectedStatus -> Data.Players.Players -> Selected 
asSelected esUserPlayer rnkId status sPlayers = 
    Selected esUserPlayer rnkId status sPlayers

asEverySet : Selected -> EverySet UserPlayer
asEverySet (Selected esSelected rnkId status sPlayers)  = 
     esSelected

created : List Data.Players.Player -> Data.Users.Users -> Internal.Types.RankingId -> Selected
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

gotStatus : Selected -> SelectedStatus
gotStatus selected = 
    case selected of 
        Selected sSelected _ status _ ->
            status

-- gotUserPlayerFromPlayerListStrAddress : List UserPlayer -> String -> UserPlayer
-- gotUserPlayerFromPlayerListStrAddress luplayer addr =
--     let
--         existingUser =
--             List.head <|
--                 List.filter (\r -> r.player.uid == (String.toLower <| addr))
--                     luplayer
--     in
--     case existingUser of
--         Nothing ->
--             emptyUserPlayer

--         Just a ->
--             a

gotUserPlayerFromPlayerListStrAddress : List UserPlayer -> String -> Maybe UserPlayer
gotUserPlayerFromPlayerListStrAddress luplayer addr =
    let
        existingUser =
            List.head <|
                List.filter (\r -> r.player.uid == (String.toLower <| addr))
                    luplayer
    in
        existingUser

gotRankingId : Selected -> Internal.Types.RankingId 
gotRankingId selected = 
    case selected of 
        Selected sSelected rnkId status sPlayers ->
            rnkId


empty : Selected
empty = Selected (EverySet.empty) (Internal.Types.RankingId "") SR.Types.UserIsNeitherOwnerNorMember Data.Players.empty

emptyUserPlayer : UserPlayer 
emptyUserPlayer =
    {player = Data.Players.empty
    , user = Data.Users.empty}


resultView : SelectedStatus -> SR.Types.UIState
resultView  status = 
    case status of
            SR.Types.UserIsOwner -> 
                SR.Types.UISelectedRankingUserIsOwner

            SR.Types.UserIsMember -> 
                SR.Types.UISelectedRankingUserIsPlayer

            SR.Types.UserIsNeitherOwnerNorMember -> 
                SR.Types.UISelectedRankingUserIsNeitherOwnerNorPlayer
                

createdUserPlayer : List Data.Users.User -> Data.Players.Player -> UserPlayer
createdUserPlayer luser player =
    let
        m_user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) player.uid
    in
        case m_user of
            Nothing ->
                emptyUserPlayer
                
            Just user ->
                let
                      newUserPlayer =
                        { player = player
                        , user = user
                        }
                in
                    newUserPlayer

    

gotUserAsPlayer : Selected -> Eth.Types.Address -> Maybe UserPlayer 
gotUserAsPlayer sSelected uaddr = 
    --todo: fix
    --emptyUserPlayer
    case sSelected of 
        Selected esUserPlayer rnkId status sPlayers -> 
            gotUserPlayerFromPlayerListStrAddress (EverySet.toList esUserPlayer) (Eth.Utils.addressToString uaddr)

addUserPlayer : UserPlayer -> Selected -> Selected
addUserPlayer uplayer sSelected = 
    case sSelected of 
        Selected rankedUserPlayers rnkId status sPlayers ->
                asSelected (EverySet.insert (addNewUserPlayerJoinRanking uplayer rnkId)  rankedUserPlayers) rnkId status sPlayers


addNewUserPlayerJoinRanking : UserPlayer -> Internal.Types.RankingId -> UserPlayer
addNewUserPlayerJoinRanking uplayer rnkId = 
        case uplayer.user of
            Data.Users.Guest ->
                uplayer

            (SR.Types.Registered userId token userInfo) ->
                 let 
                    updatedUserJoinRankings = {userInfo | userjoinrankings = Utils.MyUtils.stringFromRankingId rnkId :: userInfo.userjoinrankings}
                    newUser = SR.Types.Registered userId token updatedUserJoinRankings
                    newUserPlayer =  { uplayer | player = uplayer.player, user = newUser }
                    
                in
                    newUserPlayer
            
            (SR.Types.NoWallet userId token userInfo) ->
                let 
                    updatedUserJoinRankings = {userInfo | userjoinrankings = Utils.MyUtils.stringFromRankingId rnkId :: userInfo.userjoinrankings}
                    newUser = SR.Types.NoWallet userId token updatedUserJoinRankings
                    newUserPlayer =  { uplayer | player = uplayer.player, user = newUser }
                    
                in
                    newUserPlayer
            
            (SR.Types.NoCredit addr userId token userInfo) ->
                let 
                    updatedUserJoinRankings = {userInfo | userjoinrankings = Utils.MyUtils.stringFromRankingId rnkId :: userInfo.userjoinrankings}
                    newUser = SR.Types.NoCredit addr userId token updatedUserJoinRankings
                    newUserPlayer =  { uplayer | player = uplayer.player, user = newUser }
                    
                in
                    newUserPlayer

            (SR.Types.Credited addr userId token userInfo) ->
                let 
                    updatedUserJoinRankings = {userInfo | userjoinrankings = Utils.MyUtils.stringFromRankingId rnkId :: userInfo.userjoinrankings}
                    newUser = SR.Types.Credited addr userId token updatedUserJoinRankings
                    newUserPlayer =  { uplayer | player = uplayer.player, user = newUser }
                    
                in
                    newUserPlayer


removeUserPlayer : UserPlayer -> Selected -> Selected
removeUserPlayer uplayer sSelected = 
    --(Data.Players.asPlayers (EverySet.fromList lplayer))
        asSelected (EverySet.remove uplayer (asEverySet sSelected)) (gotRankingId sSelected) SR.Types.UserIsOwner Data.Players.empty

changedRank : UserPlayer -> Int -> Selected -> Selected
changedRank uplayer newRank sSelected = 
    let 
        newPlayer = uplayer.player
        updatedPlayer = { newPlayer | rank = newRank, challengerid = ""}
        updatedUserPlayer = { uplayer | player = updatedPlayer}
    in   
        removeUserPlayer uplayer sSelected
        |> addUserPlayer updatedUserPlayer


isCurrentUserPlayerLowerRanked : UserPlayer -> UserPlayer -> Bool 
isCurrentUserPlayerLowerRanked uplayer challenger = 
    --n.b. for ranks lower int is higher rank!
    if uplayer.player.rank > challenger.player.rank then
        True 
        else False

isUserPlayerMemberOfSelectedRanking : Selected -> Data.Users.User -> Bool
isUserPlayerMemberOfSelectedRanking sSelected user =
    --todo: fix
    False
    -- let
    --     filteredList =
    --         gotPlayer user sSelected

    --     filteredRec =
    --         List.head filteredList
    -- in
    -- case filteredRec of
    --     Nothing ->
    --         False

    --     Just a ->
    --         case user of
    --             Data.Users.Guest ->
    --                 False
                    
    --             (SR.Types.Registered userId token userInfo) ->
    --                 if (String.toLower a.player.uid) == userId then
    --                     True

    --                 else
    --                     False

    --             (SR.Types.NoWallet userId token userInfo) ->
    --                 if (String.toLower a.player.uid) == userId then
    --                     True

    --                 else
    --                     False

    --             (SR.Types.NoCredit addr userId token userInfo) ->
    --                 if (String.toLower a.player.uid) == userId then
    --                     True

    --                 else
    --                     False

    --             (SR.Types.Credited addr userId token userInfo) ->
    --                 if (String.toLower a.player.uid) == userId then
    --                     True

    --                 else
    --                     False


isUserOwnerOfSelectedUserRanking : Data.Rankings.Ranking -> List SR.Types.UserRanking -> Data.Users.User -> Bool
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
                Data.Users.Guest ->
                    False

                (SR.Types.Registered userId token userInfo) ->
                    if a.rankingInfo.rankingownerid == userId then
                        True

                    else
                        False

                (SR.Types.NoWallet userId token userInfo) ->
                    if a.rankingInfo.rankingownerid == userId then
                        True

                    else
                        False

                (SR.Types.NoCredit addr userId token userInfo) ->
                    if a.rankingInfo.rankingownerid == userId then
                        True

                    else
                        False

                (SR.Types.Credited addr userId token userInfo) ->
                    if a.rankingInfo.rankingownerid == userId then
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

gotPlayer : Data.Users.User -> Selected -> Selected
gotPlayer user (Selected sSelected rnkId sStatus players)=
    -- todo: fix
    --[emptyUserPlayer]
    case user of
                Data.Users.Guest ->
                    empty
                    
                (SR.Types.Registered userId token userInfo) ->
                --todo: fix 
                    empty
                    -- asSelected <| 
                    --     (EverySet.filter
                    --         (isThisPlayerId
                    --             (userId)
                    --         )
                    --         sSelected)
                    --         rnkId sStatus players

                     
                (SR.Types.NoWallet userId token userInfo) ->
                    empty

                (SR.Types.NoCredit addr userId token userInfo) ->
                    empty

                (SR.Types.Credited addr userId token userInfo) ->
                    empty


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
    if uplayer.player.uid == playerId then
        Just uplayer

    else
        Nothing
    

asList : Selected -> List UserPlayer 
asList srank = 
    case srank of 
        Selected rankedUserPlayers rnkId status sPlayers ->
            rankedUserPlayers
           |> EverySet.toList
           |> List.sortBy extractRank

extractRank : UserPlayer -> Int
extractRank uplayer =
    uplayer.player.rank

assignChallengerAddr : Selected -> UserPlayer -> String -> Selected
assignChallengerAddr sSelected uplayer challengerid =
    let 
        newSSelected = removeUserPlayer uplayer sSelected
        newPlayer = uplayer.player
        updatedPlayer = {newPlayer | challengerid = challengerid}
        newUPlayer = {uplayer | player = updatedPlayer}
    in
        addUserPlayer newUPlayer newSSelected

isChallenged : Selected -> Data.Users.Users -> UserPlayer -> Bool
isChallenged (Selected sSelected rnkId status sPlayers ) sUsers uplayer = 
    let
        m_challenger = Data.Users.gotUser sUsers uplayer.player.challengerid
    in
        case m_challenger of 
            Nothing ->
                False 
            Just challenger ->
            --     if challenger.username /= "" then
            --         True

            --     else
            --         False

                case challenger of
                    Data.Users.Guest ->
                        False
                    (SR.Types.Registered userId token userInfo) ->
                        if userInfo.username /= "" then
                            True
                        else
                            False

                    (SR.Types.NoWallet userId token userInfo) ->
                        if userInfo.username /= "" then
                            True
                        else
                            False
                    (SR.Types.NoCredit addr userId token userInfo) ->
                        if userInfo.username /= "" then
                            True
                        else
                            False
                    (SR.Types.Credited addr userId token userInfo) ->
                        if userInfo.username /= "" then
                            True
                        else
                            False

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
    
isPlayerCurrentUser : Data.Users.User -> UserPlayer -> Bool
isPlayerCurrentUser user uplayer = 
    case user of
        Data.Users.Guest ->
            False
        (SR.Types.Registered userId token userInfo) ->
            if uplayer.player.uid == userId then
                True

            else
                False
        (SR.Types.NoWallet userId token userInfo) ->
            if uplayer.player.uid == userId then
                True

            else
                False
        (SR.Types.NoCredit addr userId token userInfo) ->
            if uplayer.player.uid == userId then
                True

            else
                False
        (SR.Types.Credited addr userId token userInfo) ->
            if uplayer.player.uid == userId then
                True

            else
                False

printChallengerNameOrAvailable : Selected -> Data.Users.Users -> UserPlayer -> String 
printChallengerNameOrAvailable sSelected sUsers uplayer = 
    let
        m_opponent =  (gotOpponent sSelected uplayer)
    in
        case m_opponent of
            Nothing ->
                "Available"
            Just opponent ->
                if isChallenged sSelected sUsers uplayer then
                    case opponent.user of
                        Data.Users.Guest ->
                            "Available"
                        (SR.Types.Registered userId token userInfo) ->
                            userInfo.username
                        (SR.Types.NoWallet userId token userInfo) ->
                            userInfo.username
                        (SR.Types.NoCredit addr userId token userInfo) ->
                            userInfo.username
                        (SR.Types.Credited addr userId token userInfo) ->
                            userInfo.username
                else
                    "Available"

gotOpponent : Selected -> UserPlayer -> Maybe UserPlayer
gotOpponent sSelected uplayer = 
    gotUserPlayerFromPlayerListStrAddress (asList sSelected) uplayer.player.challengerid


updatePlayerRankWithWonResult : List UserPlayer -> UserPlayer -> List UserPlayer
updatePlayerRankWithWonResult luPlayer uplayer =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList uplayer.player.uid luPlayer

        m_opponentAsPlayer =
            gotUserPlayerFromPlayerListStrAddress luPlayer uplayer.player.challengerid
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


sortedRank : List UserPlayer -> List UserPlayer
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
    let
        newuserplayerplayer =
            uplayer.player

        newPlayer =
            { newuserplayerplayer
                | uid = uplayer.player.uid
                , rank = newRank
                , challengerid = uplayer.player.challengerid
            }

        newUserPlayer =
            { uplayer | player = newPlayer }
    in
    newUserPlayer


resetPlayerRankToOne : UserPlayer -> UserPlayer
resetPlayerRankToOne uplayer =
    let
        newuserplayerplayer =
            uplayer.player

        newPlayer =
            { newuserplayerplayer
                | uid = uplayer.player.uid
                , rank = 1
                , challengerid = uplayer.player.challengerid
            }

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




gotRankingOwnerAsPlayer : String -> List UserPlayer -> Data.Players.Player
gotRankingOwnerAsPlayer selectedRanking luplayer =
    let
        m_userPlayer = (gotUserPlayerFromPlayerListStrAddress luplayer selectedRanking)
    in
        case m_userPlayer of
            Nothing ->
                Data.Players.Player "" "" 0 ""
            Just userPlayer ->
                userPlayer.player


    -- case existingUser of
    --     Nothing ->
    --         emptyUserPlayer

    --     Just a ->
    --         a

filterPlayerOutOfPlayerList : String -> List UserPlayer -> List UserPlayer
filterPlayerOutOfPlayerList addr lplayer =
    List.filterMap
        (doesPlayerAddrNOTMatchAddr
            addr
        )
        lplayer

doesPlayerAddrNOTMatchAddr : String -> UserPlayer -> Maybe UserPlayer
doesPlayerAddrNOTMatchAddr addr player =
    if player.player.uid /= addr then
        Just player

    else
        Nothing


-- extractPlayersFromWebData : RemoteData.WebData (List Data.Players.Player) -> List Data.Players.Player
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

convertUserPlayersToPlayers : List UserPlayer -> List Data.Players.Player
convertUserPlayersToPlayers luplayers =
    List.map Data.Players.refEachPlayer luplayers

asUsers : Selected -> Data.Users.Users 
asUsers sSelected =
    (EverySet.fromList (List.map refEachUser (asList sSelected)))
    |> Data.Users.asUsers

refEachUser : UserPlayer -> Data.Users.User
refEachUser uplayer =
    uplayer.user

convertPlayersToUserPlayers : List Data.Players.Player -> List Data.Users.User -> List UserPlayer
convertPlayersToUserPlayers lplayer luser =
    List.map (convertEachPlayerToUserPlayer luser) lplayer


convertEachPlayerToUserPlayer : List Data.Users.User -> Data.Players.Player -> UserPlayer
convertEachPlayerToUserPlayer luser player =
    let
        user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) player.uid
    in
        case user of 
            Nothing ->
                { player = player, user = (Data.Users.Guest) }
            Just userVal ->
                { player = player, user = userVal }
                
    --{ player = player, user = Data.Users.gotUser (Data.Users.asUsers (EverySet.fromList luser)) player.uid }


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
                        
                            
                        updatedUserPlayer = Data.AppState.releasePlayerForUI (Data.AppState.updateAppState (Just appInfo.user) 
                            --appInfo.player appInfo.challenger (Data.Rankings.stringToRankingId appInfo.selectedRanking.id_))
                            appInfo.player appInfo.challenger appInfo.selectedRanking.id_)

                        newAppInfo =
                           
                            { appInfo | player = updatedUserPlayer, challenger = emptyUserPlayer }
           
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
                            { newUserPlayerPlayer | uid = "" }

                        newAppInfoPlayer =
                            appInfo.player

                        newAppInfoUserPlayer =
                            { newAppInfoPlayer | player = newUserPlayerPlayerUpdated }

                        newAppInfo =
                            { appInfo | player = newAppInfoUserPlayer, challenger = emptyUserPlayer }

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
                    { newUserPlayerPlayer | uid = "" }

                newUserPlayerUpdated =
                    { newUserPlayer | player = newPlayerUpdated }

                newAppInfo =
                    { appInfo | player = newUserPlayerUpdated, challenger = emptyUserPlayer }

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
                    { newUserPlayerPlayer | uid = "" }

                newUserPlayerUpdated =
                    { newUserPlayer | player = newUserPlayerPlayerUpdated }

                newAppInfo =
                    { appInfo | player = newUserPlayerUpdated, challenger = emptyUserPlayer }

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
                    { newUserPlayerPlayer | uid = "" }

                newUserPlayerUpdated =
                    { newUserPlayer | player = newPlayerPlayerUpdated }

                newAppInfo =
                    { appInfo | player = newUserPlayerUpdated, challenger = emptyUserPlayer }

            in
            (supdatedPlayerAndChallenger, newAppInfo)


isOpponentHigherRank : UserPlayer -> SR.Types.Opponent -> SR.Types.OpponentRelativeRank
isOpponentHigherRank uplayer opponent =
    -- nb. if player rank is 'higher' than opponent his rank integer will actually be 'less than' opponent
    -- we go by the integer ...
    if uplayer.player.rank > opponent.player.rank then
        SR.Types.OpponentRankHigher

    else
        SR.Types.OpponentRankLower

jsonEncodeNewSelectedRankingPlayerList : List UserPlayer -> Json.Encode.Value
jsonEncodeNewSelectedRankingPlayerList luplayers =
    let
        lplayers =
            convertUserPlayersToPlayers luplayers

        encodePlayerObj : Data.Players.Player -> Json.Encode.Value
        encodePlayerObj player =
            Json.Encode.object
                [ ( "uid", Json.Encode.string (String.toLower player.uid |> Debug.log "player.uid: ") )
                , ( "rank", Json.Encode.int player.rank )
                , ( "challengerid", Json.Encode.string (player.challengerid |> Debug.log "challenger id: "))
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
                sUserUpdated = assignChallengerAddr (asSelected sselected rnkId status sPlayers) appInfo.player appInfo.challenger.player.uid
                sChallengerUpdated = assignChallengerAddr sUserUpdated appInfo.challenger appInfo.player.player.uid --sUsers rnkId
            in 
                sChallengerUpdated

extractAndSortPlayerList : RemoteData.WebData (List Data.Players.Player) -> List Data.Users.User -> List UserPlayer
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


-- gotRankingOwner : Data.Rankings.Ranking -> List SR.Types.UserRanking -> List UserPlayer -> UserPlayer
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



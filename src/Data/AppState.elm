module Data.AppState exposing (AppState, updateAppState, releasePlayerForUI, releaseChallengerForUI)


--import SR.Types
import Internal.Types
--import EverySet exposing (EverySet)
import Data.Users
import Data.Selected
import Data.Players

-- First UserPlayer is current player, second is challenger
--type AppState = AppState (Maybe Data.Users.User) Data.Selected.UserPlayer Data.Selected.UserPlayer Internal.Types.RankingId
type AppState = General
    | CreateNewUser Data.Users.User
    | UpdateProfile Data.Users.User
    | CreateNewLadder Data.Users.User Internal.Types.RankingId
   
   

updateAppState : AppState -> AppState
updateAppState appState = 
    case appState of 
        General ->
            General
        CreateNewUser _ ->
            General
        UpdateProfile _ ->
            General
        CreateNewLadder _ _ ->
            General

-- updateAppInfo : AppState -> SR.Types.AppInfo 
-- updateAppInfo appState = 
--     case appState of
--         Data.AppState.AppState user uplayer uplayerChallenger rnkId ->
--             let
--                 updatedAppState = Data.AppState.releasePlayersForUI appState
--             in
            
--             Data.AppState.releasePlayersForUI appState

releasePlayerForUI : AppState -> Data.Selected.UserPlayer
releasePlayerForUI appState =
    -- todo: fix
    {player = Data.Players.Player "" "" 0 ""
    , user = Data.Users.Guest}
    -- case appState of 
    --     AppState user uplayer uplayerChallenger rnkId ->
    --         let
    --             _ = Debug.log "uplayer" uplayer
    --             newPlayer = uplayer.player
    --             updatedPlayer = {newPlayer | challengerid = ""}

    --             updatedUserPlayer = {uplayer | player = updatedPlayer}
                
    --         in
      
    --         updatedUserPlayer

releaseChallengerForUI : AppState -> Data.Selected.UserPlayer
releaseChallengerForUI appState =
    -- todo: fix
    {player = Data.Players.Player "" "" 0 ""
    , user = Data.Users.Guest}
    -- case appState of 
    --     AppState user uplayer uplayerChallenger rnkId ->
    --         let
    --             newChallenger = uplayerChallenger.player
    --             updatedChallenger = {newChallenger | challengerid = ""}

    --             updatedUplayerChallenger = {uplayerChallenger | player = updatedChallenger}
    --         in
    --         updatedUplayerChallenger





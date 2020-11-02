module Data.AppState exposing (AppState, updateAppState, releasePlayerForUI, releaseChallengerForUI)


--import SR.Types
import Internal.Types
--import EverySet exposing (EverySet)
import Data.Users
import Data.Selected

-- First UserPlayer is current player, second is challenger
type AppState = AppState (Maybe Data.Users.User) Data.Selected.UserPlayer Data.Selected.UserPlayer Internal.Types.RankingId

updateAppState : (Maybe Data.Users.User) -> Data.Selected.UserPlayer  -> Data.Selected.UserPlayer ->  Internal.Types.RankingId -> AppState
updateAppState user uplayer uplayerChallenger rnkId = 
    AppState user uplayer uplayerChallenger rnkId

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
    case appState of 
        AppState user uplayer uplayerChallenger rnkId ->
            let
                _ = Debug.log "uplayer" uplayer
                newPlayer = uplayer.player
                updatedPlayer = {newPlayer | challengerid = ""}

                updatedUserPlayer = {uplayer | player = updatedPlayer}
                
            in
      
            updatedUserPlayer

releaseChallengerForUI : AppState -> Data.Selected.UserPlayer
releaseChallengerForUI appState =
    case appState of 
        AppState user uplayer uplayerChallenger rnkId ->
            let
                newChallenger = uplayerChallenger.player
                updatedChallenger = {newChallenger | challengerid = ""}

                updatedUplayerChallenger = {uplayerChallenger | player = updatedChallenger}
            in
            updatedUplayerChallenger





-- Players will be mainly used to communicate externally to the jsonbin server
module Data.Players exposing (Players
    , gotAddress
    , validatedPlayerList
    , handleFetchedPlayers
    , extractPlayersFromWebData
    , empty
    , addPlayer
    , removePlayer
    , asList
    , asPlayers
    , playersetLength)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import Eth.Utils
import RemoteData
import Http
import List.Unique



type Players = Players (EverySet SR.Types.Player)
type PlayerNames = PlayerNames (EverySet String)


empty : Players 
empty = 
    Players (EverySet.empty)

asPlayers : EverySet SR.Types.Player -> Players 
asPlayers esPlayer  = 
    Players esPlayer

asList : Players -> List SR.Types.Player 
asList sPlayers = 
    case sPlayers of 
        Players setOfPlayers ->
            setOfPlayers
           |> EverySet.toList




addPlayer : SR.Types.Player -> Players -> Players
addPlayer player sPlayers = 
    case sPlayers of 
        Players setOfPlayers  ->
                asPlayers (EverySet.insert player setOfPlayers)


playersetLength : Players -> Int 
playersetLength (Players sPlayers) = 
    EverySet.size sPlayers


-- gotPlayer : Players  -> String -> SR.Types.Player
-- gotPlayer (Players sPlayers) uaddr =
--     let
--         existingPlayer =
--             List.head <|
--                  EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.address) == (String.toLower <| uaddr))
--                     sPlayers)
--     in
    
--     case existingPlayer of
--         Nothing ->
--             SR.Defaults.emptyPlayer

--         Just a ->
--             a




removePlayer : SR.Types.Player -> Players -> Players
removePlayer player sPlayers = 
    case sPlayers of 
        Players setOfPlayers->
           asPlayers (EverySet.remove player setOfPlayers) 


-- todo: remove?
-- gotPlayerFromPlayerList : List SR.Types.Player -> String -> SR.Types.Player
-- gotPlayerFromPlayerList lplayer uaddr =
--     let
--         existingPlayer =
--             List.head <|
--                 List.filter (\r -> (String.toLower <| r.address) == (String.toLower <| uaddr))
--                     (validatedPlayerList lplayer)
--     in
--     case existingPlayer of
--         Nothing ->
--             SR.Defaults.emptyPlayer

--         Just a ->
--             a

validatedPlayerList : List SR.Types.Player -> List SR.Types.Player
validatedPlayerList lPlayer =
    List.filterMap
        isValidPlayerAddrInList
        lPlayer


isValidPlayerAddrInList : SR.Types.Player -> Maybe SR.Types.Player
isValidPlayerAddrInList player =
    if Eth.Utils.isAddress player.address then
        Just player

    else
        Nothing




extractPlayersFromWebData : RemoteData.WebData (List SR.Types.Player) -> List SR.Types.Player
extractPlayersFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            let
                _ =
                    Debug.log "http err" "not asked"
            in
            []

        RemoteData.Loading ->
            let
                _ =
                    Debug.log "http err" "loading"
            in
            []

        RemoteData.Success players ->
            players

        RemoteData.Failure httpError ->
            let
                _ =
                    Debug.log "http err" Utils.MyUtils.gotHttpErr <| httpError
            in
            []


handleFetchedPlayers : RemoteData.WebData (List SR.Types.Player) -> (Players, String)
handleFetchedPlayers lplayer =
    case lplayer of
        RemoteData.Success a ->
            (asPlayers (EverySet.fromList a), "Success")

        RemoteData.NotAsked ->
            (empty, "Not Asked")

        RemoteData.Loading ->
            (empty, "Loading")

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    (empty, s)

                Http.Timeout ->
                    (empty, "TimeOut")

                Http.NetworkError ->
                    (empty, "Network Err")

                Http.BadStatus statuscode ->
                    (empty, (String.fromInt statuscode))

                Http.BadBody s ->
                    (empty, s)


gotAddress : SR.Types.Player -> String
gotAddress player =
    player.address

-- removeCurrentPlayerEntryFromPlayerList : List SR.Types.Player -> String -> List SR.Types.Player
-- removeCurrentPlayerEntryFromPlayerList lplayer uaddr =
--     List.filter (\r -> (String.toLower <| r.address) /= (String.toLower <| uaddr))
--         (validatedPlayerList lplayer)

--private

-- isPlayerInListStrAddr : List SR.Types.Player -> String -> Bool
-- isPlayerInListStrAddr lplayer uaddr =
--     let
--         gotSinglePlayerFromList =
--             gotPlayerFromPlayerList lplayer uaddr
--     in
--     if gotSinglePlayerFromList.address == "" then
--         False

--     else
--         True

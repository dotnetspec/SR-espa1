-- Players will be mainly used to communicate externally to the jsonbin server
module Data.Players exposing (Players
    , Player
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


--import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
--import Utils.MyUtils
import Eth.Utils
import RemoteData
import Http
import List.Unique
import SRdb.Scalar exposing (Id(..))
import SRdb.ScalarCodecs



type Players = Players (EverySet Player)
type PlayerNames = PlayerNames (EverySet String)

type alias Player =
    { rankingid : String
    , uid : String
    , rank : Int
    , challengerid : String
    }

type alias FPlayer =
    { id_ : SRdb.ScalarCodecs.Id
    , rankingid : String
    , address : String
    , rank : Int
    , challengerid : String
    }

newPlayer : FPlayer -> Player 
newPlayer fplayer = 
    Player (fromScalarCodecId fplayer.id_) fplayer.address fplayer.rank fplayer.challengerid

fromScalarCodecId : SRdb.ScalarCodecs.Id -> String
fromScalarCodecId (Id id) =
    id


empty : Players 
empty = 
    Players (EverySet.empty)

asPlayers : EverySet Player -> Players 
asPlayers esPlayer  = 
    Players esPlayer

asList : Players -> List Player 
asList sPlayers = 
    case sPlayers of 
        Players setOfPlayers ->
            setOfPlayers
           |> EverySet.toList




addPlayer : Player -> Players -> Players
addPlayer player sPlayers = 
    case sPlayers of 
        Players setOfPlayers  ->
                asPlayers (EverySet.insert player setOfPlayers)


playersetLength : Players -> Int 
playersetLength (Players sPlayers) = 
    EverySet.size sPlayers


-- gotPlayer : Players  -> String -> Player
-- gotPlayer (Players sPlayers) uaddr =
--     let
--         existingPlayer =
--             List.head <|
--                  EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.address) == (String.toLower <| uaddr))
--                     sPlayers)
--     in
    
--     case existingPlayer of
--         Nothing ->
--             Player "" "" 0 ""

--         Just a ->
--             a




removePlayer : Player -> Players -> Players
removePlayer player sPlayers = 
    case sPlayers of 
        Players setOfPlayers->
           asPlayers (EverySet.remove player setOfPlayers) 


-- todo: remove?
-- gotPlayerFromPlayerList : List Player -> String -> Player
-- gotPlayerFromPlayerList lplayer uaddr =
--     let
--         existingPlayer =
--             List.head <|
--                 List.filter (\r -> (String.toLower <| r.address) == (String.toLower <| uaddr))
--                     (validatedPlayerList lplayer)
--     in
--     case existingPlayer of
--         Nothing ->
--             Player "" "" 0 ""

--         Just a ->
--             a

validatedPlayerList : List Player -> List Player
validatedPlayerList lPlayer =
    List.filterMap
        isValidPlayerAddrInList
        lPlayer


isValidPlayerAddrInList : Player -> Maybe Player
isValidPlayerAddrInList player =
    if Eth.Utils.isAddress player.uid then
        Just player

    else
        Nothing




extractPlayersFromWebData : RemoteData.WebData (List Player) -> List Player
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
            []


handleFetchedPlayers : RemoteData.WebData (List Player) -> (Players, String)
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


gotAddress : Player -> String
gotAddress player =
    player.uid

-- removeCurrentPlayerEntryFromPlayerList : List Player -> String -> List Player
-- removeCurrentPlayerEntryFromPlayerList lplayer uaddr =
--     List.filter (\r -> (String.toLower <| r.address) /= (String.toLower <| uaddr))
--         (validatedPlayerList lplayer)

--private

-- isPlayerInListStrAddr : List Player -> String -> Bool
-- isPlayerInListStrAddr lplayer uaddr =
--     let
--         gotSinglePlayerFromList =
--             gotPlayerFromPlayerList lplayer uaddr
--     in
--     if gotSinglePlayerFromList.address == "" then
--         False

--     else
--         True

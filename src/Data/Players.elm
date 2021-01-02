-- Players will be mainly used to communicate externally to the jsonbin server
module Data.Players exposing (Players
    , Player(..)
    , FPlayer
    , PlayerInfo
    , PlayerStatus(..)
    , gotUid
    , assignChallengerUID
    --, validatedPlayerList
    , handleFetchedPlayers
    , extractPlayersFromWebData
    , empty
    , emptyIndividualPlayer
    , addPlayer
    , removePlayer
    , asList
    , asPlayers
    , playersetLength
    , convertPlayerFromFPlayer)



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
import Data.Users

type Players = Players (EverySet Player)
type PlayerNames = PlayerNames (EverySet String)

type Player = IndividualPlayer PlayerInfo PlayerStatus

type PlayerStatus = 
    Available
    | Challenged String

type alias PlayerInfo =
    { rankingid : String
    , uid : String
    , rank : Int
    --, challengerid : String
    }

type alias FPlayer =
    { id_ : SRdb.ScalarCodecs.Id
    , rankingid : String
    , uid : String
    , rank : Int
    , challengerid : Maybe String
    }

convertPlayerFromFPlayer : FPlayer -> Player 
convertPlayerFromFPlayer fplayer = 
    IndividualPlayer (PlayerInfo (fromScalarCodecId fplayer.id_) fplayer.uid fplayer.rank)
        (Maybe.withDefault Available (Just (determinePlayerStatus fplayer.challengerid)))

determinePlayerStatus : Maybe String -> PlayerStatus 
determinePlayerStatus challengerid = 
    case challengerid of
        Nothing ->
            Available
        Just chid ->
            Challenged chid

fromScalarCodecId : SRdb.ScalarCodecs.Id -> String
fromScalarCodecId (Id id) =
    id

rankFromMaybeRank : Maybe Int -> Int
rankFromMaybeRank int =
    case int of
        Nothing ->
            0

        Just a ->
            a

empty : Players 
empty = 
    Players (EverySet.empty)

emptyIndividualPlayer : Player
emptyIndividualPlayer = 
    IndividualPlayer { rankingid = "" , uid = "", rank = 1
    } 
    Available

asPlayers : EverySet Player -> Players 
asPlayers esPlayer  = 
    Players esPlayer

asList : Players -> List Player 
asList sPlayers = 
    case sPlayers of 
        Players setOfPlayers ->
            setOfPlayers
           |> EverySet.toList

assignChallengerUID : Player -> String -> Player 
assignChallengerUID player challengerUID =
    case player of 
        IndividualPlayer playerInfo playerStatus ->
            IndividualPlayer (PlayerInfo playerInfo.rankingid  playerInfo.uid playerInfo.rank)
                (Challenged challengerUID)

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

-- validatedPlayerList : List Player -> List Player
-- validatedPlayerList lPlayer =
--     List.filterMap
--         isValidPlayerAddrInList
--         lPlayer


-- isValidPlayerAddrInList : Player -> Maybe Player
-- isValidPlayerAddrInList player =
--     if Eth.Utils.isAddress player.uid then
--         Just player

--     else
--         Nothing




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


gotUid : Player -> String
gotUid (IndividualPlayer playerInfo _)  =
    playerInfo.uid

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

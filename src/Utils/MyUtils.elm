module Utils.MyUtils exposing
    ( addressFromStringResult
    , addressToString
    , convertMaybePlayerToPlayer
    , createdMaybePlayerFromPlayer
    , extractPlayersFromWebData
    , extractRankingsFromWebData
    , extractUsersFromWebData
    , splitPlayerFieldsToCreateMaybePlayer
    , stringFromBool
    , stringFromMaybeString
    , stringFromRankingId
    , stringToRankingId
    )

import Eth.Types
import Eth.Utils
import Http
import Internal.Types
import RemoteData
import SR.Defaults
import SR.Types


stringToRankingId : String -> Internal.Types.RankingId
stringToRankingId rnkId =
    Internal.Types.RankingId rnkId


stringFromRankingId : Internal.Types.RankingId -> String
stringFromRankingId (Internal.Types.RankingId rnkId) =
    rnkId


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


extractRankingsFromWebData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
extractRankingsFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success rankings ->
            rankings

        RemoteData.Failure httpError ->
            []


extractUsersFromWebData : RemoteData.WebData (List SR.Types.User) -> List SR.Types.User
extractUsersFromWebData remData =
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

        RemoteData.Success users ->
            users

        RemoteData.Failure httpError ->
            let
                _ =
                    Debug.log "http err" gotHttpErr <| httpError
            in
            []


addressFromStringResult : String -> Internal.Types.Address
addressFromStringResult uaddr =
    let
        addrFromStringResult =
            Eth.Utils.toAddress uaddr
    in
    case addrFromStringResult of
        Result.Ok a ->
            a

        _ ->
            Internal.Types.Address ""


stringFromBool : Bool -> String
stringFromBool bool =
    if
        bool
            == True
    then
        "True"

    else
        "False"


stringFromMaybeString : Maybe String -> String
stringFromMaybeString str =
    case str of
        Nothing ->
            "Not a string"

        Just a ->
            a


splitPlayerFieldsToCreateMaybePlayer : SR.Types.Player -> Maybe SR.Types.Player
splitPlayerFieldsToCreateMaybePlayer player =
    createMaybePlayer player.address player.rank player.challengeraddress


createMaybePlayer : String -> Int -> String -> Maybe SR.Types.Player
createMaybePlayer address rank challengeraddress =
    if rank > 0 && rank < 50000 then
        Just { address = address, rank = rank, challengeraddress = challengeraddress }

    else
        Nothing


createdMaybePlayerFromPlayer : SR.Types.Player -> Maybe SR.Types.Player
createdMaybePlayerFromPlayer player =
    Just
        { address = player.address
        , rank = player.rank
        , challengeraddress = player.challengeraddress
        }


convertMaybePlayerToPlayer : Maybe SR.Types.Player -> SR.Types.Player
convertMaybePlayerToPlayer mplayer =
    case mplayer of
        Nothing ->
            SR.Defaults.emptyPlayer

        Just a ->
            { address = a.address
            , rank = a.rank
            , challengeraddress = a.challengeraddress
            }


rankFromMaybeRank : Maybe Int -> Int
rankFromMaybeRank int =
    case int of
        Nothing ->
            0

        Just a ->
            a



-- this is for Maybe Address - potential re-factor


addressToString : Maybe Eth.Types.Address -> String
addressToString addr =
    case addr of
        Nothing ->
            "No address"

        Just a ->
            Eth.Utils.addressToString a



-- internal


gotHttpErr : Http.Error -> String
gotHttpErr httperr =
    case httperr of
        Http.BadUrl s ->
            "Bad" ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Err"

        Http.BadStatus statuscode ->
            String.fromInt <| statuscode

        Http.BadBody s ->
            "BadBody " ++ s



-- addressFromMaybeAddress : Maybe Address -> Address
-- addressFromMaybeAddress addr =
--     case addr of
--         Nothing ->
--             Eth.Defaults.invalidAddress
--         Just a ->
--             a

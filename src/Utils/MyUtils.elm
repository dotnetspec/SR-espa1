module Utils.MyUtils exposing
    ( addressFromStringResult
    , addressToString
    , convertListOfMaybeToList
    , convertMaybePlayerToPlayer
    , createdMaybePlayerFromPlayer
    , extractRankinigInfoFromMaybe
    , extractUserRankinigFromMaybe
    , gotHttpErr
    , refEachPlayer
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


extractUserRankinigFromMaybe : Maybe SR.Types.UserRanking -> SR.Types.UserRanking
extractUserRankinigFromMaybe valtoextract =
    case valtoextract of
        Just a ->
            a

        Nothing ->
            SR.Defaults.emptyUserRanking


extractRankinigInfoFromMaybe : Maybe SR.Types.Ranking -> SR.Types.Ranking
extractRankinigInfoFromMaybe valtoextract =
    case valtoextract of
        Just a ->
            a

        Nothing ->
            SR.Defaults.emptyRankingInfo

convertListOfMaybeToList : List (Maybe a) -> List a
convertListOfMaybeToList hasAnything =
    let
        onlyHasRealValues =
            List.filterMap (\x -> x) hasAnything
    in
    onlyHasRealValues


stringToRankingId : String -> Internal.Types.RankingId
stringToRankingId rnkId =
    Internal.Types.RankingId rnkId


stringFromRankingId : Internal.Types.RankingId -> String
stringFromRankingId (Internal.Types.RankingId rnkId) =
    rnkId


refEachPlayer : SR.Types.UserPlayer -> SR.Types.Player
refEachPlayer uplayer =
    uplayer.player


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


splitPlayerFieldsToCreateMaybePlayer : SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
splitPlayerFieldsToCreateMaybePlayer uplayer =
    if uplayer.player.rank > 0 && uplayer.player.rank < 50000 then
        Just uplayer

    else
        Nothing


createdMaybePlayerFromPlayer : SR.Types.Player -> Maybe SR.Types.Player
createdMaybePlayerFromPlayer player =
    Just
        { address = player.address
        , rank = player.rank
        , challengeraddress = player.challengeraddress
        }


convertMaybePlayerToPlayer : Maybe SR.Types.UserPlayer -> SR.Types.UserPlayer
convertMaybePlayerToPlayer mplayer =
    case mplayer of
        Nothing ->
            SR.Defaults.emptyUserPlayer

        Just a ->
            a



-- { address = a.address
-- , rank = a.rank
-- , challengeraddress = a.challengeraddress
-- }


convertMaybeUserRankingToUserRanking : Maybe SR.Types.UserRanking -> SR.Types.UserRanking
convertMaybeUserRankingToUserRanking muranking =
    case muranking of
        Nothing ->
            SR.Defaults.emptyUserRanking

        Just a ->
            a


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

module Utils.MyUtils exposing
    ( addressFromStringResult
    , maybeAddressToString
    , convertListOfMaybeToList
    , gotHttpErr
    , stringFromBool
    , stringFromMaybeString
    , removeNothingFromList
    )

import Eth.Types
import Eth.Utils
import Http
import Internal.Types
import RemoteData
import SR.Defaults
import SR.Types


-- extractUserRankinigFromMaybe : Maybe SR.Types.UserRanking -> SR.Types.UserRanking
-- extractUserRankinigFromMaybe valtoextract =
--     case valtoextract of
--         Just a ->
--             a

--         Nothing ->
--             SR.Defaults.emptyUserRanking

-- handleResult : Result err val -> a 
-- handleResult result = 
--     case result of 
--         Ok val ->
--             val
--         Err str ->
--             str

removeNothingFromList : List (Maybe a) -> List a 
removeNothingFromList list =
    List.filterMap identity list


convertListOfMaybeToList : List (Maybe a) -> List a
convertListOfMaybeToList hasAnything =
    let
        onlyHasRealValues =
            List.filterMap (\x -> x) hasAnything
    in
    onlyHasRealValues

-- convertToRankingId : String -> Internal.Types.RankingId 
-- convertToRankingId str = 
--     Internal.Types.RankingId str

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

-- this is for Maybe Address - potential re-factor


maybeAddressToString : Maybe Eth.Types.Address -> String
maybeAddressToString addr =
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

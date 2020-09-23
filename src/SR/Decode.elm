module SR.Decode exposing ( rankingsDecoder
    , decodeNewPlayerListServerResponse
    , decodeNewRankingListServerResponse
    , decodeNewUserListServerResponse
    , ladderOfPlayersDecoder
    , listOfUsersDecoder, newRankingIdDecoder, playerDecoder, userDecoder
    , decodeDeleteBinResponse
    , decodeUpdateGlobalBinResponse
    )

{-|

@docs rankingsDecoder, rankingDecoder

-}

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, optional, required)
import SR.Types
import Eth.Utils
import Utils.Validation.Validate
import Eth.Types exposing (Address)


ladderOfPlayersDecoder : Json.Decode.Decoder (List SR.Types.Player)
ladderOfPlayersDecoder =
    Json.Decode.list playerDecoder


playerDecoder : Json.Decode.Decoder SR.Types.Player
playerDecoder =
    Json.Decode.succeed SR.Types.Player
        |> Json.Decode.Pipeline.required "address" Json.Decode.string
        |> Json.Decode.Pipeline.required "rank" Json.Decode.int
        |> Json.Decode.Pipeline.required "challengeraddress" Json.Decode.string


rankingsDecoder : Json.Decode.Decoder (List SR.Types.Ranking)
rankingsDecoder =
    Json.Decode.list rankingDecoder


rankingDecoder : Json.Decode.Decoder SR.Types.Ranking
rankingDecoder =
    Json.Decode.succeed SR.Types.Ranking
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "active" Json.Decode.bool
        |> Json.Decode.Pipeline.required "rankingname" Json.Decode.string
        |> Json.Decode.Pipeline.required "rankingdesc" Json.Decode.string
        |> Json.Decode.Pipeline.required "rankingowneraddr" Json.Decode.string


decodeNewRankingListServerResponse : Decoder (List SR.Types.Ranking)
decodeNewRankingListServerResponse =
    Json.Decode.field "data" (Json.Decode.list rankingDecoder)


newRankingIdDecoder =
    Json.Decode.succeed SR.Types.RankingId
        |> Json.Decode.Pipeline.required "id" Json.Decode.string


listOfUsersDecoder : Json.Decode.Decoder (List SR.Types.User)
listOfUsersDecoder =
    Json.Decode.list userDecoder

-- ethaddress is hardcoded because we might not use this decoder anyway
userDecoder : Json.Decode.Decoder SR.Types.User
userDecoder =
    Json.Decode.succeed SR.Types.User
        |> Json.Decode.Pipeline.required "datestamp" Json.Decode.int
        |> Json.Decode.Pipeline.required "active" Json.Decode.bool
        |> Json.Decode.Pipeline.required "username" Json.Decode.string
        |> Json.Decode.Pipeline.required "password" Json.Decode.string
        --|> Json.Decode.Pipeline.optional "ethaddress" (Json.Decode.maybe Json.Decode.string) Nothing
        |> Json.Decode.Pipeline.hardcoded Nothing
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "mobile" Json.Decode.string
        |> Json.Decode.Pipeline.required "userjoinrankings" decodeUserJoinRankingsList
        |> Json.Decode.Pipeline.required "member_since" Json.Decode.int
        |> Json.Decode.Pipeline.optional "m_token" (Json.Decode.maybe Json.Decode.string) Nothing


-- decodeUserEthAddr : Decoder String
-- decodeUserEthAddr =
-- --     -- you can apply a function to any Decode.type with e.g. (Json.Decode.string |> Json.Decode.andThen isValidRankingId)
-- --     (Json.Decode.maybe Json.Decode.string) Nothing
--     Json.Decode.maybe (Json.Decode.string |> Json.Decode.andThen isValidAddr)

decodeUserJoinRankingsList : Decoder (List String)
decodeUserJoinRankingsList =
    -- you can apply a function to any Decode.type with e.g. (Json.Decode.string |> Json.Decode.andThen isValidRankingId)
    Json.Decode.list (Json.Decode.string |> Json.Decode.andThen isValidRankingId)

isValidAddr :  Maybe Eth.Types.Address -> Decoder String
isValidAddr addr = 
    case addr of
        Nothing ->
            Json.Decode.fail ""
        Just a ->
            Json.Decode.succeed (Eth.Utils.addressToString a)
    -- if Utils.Validation.Validate.isValidRankingId str  then
    --     Json.Decode.succeed str
    -- else 
    --     Json.Decode.fail str


isValidRankingId :  String -> Decoder String
isValidRankingId str = 
    if Utils.Validation.Validate.isValidRankingId str  then
        Json.Decode.succeed str
    else 
        Json.Decode.fail str


decodeNewUserListServerResponse : Decoder (List SR.Types.User)
decodeNewUserListServerResponse =
    Json.Decode.field "data" (Json.Decode.list userDecoder)


decodeNewPlayerListServerResponse : Decoder (List SR.Types.Player)
decodeNewPlayerListServerResponse =
    Json.Decode.field "data" (Json.Decode.list playerDecoder)

decodeDeleteBinResponse : Decoder SR.Types.DeleteBinResponse
decodeDeleteBinResponse = 
    Json.Decode.succeed SR.Types.DeleteBinResponse
        |> Json.Decode.Pipeline.required "success" Json.Decode.bool
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "message" Json.Decode.string

decodeUpdateGlobalBinResponse : Decoder SR.Types.UpdateGlobalBinResponse
decodeUpdateGlobalBinResponse = 
    Json.Decode.succeed SR.Types.UpdateGlobalBinResponse
        |> Json.Decode.Pipeline.required "success" Json.Decode.bool
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list rankingDecoder)
        |> Json.Decode.Pipeline.required "version" Json.Decode.int
        |> Json.Decode.Pipeline.required "parentId" Json.Decode.string

module SR.Decode exposing
    ( rankingsDecoder
    , decodeNewPlayerListServerResponse, decodeNewRankingListServerResponse, decodeNewUserListServerResponse, ladderOfPlayersDecoder, listOfUsersDecoder, newRankingIdDecoder, playerDecoder, userDecoder
    )

{-|

@docs rankingsDecoder, rankingDecoder

-}

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, optional, required)
import SR.Types
import Eth.Utils
import Utils.Validation.Validate


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


userDecoder : Json.Decode.Decoder SR.Types.User
userDecoder =
    Json.Decode.succeed SR.Types.User
        |> Json.Decode.Pipeline.required "datestamp" Json.Decode.int
        |> Json.Decode.Pipeline.required "active" Json.Decode.bool
        |> Json.Decode.Pipeline.required "username" Json.Decode.string
        |> Json.Decode.Pipeline.required "ethaddress" Json.Decode.string
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "mobile" Json.Decode.string
        |> Json.Decode.Pipeline.required "userjoinrankings" decodeUserJoinRankingsList


decodeUserJoinRankingsList : Decoder (List String)
decodeUserJoinRankingsList =
    Json.Decode.list (Json.Decode.string |> Json.Decode.andThen isValidRankingId)


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

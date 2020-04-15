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


ladderOfPlayersDecoder : Json.Decode.Decoder (List SR.Types.Player)
ladderOfPlayersDecoder =
    Json.Decode.list playerDecoder


playerDecoder : Json.Decode.Decoder SR.Types.Player
playerDecoder =
    Json.Decode.succeed SR.Types.Player
        |> Json.Decode.Pipeline.required "datestamp" Json.Decode.int
        |> Json.Decode.Pipeline.required "active" Json.Decode.bool
        |> Json.Decode.Pipeline.required "address" Json.Decode.string
        |> Json.Decode.Pipeline.required "rank" Json.Decode.int
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "id" Json.Decode.int
        |> Json.Decode.Pipeline.required "isplayercurrentlychallenged" Json.Decode.bool
        |> Json.Decode.Pipeline.required "challengeraddress" Json.Decode.string


rankingsDecoder : Json.Decode.Decoder (List SR.Types.RankingInfo)
rankingsDecoder =
    Json.Decode.list rankingDecoder


rankingDecoder : Json.Decode.Decoder SR.Types.RankingInfo
rankingDecoder =
    Json.Decode.succeed SR.Types.RankingInfo
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "active" Json.Decode.bool
        |> Json.Decode.Pipeline.required "rankingname" Json.Decode.string
        |> Json.Decode.Pipeline.required "rankingdesc" Json.Decode.string
        |> Json.Decode.Pipeline.required "rankingowneraddr" Json.Decode.string


decodeNewRankingListServerResponse : Decoder (List SR.Types.RankingInfo)
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


decodeNewUserListServerResponse : Decoder (List SR.Types.User)
decodeNewUserListServerResponse =
    Json.Decode.field "data" (Json.Decode.list userDecoder)


decodeNewPlayerListServerResponse : Decoder (List SR.Types.Player)
decodeNewPlayerListServerResponse =
    Json.Decode.field "data" (Json.Decode.list playerDecoder)

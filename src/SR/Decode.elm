module SR.Decode exposing
    ( rankingsDecoder
    , ladderOfPlayersDecoder, newRankingDecoder, newRankingIdDecoder, newRankingListDecoder, playerDecoder
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
        |> Json.Decode.Pipeline.required "DATESTAMP" Json.Decode.int
        |> Json.Decode.Pipeline.required "ACTIVE" Json.Decode.bool
        |> Json.Decode.Pipeline.required "CURRENTCHALLENGERNAME" Json.Decode.string
        |> Json.Decode.Pipeline.required "CURRENTCHALLENGERID" Json.Decode.int
        |> Json.Decode.Pipeline.required "ADDRESS" Json.Decode.string
        |> Json.Decode.Pipeline.required "RANK" Json.Decode.int
        |> Json.Decode.Pipeline.required "NAME" Json.Decode.string
        |> Json.Decode.Pipeline.required "PLAYERID" Json.Decode.int
        |> Json.Decode.Pipeline.required "CURRENTCHALLENGERADDRESS" Json.Decode.string


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


newRankingListDecoder : Json.Decode.Decoder (List SR.Types.RankingInfo)
newRankingListDecoder =
    Json.Decode.list newRankingDecoder


newRankingDecoder : Json.Decode.Decoder SR.Types.RankingInfo
newRankingDecoder =
    Json.Decode.succeed SR.Types.RankingInfo
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "active" Json.Decode.bool
        |> Json.Decode.Pipeline.required "rankingname" Json.Decode.string
        |> Json.Decode.Pipeline.required "rankingdesc" Json.Decode.string


newRankingIdDecoder : Json.Decode.Decoder SR.Types.RankingId
newRankingIdDecoder =
    Json.Decode.succeed SR.Types.RankingId
        |> Json.Decode.Pipeline.required "id" Json.Decode.string

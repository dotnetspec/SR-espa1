module SR.Decode exposing (rankingsDecoder, rankingDecoder)
{-|

@docs rankingsDecoder, rankingDecoder

-}

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, optional, required)
import SR.Types


rankingsDecoder : Json.Decode.Decoder (List SR.Types.Ranking)
rankingsDecoder =
    Json.Decode.list rankingDecoder


rankingDecoder : Json.Decode.Decoder SR.Types.Ranking
rankingDecoder =
    Json.Decode.succeed SR.Types.Ranking
        |> Json.Decode.Pipeline.required "RANKINGID" Json.Decode.string
        |> Json.Decode.Pipeline.required "ACTIVE" Json.Decode.bool
        |> Json.Decode.Pipeline.required "RANKINGNAME" Json.Decode.string
        |> Json.Decode.Pipeline.required "RANKINGDESC" Json.Decode.string
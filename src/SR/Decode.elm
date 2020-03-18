module SR.Decode exposing
    ( rankingsDecoder
    ,  decodeNewRankingListServerResponse
      , ladderOfPlayersDecoder
      , listOfUsersDecoder
        -- ,
        -- newRankingDecoder
      , newRankingIdDecoder
      , playerDecoder
      , userDecoder

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
        |> Json.Decode.Pipeline.required "rankingowneraddr" Json.Decode.string


decodeNewRankingListServerResponse : Decoder (List SR.Types.RankingInfo)
decodeNewRankingListServerResponse =
    Json.Decode.field "data" (Json.Decode.list rankingDecoder)



-- newRankingDecoder : Json.Decode.Decoder SR.Types.RankingInfo
-- newRankingDecoder =
--     Json.Decode.succeed SR.Types.RankingInfo
--         |> Json.Decode.Pipeline.required "id" Json.Decode.string
--         |> Json.Decode.Pipeline.required "active" Json.Decode.bool
--         |> Json.Decode.Pipeline.required "rankingname" Json.Decode.string
--         |> Json.Decode.Pipeline.required "rankingdesc" Json.Decode.string
-- newRankingIdDecoder : Json.Decode.Decoder SR.Types.RankingId


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

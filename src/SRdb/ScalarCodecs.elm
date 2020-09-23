-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SRdb.ScalarCodecs exposing (..)

import Json.Decode as Decode exposing (Decoder)
import SRdb.Scalar exposing (defaultCodecs)


type alias Date =
    SRdb.Scalar.Date


type alias Id =
    SRdb.Scalar.Id


type alias Long =
    SRdb.Scalar.Long


type alias Time =
    SRdb.Scalar.Time


codecs : SRdb.Scalar.Codecs Date Id Long Time
codecs =
    SRdb.Scalar.defineCodecs
        { codecDate = defaultCodecs.codecDate
        , codecId = defaultCodecs.codecId
        , codecLong = defaultCodecs.codecLong
        , codecTime = defaultCodecs.codecTime
        }
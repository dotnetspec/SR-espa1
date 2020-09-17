-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SRdb.Object.UserJoinedRanking exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import SRdb.InputObject
import SRdb.Interface
import SRdb.Object
import SRdb.Scalar
import SRdb.ScalarCodecs
import SRdb.Union


rankingid : SelectionSet String SRdb.Object.UserJoinedRanking
rankingid =
    Object.selectionForField "String" "rankingid" [] Decode.string


rankingowneraddr : SelectionSet String SRdb.Object.UserJoinedRanking
rankingowneraddr =
    Object.selectionForField "String" "rankingowneraddr" [] Decode.string


{-| The document's ID.
-}
id_ : SelectionSet SRdb.ScalarCodecs.Id SRdb.Object.UserJoinedRanking
id_ =
    Object.selectionForField "ScalarCodecs.Id" "_id" [] (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapCodecs |> .codecId |> .decoder)


rankingname : SelectionSet String SRdb.Object.UserJoinedRanking
rankingname =
    Object.selectionForField "String" "rankingname" [] Decode.string


useraddr : SelectionSet String SRdb.Object.UserJoinedRanking
useraddr =
    Object.selectionForField "String" "useraddr" [] Decode.string


active : SelectionSet Bool SRdb.Object.UserJoinedRanking
active =
    Object.selectionForField "Bool" "active" [] Decode.bool


{-| The document's timestamp.
-}
ts_ : SelectionSet SRdb.ScalarCodecs.Long SRdb.Object.UserJoinedRanking
ts_ =
    Object.selectionForField "ScalarCodecs.Long" "_ts" [] (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapCodecs |> .codecLong |> .decoder)

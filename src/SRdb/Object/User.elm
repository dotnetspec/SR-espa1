-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SRdb.Object.User exposing (..)

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


email : SelectionSet (Maybe String) SRdb.Object.User
email =
    Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


username : SelectionSet String SRdb.Object.User
username =
    Object.selectionForField "String" "username" [] Decode.string


description : SelectionSet (Maybe String) SRdb.Object.User
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


{-| The document's ID.
-}
id_ : SelectionSet SRdb.ScalarCodecs.Id SRdb.Object.User
id_ =
    Object.selectionForField "ScalarCodecs.Id" "_id" [] (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapCodecs |> .codecId |> .decoder)


ethaddress : SelectionSet String SRdb.Object.User
ethaddress =
    Object.selectionForField "String" "ethaddress" [] Decode.string


member_since : SelectionSet Int SRdb.Object.User
member_since =
    Object.selectionForField "Int" "member_since" [] Decode.int


mobile : SelectionSet (Maybe String) SRdb.Object.User
mobile =
    Object.selectionForField "(Maybe String)" "mobile" [] (Decode.string |> Decode.nullable)


active : SelectionSet Bool SRdb.Object.User
active =
    Object.selectionForField "Bool" "active" [] Decode.bool


password : SelectionSet String SRdb.Object.User
password =
    Object.selectionForField "String" "password" [] Decode.string


{-| The document's timestamp.
-}
ts_ : SelectionSet SRdb.ScalarCodecs.Long SRdb.Object.User
ts_ =
    Object.selectionForField "ScalarCodecs.Long" "_ts" [] (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapCodecs |> .codecLong |> .decoder)

-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SRdb.Object.LoginResult exposing (..)

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


token : SelectionSet (Maybe String) SRdb.Object.LoginResult
token =
    Object.selectionForField "(Maybe String)" "token" [] (Decode.string |> Decode.nullable)


logginUser :
    SelectionSet decodesTo SRdb.Object.User
    -> SelectionSet (Maybe decodesTo) SRdb.Object.LoginResult
logginUser object_ =
    Object.selectionForCompositeField "logginUser" [] object_ (identity >> Decode.nullable)

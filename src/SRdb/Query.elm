-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SRdb.Query exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import SRdb.InputObject
import SRdb.Interface
import SRdb.Object
import SRdb.Scalar
import SRdb.ScalarCodecs
import SRdb.Union


allPlayerAddresses : SelectionSet (List String) RootQuery
allPlayerAddresses =
    Object.selectionForField "(List String)" "allPlayerAddresses" [] (Decode.string |> Decode.list)


allRankings :
    SelectionSet decodesTo SRdb.Object.Ranking
    -> SelectionSet (Maybe (List (Maybe decodesTo))) RootQuery
allRankings object_ =
    Object.selectionForCompositeField "allRankings" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias FindUserJoinedRankingByIDRequiredArguments =
    { id : SRdb.ScalarCodecs.Id }


{-| Find a document from the collection of 'UserJoinedRanking' by its id.

  - id - The 'UserJoinedRanking' document's ID

-}
findUserJoinedRankingByID :
    FindUserJoinedRankingByIDRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.UserJoinedRanking
    -> SelectionSet (Maybe decodesTo) RootQuery
findUserJoinedRankingByID requiredArgs object_ =
    Object.selectionForCompositeField "findUserJoinedRankingByID" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


allPlayerChallengerAddresses : SelectionSet (List String) RootQuery
allPlayerChallengerAddresses =
    Object.selectionForField "(List String)" "allPlayerChallengerAddresses" [] (Decode.string |> Decode.list)


allPlayerRanks : SelectionSet (List Int) RootQuery
allPlayerRanks =
    Object.selectionForField "(List Int)" "allPlayerRanks" [] (Decode.int |> Decode.list)


allUserJoinedRanking :
    SelectionSet decodesTo SRdb.Object.UserJoinedRanking
    -> SelectionSet (Maybe (List (Maybe decodesTo))) RootQuery
allUserJoinedRanking object_ =
    Object.selectionForCompositeField "allUserJoinedRanking" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


allUsers :
    SelectionSet decodesTo SRdb.Object.User
    -> SelectionSet (Maybe (List (Maybe decodesTo))) RootQuery
allUsers object_ =
    Object.selectionForCompositeField "allUsers" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias FindUserByIDRequiredArguments =
    { id : SRdb.ScalarCodecs.Id }


{-| Find a document from the collection of 'User' by its id.

  - id - The 'User' document's ID

-}
findUserByID :
    FindUserByIDRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.User
    -> SelectionSet (Maybe decodesTo) RootQuery
findUserByID requiredArgs object_ =
    Object.selectionForCompositeField "findUserByID" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


type alias LoginUserRequiredArguments =
    { username : String
    , password : String
    }


loginUser :
    LoginUserRequiredArguments
    ---> SelectionSet (List String) RootQuery
    -> SelectionSet String RootQuery
loginUser requiredArgs =
    --Object.selectionForField "(List String)" "loginUser" [ Argument.required "username" requiredArgs.username Encode.string, Argument.required "password" requiredArgs.password Encode.string ] (Decode.string |> Decode.list)
    Object.selectionForField "String" "loginUser" [ Argument.required "username" requiredArgs.username Encode.string, Argument.required "password" requiredArgs.password Encode.string ] Decode.string
    --Object.selectionForField "String" "createAndOrLoginUser" (optionalArgs ++ [ Argument.required "active" requiredArgs.active Encode.bool, Argument.required "username" requiredArgs.username Encode.string, Argument.required "password" requiredArgs.password Encode.string, Argument.required "ethaddress" requiredArgs.ethaddress Encode.string ]) Decode.string


allPlayers :
    SelectionSet decodesTo SRdb.Object.Player
    -> SelectionSet (Maybe (List (Maybe decodesTo))) RootQuery
allPlayers object_ =
    Object.selectionForCompositeField "allPlayers" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias UsernameFromEthaddressRequiredArguments =
    { ethaddress : String }


usernameFromEthaddress :
    UsernameFromEthaddressRequiredArguments
    -> SelectionSet (List String) RootQuery
usernameFromEthaddress requiredArgs =
    Object.selectionForField "(List String)" "usernameFromEthaddress" [ Argument.required "ethaddress" requiredArgs.ethaddress Encode.string ] (Decode.string |> Decode.list)


type alias FindRankingByIDRequiredArguments =
    { id : SRdb.ScalarCodecs.Id }


{-| Find a document from the collection of 'Ranking' by its id.

  - id - The 'Ranking' document's ID

-}
findRankingByID :
    FindRankingByIDRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Ranking
    -> SelectionSet (Maybe decodesTo) RootQuery
findRankingByID requiredArgs object_ =
    Object.selectionForCompositeField "findRankingByID" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


type alias FindRankingByIdRequiredArguments =
    { rankingid : Int }


findRankingById :
    FindRankingByIdRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Ranking
    -> SelectionSet (List decodesTo) RootQuery
findRankingById requiredArgs object_ =
    Object.selectionForCompositeField "findRankingById" [ Argument.required "rankingid" requiredArgs.rankingid Encode.int ] object_ (identity >> Decode.list)


type alias FindPlayerByIDRequiredArguments =
    { id : SRdb.ScalarCodecs.Id }


{-| Find a document from the collection of 'Player' by its id.

  - id - The 'Player' document's ID

-}
findPlayerByID :
    FindPlayerByIDRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Player
    -> SelectionSet (Maybe decodesTo) RootQuery
findPlayerByID requiredArgs object_ =
    Object.selectionForCompositeField "findPlayerByID" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


allUserNames : SelectionSet (List String) RootQuery
allUserNames =
    Object.selectionForField "(List String)" "allUserNames" [] (Decode.string |> Decode.list)

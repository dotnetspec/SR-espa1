-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SRdb.Mutation exposing (..)

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


type alias UpdateUserRequiredArguments =
    { id : SRdb.ScalarCodecs.Id
    , data : SRdb.InputObject.UserInput
    }


{-| Update an existing document in the collection of 'User'

  - id - The 'User' document's ID
  - data - 'User' input values

-}
updateUser :
    UpdateUserRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.User
    -> SelectionSet (Maybe decodesTo) RootMutation
updateUser requiredArgs object_ =
    Object.selectionForCompositeField "updateUser" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId), Argument.required "data" requiredArgs.data SRdb.InputObject.encodeUserInput ] object_ (identity >> Decode.nullable)


type alias CreateNewUserOptionalArguments =
    { description : OptionalArgument String
    , email : OptionalArgument String
    , mobile : OptionalArgument String
    }


type alias CreateNewUserRequiredArguments =
    { active : Bool
    , username : String
    , password : String
    }


createNewUser :
    (CreateNewUserOptionalArguments -> CreateNewUserOptionalArguments)
    -> CreateNewUserRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.LoginResult
    -> SelectionSet decodesTo RootMutation
createNewUser fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { description = Absent, email = Absent, mobile = Absent }

        optionalArgs =
            [ Argument.optional "description" filledInOptionals.description Encode.string, Argument.optional "email" filledInOptionals.email Encode.string, Argument.optional "mobile" filledInOptionals.mobile Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "createNewUser" (optionalArgs ++ [ Argument.required "active" requiredArgs.active Encode.bool, Argument.required "username" requiredArgs.username Encode.string, Argument.required "password" requiredArgs.password Encode.string ]) object_ identity


type alias CreateUserRequiredArguments =
    { data : SRdb.InputObject.UserInput }


{-| Create a new document in the collection of 'User'

  - data - 'User' input values

-}
createUser :
    CreateUserRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.User
    -> SelectionSet decodesTo RootMutation
createUser requiredArgs object_ =
    Object.selectionForCompositeField "createUser" [ Argument.required "data" requiredArgs.data SRdb.InputObject.encodeUserInput ] object_ identity


type alias UpdatePlayerRequiredArguments =
    { id : SRdb.ScalarCodecs.Id
    , data : SRdb.InputObject.PlayerInput
    }


{-| Update an existing document in the collection of 'Player'

  - id - The 'Player' document's ID
  - data - 'Player' input values

-}
updatePlayer :
    UpdatePlayerRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Player
    -> SelectionSet (Maybe decodesTo) RootMutation
updatePlayer requiredArgs object_ =
    Object.selectionForCompositeField "updatePlayer" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId), Argument.required "data" requiredArgs.data SRdb.InputObject.encodePlayerInput ] object_ (identity >> Decode.nullable)


type alias DeleteRankingRequiredArguments =
    { id : SRdb.ScalarCodecs.Id }


{-| Delete an existing document in the collection of 'Ranking'

  - id - The 'Ranking' document's ID

-}
deleteRanking :
    DeleteRankingRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Ranking
    -> SelectionSet (Maybe decodesTo) RootMutation
deleteRanking requiredArgs object_ =
    Object.selectionForCompositeField "deleteRanking" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


type alias CreateNewUserJoinedRankingRequiredArguments =
    { active : Bool
    , rankingid : String
    , rankingname : String
    , useraddr : String
    , rankingownerid : String
    }


createNewUserJoinedRanking :
    CreateNewUserJoinedRankingRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.UserJoinedRanking
    -> SelectionSet decodesTo RootMutation
createNewUserJoinedRanking requiredArgs object_ =
    Object.selectionForCompositeField "createNewUserJoinedRanking" [ Argument.required "active" requiredArgs.active Encode.bool, Argument.required "rankingid" requiredArgs.rankingid Encode.string, Argument.required "rankingname" requiredArgs.rankingname Encode.string, Argument.required "useraddr" requiredArgs.useraddr Encode.string, Argument.required "rankingownerid" requiredArgs.rankingownerid Encode.string ] object_ identity


type alias UpdateRankingRequiredArguments =
    { id : SRdb.ScalarCodecs.Id
    , data : SRdb.InputObject.RankingInput
    }


{-| Update an existing document in the collection of 'Ranking'

  - id - The 'Ranking' document's ID
  - data - 'Ranking' input values

-}
updateRanking :
    UpdateRankingRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Ranking
    -> SelectionSet (Maybe decodesTo) RootMutation
updateRanking requiredArgs object_ =
    Object.selectionForCompositeField "updateRanking" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId), Argument.required "data" requiredArgs.data SRdb.InputObject.encodeRankingInput ] object_ (identity >> Decode.nullable)


type alias CreateNewRankingOptionalArguments =
    { rankingdesc : OptionalArgument String }


type alias CreateNewRankingRequiredArguments =
    { active : Bool
    , rankingname : String
    , rankingownerid : String
    }


createNewRanking :
    (CreateNewRankingOptionalArguments -> CreateNewRankingOptionalArguments)
    -> CreateNewRankingRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Ranking
    -> SelectionSet decodesTo RootMutation
createNewRanking fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { rankingdesc = Absent }

        optionalArgs =
            [ Argument.optional "rankingdesc" filledInOptionals.rankingdesc Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "createNewRanking" (optionalArgs ++ [ Argument.required "active" requiredArgs.active Encode.bool, Argument.required "rankingname" requiredArgs.rankingname Encode.string, Argument.required "rankingownerid" requiredArgs.rankingownerid Encode.string ]) object_ identity


type alias CreatePlayerRequiredArguments =
    { data : SRdb.InputObject.PlayerInput }


{-| Create a new document in the collection of 'Player'

  - data - 'Player' input values

-}
createPlayer :
    CreatePlayerRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Player
    -> SelectionSet decodesTo RootMutation
createPlayer requiredArgs object_ =
    Object.selectionForCompositeField "createPlayer" [ Argument.required "data" requiredArgs.data SRdb.InputObject.encodePlayerInput ] object_ identity


type alias CreateNewPlayerOptionalArguments =
    { challengerid : OptionalArgument String }


type alias CreateNewPlayerRequiredArguments =
    { rankingid : String
    , uid : String
    , rank : Int
    }


createNewPlayer :
    (CreateNewPlayerOptionalArguments -> CreateNewPlayerOptionalArguments)
    -> CreateNewPlayerRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Player
    -> SelectionSet decodesTo RootMutation
createNewPlayer fillInOptionals requiredArgs object_ =
    let
        filledInOptionals =
            fillInOptionals { challengerid = Absent }

        optionalArgs =
            [ Argument.optional "challengerid" filledInOptionals.challengerid Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "createNewPlayer" (optionalArgs ++ [ Argument.required "rankingid" requiredArgs.rankingid Encode.string, Argument.required "uid" requiredArgs.uid Encode.string, Argument.required "rank" requiredArgs.rank Encode.int ]) object_ identity


type alias DeleteUserRequiredArguments =
    { id : SRdb.ScalarCodecs.Id }


{-| Delete an existing document in the collection of 'User'

  - id - The 'User' document's ID

-}
deleteUser :
    DeleteUserRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.User
    -> SelectionSet (Maybe decodesTo) RootMutation
deleteUser requiredArgs object_ =
    Object.selectionForCompositeField "deleteUser" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


type alias DeleteUserJoinedRankingRequiredArguments =
    { id : SRdb.ScalarCodecs.Id }


{-| Delete an existing document in the collection of 'UserJoinedRanking'

  - id - The 'UserJoinedRanking' document's ID

-}
deleteUserJoinedRanking :
    DeleteUserJoinedRankingRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.UserJoinedRanking
    -> SelectionSet (Maybe decodesTo) RootMutation
deleteUserJoinedRanking requiredArgs object_ =
    Object.selectionForCompositeField "deleteUserJoinedRanking" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


type alias CreateAndOrLoginUserOptionalArguments =
    { description : OptionalArgument String
    , email : OptionalArgument String
    , mobile : OptionalArgument String
    }


type alias CreateAndOrLoginUserRequiredArguments =
    { active : Bool
    , username : String
    , password : String
    }


createAndOrLoginUser :
    (CreateAndOrLoginUserOptionalArguments -> CreateAndOrLoginUserOptionalArguments)
    -> CreateAndOrLoginUserRequiredArguments
    -> SelectionSet String RootMutation
createAndOrLoginUser fillInOptionals requiredArgs =
    let
        filledInOptionals =
            fillInOptionals { description = Absent, email = Absent, mobile = Absent }

        optionalArgs =
            [ Argument.optional "description" filledInOptionals.description Encode.string, Argument.optional "email" filledInOptionals.email Encode.string, Argument.optional "mobile" filledInOptionals.mobile Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForField "String" "createAndOrLoginUser" (optionalArgs ++ [ Argument.required "active" requiredArgs.active Encode.bool, Argument.required "username" requiredArgs.username Encode.string, Argument.required "password" requiredArgs.password Encode.string ]) Decode.string


type alias CreateRankingRequiredArguments =
    { data : SRdb.InputObject.RankingInput }


{-| Create a new document in the collection of 'Ranking'

  - data - 'Ranking' input values

-}
createRanking :
    CreateRankingRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Ranking
    -> SelectionSet decodesTo RootMutation
createRanking requiredArgs object_ =
    Object.selectionForCompositeField "createRanking" [ Argument.required "data" requiredArgs.data SRdb.InputObject.encodeRankingInput ] object_ identity


type alias DeletePlayerRequiredArguments =
    { id : SRdb.ScalarCodecs.Id }


{-| Delete an existing document in the collection of 'Player'

  - id - The 'Player' document's ID

-}
deletePlayer :
    DeletePlayerRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.Player
    -> SelectionSet (Maybe decodesTo) RootMutation
deletePlayer requiredArgs object_ =
    Object.selectionForCompositeField "deletePlayer" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) ] object_ (identity >> Decode.nullable)


type alias UpdateUserJoinedRankingRequiredArguments =
    { id : SRdb.ScalarCodecs.Id
    , data : SRdb.InputObject.UserJoinedRankingInput
    }


{-| Update an existing document in the collection of 'UserJoinedRanking'

  - id - The 'UserJoinedRanking' document's ID
  - data - 'UserJoinedRanking' input values

-}
updateUserJoinedRanking :
    UpdateUserJoinedRankingRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.UserJoinedRanking
    -> SelectionSet (Maybe decodesTo) RootMutation
updateUserJoinedRanking requiredArgs object_ =
    Object.selectionForCompositeField "updateUserJoinedRanking" [ Argument.required "id" requiredArgs.id (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId), Argument.required "data" requiredArgs.data SRdb.InputObject.encodeUserJoinedRankingInput ] object_ (identity >> Decode.nullable)


type alias CreateUserJoinedRankingRequiredArguments =
    { data : SRdb.InputObject.UserJoinedRankingInput }


{-| Create a new document in the collection of 'UserJoinedRanking'

  - data - 'UserJoinedRanking' input values

-}
createUserJoinedRanking :
    CreateUserJoinedRankingRequiredArguments
    -> SelectionSet decodesTo SRdb.Object.UserJoinedRanking
    -> SelectionSet decodesTo RootMutation
createUserJoinedRanking requiredArgs object_ =
    Object.selectionForCompositeField "createUserJoinedRanking" [ Argument.required "data" requiredArgs.data SRdb.InputObject.encodeUserJoinedRankingInput ] object_ identity

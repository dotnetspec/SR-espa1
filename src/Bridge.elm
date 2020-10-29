module Bridge exposing (requestLoginUser, requestCreateAndOrLoginUser, handleCreateAndOrLoginUserOptionalArguments, requestAllUserNames
    , requestAllUsers
    , requestAllRankings
    )

import Graphql.Http as Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import SRdb.Mutation as Mutation
import SRdb.Query as Query
import SRdb.Object
import SRdb.Object.User
import SRdb.Object.Ranking
import SR.Types
import SR.Constants
import Graphql.SelectionSet exposing (SelectionSet(..))
import Eth.Types

requestLoginUser : SR.Types.UserName -> SR.Types.Password -> Http.Request (SR.Types.Token)
requestLoginUser user_name password =
    let
        requiredArgs = {username = user_name, password = password}
    in
        Http.queryRequest SR.Constants.endpointURL (queryLoginUser requiredArgs)
        |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

gotToken : List String -> SR.Types.Token 
gotToken lstring = 
    "fnED42KNUgACBwPPrxU00AYHx6mKEh6FP2HmKvQZ4ePZpk-VDhY"


queryLoginUser : Query.LoginUserRequiredArguments -> SelectionSet (SR.Types.Token) RootQuery
queryLoginUser requiredArgs =
    Query.loginUser requiredArgs



mutationCreateAndOrLoginUser : (Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments) 
    -> SR.Types.UserName -> SR.Types.Password -> SelectionSet SR.Types.Token RootMutation
mutationCreateAndOrLoginUser fillInOptionals user_name password  =
    let
        -- filledInOptionals =
        --     fillInOptionals { description = Absent, email = Absent, mobile = Absent }
        required_arguments =
            Mutation.CreateAndOrLoginUserRequiredArguments True user_name password

    in
    Mutation.createAndOrLoginUser fillInOptionals required_arguments


requestCreateAndOrLoginUser : (Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments) ->
 SR.Types.UserName -> SR.Types.Password -> Http.Request SR.Types.Token
requestCreateAndOrLoginUser fillInOptionals user_name password =
    Http.mutationRequest SR.Constants.endpointURL (mutationCreateAndOrLoginUser fillInOptionals user_name password)
        |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

handleCreateAndOrLoginUserOptionalArguments : Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments
handleCreateAndOrLoginUserOptionalArguments fillInOptionals = 
    --todo: make it handle the optionals
        fillInOptionals

requestAllUserNames : SR.Types.Token -> Http.Request (List String)
requestAllUserNames token =
    Http.queryRequest SR.Constants.endpointURL queryAllUserNames
        |> Http.withHeader "authorization" ("Bearer " ++ token)

queryAllUserNames : SelectionSet (List String) RootQuery
queryAllUserNames =
    Query.allUserNames

userSelectionSet : SelectionSet SR.Types.FUser SRdb.Object.User
userSelectionSet =
    Graphql.SelectionSet.succeed SR.Types.FUser
        |> Graphql.SelectionSet.with SRdb.Object.User.id_
        |> Graphql.SelectionSet.with SRdb.Object.User.active
        |> Graphql.SelectionSet.with SRdb.Object.User.description
        |> Graphql.SelectionSet.with SRdb.Object.User.email
        |> Graphql.SelectionSet.with SRdb.Object.User.member_since
        |> Graphql.SelectionSet.with SRdb.Object.User.mobile
        |> Graphql.SelectionSet.with SRdb.Object.User.username

       
requestAllUsers : Http.Request (Maybe (List (Maybe SR.Types.FUser)))
requestAllUsers  =
    Http.queryRequest SR.Constants.endpointURL (queryAllUsers userSelectionSet)
       |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

-- requestAllUsers : SR.Types.Token -> Http.Request (List String)
-- requestAllUsers token =
--     Http.queryRequest SR.Constants.endpointURL queryAllUsers
--         |> Http.withHeader "authorization" ("Bearer " ++ token)

queryAllUsers : SelectionSet decodesTo SRdb.Object.User
    -> SelectionSet (Maybe (List (Maybe decodesTo))) RootQuery
queryAllUsers =
    Query.allUsers

-- mySelection : SelectionSet MyLocalType ObjectOnGraphqlSide
-- mySelection =
--   SelectionSet.succeed functionToConstructLocalType
--     |> with fieldFromGraphql
--     |> with otherFieldFromGraphql
--     |> ...
-- MyLocalType is FRanking -> it can then be converted
       
requestAllRankings : Http.Request (Maybe (List (Maybe SR.Types.FRanking)))
requestAllRankings  =
    Http.queryRequest SR.Constants.endpointURL (queryAllRankings rankingSelectionSet)
       |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

rankingSelectionSet : SelectionSet SR.Types.FRanking SRdb.Object.Ranking
rankingSelectionSet =
        Graphql.SelectionSet.succeed SR.Types.FRanking
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.id_
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.active
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.rankingname
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.rankingdesc
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.rankingownerid

queryAllRankings : SelectionSet SR.Types.FRanking SRdb.Object.Ranking
    -> SelectionSet (Maybe (List (Maybe SR.Types.FRanking))) RootQuery
queryAllRankings rankingSelectSet =
        Query.allRankings rankingSelectSet
      


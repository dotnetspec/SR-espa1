module Bridge exposing (requestLoginUser, requestCreateAndOrLoginUser, handleCreateAndOrLoginUserOptionalArguments, requestAllUserNames
    , requestAllUsers
    )

--import DataModel exposing (Password, Token, UserName)
import Graphql.Http as Http
import Graphql.Operation exposing (RootMutation, RootQuery)
--import Graphql.SelectionSet exposing (SelectionSet)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import SRdb.Mutation as Mutation
import SRdb.Query as Query
import SRdb.Object
import SRdb.Object.User
import SR.Types
import SR.Constants
import Graphql.SelectionSet exposing (SelectionSet(..))
import Eth.Types


mutationCreateAndOrLoginUser : (Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments) 
    -> SR.Types.UserName -> SR.Types.Password -> String -> SelectionSet SR.Types.Token RootMutation
mutationCreateAndOrLoginUser fillInOptionals user_name password ethaddress =
    let
        
        -- filledInOptionals =
        --     fillInOptionals { description = Absent, email = Absent, mobile = Absent }

        required_arguments =
            Mutation.CreateAndOrLoginUserRequiredArguments True user_name password ethaddress

    in
    Mutation.createAndOrLoginUser fillInOptionals required_arguments


requestCreateAndOrLoginUser : (Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments) ->
 SR.Types.UserName -> SR.Types.Password -> String -> Http.Request SR.Types.Token
requestCreateAndOrLoginUser fillInOptionals user_name password ethaddress =
    Http.mutationRequest SR.Constants.endpointURL (mutationCreateAndOrLoginUser fillInOptionals user_name password ethaddress)
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


requestLoginUser : SR.Types.UserName -> SR.Types.Password -> Http.Request SR.Types.Token
requestLoginUser user_name password =
    let
        requiredArgs = {username = user_name, password = password}
    in
        Http.queryRequest SR.Constants.endpointURL (queryLoginUser requiredArgs)
        |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken


queryLoginUser : Query.LoginUserRequiredArguments -> SelectionSet SR.Types.Token RootQuery
queryLoginUser requiredArgs =
        Query.loginUser requiredArgs



userSelectionSet : SelectionSet SR.Types.FUser SRdb.Object.User
userSelectionSet =
    Graphql.SelectionSet.succeed SR.Types.FUser
        |> Graphql.SelectionSet.with SRdb.Object.User.active
        |> Graphql.SelectionSet.with SRdb.Object.User.description
        |> Graphql.SelectionSet.with SRdb.Object.User.email
        |> Graphql.SelectionSet.with SRdb.Object.User.ethaddress
        |> Graphql.SelectionSet.with SRdb.Object.User.member_since
        |> Graphql.SelectionSet.with SRdb.Object.User.mobile
        |> Graphql.SelectionSet.with SRdb.Object.User.password
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
       

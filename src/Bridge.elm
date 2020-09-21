module Bridge exposing (requestCreateAndOrLoginUser, handleCreateAndOrLoginUserOptionalArguments, requestAllUserNames)

--import DataModel exposing (Password, Token, UserName)
import Graphql.Http as Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import SRdb.Mutation as Mutation
import SRdb.Query as Query
import SR.Types


-- constants
endpointURL : String
endpointURL =
    "https://graphql.fauna.com/graphql"

customKeyBearerToken : String
customKeyBearerToken =
    "Bearer fnADz_OrVEACDOQU5b_WC-fOgnXuPZG4zrrLvYOW"

mutationCreateAndOrLoginUser : (Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments) -> SR.Types.UserName -> SR.Types.Password -> String -> SelectionSet SR.Types.Token RootMutation
mutationCreateAndOrLoginUser fillInOptionals user_name password ethaddress =
    let
        
        -- filledInOptionals =
        --     fillInOptionals { description = Absent, email = Absent, mobile = Absent }

        required_arguments =
            Mutation.CreateAndOrLoginUserRequiredArguments True user_name password ethaddress

    in
    Mutation.createAndOrLoginUser fillInOptionals required_arguments


requestCreateAndOrLoginUser : (Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments) -> SR.Types.UserName -> SR.Types.Password -> String -> Http.Request SR.Types.Token
requestCreateAndOrLoginUser fillInOptionals user_name password ethaddress =
    Http.mutationRequest endpointURL (mutationCreateAndOrLoginUser fillInOptionals user_name password ethaddress)
        |> Http.withHeader "authorization" customKeyBearerToken

handleCreateAndOrLoginUserOptionalArguments : Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments
handleCreateAndOrLoginUserOptionalArguments fillInOptionals = 
    --todo: make it handle the optionals
        fillInOptionals

requestAllUserNames : SR.Types.Token -> Http.Request (List String)
requestAllUserNames token =
    Http.queryRequest endpointURL queryAllUserNames
        |> Http.withHeader "authorization" ("Bearer " ++ token)

queryAllUserNames : SelectionSet (List String) RootQuery
queryAllUserNames =
    Query.allUserNames



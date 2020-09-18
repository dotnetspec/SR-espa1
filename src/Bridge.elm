module Bridge exposing (requestCreateAndOrLoginPlayer)

import DataModel exposing (Password, Token, UserName)
import Graphql.Http as Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet exposing (SelectionSet)
import SRdb.Mutation as Mutation

-- constants
endpointURL : String
endpointURL =
    "https://graphql.fauna.com/graphql"

customKeyBearerToken : String
customKeyBearerToken =
    "Bearer fnADz_OrVEACDOQU5b_WC-fOgnXuPZG4zrrLvYOW"

mutationCreateAndOrLoginPlayer : UserName -> Password -> SelectionSet Token RootMutation
mutationCreateAndOrLoginPlayer user_name password =
    let
        required_arguments =
            Mutation.CreateAndOrLoginPlayerRequiredArguments user_name password
    in
    Mutation.createAndOrLoginPlayer required_arguments


requestCreateAndOrLoginPlayer : UserName -> Password -> Http.Request Token
requestCreateAndOrLoginPlayer user_name password =
    Http.mutationRequest endpointURL (mutationCreateAndOrLoginPlayer user_name password)
        |> Http.withHeader "authorization" customKeyBearerToken
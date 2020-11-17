module Bridge exposing (requestLoginUser, requestCreateAndOrLoginUser, handleCreateAndOrLoginUserOptionalArguments, requestAllUserNames
    , requestAllUsers
    , requestAllRankings
    , requestAllPlayers
    , requestPlayersByRankingId
    , LoginResult
    )

import Graphql.Http as Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import SRdb.ScalarCodecs
import SRdb.Scalar exposing (Id(..))
import SRdb.Mutation as Mutation
import SRdb.Query as Query
import SRdb.Object
import SRdb.Object.User
--import SRdb.Object.Token
import SRdb.Object.Ranking
import SRdb.Object.Player
import SRdb.Object.LoginResult
import SR.Types
import SR.Constants
import Graphql.SelectionSet exposing (SelectionSet(..))
import Eth.Types
import Data.Rankings
import Data.Users
import Data.Players




--type alias Response = { loginResult : Maybe LoginResult }
--type alias User = { name : Maybe String }


-- query : SelectionSet Response RootQuery
-- query =
--     Query.selection Response
--         |> with (Query.user { login = "octocat" } user)
-- user : SelectionSet User Github.Object.User
-- user =
--     User.selection User
--         |> with User.name


gotToken : List String -> Data.Users.Token 
gotToken lstring = 
    "fnED42KNUgACBwPPrxU00AYHx6mKEh6FP2HmKvQZ4ePZpk-VDhY"

type alias LoginResult =
    { token : Maybe String, user : Maybe Data.Users.FUser }

requestLoginUser : Data.Users.UserName -> Data.Users.Password -> Http.Request LoginResult
requestLoginUser user_name password =
    let
        requiredArgs = {username = user_name, password = password}
    in
        Http.queryRequest SR.Constants.endpointURL (queryLoginUser requiredArgs loginResultSelectionSet)
        |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

-- nb. LoginResult (after succeed) is the type alias created for storing the return data
-- SRdb.Object.LoginResult is the typelock that was auto-generated
loginResultSelectionSet : SelectionSet LoginResult SRdb.Object.LoginResult
loginResultSelectionSet =
    Graphql.SelectionSet.succeed LoginResult
    |> Graphql.SelectionSet.with SRdb.Object.LoginResult.token 
    |> Graphql.SelectionSet.with (SRdb.Object.LoginResult.user (userSelectionSet))


--The queryLoginUser function maps our LoginResult type alias to that of SRdb’s GraphQL response.
--for the token field in LoginResult, the first type variable is specified as "Maybe String", meaning this field should be decoded to a Maybe String
-- decodesTo represents the Elm type that this particular SelectionSet should decode to (Maybe LoginResult)
-- SRdb.Object.LoginResult here is the typeLock - making sure we get the type right when we decodeTo
queryLoginUser : Query.LoginUserRequiredArguments
 -> SelectionSet decodesTo SRdb.Object.LoginResult
    -> SelectionSet decodesTo RootQuery
queryLoginUser requiredArgs =
    Query.loginUser requiredArgs


mutationCreateAndOrLoginUser : (Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments) 
    -> Data.Users.UserName -> Data.Users.Password -> SelectionSet Data.Users.Token RootMutation
mutationCreateAndOrLoginUser fillInOptionals user_name password  =
    let
        -- filledInOptionals =
        --     fillInOptionals { description = Absent, email = Absent, mobile = Absent }
        required_arguments =
            Mutation.CreateAndOrLoginUserRequiredArguments True user_name password

    in
    Mutation.createAndOrLoginUser fillInOptionals required_arguments


requestCreateAndOrLoginUser : (Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments) ->
 Data.Users.UserName -> Data.Users.Password -> Http.Request Data.Users.Token
requestCreateAndOrLoginUser fillInOptionals user_name password =
    Http.mutationRequest SR.Constants.endpointURL (mutationCreateAndOrLoginUser fillInOptionals user_name password)
        |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

handleCreateAndOrLoginUserOptionalArguments : Mutation.CreateAndOrLoginUserOptionalArguments -> Mutation.CreateAndOrLoginUserOptionalArguments
handleCreateAndOrLoginUserOptionalArguments fillInOptionals = 
    --todo: make it handle the optionals
        fillInOptionals

requestAllUserNames : Data.Users.Token -> Http.Request (List String)
requestAllUserNames token =
    Http.queryRequest SR.Constants.endpointURL queryAllUserNames
        |> Http.withHeader "authorization" ("Bearer " ++ token)

queryAllUserNames : SelectionSet (List String) RootQuery
queryAllUserNames =
    Query.allUserNames


userSelectionSet : SelectionSet Data.Users.FUser SRdb.Object.User
userSelectionSet =
    Graphql.SelectionSet.succeed Data.Users.FUser
        |> Graphql.SelectionSet.with SRdb.Object.User.id_
        |> Graphql.SelectionSet.with SRdb.Object.User.active
        |> Graphql.SelectionSet.with SRdb.Object.User.description
        |> Graphql.SelectionSet.with SRdb.Object.User.email
        |> Graphql.SelectionSet.with SRdb.Object.User.member_since
        |> Graphql.SelectionSet.with SRdb.Object.User.mobile
        |> Graphql.SelectionSet.with SRdb.Object.User.username

       
requestAllUsers : Http.Request (Maybe (List (Maybe Data.Users.FUser)))
requestAllUsers  =
    Http.queryRequest SR.Constants.endpointURL (queryAllUsers userSelectionSet)
       |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

-- requestAllUsers : Data.Users.Token -> Http.Request (List String)
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
       
requestAllRankings : Http.Request (Maybe (List (Maybe Data.Rankings.FRanking)))
requestAllRankings  =
    Http.queryRequest SR.Constants.endpointURL (queryAllRankings rankingSelectionSet)
       |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

queryAllRankings : SelectionSet Data.Rankings.FRanking SRdb.Object.Ranking
    -> SelectionSet (Maybe (List (Maybe Data.Rankings.FRanking))) RootQuery
queryAllRankings rankingSelectSet =
        Query.allRankings rankingSelectSet

rankingSelectionSet : SelectionSet Data.Rankings.FRanking SRdb.Object.Ranking
rankingSelectionSet =
        Graphql.SelectionSet.succeed Data.Rankings.FRanking
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.id_
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.active
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.rankingname
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.rankingdesc
            |> Graphql.SelectionSet.with SRdb.Object.Ranking.rankingownerid

--280892229782864389

requestAllPlayers : Data.Users.Token ->  Http.Request (Maybe (List (Maybe Data.Players.FPlayer)))
requestAllPlayers token =
    Http.queryRequest SR.Constants.endpointURL (queryAllPlayers playerSelectionSet)
        |> Http.withHeader "authorization" ("Bearer " ++ token)

queryAllPlayers : SelectionSet Data.Players.FPlayer SRdb.Object.Player
     -> SelectionSet (Maybe (List (Maybe Data.Players.FPlayer))) RootQuery
queryAllPlayers playerSelectSet =
    Query.allPlayers playerSelectSet

playerSelectionSet :  SelectionSet Data.Players.FPlayer SRdb.Object.Player
playerSelectionSet =
    Graphql.SelectionSet.succeed Data.Players.FPlayer
        |> Graphql.SelectionSet.with SRdb.Object.Player.id_
        |> Graphql.SelectionSet.with SRdb.Object.Player.rankingid
        |> Graphql.SelectionSet.with SRdb.Object.Player.uid
        |> Graphql.SelectionSet.with SRdb.Object.Player.rank
        |> Graphql.SelectionSet.with SRdb.Object.Player.challengerid


requestPlayersByRankingId : String -> Http.Request (List Data.Players.FPlayer)
requestPlayersByRankingId rankingId  =
    let
        --requiredArgs = {id  = (SRdb.Scalar.Id rankingId)}
        requiredArgs = {rankingid = rankingId}
    in
        Http.queryRequest SR.Constants.endpointURL (queryfindPlayersByRankingID requiredArgs playerSelectionSet)
        |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken


queryfindPlayersByRankingID : Query.FindPlayersByRankingIdRequiredArguments ->  SelectionSet Data.Players.FPlayer SRdb.Object.Player
     -> SelectionSet (List Data.Players.FPlayer) RootQuery
queryfindPlayersByRankingID requiredArgs playerSelectSet =
    Query.findPlayersByRankingId requiredArgs playerSelectSet


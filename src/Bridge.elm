module Bridge exposing (requestLoginUser, requestCreateNewUser, requestAllUserNames
    , requestCreateNewRanking
    , requestAllUsers
    , requestAllRankings
    , requestAllPlayers
    --, requestPlayersByRankingId
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

-- mySelection : SelectionSet MyLocalType ObjectOnGraphqlSide
-- mySelection =
--   SelectionSet.succeed functionToConstructLocalType
--     |> with fieldFromGraphql
--     |> with otherFieldFromGraphql
--     |> ...
-- MyLocalType is FRanking -> it can then be converted

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
    |> Graphql.SelectionSet.with (SRdb.Object.LoginResult.logginUser (userSelectionSet))


--The queryLoginUser function maps our LoginResult type alias to that of SRdb’s GraphQL response.
--for the token field in LoginResult, the first type variable is specified as "Maybe String", meaning this field should be decoded to a Maybe String
-- decodesTo represents the Elm type that this particular SelectionSet should decode to (Maybe LoginResult)
-- SRdb.Object.LoginResult here is the typeLock - making sure we get the type right when we decodeTo
queryLoginUser : Query.LoginUserRequiredArguments
 -> SelectionSet decodesTo SRdb.Object.LoginResult
    -> SelectionSet decodesTo RootQuery
queryLoginUser requiredArgs =
    Query.loginUser requiredArgs


requestCreateNewUser : Data.Users.UserInfo -> Http.Request LoginResult
requestCreateNewUser userInfo =
    -- handling of optional args is addressed at https://elmlang.slack.com/archives/C0RSQNQ92/p1606277449291800
    -- and https://thoughtbot.com/blog/optional-arguments-in-elm-queries
    Http.mutationRequest SR.Constants.endpointURL (Mutation.createNewUser 
        (\default -> { default | description = Present userInfo.extrauserinfo.description
        , email = Present userInfo.extrauserinfo.email, mobile = Present userInfo.extrauserinfo.mobile })
        ({ active = True, username = userInfo.username, password = userInfo.password } ) loginResultSelectionSet)
        |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken
        

requestCreateNewRanking : Data.Rankings.Ranking -> Http.Request Data.Rankings.FRanking
requestCreateNewRanking ranking =
    -- handling of optional args is addressed at https://elmlang.slack.com/archives/C0RSQNQ92/p1606277449291800
    -- and https://thoughtbot.com/blog/optional-arguments-in-elm-queries
    Http.mutationRequest SR.Constants.endpointURL (Mutation.createNewRanking
        (\default -> { default | rankingdesc = Present (Maybe.withDefault "" ranking.rankingdesc) })
        ({ active = True, rankingname = ranking.rankingname, rankingownerid = ranking.rankingownerid } ) rankingSelectionSet)
        |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken

--  id_ : String
--      --id_ : SRdb.ScalarCodecs.Id
--     , active : Bool
--     , rankingname : String
--     , rankingdesc : Maybe String
--     , rankingownerid : String

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
        |> Graphql.SelectionSet.with SRdb.Object.User.ts_
        |> Graphql.SelectionSet.with SRdb.Object.User.mobile
        |> Graphql.SelectionSet.with SRdb.Object.User.username

       
requestAllUsers : Http.Request (Maybe (List (Maybe Data.Users.FUser)))
requestAllUsers  =
    Http.queryRequest SR.Constants.endpointURL (queryAllUsers userSelectionSet)
       |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken


queryAllUsers : SelectionSet decodesTo SRdb.Object.User
    -> SelectionSet (Maybe (List (Maybe decodesTo))) RootQuery
queryAllUsers =
    Query.allUsers
       
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



-- it needs to be players by ranking id - all players for a given ranking
-- requestPlayersByRankingId : String -> Http.Request (List Data.Players.FPlayer)
-- requestPlayersByRankingId rankingId  =
--     let
--         --requiredArgs = {id  = (SRdb.Scalar.Id rankingId)}
--         requiredArgs = {rankingid = rankingId}
--     in
--         Http.queryRequest SR.Constants.endpointURL (queryfindPlayersByRankingID requiredArgs playerSelectionSet)
--         |> Http.withHeader "authorization" SR.Constants.customKeyBearerToken


-- queryfindPlayersByRankingID : Query.FindPlayerByIDRequiredArguments ->  SelectionSet Data.Players.FPlayer SRdb.Object.Player
--      -> SelectionSet (List Data.Players.FPlayer) RootQuery
-- queryfindPlayersByRankingID requiredArgs playerSelectSet =
--     Query.findPlayersByRankingId requiredArgs playerSelectSet


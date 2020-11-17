-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SRdb.InputObject exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import SRdb.Interface
import SRdb.Object
import SRdb.Scalar
import SRdb.ScalarCodecs
import SRdb.Union


buildLoginResultInput :
    LoginResultInputRequiredFields
    -> (LoginResultInputOptionalFields -> LoginResultInputOptionalFields)
    -> LoginResultInput
buildLoginResultInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { user = Absent }
    in
    { token = required.token, user = optionals.user }


type alias LoginResultInputRequiredFields =
    { token : String }


type alias LoginResultInputOptionalFields =
    { user : OptionalArgument SRdb.ScalarCodecs.Id }


{-| Type for the LoginResultInput input object.
-}
type alias LoginResultInput =
    { token : String
    , user : OptionalArgument SRdb.ScalarCodecs.Id
    }


{-| Encode a LoginResultInput into a value that can be used as an argument.
-}
encodeLoginResultInput : LoginResultInput -> Value
encodeLoginResultInput input =
    Encode.maybeObject
        [ ( "token", Encode.string input.token |> Just ), ( "user", (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) |> Encode.optional input.user ) ]


buildLoginResultUserRelation :
    (LoginResultUserRelationOptionalFields -> LoginResultUserRelationOptionalFields)
    -> LoginResultUserRelation
buildLoginResultUserRelation fillOptionals =
    let
        optionals =
            fillOptionals
                { create = Absent, connect = Absent, disconnect = Absent }
    in
    { create = optionals.create, connect = optionals.connect, disconnect = optionals.disconnect }


type alias LoginResultUserRelationOptionalFields =
    { create : OptionalArgument UserInput
    , connect : OptionalArgument SRdb.ScalarCodecs.Id
    , disconnect : OptionalArgument Bool
    }


{-| Type for the LoginResultUserRelation input object.
-}
type alias LoginResultUserRelation =
    { create : OptionalArgument UserInput
    , connect : OptionalArgument SRdb.ScalarCodecs.Id
    , disconnect : OptionalArgument Bool
    }


{-| Encode a LoginResultUserRelation into a value that can be used as an argument.
-}
encodeLoginResultUserRelation : LoginResultUserRelation -> Value
encodeLoginResultUserRelation input =
    Encode.maybeObject
        [ ( "create", encodeUserInput |> Encode.optional input.create ), ( "connect", (SRdb.ScalarCodecs.codecs |> SRdb.Scalar.unwrapEncoder .codecId) |> Encode.optional input.connect ), ( "disconnect", Encode.bool |> Encode.optional input.disconnect ) ]


buildPlayerInput :
    PlayerInputRequiredFields
    -> (PlayerInputOptionalFields -> PlayerInputOptionalFields)
    -> PlayerInput
buildPlayerInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { challengerid = Absent }
    in
    { rankingid = required.rankingid, uid = required.uid, rank = required.rank, challengerid = optionals.challengerid }


type alias PlayerInputRequiredFields =
    { rankingid : String
    , uid : String
    , rank : Int
    }


type alias PlayerInputOptionalFields =
    { challengerid : OptionalArgument String }


{-| Type for the PlayerInput input object.
-}
type alias PlayerInput =
    { rankingid : String
    , uid : String
    , rank : Int
    , challengerid : OptionalArgument String
    }


{-| Encode a PlayerInput into a value that can be used as an argument.
-}
encodePlayerInput : PlayerInput -> Value
encodePlayerInput input =
    Encode.maybeObject
        [ ( "rankingid", Encode.string input.rankingid |> Just ), ( "uid", Encode.string input.uid |> Just ), ( "rank", Encode.int input.rank |> Just ), ( "challengerid", Encode.string |> Encode.optional input.challengerid ) ]


buildRankingInput :
    RankingInputRequiredFields
    -> (RankingInputOptionalFields -> RankingInputOptionalFields)
    -> RankingInput
buildRankingInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { rankingdesc = Absent }
    in
    { active = required.active, rankingname = required.rankingname, rankingdesc = optionals.rankingdesc, rankingownerid = required.rankingownerid }


type alias RankingInputRequiredFields =
    { active : Bool
    , rankingname : String
    , rankingownerid : String
    }


type alias RankingInputOptionalFields =
    { rankingdesc : OptionalArgument String }


{-| Type for the RankingInput input object.
-}
type alias RankingInput =
    { active : Bool
    , rankingname : String
    , rankingdesc : OptionalArgument String
    , rankingownerid : String
    }


{-| Encode a RankingInput into a value that can be used as an argument.
-}
encodeRankingInput : RankingInput -> Value
encodeRankingInput input =
    Encode.maybeObject
        [ ( "active", Encode.bool input.active |> Just ), ( "rankingname", Encode.string input.rankingname |> Just ), ( "rankingdesc", Encode.string |> Encode.optional input.rankingdesc ), ( "rankingownerid", Encode.string input.rankingownerid |> Just ) ]


buildUserInput :
    UserInputRequiredFields
    -> (UserInputOptionalFields -> UserInputOptionalFields)
    -> UserInput
buildUserInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { description = Absent, email = Absent, mobile = Absent }
    in
    { active = required.active, username = required.username, password = required.password, description = optionals.description, email = optionals.email, mobile = optionals.mobile, member_since = required.member_since }


type alias UserInputRequiredFields =
    { active : Bool
    , username : String
    , password : String
    , member_since : Int
    }


type alias UserInputOptionalFields =
    { description : OptionalArgument String
    , email : OptionalArgument String
    , mobile : OptionalArgument String
    }


{-| Type for the UserInput input object.
-}
type alias UserInput =
    { active : Bool
    , username : String
    , password : String
    , description : OptionalArgument String
    , email : OptionalArgument String
    , mobile : OptionalArgument String
    , member_since : Int
    }


{-| Encode a UserInput into a value that can be used as an argument.
-}
encodeUserInput : UserInput -> Value
encodeUserInput input =
    Encode.maybeObject
        [ ( "active", Encode.bool input.active |> Just ), ( "username", Encode.string input.username |> Just ), ( "password", Encode.string input.password |> Just ), ( "description", Encode.string |> Encode.optional input.description ), ( "email", Encode.string |> Encode.optional input.email ), ( "mobile", Encode.string |> Encode.optional input.mobile ), ( "member_since", Encode.int input.member_since |> Just ) ]


buildUserJoinedRankingInput :
    UserJoinedRankingInputRequiredFields
    -> UserJoinedRankingInput
buildUserJoinedRankingInput required =
    { active = required.active, rankingid = required.rankingid, rankingname = required.rankingname, useraddr = required.useraddr, rankingownerid = required.rankingownerid }


type alias UserJoinedRankingInputRequiredFields =
    { active : Bool
    , rankingid : String
    , rankingname : String
    , useraddr : String
    , rankingownerid : String
    }


{-| Type for the UserJoinedRankingInput input object.
-}
type alias UserJoinedRankingInput =
    { active : Bool
    , rankingid : String
    , rankingname : String
    , useraddr : String
    , rankingownerid : String
    }


{-| Encode a UserJoinedRankingInput into a value that can be used as an argument.
-}
encodeUserJoinedRankingInput : UserJoinedRankingInput -> Value
encodeUserJoinedRankingInput input =
    Encode.maybeObject
        [ ( "active", Encode.bool input.active |> Just ), ( "rankingid", Encode.string input.rankingid |> Just ), ( "rankingname", Encode.string input.rankingname |> Just ), ( "useraddr", Encode.string input.useraddr |> Just ), ( "rankingownerid", Encode.string input.rankingownerid |> Just ) ]

-- Users is an opaque type - create and expose functions to work with it
module Data.Users exposing 
    (Users
    , User(..)
    , UserState(..)
    , WalletState(..)
    , FUser
    , UserInfo
    , Token
    , UserName
    , Password
    , updatedDesc
    , updatedEmail
    , updatedMobile
    , convertFUserToUser
    , newUser
    , updatedUserInSet
    , addedNewJoinedRankingId
    , removedRankingIdFromAll
    , removedRankindIdFromUser
    , isEmpty
    , isNameValid
    , isPasswordValid
    , isDescValid
    , isEmailValid
    , isMobileValid
    , extractUsersFromWebData
    , empty
    , emptyUser
    , emptyUserInfo
    , emptyFUser
    , addUser
    , removeUser
    , asList
    , asUsers
    , gotUser
    , userSetLength
    , removedInvalidRankingId
    , handleDeletionFromUserJoined
    , removedDeletedRankingsFromUserJoined
    , gotName
    , gotId
    , dummyUserWithUserJoinedRankings
    )


--import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
--import Utils.MyUtils
import Eth.Utils
import RemoteData
import Http
import List.Unique
import Utils.Validation.Validate
import Eth.Types
--import SR.Defaults
import Data.Rankings
import SRdb.Scalar exposing (Id(..))
import SRdb.Scalar exposing (Long(..))
import SRdb.ScalarCodecs
import Regex


-- Users (EverySet User) is not the same type as (EverySet User)
-- Peter Damoc
-- You can think about the tag ('Users') as a box containing a type.
-- There can only ever be registered users in the User set
-- Type 'Users' is a type with a variant 'Users' and assoc. data (EverySet User)
type Users = Users (EverySet User)
type UserNames = UserNames (EverySet String)
type User =
    Spectator UserInfo UserState
    | Registered UserInfo UserState


type alias UserInfo =
    { --datestamp to become creditsremaining - check member_since works as expected
     id : String
    , token : Maybe String
    , walletState : WalletState
    , datestamp : Int
    , active : Bool
    , username : String
    , password : String
    , extrauserinfo : ExtraUserInfo
    , userjoinedrankings : List String
    , member_since : String
    }

type WalletState = 
    NoWallet
    | NoCredit Eth.Types.Address
    | Credited Eth.Types.Address

type UserState = 
    General
    | CreatingNew
    | Updating
    | Updated
    | LoginError
    | WalletOperational
    | WalletWaitingForTransactionReceipt
    -- these 2 might replace the 2 above (?):
    | Subscribe 
    | StopSubscription


-- these states might transfer over to UserState
-- type UIState
--     = UIRenderAllRankings
--     | UICreateNewLadder
--     | UIRegisterNewUser
--     | UIUpdateExistingUser
--     | UIWalletMissingInstructions
--     | UIDisplayWalletLockedInstructions
--     | UIDisplayWalletInfoToUser
--     | UISelectedRankingUserIsOwner
--     | UISelectedRankingUserIsPlayer
--     | UISelectedRankingUserIsNeitherOwnerNorPlayer
--     | UIEnterResult
--     | UIEnterResultTxProblem
--     | UIChallenge
--     | UILoading
--     | UIWaitingForTxReceipt
--     | UIDeleteRankingConfirm
--     | UIEnableEthereum
--     | UIOwnerDeletedRanking
--     | UIUnableToFindGlobalRankings
--     | UIEthAlreadyEnabled
--     | UILogIn

-- type WalletState
--     = 
--     WalletStateMissing
--     | WalletStateLocked
--     | WalletStateAwaitOpening
--     | WalletWaitingForTransactionReceipt
--     | WalletOpened
--     | WalletOperational
--     | WalletStopSub
--     | WalletOpenedNoUserAccount


emptyUserInfo : UserInfo
emptyUserInfo =
    UserInfo "" Nothing NoWallet 0 True "" "" (ExtraUserInfo "" "" "") [] ""

dummyUserWithUserJoinedRankings : User
dummyUserWithUserJoinedRankings = 
    Registered (UserInfo "" Nothing NoWallet 0 True "" "" 
        (ExtraUserInfo "" "" "") ["282953512300577285", "283673261521240581"] "") General


type alias Token =
    String

type alias UserName =
    String

type alias Password =
    String

type alias ExtraUserInfo =
    {
    description : String
    , email : String
    , mobile : String
    }



-- assignChallengerId : Data.Users.User -> String -> Data.Users.User 
-- assignChallengerId user challengerUID = 
--     case user of 
--         Registered userInfo userStatus ->
--             Registered userInfo.id token (Data.Users.UserInfo userInfo.datestamp 
--     userInfo.active
--     userInfo.username
--     userInfo.password
--     (ExtraUserInfo userInfo.extrauserinfo.
--     userInfo.userjoinedrankings
--     userInfo.member_since 
--     userStatus




updatedDesc : UserInfo -> String -> UserInfo 
updatedDesc userInfo str = 
    let
        newExtrUserInfo = userInfo.extrauserinfo
        updatedExtraUserInfo = {newExtrUserInfo | description = str}
    in
        {userInfo | extrauserinfo = updatedExtraUserInfo}

updatedEmail : UserInfo -> String -> UserInfo 
updatedEmail userInfo str = 
    let
        newExtrUserInfo = userInfo.extrauserinfo
        updatedExtraUserInfo = {newExtrUserInfo | email = str}
    in
        {userInfo | extrauserinfo = updatedExtraUserInfo}

updatedMobile : UserInfo -> String -> UserInfo 
updatedMobile userInfo str = 
    let
        newExtrUserInfo = userInfo.extrauserinfo
        updatedExtraUserInfo = {newExtrUserInfo | mobile = str}
    in
        {userInfo | extrauserinfo = updatedExtraUserInfo}



convertFUserToUser : FUser -> User 
convertFUserToUser fuser =
    let 
        desc = Maybe.withDefault "" fuser.description
        email = Maybe.withDefault "" fuser.email
        mobile = Maybe.withDefault "" fuser.mobile
    in
    Registered (UserInfo (fromScalarCodecId fuser.id_) Nothing NoWallet 1 True fuser.username "" (ExtraUserInfo desc email mobile) [""] (fromScalarCodecLong fuser.ts_)) General

type alias FUser = {
    id_ :  SRdb.ScalarCodecs.Id
    , active : Bool
    , description : Maybe String
    , email : Maybe String
    , ts_ : SRdb.ScalarCodecs.Long
    , mobile : Maybe String
    , username : String
    }

emptyFUser : FUser
emptyFUser = {
    id_ =  SRdb.Scalar.Id ""
    , active = False
    , description = Nothing
    , email = Nothing
    , ts_ = SRdb.Scalar.Long ""
    , mobile = Nothing
    , username = ""
    }


fromScalarCodecId : SRdb.ScalarCodecs.Id -> String
fromScalarCodecId (Id id) =
    id


fromScalarCodecLong : SRdb.ScalarCodecs.Long -> String
fromScalarCodecLong (Long ts) =
    ts

newUser : String -> String -> String -> String -> String -> User
newUser username password desc email mobile =
    Registered (UserInfo "" Nothing NoWallet 10 True username password (ExtraUserInfo desc email mobile) [""] "") General

--nb. this is not an EverySet, it's a Users type.
empty : Users
empty = 
    Users (EverySet.empty)

emptyUser : User
emptyUser =
    Registered (UserInfo "" Nothing NoWallet 10 True "" "" (ExtraUserInfo "" "" "") [""] "") General

isEmpty : Users -> Bool
-- 'Users' is a tag containing a box (of EverySet)
-- using the tag here you can open the box
isEmpty (Users sUsers) =
    EverySet.isEmpty sUsers

asUsers : EverySet User -> Users 
asUsers esUser  = 
    Users esUser

gotUserName : User -> String 
gotUserName user = 
    case user of
        Spectator _ _ ->
            "Spectator"
        (Registered userInfo userState) ->
            userInfo.username

removedDeletedRankingsFromUserJoined : User -> Data.Rankings.Rankings -> User 
removedDeletedRankingsFromUserJoined user sRankings = 
    case user of
        Spectator userInfo _ ->
            Spectator userInfo General
        (Registered userInfo userState) ->
            Registered (handleDeletionFromUserJoined userInfo sRankings) userState

-- isUserNameValidated : String -> List User -> Bool
-- isUserNameValidated username luser =
--     if String.length username > 3 && String.length username < 9 && isUniqueUserName username luser then
--         True

--     else
--         False

-- isUniqueUserName : String -> List User -> Bool
-- isUniqueUserName str luser =
--     let
--         newList =
--             List.filter (\r -> (String.toLower <| r.username) == (String.toLower <| str))
--                 (validatedUserList luser)
--     in
--     if List.isEmpty newList then
--         True

--     else
--         False

addUser : User -> Users -> Users
addUser user susers = 
    case susers of 
        Users setOfUsers  ->
                asUsers (EverySet.insert user setOfUsers)

gotUserNames : Users -> EverySet String 
gotUserNames (Users users) = 
    EverySet.map gotName users

gotName : User -> String 
gotName user = 
    case user of 
        Spectator userInfo _ ->
            userInfo.username
        Registered userInfo _ ->
            userInfo.username

gotId : User -> String
gotId user = 
    case user of 
        Spectator _ _ ->
            ""
        Registered userInfo _ ->
            userInfo.id

    
userSetLength : Users -> Int 
userSetLength (Users susers) = 
    EverySet.size susers


gotUser : Users -> String -> Maybe User
gotUser (Users susers) userId =
    let 
        esUser = EverySet.filter (\user -> (gotId user) == userId) susers
    in
        List.head (EverySet.toList esUser)

-- probably should be updated to return a set, not a list:
addedNewJoinedRankingId : String -> User -> List User -> List User
addedNewJoinedRankingId rankingId user lUser =
    let
        -- currentUser =
        --     gotUserFromUserList lUser user.username
        

        -- if there's anything wrong with the existing joinrankings data fix it here:
        -- userJoinRankings =
        --     List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId user.userjoinedrankings)
        -- todo: temp fix

        userJoinRankings =
            List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId [""])
        

        --_ = Debug.log "userJoinRankings in added" userJoinRankings

        validatedRankingAdded = 
            if Utils.Validation.Validate.isValidRankingId rankingId then
                rankingId :: userJoinRankings
            else 
                userJoinRankings

        -- if, somehow, an existing id was added we can again filter out here:
        validatedUserJoinRankings =
            List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId validatedRankingAdded)

        -- userUpdated =
        --     --{ user | userjoinedrankings =  validatedRankingAdded}
        --     { user | userjoinedrankings =  validatedUserJoinRankings}

        -- newUserList =
        --     userUpdated :: lUser
    in
    --todo: temp fix
    --newUserList
    [Spectator emptyUserInfo General]


removedInvalidRankingId : String -> Maybe String 
removedInvalidRankingId rankingId = 
    if Utils.Validation.Validate.isValidRankingId rankingId then
        Just rankingId
    else 
        Nothing




removedRankingIdFromAll : Users -> String -> Users
removedRankingIdFromAll susers rnkId = 
    case susers of 
        Users setOfUsers->
           asUsers (EverySet.map (removedRankindIdFromUser rnkId) setOfUsers)


removedRankindIdFromUser : String -> User -> User
removedRankindIdFromUser  rnkId user = 
    let
        -- if there's anything wrong with the existing joinrankings data fix it here:
        -- todo: temp fix
        --userJoinRankings = List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId user.userjoinedrankings)
        userJoinRankings = List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId [""])
        --_ = Debug.log "userJoinRankings" userJoinRankings

        filteredOutRanking =
            List.filterMap (filterRankingIds rnkId) userJoinRankings

        --_ = Debug.log "filteredOutRanking" filteredOutRanking

        --userUpdated = {user | userjoinedrankings = filteredOutRanking}
    in
        --userUpdated
        -- todo: temp fix
        Spectator emptyUserInfo General

filterRankingIds : String -> String -> Maybe String 
filterRankingIds rnkIdToFilter currentRnkId =
    if currentRnkId == rnkIdToFilter then
        Nothing

    else
        Just currentRnkId

    

removeUser : Maybe User -> Users -> Users
removeUser m_user (Users sUsers) = 
    -- case susers of 
    --     Users setOfUsers->
            case m_user of 
                Nothing ->
                    asUsers sUsers
                Just user ->
                    asUsers (EverySet.remove user sUsers)

asList : Users -> List User 
asList susers = 
    case susers of 
        Users setOfUsers ->
            setOfUsers
           |> EverySet.toList

handleDeletionFromUserJoined : UserInfo -> Data.Rankings.Rankings -> UserInfo
handleDeletionFromUserJoined userInfo sRankings = 
    let
        lwithDeletedRankingIdsRemoved = List.filter (Data.Rankings.isIdInSet sRankings) (Data.Rankings.stringListToRankingIdList userInfo.userjoinedrankings)
        newUserInfo = {userInfo | userjoinedrankings = Data.Rankings.rankingIdListToStringList lwithDeletedRankingIdsRemoved} 
    in
        newUserInfo



updatedUserInSet : Users -> User -> Users
updatedUserInSet susers updatedUser =
--the user is 'Registered' for the purposes of updating the Set
    case updatedUser of
        Spectator userInfo user ->
            susers
        (Registered userInfo userState) ->
            -- remove the original user, then add the new one
            addUser updatedUser <| removeUser (gotUser susers userInfo.id) susers



-- gotUserFromUserList : List User -> String -> Maybe User
-- gotUserFromUserList userList uaddr =
--     let
--         existingUser =
--             List.head <|
--                 --List.filter (\r -> (String.toLower <| r.m_ethaddress) == (String.toLower <| Just uaddr))
--                 List.filter (\r -> (r.m_ethaddress) == (Result.toMaybe (Eth.Utils.toAddress uaddr)))
--                     (validatedUserList userList)
        
--     in
--         existingUser
 

-- validatedUserList : List User -> List User
-- validatedUserList luser =
--     List.filterMap
--         isValidUserAddrInList
--         luser


-- isValidUserAddrInList : User -> Maybe User
-- isValidUserAddrInList user =
--     case user.m_ethaddress of 
--         Nothing ->
--             Nothing
--         Just addr ->
--             if Eth.Utils.isAddress (Eth.Utils.addressToString addr) then
--                 Just user

--             else
--                 Nothing


extractUsersFromWebData : RemoteData.WebData (List User) -> List User
extractUsersFromWebData remData =
    case remData of
        RemoteData.NotAsked ->
            let
                _ =
                    Debug.log "http err" "not asked"
            in
            []

        RemoteData.Loading ->
            let
                _ =
                    Debug.log "http err" "loading"
            in
            []

        RemoteData.Success users ->
            users

        RemoteData.Failure httpError ->
            []

isNameValid : String -> Users -> Bool 
isNameValid newName sUsers =
    if EverySet.member newName <| gotUserNames sUsers then
        False 
    else 
        Regex.contains (Maybe.withDefault Regex.never (Regex.fromString "(?!.*[\\.\\-\\_]{2,})^[a-zA-Z0-9\\.\\-\\_]{4,8}$")) newName

isPasswordValid : String -> Bool 
isPasswordValid newPassword  =
    Regex.contains (Maybe.withDefault Regex.never (Regex.fromString "(?!.*[\\.\\-\\_]{2,})^[a-zA-Z0-9\\.\\-\\_]{4,8}$")) newPassword


isDescValid : String -> Bool
isDescValid str =
    if String.length str <= 20 then
        True

    else
        False

isEmailValid : String -> Bool
isEmailValid newEmail = 
    if newEmail == "" then
        True 
    else
    Regex.contains (Maybe.withDefault Regex.never 
    (Regex.fromString "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")) newEmail
    

isMobileValid : String -> Bool
isMobileValid newMobile = 
    if newMobile == "" then
        True 
    else 
        Regex.contains (Maybe.withDefault Regex.never 
        (Regex.fromString "^\\+((?:9[679]|8[035789]|6[789]|5[90]|42|3[578]|2[1-689])|9[0-58]|8[1246]|6[0-6]|5[1-8]|4[013-9]|3[0-469]|2[70]|7|1)(?:\\W*\\d){0,13}\\d$")) newMobile
 



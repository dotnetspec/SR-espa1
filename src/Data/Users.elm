-- Users will be mainly used to communicate externally to the jsonbin server
module Data.Users exposing (Users
    , User
    , newUser
    , newUserFromFUser
    , updatedUserInSet
    --, validatedUserList
    , addedNewJoinedRankingId
    , removedRankingIdFromAll
    , removedRankindIdFromUser
    --, removeCurrentUserEntryFromUserList
    --, removedDuplicateUserFromUserList
    --, isRegistered
    --, isUniqueUserName
    , isEmpty
    --, gotUserListFromRemData
    , isNameValid
    , extractUsersFromWebData
    --, gotUserFromUserList
    , empty
    --, updateAddr
    , addUser
    , removeUser
    , asList
    , asUsers
    , gotUser
    , userSetLength
    --, isUserNameValidated
    , removedInvalidRankingId
    , handleDeletionFromUserJoined
    , removedDeletedRankingsFromUserJoined
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
import SRdb.ScalarCodecs


-- Users (EverySet User) is not the same type as (EverySet User)
-- Peter Damoc
-- You can think about the tag ('Users') as a box containing a type.
-- There can only ever be registered users in the User set
type Users = Users (EverySet User)
type UserNames = UserNames (EverySet String)

type alias UserInfo =
    { --datestamp to become creditsremaining
    datestamp : Int
    , active : Bool
    , username : String
    , password : String
    , extrauserinfo : ExtraUserInfo
    --, m_ethaddress : Maybe Eth.Types.Address
    -- , description : String
    -- , email : String
    -- , mobile : String
    , userjoinrankings : List String
    , member_since : Int
    --, m_token : Maybe Token
    }

type alias UserId =
    String

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


type User =
    Guest
    | Registered UserId Token UserInfo
    | NoWallet UserId Token UserInfo
    | NoCredit Eth.Types.Address UserId Token UserInfo
    | Credited Eth.Types.Address UserId Token UserInfo

-- case user of
--         Guest ->
--             Guest
--         (Registered userId token userInfo) ->
--             Registered userId token userInfo
--         (NoWallet userId token userInfo) ->
--             NoWallet userId token userInfo
--         (NoCredit addr userId token userInfo) ->
--             NoCredit addr userId token userInfo
--         (Credited addr userId token userInfo) ->
--             Credited addr userId token userInfo


-- new empty User:
-- User 0 True "" "" Nothing "" "" "" [""] 0 Nothing


newUserFromFUser : FUser -> User 
newUserFromFUser fuser = 
    Registered (fromScalarCodecId fuser.id_) "5678" (UserInfo 1 True "" "" (ExtraUserInfo "" "" "") [""] 1)

type alias FUser = {
    id_ :  SRdb.ScalarCodecs.Id
    , active : Bool
    , description : Maybe String
    , email : Maybe String
    , member_since : Int
    , mobile : Maybe String
    , username : String
    }




newUser : String -> String -> String -> String -> String -> User
newUser username password desc email mobile =
    Registered "" "" (UserInfo 10 True username password (SR.Types.ExtraUserInfo desc email mobile) [""] 0)

empty : Users 
empty = 
    Users (EverySet.empty)

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
        Guest ->
            "Guest"
        (Registered userId token userInfo) ->
            userInfo.username
        (NoWallet userId token userInfo) ->
            userInfo.username
        (NoCredit addr userId token userInfo) ->
            userInfo.username
        (Credited addr userId token userInfo) ->
            userInfo.username

removedDeletedRankingsFromUserJoined : User -> Data.Rankings.Rankings -> User 
removedDeletedRankingsFromUserJoined user sRankings = 
    case user of
        Guest ->
            Guest
        (Registered userId token userInfo) ->
            Registered userId token (handleDeletionFromUserJoined userInfo sRankings)
        (NoWallet userId token userInfo) ->
            NoWallet userId token <| handleDeletionFromUserJoined userInfo sRankings
        (NoCredit addr userId token userInfo) ->
            NoCredit addr userId token <| handleDeletionFromUserJoined userInfo sRankings
        (Credited addr userId token userInfo) ->
            Credited addr userId token <| handleDeletionFromUserJoined userInfo sRankings

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
    --user.username
    -- todo: fix
    ""

userSetLength : Users -> Int 
userSetLength (Users susers) = 
    EverySet.size susers


gotUser : Users -> UserId -> Maybe User
gotUser (Users susers) userId =
    let 
        esUser = EverySet.filter (\user -> (gotUIDFromUser user) == userId) susers
    in
        List.head (EverySet.toList esUser)

gotUIDFromUser : User -> UserId
gotUIDFromUser user = 
    case user of
        Guest ->
            ""
        (Registered userId _ _) ->
            userId
        (NoWallet userId _ _) ->
            userId
        (NoCredit _ userId _ _) ->
            userId
        (Credited _ userId _ _) ->
            userId




-- probably should be updated to return a set, not a list:
addedNewJoinedRankingId : String -> User -> List User -> List User
addedNewJoinedRankingId rankingId user lUser =
    let
        -- currentUser =
        --     gotUserFromUserList lUser user.username
        

        -- if there's anything wrong with the existing joinrankings data fix it here:
        -- userJoinRankings =
        --     List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId user.userjoinrankings)
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
        --     --{ user | userjoinrankings =  validatedRankingAdded}
        --     { user | userjoinrankings =  validatedUserJoinRankings}

        -- newUserList =
        --     userUpdated :: lUser
    in
    --todo: temp fix
    --newUserList
    [Guest]


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
        --userJoinRankings = List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId user.userjoinrankings)
        userJoinRankings = List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId [""])
        --_ = Debug.log "userJoinRankings" userJoinRankings

        filteredOutRanking =
            List.filterMap (filterRankingIds rnkId) userJoinRankings

        --_ = Debug.log "filteredOutRanking" filteredOutRanking

        --userUpdated = {user | userjoinrankings = filteredOutRanking}
    in
        --userUpdated
        -- todo: temp fix
        Guest

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
        lwithDeletedRankingIdsRemoved = List.filter (Data.Rankings.isIdInSet sRankings) (Data.Rankings.stringListToRankingIdList userInfo.userjoinrankings)
        newUserInfo = {userInfo | userjoinrankings = Data.Rankings.rankingIdListToStringList lwithDeletedRankingIdsRemoved} 
    in
        newUserInfo



-- updateAddr : Users -> String -> Users
-- updateAddr susers addr =  
--     case addr of
--         Nothing ->
--             susers
--         Just address -> 
--             let 
--                 user = gotUser susers address
--                 userRemoved = removeUser user susers
--                 updatedUserAddr =
--                         { user | m_ethaddress = address }
--             in 
--             addUser updatedUserAddr userRemoved


updatedUserInSet : Users -> User -> Users
updatedUserInSet susers updatedUser =
--the user is 'Registered' for the purposes of updating the Set
    case updatedUser of
        Guest ->
            susers
        (Registered userId token userInfo) ->
            -- remove the original user, then add the new one
            addUser updatedUser <| removeUser (gotUser susers userId) susers
        (NoWallet userId token userInfo) ->
            addUser updatedUser <| removeUser (gotUser susers userId) susers
        (NoCredit addr userId token userInfo) ->
            addUser updatedUser <| removeUser (gotUser susers userId) susers
        (Credited addr userId token userInfo) ->
            addUser updatedUser <| removeUser (gotUser susers userId) susers



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
    let 
        userNameSet = gotUserNames sUsers
    in
    if EverySet.member newName userNameSet then
        -- let 
        
        --     _ = Debug.log "EverySet.member" "True"
        -- in
        False 
    else if (String.length newName <= 4) then
        False
    else True

-- gotUserListFromRemData : RemoteData.WebData (List User) -> List User
-- gotUserListFromRemData userList =
--     case userList of
--         RemoteData.Success a ->
--             a

--         RemoteData.NotAsked ->
--             [ (User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
--             ]

--         RemoteData.Loading ->
--             [ (User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
--             ]

--         RemoteData.Failure err ->
--             case err of
--                 Http.BadUrl s ->
--                     [ (User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
--                     ]

--                 Http.Timeout ->
--                     [ (User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
--                     ]

--                 Http.NetworkError ->
--                     [ (User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
--                     ]

--                 Http.BadStatus statuscode ->
--                     [ (User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
--                     ]

--                 Http.BadBody s ->
--                     [ (User 0 True "" "" Nothing "" "" "" [""] 0 Nothing)
--                     ]



-- removeCurrentUserEntryFromUserList : List User -> Eth.Types.Address -> List User
-- removeCurrentUserEntryFromUserList userList uaddr =
--     List.filter (\r -> (String.toLower <| r.m_ethaddress) /= (String.toLower <| (Eth.Utils.addressToString uaddr)))
--         (validatedUserList userList)

--private

-- isUserInListStrAddr : List User -> String -> Bool
-- isUserInListStrAddr userlist uaddr =
--     let
--         gotSingleUserFromList =
--             gotUserFromUserList userlist uaddr
--     in
--     if gotSingleUserFromList.m_ethaddress == "" then
--         False

--     else
--         True

-- isRankingId : String -> Bool
-- isRankingId =
--     Regex.contains (Maybe.withDefault Regex.never (Regex.fromString "^((0[Xx]){1})?[0-9A-Fa-f]{40}$"))

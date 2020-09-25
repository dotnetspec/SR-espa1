-- Users will be mainly used to communicate externally to the jsonbin server
module Data.Users exposing (Users
    , newUser
    , updatedUserInSet
    , validatedUserList
    , addedNewJoinedRankingId
    , removedRankingIdFromAll
    , removedRankindIdFromUser
    --, removeCurrentUserEntryFromUserList
    , removedDuplicateUserFromUserList
    , isRegistered
    , isUniqueUserName
    , isEmpty
    , gotUserListFromRemData
    , isNameValidationErr
    , extractUsersFromWebData
    , gotUserFromUserList
    , emptyUsers
    --, updateAddr
    , addUser
    , removeUser
    , asList
    , asUsers
    , getUser
    , gotUser
    , userSetLength
    , isUserNameValidated
    , removedInvalidRankingId
    )


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import SR.Defaults
import Eth.Utils
import RemoteData
import Http
import List.Unique
import Utils.Validation.Validate
import Eth.Types


-- Users (EverySet SR.Types.User) is not the same type as (EverySet SR.Types.User)
-- Peter Damoc
-- You can think about the tag ('Users') as a box containing a type.
type Users = Users (EverySet SR.Types.User)
type UserNames = UserNames (EverySet String)


newUser : String -> String -> Maybe Eth.Types.Address -> String -> String -> String -> SR.Types.User
newUser username password ethaddr desc email mobile =
    SR.Types.User 12345 True username password ethaddr desc email mobile [""] 0 Nothing

emptyUsers : Users 
emptyUsers = 
    Users (EverySet.empty)

isEmpty : Users -> Bool
-- 'Users' is a tag containing a box (of EverySet)
-- using the tag here you can open the box
isEmpty (Users sUsers) =
    EverySet.isEmpty sUsers

asUsers : EverySet SR.Types.User -> Users 
asUsers esUser  = 
    Users esUser

isRegistered : List SR.Types.User -> SR.Types.User -> Bool
isRegistered luser user =
    case user.m_ethaddress of 
        Nothing ->
            False

        Just addr ->
            True

isUserNameValidated : SR.Types.User -> List SR.Types.User -> Bool
isUserNameValidated user luser =
    if String.length user.username > 3 && String.length user.username < 9 && isUniqueUserName user.username luser then
        True

    else
        False

isUniqueUserName : String -> List SR.Types.User -> Bool
isUniqueUserName str luser =
    let
        newList =
            List.filter (\r -> (String.toLower <| r.username) == (String.toLower <| str))
                (validatedUserList luser)
    in
    if List.isEmpty newList then
        True

    else
        False

addUser : SR.Types.User -> Users -> Users
addUser user susers = 
    case susers of 
        Users setOfUsers  ->
                asUsers (EverySet.insert user setOfUsers)

gotUserNames : Users -> EverySet String 
gotUserNames (Users users) = 
    EverySet.map gotName users

gotName : SR.Types.User -> String 
gotName user = 
    user.username

userSetLength : Users -> Int 
userSetLength (Users susers) = 
    EverySet.size susers


gotUser : Users  -> String -> SR.Types.User
gotUser (Users susers) uaddr =
    SR.Defaults.emptyUser
    -- let
    --     existingUser =
    --         List.head <|
    --              EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.m_ethaddress) == (String.toLower <| uaddr))
    --                 susers)
    -- in
    
    -- case existingUser of
    --     Nothing ->
    --         SR.Defaults.emptyUser

    --     Just a ->
    --         --a
    --         SR.Defaults.emptyUser


-- probably should be updated to return a set, not a list:
addedNewJoinedRankingId : String -> SR.Types.User -> List SR.Types.User -> List SR.Types.User
addedNewJoinedRankingId rankingId user lUser =
    let
        -- currentUser =
        --     gotUserFromUserList lUser user.username
        

        -- if there's anything wrong with the existing joinrankings data fix it here:
        userJoinRankings =
            List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId user.userjoinrankings)

        --_ = Debug.log "userJoinRankings in added" userJoinRankings

        validatedRankingAdded = 
            if Utils.Validation.Validate.isValidRankingId rankingId then
                rankingId :: userJoinRankings
            else 
                userJoinRankings

        -- if, somehow, an existing id was added we can again filter out here:
        validatedUserJoinRankings =
            List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId validatedRankingAdded)

        userUpdated =
            --{ user | userjoinrankings =  validatedRankingAdded}
            { user | userjoinrankings =  validatedUserJoinRankings}

        newUserList =
            userUpdated :: lUser
    in
    newUserList


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


removedRankindIdFromUser : String -> SR.Types.User -> SR.Types.User
removedRankindIdFromUser  rnkId user = 
    let
        -- if there's anything wrong with the existing joinrankings data fix it here:
        userJoinRankings = List.Unique.filterDuplicates (List.filterMap removedInvalidRankingId user.userjoinrankings)
        --_ = Debug.log "userJoinRankings" userJoinRankings

        filteredOutRanking =
            List.filterMap (filterRankingIds rnkId) userJoinRankings

        --_ = Debug.log "filteredOutRanking" filteredOutRanking

        userUpdated = {user | userjoinrankings = filteredOutRanking}
    in
        userUpdated

filterRankingIds : String -> String -> Maybe String 
filterRankingIds rnkIdToFilter currentRnkId =
    if currentRnkId == rnkIdToFilter then
        Nothing

    else
        Just currentRnkId

    

removeUser : SR.Types.User -> Users -> Users
removeUser user susers = 
    case susers of 
        Users setOfUsers->
           asUsers (EverySet.remove user setOfUsers) 

--todo: remove
getUser : List SR.Types.UserRanking -> String -> Maybe SR.Types.UserRanking
getUser luranking rankingid =
    List.filterMap
        (isUserRankingIdInList
            rankingid
        )
        luranking
        |> List.head

isUserRankingIdInList : String -> SR.Types.UserRanking -> Maybe SR.Types.UserRanking
isUserRankingIdInList rankingid urnk =
    if urnk.rankingInfo.id == rankingid then
        Just urnk

    else
        Nothing


asList : Users -> List SR.Types.User 
asList susers = 
    case susers of 
        Users setOfUsers ->
            setOfUsers
           |> EverySet.toList


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


updatedUserInSet : Users -> SR.Types.User -> Users
updatedUserInSet susers userToUpdate =
    case userToUpdate.m_ethaddress of 
        Nothing ->
            susers
        Just address ->
            let 
                -- use the address to get the existing user entry to remove
                user = gotUser susers (Eth.Utils.addressToString address)
                userRemoved = removeUser user susers
            in 
                addUser userToUpdate userRemoved


-- todo: remove?
gotUserFromUserList : List SR.Types.User -> String -> SR.Types.User
gotUserFromUserList userList uaddr =
--todo: short term fix to get to compile, this won't currently get a user
    SR.Defaults.emptyUser
    -- let
    --     existingUser =
    --         List.head <|
    --             List.filter (\r -> (String.toLower <| r.m_ethaddress) == (String.toLower <| uaddr))
    --                 (validatedUserList userList)
    -- in
    -- case existingUser of
    --     Nothing ->
    --         SR.Defaults.emptyUser

    --     Just a ->
    --         a

validatedUserList : List SR.Types.User -> List SR.Types.User
validatedUserList luser =
    List.filterMap
        isValidUserAddrInList
        luser


isValidUserAddrInList : SR.Types.User -> Maybe SR.Types.User
isValidUserAddrInList user =
    case user.m_ethaddress of 
        Nothing ->
            Nothing
        Just addr ->
            if Eth.Utils.isAddress (Eth.Utils.addressToString addr) then
                Just user

            else
                Nothing


extractUsersFromWebData : RemoteData.WebData (List SR.Types.User) -> List SR.Types.User
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
            let
                _ =
                    Debug.log "http err" Utils.MyUtils.gotHttpErr <| httpError
            in
            []

isNameValidationErr : String -> Users -> Bool 
isNameValidationErr newName sUsers =
    let 
        userNameSet = gotUserNames sUsers 
    in
    if EverySet.member newName userNameSet then
        True 
    else 
        False

gotUserListFromRemData : RemoteData.WebData (List SR.Types.User) -> List SR.Types.User
gotUserListFromRemData userList =
    case userList of
        RemoteData.Success a ->
            a

        RemoteData.NotAsked ->
            [ SR.Defaults.emptyUser
            ]

        RemoteData.Loading ->
            [ SR.Defaults.emptyUser
            ]

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.Timeout ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.NetworkError ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.BadStatus statuscode ->
                    [ SR.Defaults.emptyUser
                    ]

                Http.BadBody s ->
                    [ SR.Defaults.emptyUser
                    ]

removedDuplicateUserFromUserList : List SR.Types.User -> List SR.Types.User
removedDuplicateUserFromUserList userList =
    let
        laddresses =
            List.map gotAddressesFromUserList userList

        lremovedDuplicateAddresses =
            List.Unique.filterDuplicates laddresses

        lusersWithDuplicatesRemoved =
            List.map (gotUserFromUserList userList) lremovedDuplicateAddresses
    in
    lusersWithDuplicatesRemoved


gotAddressesFromUserList : SR.Types.User -> String
gotAddressesFromUserList user =
    case user.m_ethaddress of
        Nothing ->
            ""
        Just addr ->
            Eth.Utils.addressToString addr

-- removeCurrentUserEntryFromUserList : List SR.Types.User -> Eth.Types.Address -> List SR.Types.User
-- removeCurrentUserEntryFromUserList userList uaddr =
--     List.filter (\r -> (String.toLower <| r.m_ethaddress) /= (String.toLower <| (Eth.Utils.addressToString uaddr)))
--         (validatedUserList userList)

--private

-- isUserInListStrAddr : List SR.Types.User -> String -> Bool
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

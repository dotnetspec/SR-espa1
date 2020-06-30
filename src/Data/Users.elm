-- Users will be mainly used to communicate externally to the jsonbin server
module Data.Users exposing (Users, isNameValidationErr, extractUsersFromWebData, gotUserFromUserList, emptyUsers, updateAddr, addUser, removeUser, asList, asUsers, getUser, gotUser, userSetLength)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import SR.Defaults
import Eth.Utils
import RemoteData


type Users = Users (EverySet SR.Types.User)
type UserNames = UserNames (EverySet String)

emptyUsers : Users 
emptyUsers = 
    Users (EverySet.empty)

asUsers : EverySet SR.Types.User -> Users 
asUsers esUser  = 
    Users esUser 

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
    let
        existingUser =
            List.head <|
                 EverySet.toList (EverySet.filter (\r -> (String.toLower <| r.ethaddress) == (String.toLower <| uaddr))
                    susers)
    in
    
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUser

        Just a ->
            a


-- probably should return a set, not a list:
-- addedNewJoinedRankingIdToUser : String -> SR.Types.User -> List SR.Types.User -> List SR.Types.User
-- addedNewJoinedRankingIdToUser rankingId user lUser =
--     let
--         currentUser =
--             gotUserFromUserList lUser user.username

--         userJoinRankings =
--             currentUser.userjoinrankings

--         newUserJoinRankings =
--             rankingId :: userJoinRankings

--         newUser =
--             { user | userjoinrankings = newUserJoinRankings }

--         newUserList =
--             newUser :: lUser
--     in
--     newUserList

removeUser : SR.Types.User -> Users -> Users
removeUser user susers = 
    case susers of 
        Users setOfUsers->
        --    rnkId 
        --    |> 
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

-- or should we just use:
-- findPlayerInList : SR.Types.User -> List SR.Types.User -> List SR.Types.User
-- findPlayerInList user luPlayer =
--     List.filterMap
--         (isThisPlayerAddr
--             (String.toLower user.ethaddress)
--         )
--         luPlayer

-- isThisPlayerAddr : String -> SR.Types.User -> Maybe SR.Types.User
-- isThisPlayerAddr playerAddr uplayer =
--     if (String.toLower uplayer.player.address) == (String.toLower playerAddr) then
--         Just uplayer

--     else
--         Nothing

asList : Users -> List SR.Types.User 
asList susers = 
    case susers of 
        Users setOfUsers ->
            setOfUsers
           |> EverySet.toList


updateAddr : Users -> String -> Users
updateAddr susers addr =
            let 
                user = gotUser susers addr
                userRemoved = removeUser user susers
                updatedUserAddr =
                        { user | ethaddress = addr }
            in 
                addUser updatedUserAddr userRemoved


-- todo: remove?
gotUserFromUserList : List SR.Types.User -> String -> SR.Types.User
gotUserFromUserList userList uaddr =
    let
        existingUser =
            List.head <|
                List.filter (\r -> (String.toLower <| r.ethaddress) == (String.toLower <| uaddr))
                    (validatedUserList userList)
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUser

        Just a ->
            a

validatedUserList : List SR.Types.User -> List SR.Types.User
validatedUserList luser =
    List.filterMap
        isValidUserAddrInList
        luser


isValidUserAddrInList : SR.Types.User -> Maybe SR.Types.User
isValidUserAddrInList user =
    if Eth.Utils.isAddress user.ethaddress then
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
--a -> EverySet a -> Bool
    let 
        userNameSet = gotUserNames sUsers
        -- uNames = 
        --     case userNameSet of 
        --         UserNames unames -> 
        --             unames 
    in
    if EverySet.member newName userNameSet then
        True 
    else 
        False


-- updatedUserList : Model -> List SR.Types.User -> Model
-- updatedUserList model lusers =
--     case model of
--         AppOps walletState allLists appInfo uiState txRec ->
--             let
--                 resetUserList =
--                     { allLists | users = (Data.Users.asUsers (EverySet.fromList lusers)) }

--                 -- uiState =
--                 --     ensuredCorrectSelectedUI appInfo allLists
--             in
--             AppOps walletState resetUserList appInfo uiState txRec

--         _ ->
--             Failure <| "updateSelectedRankingPlayerList : "


--nb. not sure if this will be used:
-- updateOnUserListReceived : Model -> List SR.Types.User -> ( Model, Cmd Msg )
-- updateOnUserListReceived model userList =
--     case model of
--         AppOps walletState allLists appInfo uiState txRec ->
--             let
--                 gotUserToUpdateAddr =
--                     --SR.ListOps.gotUserFromUserList userList appInfo.user.ethaddress
--                     --Data.Users.getUser (Data.Users.asUsers (EverySet.fromList userList)) appInfo.user.ethaddress
--                     Data.Users.gotUser allLists.users appInfo.user.ethaddress


--                 userWithUpdatedAddr =
--                     { gotUserToUpdateAddr | ethaddress = appInfo.user.ethaddress }

--                 userUpdatedInAppInfo =
--                     { appInfo | user = userWithUpdatedAddr }

--                 newAllLists =
--                     { allLists | users = EverySet.fromList userList }
--             in
--             ( AppOps SR.Types.WalletOperational newAllLists userUpdatedInAppInfo SR.Types.UIRenderAllRankings emptyTxRecord, gotRankingList )

--         _ ->
--             ( Failure "should be in AppOps", Cmd.none )
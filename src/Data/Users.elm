module Data.Users exposing (Users, addUser, removeUser, asList, asUsers, getUser, gotUser, userSetLength)


import SR.Types
import EverySet exposing (EverySet)
import Internal.Types
import Utils.MyUtils
import SR.Defaults
--import SR.ListOps



type Users = Users (EverySet SR.Types.User) 

asUsers : EverySet SR.Types.User -> Users 
asUsers esUser  = 
    Users esUser 

addUser : SR.Types.User -> Users -> Users
addUser user susers = 
    case susers of 
        Users setOfUsers  ->
                asUsers (EverySet.insert user setOfUsers)

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
           --|> List.sortBy extractRank

-- extractRank : SR.Types.User -> Int
-- extractRank uplayer =
--     uplayer.player.rank
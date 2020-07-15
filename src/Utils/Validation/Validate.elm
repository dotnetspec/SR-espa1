module Utils.Validation.Validate exposing (

    validatedUserList
    , validatedMaxTextLength
    , isValidRankingId)


import SR.Types
import Eth.Utils
import Char
import Parser exposing (..)
import Set


validatedMaxTextLength : String -> Int -> String
validatedMaxTextLength str maxLength =
    if String.length str > maxLength then
        String.dropRight 1 str
    else
        str

--there is a small degree of flexibility (+/- 4 chars) to account for server changes
isValidRankingId : String -> Bool 
isValidRankingId str = 
    let 
        result = run rankingIdVar str
    in
    case result of 
        Ok a ->
            --if String.length a > 20 && String.length (validatedMaxTextLength a 24) < 25 then
            if String.length a == 24 then
                True
            else 
                False
        Err a ->
            False

rankingIdVar : Parser String
rankingIdVar =
  variable
    { start = Char.isAlphaNum
    --, inner = \c -> Char.isAlphaNum c || c == '_'
    , inner = \c -> Char.isAlphaNum c
    --, reserved = Set.fromList [ "let", "in", "case", "of" ]
    , reserved = Set.fromList [""]
    }





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

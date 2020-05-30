module Utils.Validation.Validate exposing (

    isUserNameValidated, validatedMaxTextLength)

import SR.ListOps
import SR.Types





validatedMaxTextLength : String -> Int -> String
validatedMaxTextLength str maxLength =
    if String.length str > maxLength then
        String.dropRight 1 str

    else
        str


isUserNameValidated : SR.Types.User -> List SR.Types.User -> Bool
isUserNameValidated user luser =
    if String.length user.username > 3 && String.length user.username < 9 && SR.ListOps.isUniqueUserName user.username luser then
        True

    else
        False

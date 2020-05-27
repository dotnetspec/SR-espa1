module Utils.Validation.Validate exposing (isUserNameValidated, validatedMaxTextLength)

import SR.ListOps
import SR.Types


validatedMaxTextLength : String -> Int -> String
validatedMaxTextLength str maxLength =
    if String.length str > maxLength then
        String.dropRight 1 str

    else
        str


isUserNameValidated : String -> List SR.Types.User -> Bool
isUserNameValidated uname luser =
    if String.length uname > 3 && String.length uname < 9 && SR.ListOps.isUniqueUserName uname luser then
        True

    else
        False

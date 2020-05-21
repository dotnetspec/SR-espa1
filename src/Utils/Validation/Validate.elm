module Utils.Validation.Validate exposing (isUserNameValidated, validatedMaxTextLength)


validatedMaxTextLength : String -> Int -> String
validatedMaxTextLength str maxLength =
    if String.length str > maxLength then
        String.dropRight 1 str

    else
        str


isUserNameValidated : String -> Bool
isUserNameValidated uname =
    if String.length uname > 3 && String.length uname < 9 then
        True

    else
        False

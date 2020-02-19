module Utils.MyUtils exposing (stringFromBool, stringFromMaybeString)


stringFromBool : Bool -> String
stringFromBool bool =
    case bool of
        True ->
            "True"

        False ->
            "False"


stringFromMaybeString : Maybe String -> String 
stringFromMaybeString str = 
 case str of 
    Nothing -> "Not a string"
    Just a -> a

module Utils.MyUtils exposing (stringFromBool)


stringFromBool : Bool -> String
stringFromBool bool =
    case bool of
        True ->
            "True"

        False ->
            "False"

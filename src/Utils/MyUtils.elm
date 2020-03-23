module Utils.MyUtils exposing (addressToString, stringFromBool, stringFromMaybeString)

import Eth.Types
import Eth.Utils


stringFromBool : Bool -> String
stringFromBool bool =
    if
        bool
            == True
    then
        "True"

    else
        "False"


stringFromMaybeString : Maybe String -> String
stringFromMaybeString str =
    case str of
        Nothing ->
            "Not a string"

        Just a ->
            a


addressToString : Maybe Eth.Types.Address -> String
addressToString addr =
    case addr of
        Nothing ->
            "No address"

        Just a ->
            Eth.Utils.addressToString a



-- addressFromMaybeAddress : Maybe Address -> Address
-- addressFromMaybeAddress addr =
--     case addr of
--         Nothing ->
--             Eth.Defaults.invalidAddress
--         Just a ->
--             a

port module Ports exposing (..)

import Json.Decode exposing (Value)
import Json.Encode as Json



--elm-spa example port


port outgoing : { action : String, data : Json.Value } -> Cmd msg


log : String -> Cmd msg
log message =
    outgoing
        { action = "LOG"
        , data = Json.string message
        }



--web3 ports


port walletSentry : (Value -> msg) -> Sub msg


port output : Value -> Cmd msg


port input : (Value -> msg) -> Sub msg



--port input : { action : String, data : Json.Value } -> Sub msg


port txOut : Value -> Cmd msg


port txIn : (Value -> msg) -> Sub msg

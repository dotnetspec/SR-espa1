--standard elm-spa file + handle js messages
-- currently using both flags and port to receive data from js
module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Generated.Routes as Routes exposing (Route)
import Ports exposing (..)
import Debug
import Json.Decode as Decode exposing (Value)


type alias Flags =
      {networkid : Int, comment : String}


type alias Model =
    {incomingData : String}


type Msg =  
    ReceivedDataFromJS String


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ flags =
            let
                dataStrVal = String.fromInt flags.networkid ++ flags.comment

                _ =
                    Debug.log "flag data is : " dataStrVal
            in
            ( {incomingData = ""}
            , Cmd.none
            , Cmd.none
            )


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    case msg of
    ReceivedDataFromJS data ->
            let
                _ =
                    Debug.log "data is: " data

                networkName = if data == "4" then "I see you are using Rinkeby" else "Not sure which network you are on"
            in
            ( { model | incomingData = networkName }, Cmd.none, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    incoming decodeValue

--decodeValue just used to help understanding of incoming port functionality
decodeValue : Value -> Msg
decodeValue x =
    let
        result =
            Decode.decodeValue Decode.string x
    in
        case result of
            Ok string ->
                ReceivedDataFromJS string            
            Err _ -> 
                ReceivedDataFromJS "Silly JavaScript, you can't kill me!"

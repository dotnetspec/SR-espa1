--standard elm-spa file. If needed to handle js messages could do it here
-- currently using port to receive data from wallet


module Global exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , init
    , subscriptions
    , update
    )

--n.b 'as Routes' alias was rm here:

import Generated.Routes exposing (Route)



-- if you needed to you could define a GlobalVariant here
-- that could be accessed via a PageContext which might be defined as context.global


type Model
    = Failure String


type alias Flags =
    {}


type Msg
    = Fail String
    | NoOp


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    ( Failure ""
    , Cmd.none
    , Cmd.none
    )



-- Update needs to take two things: a message Msg (which
-- is a description of the transition that needs to happen),
--  and the model Model (which is the model before the update is applied),
--  and it will return a new model.
-- branching on Model is just a reflection of current state
-- Only branching on Msg will actually change a value or the state


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ msg model =
    case msg of
        Fail str ->
            ( Failure str, Cmd.none, Cmd.none )

        NoOp ->
            ( Failure "NoOp", Cmd.none, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

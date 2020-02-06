module Global exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

--import Eth.Sentry.ChainCmd as ChainCmd exposing (ChainCmd)
--import Browser exposing (..)

import Challenge exposing (..)
import Eth.Net as EthNet exposing (NetworkId(..))
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (..)
import Generated.Routes as Routes exposing (Route)
import Ports exposing (..)



--import Utils.Spa exposing (Page, PageContext)


type alias Flags =
    Int


type alias Model =
    { --browserEnv : BrowserEnv
      --, settings : Maybe SettingsData
    }


type Msg
    = Msg
      -- Port/Sub Related Msgs
    | WalletStatus WalletSentry
    | EventSentryMsg EventSentry.Msg
    | TxSentryMsg TxSentry.Msg
    | Fail String


type alias Commands msg =
    { navigate : Route -> Cmd msg
    }


init : Commands msg -> Flags -> ( Model, Cmd Msg, Cmd msg )
init _ _ =
    ( { --  browserEnv = Nothing
        --, settings = Nothing
      }
    , Cmd.none
    , Ports.log "Hello there!"
    )


update : Commands msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update _ _ model =
    ( model
    , Cmd.none
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ --pageSubscriptions model.page
          Ports.walletSentry (WalletSentry.decodeToMsg Fail WalletStatus)

        --, EventSentry.listen model.eventSentry
        --, TxSentry.listen model.txSentry
        ]

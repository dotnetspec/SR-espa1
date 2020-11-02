module SR.Types exposing
    ( PlayerId, RankingId(..)
    --, Player, 
    , Opponent, OpponentRelativeRank(..), Options, ResultOfMatch(..), WalletState(..)
    , UIState(..)
    , AppInfo, Colors, CreateNewLadderFormFields, FormValidations, LadderState(..), ModalState(..)
    , NewRankingListServerResponse, ResultRadioOptions(..), User(..), UserListState(..), UserPlayer
    , colors
    ,  AppState(..)
    , SelectedStatus(..)
    , SubState(..)
    , DeleteBinResponse
    , FUser
    , newUser
    , FRanking
    , newRanking
    , fromScalarCodecId
    , FPlayer
    , newPlayer
    , UserInfo
    , ExtraUserInfo
    , UserId
    )

{-| Types


# Simple

@docs PlayerId, RankingId


# Complex

@docs Player, Opponent, OpponentRelativeRank, Options, PlayerAvailability, ResultOfMatch, SRState, UserState, WalletState
@docs UIState


# Misc

@docs examples from original = HttpProvider, WebsocketProvider, FilterId

-}

import Element exposing (..)
import Eth.Types
import Http
import Internal.Types as Internal
import Ports
import RemoteData
import EverySet exposing (EverySet)

import SRdb.ScalarCodecs
import Eth.Utils
import Eth.Utils
import Css exposing (em)
import Result
--import SR.Defaults
import SRdb.Scalar exposing (Id(..))
import Data.Rankings
import Data.Players
import Data.Users
--import Json.Decode exposing (Decoder)


type Error
    = Http Http.Error -- Standard HTTP Errors
    | Encoding String -- Most likely an overflow of int/uint
      -- Call returns 0x, could mean:
      -- Contract doesn't exist
      -- Contract function doesn't exist
      -- Other things (look at the talk by Augur team at Devcon4 on mainstage)
    | ZeroX String
      -- TxSentry Errors:
    | UserRejected -- User dissapproved of tx in Wallet
    | Web3Undefined -- Web3 object, or provider not found.



-- Simple

type ModalState
    = Open
    | Closed


type ResultRadioOptions
    = WonRadio
    | LostRadio
    | UndecidedRadio


type alias PlayerId =
    Internal.PlayerId


type RankingId
    = RankingId String


type alias Opponent =
    UserPlayer


type Username
    = Username String


type WalletState
    = 
    WalletStateMissing
    | WalletStateLocked
    | WalletStateAwaitOpening
    | WalletWaitingForTransactionReceipt
    | WalletOpened
    | WalletOperational
    | WalletStopSub
    | WalletOpenedNoUserAccount


type LadderState
    = ExistingLadder Data.Rankings.Ranking
    | NewLadder Data.Rankings.Ranking


type UIState
    = UIRenderAllRankings
    | UICreateNewLadder
    | UIRegisterNewUser
    | UIUpdateExistingUser
    | UIWalletMissingInstructions
    | UIDisplayWalletLockedInstructions
    | UIDisplayWalletInfoToUser
    | UISelectedRankingUserIsOwner
    | UISelectedRankingUserIsPlayer
    | UISelectedRankingUserIsNeitherOwnerNorPlayer
    | UIEnterResult
    | UIEnterResultTxProblem
    | UIChallenge
    | UILoading
    | UIWaitingForTxReceipt
    | UIDeleteRankingConfirm
    | UIEnableEthereum
    | UIOwnerDeletedRanking
    | UIUnableToFindGlobalRankings
    | UIEthAlreadyEnabled
    | UILogIn

type SubState 
    = Subscribe 
    | StopSubscription



type UserListState
    = Success (List User)
    | Loading
    | NotAsked
    | Failure String


type SelectedStatus
 = UserIsOwner
 | UserIsMember
 | UserIsNeitherOwnerNorMember

type alias CreateNewLadderFormFields =
    { name : String
    , desc : String
    }


type Options
    = MatchChallenge
    | Result


type ResultOfMatch
    = Won
    | Lost
    | Undecided


type OpponentRelativeRank
    = OpponentRankHigher
    | OpponentRankLower


--todo probably remove AccountState
-- type AccountState 
--     = 
    --Guest 
    --changed Registered to get to compile
    --| Registered
    -- EthEnabled
    -- | EthEnabledAndRegistered


type alias UserPlayer =
    { player : Data.Players.Player
    , user : Data.Users.User
    }





type alias AppInfo =
    { selectedRanking : Data.Rankings.Ranking
    , player : UserPlayer
    , user : User
    , challenger : UserPlayer
    , appState : AppState
    }

type AppState
    = AppStateGeneral
    | AppStateCreateNewUser
    | AppStateUpdateProfile
    | AppStateCreateNewLadder 
    | AppStateEnterWon 
    | AppStateEnterLost 
    | AppStateEnterUndecided


type alias FormValidations =
    { username : String
    , userdesc : String
    , laddername : String
    , ladderdesc : String
    , email : String
    , mobile : String
    }


type alias DeleteBinResponse =
    {
    success : Bool,
    id : String,
    message : String
    }


type alias NewRankingListServerResponse =
    { success : Bool
    , data :
        List
            { id : String
            , active : Bool
            , rankingname : String
            , rankingdesc : String
            }
    , version : Int
    , parentId : String
    }


type alias Colors =
    { coral : Element.Color
    , white : Element.Color
    , lightblue : Element.Color
    , blue : Element.Color
    , green : Element.Color
    , purple : Element.Color
    , black : Element.Color
    , red : Element.Color
    , darkBlue : Element.Color
    , grey : Element.Color
    }


colors : Colors
colors =
    { coral = rgb255 204 75 75
    , white = rgb255 255 255 255
    , lightblue = rgb255 0 128 255
    , blue = rgb255 2 7 239
    , green = rgb255 0 153 0
    , purple = rgb255 102 0 102
    , black = rgb255 0 0 0
    , red = rgb 0.8 0 0
    , darkBlue = rgb 0 0 0.9
    , grey = rgb 0.9 0.9 0.9
    }
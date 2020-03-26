module SR.Types exposing
    ( PlayerId, RankingId(..)
    , Player, Opponent, OpponentRelativeRank(..), Options, PlayerAvailability(..), ResultOfMatch(..), SRState(..), UserState(..), WalletState(..)
    , UIState(..)
    ,  CreateNewLadderFormFields
      , ModalState(..)
      , NewRankingListServerResponse
      , RankingInfo
      , ResultRadioOptions(..)
        --, JsonApiURLs(..)
      , User
      , UserListState(..)
      , Username(..)

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

import Eth.Types
import Http
import Internal.Types as Internal
import Ports
import RemoteData



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



-- type Ranking
--     = Global (Result Http.Error String)
--     | Random (Result Http.Error String)


type alias Opponent =
    Player


type Username
    = Username String



-- type AddressState
--     = Exists String
--     | None
-- Complex


type WalletState
    = Missing
    | Locked --Ports.EthNode
    | Opened --Ports.EthNode Eth.Types.Address
    | Active
    | Inactive



--| Transaction --Ports.EthNode Eth.Types.Address


type SRState
    = AllRankingsJson (RemoteData.WebData (List RankingInfo))
    | NewEmpty
      --| SingleRanking (RemoteData.WebData (List Player)) RankingId
      --| EnterResult
    | SRStateFailure String


type UserState
    = ExistingUser Eth.Types.Address
    | NewUser


type UIState
    = RenderAllRankings
    | CreateNewLadder


type UserListState
    = Success (List User)
    | Loading
    | NotAsked
    | Failure String


type alias CreateNewLadderFormFields =
    { name : String
    , desc : String
    }



-- MissingWalletDialogOpen
-- | LockedWalletDialogOpen
-- | DialogClosed


type Options
    = Challenge
    | Result


type ResultOfMatch
    = Won
    | Lost
    | Undecided


type OpponentRelativeRank
    = OpponentRankHigher
    | OpponentRankLower


type PlayerAvailability
    = Available
    | Unavailable


{-| -}
type alias Player =
    { datestamp : Int
    , active : Bool
    , currentchallengername : String
    , currentchallengerid : Int
    , address : String
    , rank : Int
    , name : String
    , id : Int
    , currentchallengeraddress : String
    }


type alias RankingInfo =
    { id : String
    , active : Bool
    , rankingname : String
    , rankingdesc : String
    , rankingowneraddr : String
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


type alias User =
    { datestamp : Int
    , active : Bool
    , username : String
    , ethaddress : String
    , description : String
    , email : String
    , mobile : String
    }

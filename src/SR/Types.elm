module SR.Types exposing
    ( PlayerId, RankingId(..)
    , Player, Opponent, OpponentRelativeRank(..), Options, PlayerAvailability(..), ResultOfMatch(..), SRState(..), UserState(..), WalletState(..)
    , UIState(..)
    ,  AllLists
      , AppInfo
      , Challenge
      , CreateNewLadderFormFields
      , LadderState(..)
      , ModalState(..)
      , NewRankingListServerResponse
      , RankingInfo
      , ResultRadioOptions(..)
        --, JsonApiURLs(..)
      , User
      , UserListState(..)

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
      --| Opened --Ports.EthNode Eth.Types.Address
    | WalletOpenedWithoutUserCheck Eth.Types.Address
    | WalletOpenedUserCheckDone User Eth.Types.Address
    | WalletOpenedAndOperational



--| Inactive
--| Transaction --Ports.EthNode Eth.Types.Address


type SRState
    = AllRankingsJson (RemoteData.WebData (List RankingInfo))
    | NewEmpty
      --| SingleRanking (RemoteData.WebData (List Player)) RankingId
      --| EnterResult
    | SRStateFailure String


type UserState
    = ExistingUser User
    | NewUser User


type LadderState
    = ExistingLadder RankingInfo
    | NewLadder RankingInfo


type UIState
    = UIRenderAllRankings
    | CreateNewLadder
    | CreateNewUser
    | DisplayWalletInfoToUser
    | UISelectedRankingUserIsOwner
    | UISelectedRankingUserIsPlayer
    | UISelectedRankingUserIsNeitherOwnerNorPlayer
    | UIChallenge


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
    = MatchChallenge
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


{--}
type alias AllLists =
    { globalRankings : List RankingInfo
    , players : List Player
    , users : List User
    }
--}


type alias AppInfo =
    { selectedRanking : RankingInfo
    , player : Player
    , user : User
    , challenge : Challenge
    }


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


type alias Challenge =
    { playerid : Int
    , player : Player
    , opponent : Player
    , playerRank : Int
    , opponentRank : Int
    , playerStatus : PlayerAvailability
    , opponentStatus : PlayerAvailability
    , rankingid : String
    , opponentEmail : String
    , opponentMobile : String
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

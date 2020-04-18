module SR.Types exposing
    ( PlayerId, RankingId(..)
    , Player, Opponent, OpponentRelativeRank(..), Options, ResultOfMatch(..), UserState(..), WalletState(..)
    , UIState(..)
    , AllLists, AppInfo, CreateNewLadderFormFields, LadderState(..), ModalState(..), NewRankingListServerResponse, RankingInfo, ResultRadioOptions(..), User, UserListState(..)
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


type alias Opponent =
    Player


type Username
    = Username String


type WalletState
    = Missing
    | Locked
    | WalletOpenedWithoutUserCheck Eth.Types.Address
    | WalletOpenedUserCheckDone User Eth.Types.Address
    | WalletWaitingForTransactionReceipt
    | WalletOpenedAndOperational


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
    | UIDisplayWalletLockedInstructions
    | UIDisplayWalletInfoToUser
    | UISelectedRankingUserIsOwner
    | UISelectedRankingUserIsPlayer
    | UISelectedRankingUserIsNeitherOwnerNorPlayer
    | UIEnterResult
    | UIEnterResultTxProblem
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
    , challenger : Player
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


type alias Player =
    { address : String
    , rank : Int
    , challengeraddress : String
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

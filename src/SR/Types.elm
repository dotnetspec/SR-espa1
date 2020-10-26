module SR.Types exposing
    ( PlayerId, RankingId(..)
    , Player, Opponent, OpponentRelativeRank(..), Options, ResultOfMatch(..), WalletState(..)
    , UIState(..)
    --, AllLists
    , AppInfo, Colors, CreateNewLadderFormFields, FormValidations, LadderState(..), ModalState(..), NewRankingListServerResponse, Ranking, ResultRadioOptions(..), User, UserListState(..), UserPlayer, UserRanking, colors
    ,  AppState(..)
    , SelectedStatus(..)
    , SubState(..)
    , DeleteBinResponse
    , UpdateGlobalBinResponse
    , AccountState(..)
    , Token
    , UserName
    , Password
    , FUser
    , newUser
    , FRanking
    , newRanking
    , newUserRanking
    , fromScalarCodecId
    , FPlayer
    , newPlayer
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
--import Data.Users
import SRdb.ScalarCodecs
import Eth.Utils
import Eth.Utils
import Css exposing (em)
import Result
--import SR.Defaults
import SRdb.Scalar exposing (Id(..))




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
    = ExistingLadder Ranking
    | NewLadder Ranking


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
type AccountState 
    = 
    --Guest 
    --changed Registered to get to compile
    --| Registered
    EthEnabled
    | EthEnabledAndRegistered


type alias UserPlayer =
    { player : Player
    , user : User
    }


type alias UserRanking =
    { rankingInfo : Ranking
    , userInfo : User
    }

newUserRanking ranking user =
    UserRanking ranking user


type alias AppInfo =
    { selectedRanking : Ranking
    , player : UserPlayer
    , m_user : Maybe User
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


type alias UserInfo =
    { --datestamp to become creditsremaining
    datestamp : Int
    , active : Bool
    , username : String
    , password : String
    , extrauserinfo : ExtraUserInfo
    --, m_ethaddress : Maybe Eth.Types.Address
    -- , description : String
    -- , email : String
    -- , mobile : String
    , userjoinrankings : List String
    , member_since : Int
    --, m_token : Maybe Token
    }

type alias UserId =
    String

-- type alias Member_Since =
--     Int

-- type alias UserName2 =
--     String

type alias ExtraUserInfo =
    {
    description : String
    , email : String
    , mobile : String
    }

-- type alias UserJoinedRankings2 =
--     List String


type User =
    Guest
    | Registered UserId Token UserInfo
    | NoWallet UserId Token UserInfo
    | NoCredit Eth.Types.Address UserId Token UserInfo
    | Credited Eth.Types.Address UserId Token UserInfo


-- new empty User:
-- SR.Types.User 0 True "" "" Nothing "" "" "" [""] 0 Nothing


newUser : FUser -> User 
newUser fuser = 
    let 
        ethaddrResult = Result.toMaybe (Eth.Utils.toAddress fuser.ethaddress)
    in
        --User 1234 True fuser.username "" (Maybe.withDefault Nothing (Just ethaddrResult)) (Maybe.withDefault "" fuser.description) (Maybe.withDefault "" fuser.email) (Maybe.withDefault "" fuser.mobile) [] fuser.member_since  Nothing
        Registered "1234" "5678" (UserInfo 1 True "" "" (ExtraUserInfo "" "" "") [""] 1)

type alias FUser = {
    active : Bool
    , description : Maybe String
    , email : Maybe String
    , ethaddress : String
    , member_since : Int
    , mobile : Maybe String
    , username : String
    }

type alias Token =
    String

type alias UserName =
    String

type alias Password =
    String


type alias Player =
    { rankingid : String
    , address : String
    , rank : Int
    , challengeraddress : String
    }

-- empty player
--  SR.Types.Player "" "" 0 ""

type alias FPlayer =
    { id_ : SRdb.ScalarCodecs.Id
    , rankingid : String
    , address : String
    , rank : Int
    , challengeraddress : String
    }

newPlayer : FPlayer -> Player 
newPlayer fplayer = 
    Player (fromScalarCodecId fplayer.id_) fplayer.address fplayer.rank fplayer.challengeraddress




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

type alias UpdateGlobalBinResponse =
    {
    success : Bool,
    data : List Ranking,
    version : Int,
    parentId : String
    }
    

type alias Ranking =
    { 
        id_ : String
     --id_ : SRdb.ScalarCodecs.Id
    , active : Bool
    , rankingname : String
    , rankingdesc : Maybe String
    , rankingowneraddr : String
    }

-- empty ranking
-- SR.Types.Ranking "" True "" Nothing ""

newRanking : FRanking -> Ranking 
newRanking franking = 
    Ranking (fromScalarCodecId franking.id_) True franking.rankingname franking.rankingdesc franking.rankingowneraddr

type alias FRanking =

    { id_ : SRdb.ScalarCodecs.Id
    , active : Bool
    , rankingname : String
    , rankingdesc : Maybe String
    , rankingowneraddr : String
    }

fromScalarCodecId : SRdb.ScalarCodecs.Id -> String
fromScalarCodecId (Id id) =
    let
        _ = Debug.log "id : " id
    in
        id



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
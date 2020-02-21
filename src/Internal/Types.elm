module Internal.Types exposing (Address(..), BlockHash(..), DebugLogger, Hex(..), PlayerId(..), RankingId(..), TxHash(..), WhisperId(..))


type Address
    = Address String


type TxHash
    = TxHash String


type BlockHash
    = BlockHash String


type WhisperId
    = WhisperId String


type Hex
    = Hex String

type PlayerId
    = PlayerId Int

type RankingId
    = RankingId String


type alias DebugLogger a =
    String -> a -> a

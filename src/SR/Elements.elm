module SR.Elements exposing (selectedRankingHeaderEl)

--import RemoteData

import Element exposing (Element)
import Eth.Types
import Eth.Utils
import Framework.Heading as Heading
import Internal.Types
import SR.Defaults
import SR.Types
import Utils.MyUtils


selectedRankingHeaderEl : SR.Types.RankingInfo -> Element msg
selectedRankingHeaderEl rnkInfo =
    Element.el Heading.h2 <| Element.text "Selected Ranking here"

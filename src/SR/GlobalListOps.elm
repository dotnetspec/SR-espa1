module SR.GlobalListOps exposing
    ( filterSelectedRankingOutOfGlobalList
    , gotRankingFromRankingList
    , ownerValidatedRankingList
    )

import Eth.Types
import Eth.Utils
import Http
import Internal.Types
import RemoteData
import SR.Defaults
import SR.Types
import Utils.MyUtils



-- external


ownerValidatedRankingList : List SR.Types.RankingInfo -> List SR.Types.RankingInfo
ownerValidatedRankingList lrankinginfo =
    lrankinginfo



-- current


reorderPlayerListToStartAtOne : List SR.Types.Player -> List SR.Types.Player
reorderPlayerListToStartAtOne lplayer =
    let
        newPlayerListAllRankIsOne =
            List.map resetPlayerRankToOne lplayer

        newListLength =
            List.length lplayer

        newAscendingList =
            List.range 1 newListLength

        listscombined =
            List.map2 resetPlayerRankingList newAscendingList newPlayerListAllRankIsOne
    in
    listscombined


resetPlayerRankingList : Int -> SR.Types.Player -> SR.Types.Player
resetPlayerRankingList newRank player =
    let
        newPlayer =
            { player
                | address = player.address
                , rank = newRank
                , challengeraddress = player.challengeraddress
            }
    in
    newPlayer


addOne : Int -> Int
addOne int =
    if int == 1 then
        1

    else
        int + 1


resetPlayerRankToOne : SR.Types.Player -> SR.Types.Player
resetPlayerRankToOne player =
    let
        newPlayer =
            { player
                | address = player.address
                , rank = 1
                , challengeraddress = player.challengeraddress
            }
    in
    newPlayer



-- isThisRankGreaterThanPrevious : Int -> Int -> Bool
-- isThisRankGreaterThanPrevious previousRank currentRank =


canPlayerBeInList : Maybe SR.Types.Player -> Bool
canPlayerBeInList player =
    case player of
        Nothing ->
            False

        Just a ->
            True


gotRankingFromRankingList : List SR.Types.RankingInfo -> Internal.Types.RankingId -> SR.Types.RankingInfo
gotRankingFromRankingList rankingList (Internal.Types.RankingId rnkid) =
    let
        existingRanking =
            List.head <|
                List.filter (\r -> r.id == String.toLower rnkid)
                    rankingList
    in
    case existingRanking of
        Nothing ->
            SR.Defaults.emptyRankingInfo

        Just a ->
            a


filterSelectedRankingOutOfGlobalList : String -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
filterSelectedRankingOutOfGlobalList rankingid lrankinginfo =
    List.filterMap
        (doesCurrentRankingIdNOTMatchId
            rankingid
        )
        lrankinginfo


filterPlayerOutOfPlayerList : String -> List SR.Types.Player -> List SR.Types.Player
filterPlayerOutOfPlayerList addr lplayer =
    List.filterMap
        (doesPlayerAddrNOTMatchAddr
            addr
        )
        lplayer



--internal


doesCurrentRankingIdNOTMatchId : String -> SR.Types.RankingInfo -> Maybe SR.Types.RankingInfo
doesCurrentRankingIdNOTMatchId rankingid rankingInfo =
    if rankingInfo.id /= rankingid then
        Just rankingInfo

    else
        Nothing


doesPlayerAddrNOTMatchAddr : String -> SR.Types.Player -> Maybe SR.Types.Player
doesPlayerAddrNOTMatchAddr addr player =
    if player.address /= addr then
        Just player

    else
        Nothing


findSelectedRankingInGlobalList : String -> List SR.Types.RankingInfo -> List SR.Types.RankingInfo
findSelectedRankingInGlobalList rankingid lrankinginfo =
    List.filterMap
        (isRankingIdInList
            rankingid
        )
        lrankinginfo


isRankingIdInList : String -> SR.Types.RankingInfo -> Maybe SR.Types.RankingInfo
isRankingIdInList rankingid rnk =
    if rnk.id == rankingid then
        Just rnk

    else
        Nothing


findPlayerInList : SR.Types.User -> List SR.Types.Player -> List SR.Types.Player
findPlayerInList user lPlayer =
    List.filterMap
        (isThisPlayerAddr
            user.ethaddress
        )
        lPlayer


isThisPlayerAddr : String -> SR.Types.Player -> Maybe SR.Types.Player
isThisPlayerAddr playerAddr player =
    if player.address == playerAddr then
        Just player

    else
        Nothing


gotRankingListFromRemData : RemoteData.WebData (List SR.Types.RankingInfo) -> List SR.Types.RankingInfo
gotRankingListFromRemData globalList =
    case globalList of
        RemoteData.Success a ->
            a

        RemoteData.NotAsked ->
            [ SR.Defaults.emptyRankingInfo
            ]

        RemoteData.Loading ->
            [ SR.Defaults.emptyRankingInfo
            ]

        RemoteData.Failure err ->
            case err of
                Http.BadUrl s ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.Timeout ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.NetworkError ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.BadStatus statuscode ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

                Http.BadBody s ->
                    [ SR.Defaults.emptyRankingInfo
                    ]

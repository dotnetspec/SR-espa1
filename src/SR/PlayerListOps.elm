module SR.PlayerListOps exposing
    ( findPlayerInList
    , gotCurrentUserAsPlayerFromPlayerList
    , gotPlayerFromPlayerListStrAddress
    , setPlayerInPlayerListWithChallengeResult
    , setPlayerInPlayerListWithNewChallengerAddr
    , sortedPlayerListByRank
    , updatePlayerRankWithWonResult
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


setPlayerInPlayerListWithChallengeResult : List SR.Types.Player -> SR.Types.Player -> Int -> List SR.Types.Player
setPlayerInPlayerListWithChallengeResult lPlayer player rank =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList player.address lPlayer

        updatedPlayer =
            { player | challengeraddress = "", rank = rank }

        newPlayerList =
            updatedPlayer :: filteredPlayerList

        newPlayerListSorted =
            sortedPlayerListByRank newPlayerList
    in
    newPlayerListSorted


setPlayerInPlayerListWithNewChallengerAddr : List SR.Types.Player -> SR.Types.Player -> String -> List SR.Types.Player
setPlayerInPlayerListWithNewChallengerAddr lPlayer player challengeraddress =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList player.address lPlayer

        updatedPlayer =
            { player | challengeraddress = challengeraddress }

        newPlayerList =
            updatedPlayer :: filteredPlayerList
    in
    newPlayerList


updatePlayerRankWithWonResult : List SR.Types.Player -> SR.Types.Player -> List SR.Types.Player
updatePlayerRankWithWonResult lPlayer player =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList player.address lPlayer

        opponentAsPlayer =
            gotPlayerFromPlayerListStrAddress lPlayer player.challengeraddress

        -- this needs more ?:
        updatedPlayer =
            { player | rank = opponentAsPlayer.rank }

        newPlayerList =
            updatedPlayer :: filteredPlayerList
    in
    newPlayerList


sortedPlayerListByRank : List SR.Types.Player -> List SR.Types.Player
sortedPlayerListByRank lplayer =
    let
        validatedMaybePlayerLst =
            List.map Utils.MyUtils.splitPlayerFieldsToCreateMaybePlayer lplayer

        filteredValidatedMaybePlayerLst =
            List.filter canPlayerBeInList validatedMaybePlayerLst

        convertedValidatedPlayerList =
            List.map Utils.MyUtils.convertMaybePlayerToPlayer filteredValidatedMaybePlayerLst

        reorderedConvertedValidatedPlayerList =
            reorderPlayerListToStartAtOne
                convertedValidatedPlayerList

        -- _ =
        --     Debug.log "reorderedConvertedValidatedPlayerList" reorderedConvertedValidatedPlayerList
    in
    List.sortBy .rank reorderedConvertedValidatedPlayerList



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


canPlayerBeInList : Maybe SR.Types.Player -> Bool
canPlayerBeInList player =
    case player of
        Nothing ->
            False

        Just a ->
            True


gotCurrentUserAsPlayerFromPlayerList : List SR.Types.Player -> SR.Types.User -> SR.Types.Player
gotCurrentUserAsPlayerFromPlayerList lPlayer userRec =
    let
        existingPlayer =
            List.head <|
                List.filter (\r -> r.address == (String.toLower <| userRec.ethaddress))
                    lPlayer
    in
    case existingPlayer of
        Nothing ->
            SR.Defaults.emptyPlayer

        Just a ->
            a


gotPlayerFromPlayerListStrAddress : List SR.Types.Player -> String -> SR.Types.Player
gotPlayerFromPlayerListStrAddress lplayer addr =
    let
        existingUser =
            List.head <|
                List.filter (\r -> r.address == (String.toLower <| addr))
                    lplayer
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyPlayer

        Just a ->
            a


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



-- gotUserListFromRemData : RemoteData.WebData (List SR.Types.User) -> List SR.Types.User
-- gotUserListFromRemData userList =
--     case userList of
--         RemoteData.Success a ->
--             a
--         RemoteData.NotAsked ->
--             [ SR.Defaults.emptyUser
--             ]
--         RemoteData.Loading ->
--             [ SR.Defaults.emptyUser
--             ]
--         RemoteData.Failure err ->
--             case err of
--                 Http.BadUrl s ->
--                     [ SR.Defaults.emptyUser
--                     ]
--                 Http.Timeout ->
--                     [ SR.Defaults.emptyUser
--                     ]
--                 Http.NetworkError ->
--                     [ SR.Defaults.emptyUser
--                     ]
--                 Http.BadStatus statuscode ->
--                     [ SR.Defaults.emptyUser
--                     ]
--                 Http.BadBody s ->
--                     [ SR.Defaults.emptyUser
--                     ]

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


setPlayerInPlayerListWithChallengeResult : List SR.Types.UserPlayer -> SR.Types.UserPlayer -> Int -> List SR.Types.UserPlayer
setPlayerInPlayerListWithChallengeResult luPlayer uplayer rank =
    let
        playerToUserPlayer =
            uplayer.player

        filteredPlayerList =
            filterPlayerOutOfPlayerList playerToUserPlayer.address luPlayer

        updatedPlayer =
            { playerToUserPlayer | challengeraddress = "", rank = rank }

        newUserPlayer =
            { uplayer | player = updatedPlayer }

        newPlayerList =
            newUserPlayer :: filteredPlayerList

        newPlayerListSorted =
            sortedPlayerListByRank newPlayerList
    in
    newPlayerListSorted


setPlayerInPlayerListWithNewChallengerAddr : List SR.Types.UserPlayer -> SR.Types.UserPlayer -> String -> List SR.Types.UserPlayer
setPlayerInPlayerListWithNewChallengerAddr lPlayer uplayer challengeraddress =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList uplayer.player.address lPlayer

        newUserPlayerPlayerField =
            uplayer.player

        updatedPlayer =
            { newUserPlayerPlayerField | challengeraddress = uplayer.player.challengeraddress }

        newUserPlayer =
            { uplayer | player = updatedPlayer }

        newPlayerList =
            newUserPlayer :: filteredPlayerList
    in
    newPlayerList


updatePlayerRankWithWonResult : List SR.Types.UserPlayer -> SR.Types.UserPlayer -> List SR.Types.UserPlayer
updatePlayerRankWithWonResult luPlayer uplayer =
    let
        filteredPlayerList =
            filterPlayerOutOfPlayerList uplayer.player.address luPlayer

        opponentAsPlayer =
            gotPlayerFromPlayerListStrAddress luPlayer uplayer.player.challengeraddress

        -- this needs more ?:
        newUserPlayerPlayerField =
            uplayer.player

        updatedPlayer =
            { newUserPlayerPlayerField | rank = opponentAsPlayer.player.rank }

        newUserPlayer =
            { uplayer | player = updatedPlayer }

        newPlayerList =
            newUserPlayer :: filteredPlayerList
    in
    newPlayerList


sortedPlayerListByRank : List SR.Types.UserPlayer -> List SR.Types.UserPlayer
sortedPlayerListByRank luplayer =
    let
        validatedMaybePlayerLst =
            List.map Utils.MyUtils.splitPlayerFieldsToCreateMaybePlayer luplayer

        filteredValidatedMaybePlayerLst =
            List.filter canPlayerBeInList validatedMaybePlayerLst

        convertedValidatedPlayerList =
            List.map Utils.MyUtils.convertMaybePlayerToPlayer filteredValidatedMaybePlayerLst

        reorderedConvertedValidatedPlayerList =
            reorderPlayerListToStartAtOne
                convertedValidatedPlayerList
    in
    List.sortBy extractRank reorderedConvertedValidatedPlayerList


extractRank : SR.Types.UserPlayer -> Int
extractRank uplayer =
    uplayer.player.rank


reorderPlayerListToStartAtOne : List SR.Types.UserPlayer -> List SR.Types.UserPlayer
reorderPlayerListToStartAtOne luplayer =
    let
        newPlayerListAllRankIsOne =
            List.map resetPlayerRankToOne luplayer

        newListLength =
            List.length luplayer

        newAscendingList =
            List.range 1 newListLength

        listscombined =
            List.map2 resetPlayerRankingList newAscendingList newPlayerListAllRankIsOne
    in
    listscombined


resetPlayerRankingList : Int -> SR.Types.UserPlayer -> SR.Types.UserPlayer
resetPlayerRankingList newRank uplayer =
    let
        newuserplayerplayer =
            uplayer.player

        newPlayer =
            { newuserplayerplayer
                | address = uplayer.player.address
                , rank = newRank
                , challengeraddress = uplayer.player.challengeraddress
            }

        newUserPlayer =
            { uplayer | player = newPlayer }
    in
    newUserPlayer


resetPlayerRankToOne : SR.Types.UserPlayer -> SR.Types.UserPlayer
resetPlayerRankToOne uplayer =
    let
        newuserplayerplayer =
            uplayer.player

        newPlayer =
            { newuserplayerplayer
                | address = uplayer.player.address
                , rank = 1
                , challengeraddress = uplayer.player.challengeraddress
            }

        newUserPlayer =
            { uplayer | player = newPlayer }
    in
    newUserPlayer


canPlayerBeInList : Maybe SR.Types.UserPlayer -> Bool
canPlayerBeInList uplayer =
    case uplayer of
        Nothing ->
            False

        Just a ->
            True


gotCurrentUserAsPlayerFromPlayerList : List SR.Types.UserPlayer -> SR.Types.User -> SR.Types.UserPlayer
gotCurrentUserAsPlayerFromPlayerList luPlayer userRec =
    let
        existingPlayer =
            List.head <|
                List.filter (\r -> r.player.address == (String.toLower <| userRec.ethaddress))
                    luPlayer
    in
    case existingPlayer of
        Nothing ->
            SR.Defaults.emptyUserPlayer

        Just a ->
            a


gotPlayerFromPlayerListStrAddress : List SR.Types.UserPlayer -> String -> SR.Types.UserPlayer
gotPlayerFromPlayerListStrAddress luplayer addr =
    let
        existingUser =
            List.head <|
                List.filter (\r -> r.player.address == (String.toLower <| addr))
                    luplayer
    in
    case existingUser of
        Nothing ->
            SR.Defaults.emptyUserPlayer

        Just a ->
            a


filterPlayerOutOfPlayerList : String -> List SR.Types.UserPlayer -> List SR.Types.UserPlayer
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


doesPlayerAddrNOTMatchAddr : String -> SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
doesPlayerAddrNOTMatchAddr addr player =
    if player.player.address /= addr then
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


findPlayerInList : SR.Types.User -> List SR.Types.UserPlayer -> List SR.Types.UserPlayer
findPlayerInList user luPlayer =
    List.filterMap
        (isThisPlayerAddr
            user.ethaddress
        )
        luPlayer


isThisPlayerAddr : String -> SR.Types.UserPlayer -> Maybe SR.Types.UserPlayer
isThisPlayerAddr playerAddr uplayer =
    if uplayer.player.address == playerAddr then
        Just uplayer

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

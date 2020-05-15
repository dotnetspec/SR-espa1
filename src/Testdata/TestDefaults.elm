module Testdata.TestDefaults exposing (simWalletSentryDataT1, simWalletSentryDataT11)

import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Wallet
import Internal.Types



--change address for relevant test user:
-- Test 1 - 0x4A0a14bA869bEe85c490A5E6401D3f740039a01F
-- Test 2 - 0xeb6a4B79dEd304AAEa7344fd58CE5f8EbC8424b9
-- Test 3 - 0xAC5491BB066c98fec13046928a78761c0B1E5603
-- Test 4 - 0x3Bb244dEc13253D39E22606850f4704B469A4B93
-- Test 5 - 0xF5003ceA9657a15123b1cc83C305f87555d190Cf
-- Test 6 - 0x9A82D050CcB98C31F7817b6263980C21167196c4
-- Test 7 - 0x2B5Fa24358F7bDa9517c66F9f44aA906070Fc5A2
-- Test 8 - 0xf9Bd658a9F3e23Ed77192A14A9dA6A5c37566218
-- Test 9 - 0xB1eC7fC5aF82fa7aAB80837fF89525f9f12fd716
-- Test 10 - 0xCe987A7e670655f30e582fBdE1573b5Be8FfB9A8
-- 11 is a new user
-- Test 11 - 0x450dcBeB535029B62f042222D95a009F59408D5d


simWalletSentryDataT1 : Eth.Sentry.Wallet.WalletSentry
simWalletSentryDataT1 =
    { account = Just (Internal.Types.Address "0x4A0a14bA869bEe85c490A5E6401D3f740039a01F"), networkId = Net.toNetworkId 4 }


simWalletSentryDataT11 : Eth.Sentry.Wallet.WalletSentry
simWalletSentryDataT11 =
    { account = Just (Internal.Types.Address "0x450dcBeB535029B62f042222D95a009F59408D5d"), networkId = Net.toNetworkId 4 }

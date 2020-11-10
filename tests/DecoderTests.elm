module DecoderTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, intRange)
import SR.Types
import Shrink
import Test exposing (..)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import Test exposing (..)
import SR.Decode
import Eth.Utils
import Html.Styled.Attributes exposing (value)


--nb. currently these tests are not ideal, and are not really testing the decoders. They demo how to use fuzz tests with encode/decoded json, but rmInvalidAddr is
-- only used with these tests and not in the main code base.

decoderTest : Test
decoderTest =
    --only <|
        describe "rmInvalidAddr - a decoded player addr must either be valid or default to ''"
            [
                fuzz3 string int string "challengerid defaults to (valid address)" <|
                \address rank challengerid ->
                        [ ( "address", Encode.string address )
                        , ( "rank", Encode.int rank )
                        ,  ( "challengerid", Encode.string "0x3bb244dec13253d39e22606850f4704b469a4b93")
                        ]
                            |> Encode.object
                            |> decodeValue SR.Decode.playerDecoder
                            |> Result.map .challengerid
                            |> rmInvalidAddr
                            |> Expect.equal ("0x3bb244dec13253d39e22606850f4704b469a4b93")
            
            ,
                fuzz3 string int string "challengerid defaults to ('')" <|
                \address rank challengerid ->
                        [ ( "address", Encode.string address )
                        , ( "rank", Encode.int rank )
                        , ( "challengerid", Encode.string challengerid)
                        ]
                            |> Encode.object
                            |> decodeValue SR.Decode.playerDecoder
                            |> Result.map .challengerid
                            |> rmInvalidAddr
                            |> Expect.equal ("")
            ]


rmInvalidAddr : Result e String -> String
rmInvalidAddr result =
    case result of 
            Err error ->
                ""
            Ok val ->
                if Eth.Utils.isAddress val then 
                    val 
                else
                    ""
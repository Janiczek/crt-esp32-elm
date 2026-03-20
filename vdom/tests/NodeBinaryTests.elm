module NodeBinaryTests exposing (suite)

import Bytes
import Bitmap
import Bytes.Decode
import Bytes.Decode.Extra
import Bytes.Encode
import Expect
import Node
import Test exposing (Test)


type alias EncodedBitmapHeader =
    { tag : Int
    , x : Int
    , y : Int
    , w : Int
    , h : Int
    , bitDepth : Int
    , decompressedLen : Int
    , compressedLen : Int
    }


encodedBitmapHeaderDecoder : Bytes.Decode.Decoder ( EncodedBitmapHeader, Bytes.Bytes )
encodedBitmapHeaderDecoder =
    Bytes.Decode.succeed EncodedBitmapHeader
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.signedInt32 Bytes.LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.signedInt32 Bytes.LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.signedInt32 Bytes.LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.signedInt32 Bytes.LE)
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 Bytes.LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 Bytes.LE)
        |> Bytes.Decode.andThen
            (\header ->
                Bytes.Decode.bytes header.compressedLen
                    |> Bytes.Decode.map (\payload -> ( header, payload ))
            )


suite : Test
suite =
    Test.describe "Node binary"
        [ Test.test "Bitmap encoder writes the expected header fields" <|
            \_ ->
                let
                    encoded =
                        Node.bitmap "bm" { x = 3, y = 4, w = 2, h = 2, bitDepth = Bitmap.BitDepth4, data = [ 0x12, 0x34 ] }
                            |> Node.bytesEncoder
                            |> Bytes.Encode.encode
                in
                case Bytes.Decode.decode encodedBitmapHeaderDecoder encoded of
                    Nothing ->
                        Expect.fail "Failed to decode bitmap header"

                    Just ( header, payload ) ->
                        Expect.all
                            [ .tag >> Expect.equal 6
                            , .x >> Expect.equal 3
                            , .y >> Expect.equal 4
                            , .w >> Expect.equal 2
                            , .h >> Expect.equal 2
                            , .bitDepth >> Expect.equal 4
                            , .decompressedLen >> Expect.equal 2
                            , .compressedLen >> Expect.equal (Bytes.width payload)
                            ]
                            header
        , Test.test "Bitmap constructor normalizes packed byte length" <|
            \_ ->
                let
                    node =
                        Node.bitmap "bm" { x = 0, y = 0, w = 3, h = 3, bitDepth = Bitmap.BitDepth1, data = [ 255 ] }
                in
                case node.type_ of
                    Node.Bitmap r ->
                        Expect.equal 2 (List.length r.data)

                    _ ->
                        Expect.fail "Expected bitmap node"
        ]

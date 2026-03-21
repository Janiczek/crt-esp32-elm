module NodeJsonTests exposing (suite)

import Bitmap
import Color
import ESP32 exposing (ESP32)
import Expect
import Font exposing (Font)
import Fuzzers
import Json.Decode as Decode
import Json.Encode as Encode
import Node exposing (Node)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Node JSON"
        [ Test.fuzz Fuzzers.esp32AndFittingNode "roundtrip: encode then decode returns the same node" <|
            \( esp32, node ) ->
                node
                    |> Node.jsonEncoder
                    |> Encode.encode 0
                    |> Decode.decodeString (Node.jsonDecoder esp32)
                    |> Expect.equal (Ok node)
        , Test.fuzz Fuzzers.esp32 "Text node decoded with fonts has non-zero bbox" <|
            \esp32 ->
                let
                    font : Font
                    font =
                        { name = "TestFont"
                        , asciiFirst = 32
                        , asciiLast = 126
                        , numGlyphs = 95
                        , glyphWidth = 6
                        , glyphHeight = 8
                        , extraLineHeight = 0
                        , bits = []
                        }

                    jsonTextNode : String
                    jsonTextNode =
                        """{"type":"Text","key":"t","x":0,"y":0,"text":"A","fontIndex":0,"color":255}"""

                    esp32WithAtLeastOneFont : ESP32
                    esp32WithAtLeastOneFont =
                        { esp32 | fonts = font :: esp32.fonts }
                in
                case Decode.decodeString (Node.jsonDecoder esp32WithAtLeastOneFont) jsonTextNode of
                    Err err ->
                        Expect.fail (Decode.errorToString err)

                    Ok decoded ->
                        decoded.bbox.w
                            |> Expect.greaterThan 0
        , Test.test "example node encodes to expected JSON string" <|
            \_ ->
                let
                    example : Node
                    example =
                        Node.group "root"
                            [ Node.rect "r1" { x = 0, y = 0, w = 100, h = 50, color = Color.black }
                            , Node.rectFill "rf1" { x = 10, y = 10, w = 80, h = 30, color = Color.white }
                            , Node.xLine "xl1" { x = 5, y = 25, len = 90, color = Color.gray }
                            , Node.yLine "yl1" { x = 50, y = 0, len = 50, color = Color.gray }
                            , Node.text [] "t1" { x = 20, y = 15, text = "hello", fontIndex = 0, color = Color.white }
                            , Node.bitmap "bm1" { x = 2, y = 3, w = 2, h = 2, bitDepth = Bitmap.BitDepth4, data = [ 1, 35 ] }
                            , Node.group "inner"
                                [ Node.rect "r2" { x = 1, y = 2, w = 3, h = 4, color = Color.black }
                                ]
                            ]

                    expected : String
                    expected =
                        """{"type":"Group","key":"root","children":[{"type":"Rect","key":"r1","x":0,"y":0,"w":100,"h":50,"color":0},{"type":"RectFill","key":"rf1","x":10,"y":10,"w":80,"h":30,"color":255},{"type":"XLine","key":"xl1","x":5,"y":25,"len":90,"color":127},{"type":"YLine","key":"yl1","x":50,"y":0,"len":50,"color":127},{"type":"Text","key":"t1","x":20,"y":15,"text":"hello","fontIndex":0,"color":255},{"type":"Bitmap","key":"bm1","x":2,"y":3,"w":2,"h":2,"bitDepth":4,"data":[1,35]},{"type":"Group","key":"inner","children":[{"type":"Rect","key":"r2","x":1,"y":2,"w":3,"h":4,"color":0}]}]}"""
                in
                example
                    |> Node.jsonEncoder
                    |> Encode.encode 0
                    |> Expect.equal expected
        , Test.fuzz Fuzzers.esp32 "Bitmap node rejects unsupported bitDepth" <|
            \esp32 ->
                """{"type":"Bitmap","key":"bm","x":0,"y":0,"w":1,"h":1,"bitDepth":3,"data":[255]}"""
                    |> Decode.decodeString (Node.jsonDecoder esp32)
                    |> Expect.err
        , Test.fuzz Fuzzers.esp32 "decoder fails when total node count exceeds maxTotalNodes" <|
            \esp32 ->
                let
                    jsonText : String
                    jsonText =
                        """{"type":"Group","key":"root","children":[{"type":"Rect","key":"a","x":0,"y":0,"w":1,"h":1,"color":255},{"type":"Rect","key":"b","x":1,"y":1,"w":1,"h":1,"color":255}]}"""

                    esp32WithLimit : ESP32
                    esp32WithLimit =
                        { esp32
                            | maxTotalNodes = 2
                            , nodeGroupMaxChildren = 10
                        }

                    result : Result String Node
                    result =
                        Decode.decodeString
                            (Node.jsonDecoder esp32WithLimit)
                            jsonText
                            |> Result.mapError Decode.errorToString
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected decoder to fail for total node limit."

                    Err err ->
                        if String.contains "Total node limit exceeded" err then
                            Expect.pass

                        else
                            Expect.fail ("Expected total node limit error, got: " ++ err)
        , Test.fuzz Fuzzers.esp32 "decoder fails when a group exceeds nodeGroupMaxChildren" <|
            \esp32 ->
                let
                    jsonText : String
                    jsonText =
                        """{"type":"Group","key":"root","children":[{"type":"Rect","key":"a","x":0,"y":0,"w":1,"h":1,"color":255},{"type":"Rect","key":"b","x":1,"y":1,"w":1,"h":1,"color":255}]}"""

                    esp32WithLimit : ESP32
                    esp32WithLimit =
                        { esp32
                            | maxTotalNodes = 10
                            , nodeGroupMaxChildren = 1
                        }

                    result : Result String Node
                    result =
                        Decode.decodeString
                            (Node.jsonDecoder esp32WithLimit)
                            jsonText
                            |> Result.mapError Decode.errorToString
                in
                case result of
                    Ok _ ->
                        Expect.fail "Expected decoder to fail for group child limit."

                    Err err ->
                        if String.contains "Group child limit exceeded" err then
                            Expect.pass

                        else
                            Expect.fail ("Expected group child limit error, got: " ++ err)
        ]

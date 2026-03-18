module CompileTests exposing (suite)

import BoundingBox
import BytesExtraExtra
import Color
import Dirty
import ESP32
import Expect
import FNV1a
import Font
import Main
import Node
import Test exposing (Test)


suite : Test
suite =
    Test.describe "All modules compile?"
        [ Test.test "BoundingBox" <|
            \() ->
                let
                    _ =
                        BoundingBox.BoundingBox
                in
                Expect.pass
        , Test.test "BytesExtraExtra" <|
            \() ->
                let
                    _ =
                        BytesExtraExtra.sizedStringDecoder
                in
                Expect.pass
        , Test.test "Color" <|
            \() ->
                let
                    _ =
                        Color.black
                in
                Expect.pass
        , Test.test "Dirty" <|
            \() ->
                let
                    _ =
                        Dirty.TileGrid
                in
                Expect.pass
        , Test.test "ESP32" <|
            \() ->
                let
                    _ =
                        ESP32.ESP32
                in
                Expect.pass
        , Test.test "FNV1a" <|
            \() ->
                let
                    _ =
                        FNV1a.initialSeed
                in
                Expect.pass
        , Test.test "Font" <|
            \() ->
                let
                    _ =
                        Font.Font
                in
                Expect.pass
        , Test.test "Main" <|
            \() ->
                let
                    _ =
                        Main.main
                in
                Expect.pass
        , Test.test "Node" <|
            \() ->
                let
                    _ =
                        Node.Node
                in
                Expect.pass
        ]

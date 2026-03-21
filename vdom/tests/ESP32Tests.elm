module ESP32Tests exposing (suite)

import BoundingBox
import ESP32 exposing (ESP32)
import Expect
import Fuzzers
import Test exposing (Test)


suite : Test
suite =
    Test.describe "ESP32.growToAccommodate"
        [ Test.fuzz Fuzzers.esp32 "empty bounding box is a no-op" <|
            \esp32 ->
                ESP32.growToAccommodate BoundingBox.empty esp32
                    |> Expect.equal esp32
        , Test.test "grows width so an 8x4 strip at the usable origin fits" <|
            \() ->
                let
                    esp32 : ESP32
                    esp32 =
                        { videoWidth = 20
                        , videoHeight = 20
                        , crtPaddingLeft = 10
                        , crtPaddingRight = 10
                        , crtPaddingTop = 0
                        , crtPaddingBottom = 0
                        , maxTotalNodes = 64
                        , nodeGroupMaxChildren = 8
                        , tileSize = 8
                        , fonts = []
                        }

                    vc =
                        ESP32.videoConstants esp32

                    layoutBbox =
                        { x = vc.xMin, y = vc.yMin, w = 8, h = 4 }

                    grown =
                        ESP32.growToAccommodate layoutBbox esp32

                    vcGrown =
                        ESP32.videoConstants grown
                in
                Expect.all
                    [ \() -> vcGrown.xMin |> Expect.atMost layoutBbox.x
                    , \() -> layoutBbox.x + layoutBbox.w - 1 |> Expect.atMost vcGrown.xMax
                    , \() -> vcGrown.yMin |> Expect.atMost layoutBbox.y
                    , \() -> layoutBbox.y + layoutBbox.h - 1 |> Expect.atMost vcGrown.yMax
                    ]
                    ()
        ]

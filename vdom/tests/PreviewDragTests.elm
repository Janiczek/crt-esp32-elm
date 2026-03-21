module PreviewDragTests exposing (suite)

import ESP32 exposing (ESP32, VideoConstants)
import Expect
import Fuzz
import Fuzzers
import PreviewDrag
import Test exposing (Test)


{-| Fixed ESP32 so `VideoConstants` bounds are stable in unit tests.
-}
sampleEsp32 : ESP32
sampleEsp32 =
    { videoWidth = 120
    , videoHeight = 80
    , crtPaddingLeft = 8
    , crtPaddingRight = 8
    , crtPaddingTop = 4
    , crtPaddingBottom = 4
    , maxTotalNodes = 256
    , nodeGroupMaxChildren = 32
    , tileSize = 8
    , fonts = []
    }


sampleVc : VideoConstants
sampleVc =
    ESP32.videoConstants sampleEsp32


suite : Test
suite =
    Test.describe "PreviewDrag.clampedNodeXYFromClientDrag"
        [ Test.describe "unit tests"
            (let
                testCases : List ( String, ( Int, ( ( Float,Float ), ( Float,Float ), ( Int, Int ) ) ), ( Int, Int ) )
                testCases =
                    [ ( "zero client delta keeps position"
                      , ( 1, ( ( 10, 20 ), ( 10, 20 ), ( 15, 25 ) ) )
                      , ( 15, 25 )
                      )
                    , ( "zoom 2: +2 client X becomes +1 video X"
                      , ( 2, ( ( 0, 0 ), ( 2, 0 ), ( 20, 30 ) ) )
                      , ( 21, 30 )
                      )
                    , ( "zoom 3: +3 client Y becomes +1 video Y"
                      , ( 3, ( ( 0, 0 ), ( 0, 3 ), ( 20, 30 ) ) )
                      , ( 20, 31 )
                      )
                    , ( "negative client delta truncates toward zero"
                      , ( 2, ( ( 0, 0 ), ( -3, 0 ), ( 20, 30 ) ) )
                      , ( 19, 30 )
                      )
                    , ( "clamp low on X"
                      , ( 1, ( ( 0, 0 ), ( -500, 0 ), ( sampleVc.xMin, 30 ) ) )
                      , ( sampleVc.xMin, 30 )
                      )
                    , ( "clamp high on X and Y"
                      , ( 1, ( ( 0, 0 ), ( 500, 500 ), ( sampleVc.xMax, sampleVc.yMax ) ) )
                      , ( sampleVc.xMax, sampleVc.yMax )
                      )
                    , ( "delta only in X leaves ny unchanged"
                      , ( 2, ( ( 5, 100 ), ( 15, 100 ), ( 10, 42 ) ) )
                      , ( 15, 42 )
                      )
                    , ( "delta only in Y leaves nx unchanged"
                      , ( 2, ( ( 100, 5 ), ( 100, 9 ), ( 33, 20 ) ) )
                      , ( 33, 22 )
                      )
                    ]

                toTest : ( String, ( Int, ( ( Float,Float ), ( Float,Float ), ( Int, Int ) ) ), ( Int, Int ) ) -> Test
                toTest ( desc, ( zoom, ( clientStart, clientCurrent, pos ) ), expected ) =
                    Test.test desc <|
                        \() ->
                            PreviewDrag.clampedNodeXYFromClientDrag sampleVc zoom clientStart clientCurrent pos
                                |> Expect.equal expected
             in
             List.map toTest testCases
            )
        , Test.fuzz3
            Fuzzers.zoom
            (Fuzz.map2 Tuple.pair
                (Fuzz.intRange sampleVc.xMin sampleVc.xMax)
                (Fuzz.intRange sampleVc.yMin sampleVc.yMax)
            )
            (Fuzz.map2 Tuple.pair (Fuzz.floatRange -400 400) (Fuzz.floatRange -400 400))
            "fuzz: result always within video bounds"
          <|
            \zoom ( nx, ny ) ( dcx, dcy ) ->
                let
                    ( ox, oy ) =
                        PreviewDrag.clampedNodeXYFromClientDrag sampleVc zoom ( 0, 0 ) ( dcx, dcy ) ( nx, ny )
                in
                Expect.all
                    [ \() -> sampleVc.xMin |> Expect.atMost ox
                    , \() -> sampleVc.xMax |> Expect.atLeast ox
                    , \() -> sampleVc.yMin |> Expect.atMost oy
                    , \() -> sampleVc.yMax |> Expect.atLeast oy
                    ]
                    ()
        ]

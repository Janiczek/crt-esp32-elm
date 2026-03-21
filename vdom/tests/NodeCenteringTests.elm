module NodeCenteringTests exposing (suite)

import Bitmap exposing (BitDepth(..))
import Color
import ESP32
import Expect
import Font
import Font.Fallback
import Fuzz
import Fuzzers
import Node
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Node centering"
        [ Test.describe "unit tests" <|
            let
                testCases =
                    [ ( "Rect"
                      , Node.rect "r" { x = 0, y = 0, w = 5, h = 7, color = Color.white }
                      , ( 8, 17 )
                      )
                    , ( "RectFill"
                      , Node.rectFill "rf" { x = 1, y = 2, w = 6, h = 8, color = Color.gray }
                      , ( 7, 16 )
                      )
                    , ( "XLine"
                      , Node.xLine "xl" { x = 3, y = 4, len = 9, color = Color.black }
                      , ( 6, 20 )
                      )
                    , ( "YLine"
                      , Node.yLine "yl" { x = 5, y = 6, len = 10, color = Color.white }
                      , ( 10, 15 )
                      )
                    , ( "Text"
                      , Node.text [ Font.Fallback.fallback ] "txt" { x = 7, y = 8, text = "abc", fontIndex = 0, color = Color.white }
                      , ( 3, 17 )
                      )
                    , ( "Group"
                      , Node.group "g"
                            [ Node.rect "child" { x = 50, y = 60, w = 5, h = 4, color = Color.white } ]
                      , ( 8, 18 )
                      )
                    , ( "Bitmap"
                      , Node.bitmap "bm"
                            { x = 0
                            , y = 0
                            , w = 6
                            , h = 4
                            , bitDepth = BitDepth8
                            , data = []
                            }
                      , ( 7, 18 )
                      )
                    ]

                toTest ( name, node, expected ) =
                    Test.test name <|
                        \() ->
                            Node.centerPosition baseVideoConstants node
                                |> Expect.equal expected
            in
            List.map toTest testCases
        , Test.fuzz
            Fuzzers.esp32
            "centering result is always clamped to xMin/yMin for all x,y node types"
          <|
            \esp32 ->
                let
                    vc =
                        ESP32.videoConstants esp32

                    nodes =
                        [ Node.rect "r" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                        , Node.rectFill "rf" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                        , Node.xLine "xl" { x = 0, y = 0, len = 1, color = Color.white }
                        , Node.yLine "yl" { x = 0, y = 0, len = 1, color = Color.white }
                        , Node.text [ Font.Fallback.fallback ] "txt" { x = 0, y = 0, text = "t", fontIndex = 0, color = Color.white }
                        , Node.bitmap "bm" { x = 0, y = 0, w = 1, h = 1, bitDepth = BitDepth8, data = [] }
                        , Node.group "g" [ Node.rect "child" { x = 3, y = 4, w = 1, h = 1, color = Color.white } ]
                        ]

                    isClamped node_ =
                        let
                            ( x, y ) =
                                Node.centerPosition vc node_
                        in
                        x >= vc.xMin && y >= vc.yMin
                in
                if List.all isClamped nodes then
                    Expect.pass

                else
                    Expect.fail "Expected centered position to be clamped to xMin/yMin for every x,y-capable type."
        , Test.fuzz
            (Fuzz.triple
                Fuzzers.esp32
                (Fuzz.intRange 1 500)
                (Fuzz.intRange 1 500)
            )
            "centering uses bbox half-size offsets using real videoConstants"
          <|
            \( esp32, wRaw, hRaw ) ->
                let
                    vc =
                        ESP32.videoConstants esp32

                    w =
                        clamp 1 vc.usableWidth wRaw

                    h =
                        clamp 1 vc.usableHeight hRaw

                    expectedBySize width height =
                        ( max vc.xMin (vc.xCenter - (width // 2))
                        , max vc.yMin (vc.yCenter - (height // 2))
                        )

                    checks =
                        [ ( "rect"
                          , Node.centerPosition vc (Node.rect "r" { x = 0, y = 0, w = w, h = h, color = Color.white })
                          , expectedBySize w h
                          )
                        , ( "rectFill"
                          , Node.centerPosition vc (Node.rectFill "rf" { x = 0, y = 0, w = w, h = h, color = Color.white })
                          , expectedBySize w h
                          )
                        , ( "xLine"
                          , Node.centerPosition vc (Node.xLine "xl" { x = 0, y = 0, len = w, color = Color.white })
                          , expectedBySize w 1
                          )
                        , ( "yLine"
                          , Node.centerPosition vc (Node.yLine "yl" { x = 0, y = 0, len = h, color = Color.white })
                          , expectedBySize 1 h
                          )
                        , ( "bitmap"
                          , Node.centerPosition vc (Node.bitmap "bm" { x = 0, y = 0, w = w, h = h, bitDepth = BitDepth8, data = [] })
                          , expectedBySize w h
                          )
                        , ( "text"
                          , Node.centerPosition vc
                                (Node.text [ Font.Fallback.fallback ] "txt" { x = 0, y = 0, text = String.repeat w "a", fontIndex = 0, color = Color.white })
                          , expectedBySize
                                (w * Font.Fallback.fallback.glyphWidth)
                                (Font.Fallback.fallback.glyphHeight + Font.Fallback.fallback.extraLineHeight)
                          )
                        , ( "group"
                          , Node.centerPosition vc
                                (Node.group "g"
                                    [ Node.rect "child" { x = 0, y = 0, w = w, h = h, color = Color.white } ]
                                )
                          , expectedBySize w h
                          )
                        ]

                    isCorrect ( label, actualPos, expectedPos ) () =
                        Expect.equal actualPos expectedPos
                            |> Expect.onFail ("Failed for node type: " ++ label)
                in
                Expect.all (List.map isCorrect checks) ()
        ]


baseVideoConstants : ESP32.VideoConstants
baseVideoConstants =
    { xMin = 0
    , xMax = 99
    , yMin = 0
    , yMax = 79
    , usableWidth = 100
    , usableHeight = 80
    , xCenter = 10
    , yCenter = 20
    , tileCols = 12
    , tileRows = 10
    }

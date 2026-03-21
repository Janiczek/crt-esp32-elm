module MainPreviewInteractionTests exposing (suite)

import Bitmap exposing (BitDepth(..))
import BoundingBox exposing (BoundingBox)
import Color
import ESP32 exposing (ESP32)
import Expect
import Font
import Fuzz exposing (Fuzzer)
import Fuzzers
import Html.Attributes
import Json.Encode as Encode
import Main
import Node exposing (Node, Type(..))
import ProgramTest
import Test exposing (Test)
import Test.Distribution as Distribution
import Test.Html.Query as Query
import Test.Html.Selector


suite : Test
suite =
    Test.describe "Preview interaction"
        [ Test.fuzz
            (scenarioClickFuzzer rectScenario)
            "left click selects topmost node by bbox even when top node draws no pixel there"
          <|
            \clickCase ->
                start clickCase.zoom clickCase.esp32 clickCase.scenario.root
                    |> leftClick clickCase.esp32 clickCase.zoom clickCase.clickX clickCase.clickY
                    |> expectSelected clickCase.scenario.topHash
        , Test.fuzz2
            Fuzzers.zoom
            Fuzzers.esp32AndFittingNode
            "left click selects the topmost layer on an overlapping black bitmap pixel"
          <|
            \zoom ( esp32, node ) ->
                let
                    s =
                        overlapPixelScenario esp32 node
                in
                start zoom esp32 s.root
                    |> leftClick esp32 zoom s.clickX s.clickY
                    |> expectSelected s.topHash
        , Test.fuzzWith
            { runs = 100
            , distribution =
                Test.expectDistribution
                    [ ( Distribution.atLeast 80
                      , "has a clear pixel"
                      , \( _, ( esp32, node ) ) ->
                            clearPixelOutsideRootBbox esp32 (overlapPixelScenario esp32 node).root
                                /= Nothing
                      )
                    ]
            }
            (Fuzz.pair
                Fuzzers.zoom
                Fuzzers.esp32AndFittingNode
            )
            "left click on empty preview space clears the current selection"
          <|
            \( zoom, ( esp32, node ) ) ->
                let
                    s =
                        overlapPixelScenario esp32 node
                in
                case clearPixelOutsideRootBbox esp32 s.root of
                    Nothing ->
                        Expect.pass

                    Just ( clearVideoX, clearVideoY ) ->
                        start zoom esp32 s.root
                            |> leftClick esp32 zoom s.clickX s.clickY
                            |> leftClick esp32 zoom clearVideoX clearVideoY
                            |> ProgramTest.expectView (expectSelectedCount 0)
        , Test.fuzz2
            Fuzzers.zoom
            Fuzzers.esp32AndFittingNode
            "right click opens a layer menu and can select a lower layer"
          -- TODO this test is very slow
          <|
            \zoom ( esp32, node ) ->
                let
                    s =
                        overlapPixelScenario esp32 node
                in
                start zoom esp32 s.root
                    |> rightClick esp32 zoom s.clickX s.clickY
                    |> ensureLayerMenuOpen
                    |> withinLayerMenu (ProgramTest.clickButton s.lowerLayerLabel)
                    |> expectSelected s.lowerHash
        , Test.fuzz
            (scenarioClickFuzzer overlapBboxScenario)
            "right click includes all nodes whose bboxes contain the coordinate"
          <|
            \clickCase ->
                start clickCase.zoom clickCase.esp32 clickCase.scenario.root
                    |> rightClick clickCase.esp32 clickCase.zoom clickCase.clickX clickCase.clickY
                    |> ensureLayerMenuOpen
                    |> expectWithinLayerMenuHas
                        [ Test.Html.Selector.text clickCase.scenario.lowerLabel
                        , Test.Html.Selector.text clickCase.scenario.upperLabel
                        ]
        , Test.fuzz
            (scenarioClickFuzzer withGroupMenuScenario)
            "group node is not left-click selected, but can still be right-clicked"
          <|
            \clickCase ->
                let
                    menuRoot =
                        clickCase.scenario.menuRoot
                in
                start clickCase.zoom clickCase.esp32 clickCase.scenario.root
                    |> leftClick clickCase.esp32 clickCase.zoom clickCase.clickX clickCase.clickY
                    |> ensureNotSelected (String.fromInt menuRoot.hash)
                    |> rightClick clickCase.esp32 clickCase.zoom clickCase.clickX clickCase.clickY
                    |> expectLayerMenuOpen
        , Test.fuzz
            (scenarioClickFuzzer withTextScenario)
            "text hit-test uses text bbox, not glyph pixels"
          <|
            \clickCase ->
                let
                    esp32 =
                        clickCase.esp32
                in
                start clickCase.zoom
                    { esp32 | fonts = textFont :: esp32.fonts }
                    clickCase.scenario.root
                    |> leftClick clickCase.esp32 clickCase.zoom clickCase.clickX clickCase.clickY
                    |> expectSelected clickCase.scenario.textHash
        , Test.fuzz
            (scenarioClickFuzzer rectScenario)
            "click on node A then mousedown on node B selects B for drag"
          <|
            \clickCase ->
                start clickCase.zoom clickCase.esp32 clickCase.scenario.root
                    |> leftClick clickCase.esp32 clickCase.zoom clickCase.clickX clickCase.clickY
                    |> previewMouseDown
                        clickCase.esp32
                        clickCase.zoom
                        clickCase.scenario.mouseDownOtherX
                        clickCase.scenario.mouseDownOtherY
                    |> expectSelected clickCase.scenario.mouseDownTargetHash
        , Test.fuzz
            clickSequenceCaseFuzzer
            "after any click sequence, preview has at most one selected node"
          <|
            \case_ ->
                applyClickSteps case_.esp32 case_.zoom case_.steps (start case_.zoom case_.esp32 case_.root)
                    |> ProgramTest.expectView
                        (\v ->
                            v
                                |> Query.find [ Test.Html.Selector.id "preview-surface" ]
                                |> Query.findAll
                                    [ Test.Html.Selector.attribute (Html.Attributes.attribute "data-selected" "true") ]
                                |> Query.count
                                    (\n ->
                                        if n <= 1 then
                                            Expect.pass

                                        else
                                            Expect.fail ("expected <= 1 selected node, got " ++ String.fromInt n)
                                    )
                        )
        , Test.fuzz Fuzzers.esp32 "add dropdown disables and shows warning at max total nodes limit" <|
            \esp32 ->
                start 1
                    { esp32
                        | maxTotalNodes = 2
                        , nodeGroupMaxChildren = 32
                    }
                    (Node.group "root" [])
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ Test.Html.Selector.id "add-child-select-" ])
                        ( "change"
                        , Encode.object
                            [ ( "target"
                              , Encode.object
                                    [ ( "value", Encode.string "rect" ) ]
                              )
                            ]
                        )
                    |> ProgramTest.ensureViewHas
                        [ Test.Html.Selector.text "Add disabled: max total nodes reached (2)." ]
                    |> ProgramTest.expectView
                        (\v ->
                            v
                                |> Query.find [ Test.Html.Selector.id "add-child-select-" ]
                                |> Query.has
                                    [ Test.Html.Selector.attribute (Html.Attributes.disabled True) ]
                        )
        ]


clearPixelOutsideRootBbox : ESP32 -> Node -> Maybe ( Int, Int )
clearPixelOutsideRootBbox esp32 root =
    let
        vc =
            ESP32.videoConstants esp32

        xRange =
            List.range vc.xMin vc.xMax

        yRange =
            List.range vc.yMin vc.yMax

        isOutsideRootBbox x y =
            let
                bbox =
                    root.bbox
            in
            x
                < bbox.x
                || x
                >= bbox.x
                + bbox.w
                || y
                < bbox.y
                || y
                >= bbox.y
                + bbox.h

        findInRow y remainingX =
            case remainingX of
                [] ->
                    Nothing

                x :: rest ->
                    if isOutsideRootBbox x y then
                        Just ( x, y )

                    else
                        findInRow y rest

        findInRows remainingY =
            case remainingY of
                [] ->
                    Nothing

                y :: rest ->
                    case findInRow y xRange of
                        Just pixel ->
                            Just pixel

                        Nothing ->
                            findInRows rest
    in
    findInRows yRange


pointInBbox : BoundingBox -> Fuzzer ( Int, Int )
pointInBbox bbox =
    Fuzz.map2 Tuple.pair
        (Fuzz.intRange bbox.x (bbox.x + bbox.w - 1))
        (Fuzz.intRange bbox.y (bbox.y + bbox.h - 1))


type alias ScenarioClick scenario =
    { zoom : Int
    , esp32 : ESP32
    , scenario : scenario
    , clickX : Int
    , clickY : Int
    }


scenarioClickFuzzer :
    (ESP32 -> Node -> { scenario | hitBbox : BoundingBox, previewEsp32 : ESP32 })
    -> Fuzzer (ScenarioClick { scenario | hitBbox : BoundingBox, previewEsp32 : ESP32 })
scenarioClickFuzzer toScenario =
    Fuzz.map2
        (\zoom ( esp32, node ) ->
            let
                scenario =
                    toScenario esp32 node
            in
            { zoom = zoom
            , esp32 = scenario.previewEsp32
            , scenario = scenario
            , clickX = 0
            , clickY = 0
            }
        )
        Fuzzers.zoom
        Fuzzers.esp32AndFittingNode
        |> Fuzz.andThen
            (\clickCase ->
                pointInBbox clickCase.scenario.hitBbox
                    |> Fuzz.map
                        (\( clickX, clickY ) ->
                            { clickCase
                                | clickX = clickX
                                , clickY = clickY
                            }
                        )
            )


type alias ClickStep =
    { eventName : String
    , button : Int
    , x : Int
    , y : Int
    }


type alias ClickSequenceCase =
    { zoom : Int
    , esp32 : ESP32
    , root : Node
    , steps : List ClickStep
    }


clickSequenceCaseFuzzer : Fuzzer ClickSequenceCase
clickSequenceCaseFuzzer =
    Fuzz.map2
        (\zoom ( esp32, root ) ->
            { zoom = zoom
            , esp32 = esp32
            , root = root
            , steps = []
            }
        )
        Fuzzers.zoom
        Fuzzers.esp32AndFittingNode
        |> Fuzz.andThen
            (\case_ ->
                clickStepsFuzzer case_.esp32
                    |> Fuzz.map (\steps -> { case_ | steps = steps })
            )


clickStepsFuzzer : ESP32 -> Fuzzer (List ClickStep)
clickStepsFuzzer esp32 =
    let
        vc =
            ESP32.videoConstants esp32

        coordFuzzer min_ max_ =
            if min_ <= max_ then
                Fuzz.intRange min_ max_

            else
                Fuzz.constant min_

        clickKindFuzzer =
            Fuzz.oneOf
                [ Fuzz.constant ( "click", 0 )
                , Fuzz.constant ( "contextmenu", 2 )
                ]
    in
    Fuzz.listOfLengthBetween
        0
        12
        (Fuzz.map3
            (\( eventName, button ) x y ->
                { eventName = eventName
                , button = button
                , x = x
                , y = y
                }
            )
            clickKindFuzzer
            (coordFuzzer vc.xMin vc.xMax)
            (coordFuzzer vc.yMin vc.yMax)
        )


applyClickSteps :
    ESP32
    -> Int
    -> List ClickStep
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
applyClickSteps esp32 zoom steps program =
    case steps of
        [] ->
            program

        step :: rest ->
            let
                nextProgram =
                    program
                        |> previewClick esp32 zoom step.eventName step.x step.y step.button
            in
            applyClickSteps esp32 zoom rest nextProgram


{-| If already a group, keep it; otherwise wrap so injections become top-level siblings.
-}
asGroup : Node -> Node
asGroup n =
    case n.type_ of
        Group _ ->
            n

        _ ->
            Node.group "fuzz-root" [ n ]


{-| Append nodes after existing group children (or after a wrapped singleton).
-}
appendInjected : Node -> List Node -> Node
appendInjected base injected =
    case base.type_ of
        Group r ->
            Node.group base.key (r.children ++ injected)

        _ ->
            Node.group "fuzz-root" (base :: injected)


type alias OverlapBboxScenario =
    { root : Node
    , hitBbox : BoundingBox
    , lowerLabel : String
    , upperLabel : String
    , previewEsp32 : ESP32
    }


type alias BboxSelectScenario =
    { root : Node
    , hitBbox : BoundingBox
    , topHash : String
    , mouseDownOtherX : Int
    , mouseDownOtherY : Int
    , mouseDownTargetHash : String
    , previewEsp32 : ESP32
    }


rectScenario : ESP32 -> Node -> BboxSelectScenario
rectScenario esp32 base =
    let
        vcBefore =
            ESP32.videoConstants esp32

        layoutBbox =
            { x = vcBefore.xMin
            , y = vcBefore.yMin
            , w = 8
            , h = 4
            }

        esp32_ =
            ESP32.growToAccommodate layoutBbox esp32

        vc =
            ESP32.videoConstants esp32_

        leftRect =
            Node.rect "bbox-left"
                { x = vc.xMin
                , y = vc.yMin
                , w = 4
                , h = 4
                , color = Color.white
                }

        rightRect =
            Node.rect "bbox-right"
                { x = vc.xMin + 4
                , y = vc.yMin
                , w = 4
                , h = 4
                , color = Color.white
                }

        root =
            appendInjected (asGroup base) [ rightRect, leftRect ]
    in
    { root = root
    , hitBbox = leftRect.bbox
    , topHash = String.fromInt leftRect.hash
    , mouseDownOtherX = rightRect.bbox.x + rightRect.bbox.w // 2
    , mouseDownOtherY = rightRect.bbox.y + rightRect.bbox.h // 2
    , mouseDownTargetHash = String.fromInt rightRect.hash
    , previewEsp32 = esp32_
    }


overlapBboxScenario : ESP32 -> Node -> OverlapBboxScenario
overlapBboxScenario esp32 base =
    let
        vc =
            ESP32.videoConstants esp32

        fill =
            Node.rectFill "test-fill"
                { x = vc.xMin
                , y = vc.yMin
                , w = 4
                , h = 4
                , color = Color.white
                }

        outline =
            Node.rect "test-outline"
                { x = vc.xMin
                , y = vc.yMin
                , w = 4
                , h = 4
                , color = Color.white
                }

        root =
            appendInjected (asGroup base) [ fill, outline ]
    in
    { root = root
    , hitBbox = outline.bbox
    , lowerLabel = nodeMenuLabel fill
    , upperLabel = nodeMenuLabel outline
    , previewEsp32 = esp32
    }


type alias GroupMenuScenario =
    { root : Node
    , hitBbox : BoundingBox
    , menuRoot : Node
    , previewEsp32 : ESP32
    }


withGroupMenuScenario : ESP32 -> Node -> GroupMenuScenario
withGroupMenuScenario esp32 _ =
    let
        vc =
            ESP32.videoConstants esp32

        child =
            Node.rectFill "group-child"
                { x = vc.xMin
                , y = vc.yMin
                , w = 4
                , h = 4
                , color = Color.white
                }

        menuRoot =
            Node.group "menu-root" [ child ]

        root =
            Node.group "fuzz-root" [ menuRoot ]
    in
    { root = root
    , hitBbox = child.bbox
    , menuRoot = menuRoot
    , previewEsp32 = esp32
    }


type alias OverlapPixelScenario =
    { root : Node
    , clickX : Int
    , clickY : Int
    , topHash : String
    , lowerHash : String
    , lowerLayerLabel : String
    }


overlapPixelScenario : ESP32 -> Node -> OverlapPixelScenario
overlapPixelScenario esp32 base =
    let
        vc =
            ESP32.videoConstants esp32

        line =
            Node.xLine "test-line"
                { x = vc.xMin
                , y = vc.yMin
                , len = 4
                , color = Color.white
                }

        bitmap =
            Node.bitmap "test-bitmap"
                { x = vc.xMin
                , y = vc.yMin
                , w = 1
                , h = 1
                , bitDepth = BitDepth8
                , data = [ Color.black ]
                }

        root =
            appendInjected (asGroup base) [ line, bitmap ]
    in
    { root = root
    , clickX = vc.xMin
    , clickY = vc.yMin
    , topHash = String.fromInt bitmap.hash
    , lowerHash = String.fromInt line.hash
    , lowerLayerLabel = nodeMenuLabel line
    }


type alias TextScenario =
    { root : Node
    , hitBbox : BoundingBox
    , textHash : String
    , previewEsp32 : ESP32
    }


withTextScenario : ESP32 -> Node -> TextScenario
withTextScenario esp32 base =
    let
        vc =
            ESP32.videoConstants esp32

        textNode =
            Node.text [ textFont ]
                "test-text"
                { x = vc.xMin
                , y = vc.yMin
                , text = "ABC\nDEF"
                , fontIndex = 0
                , color = Color.white
                }

        root =
            appendInjected (asGroup base) [ textNode ]
    in
    { root = root
    , hitBbox = textNode.bbox
    , textHash = String.fromInt textNode.hash
    , previewEsp32 = esp32
    }


nodeMenuLabel : Node -> String
nodeMenuLabel node =
    case node.type_ of
        Rect _ ->
            "Rect \"" ++ node.key ++ "\""

        RectFill _ ->
            "RectFill \"" ++ node.key ++ "\""

        XLine _ ->
            "XLine \"" ++ node.key ++ "\""

        YLine _ ->
            "YLine \"" ++ node.key ++ "\""

        Text _ ->
            "Text \"" ++ node.key ++ "\""

        Bitmap _ ->
            "Bitmap \"" ++ node.key ++ "\""

        Group _ ->
            "Group \"" ++ node.key ++ "\""


start : Int -> ESP32 -> Node -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
start previewZoom esp32 root =
    ProgramTest.createDocument
        { init =
            \() ->
                ( connectedModel
                    { esp32 = esp32
                    , root = root
                    , previewZoom = previewZoom
                    }
                , Cmd.none
                )
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


connectedModel : { esp32 : ESP32, root : Node, previewZoom : Int } -> Main.Model
connectedModel config =
    Main.Connected
        { esp32 = config.esp32
        , videoConstants = ESP32.videoConstants config.esp32
        , lastError = ""
        , rootNode = Main.RootSynced { root = config.root }
        , rootNodeJsonText = Encode.encode 0 (Node.jsonEncoder config.root)
        , rootNodeJsonError = Nothing
        , selectedPath = Nothing
        , previewMenu = Nothing
        , previewZoom = config.previewZoom
        , previewDrag = Nothing
        , previewIgnoreClick = False
        }


textFont : Font.Font
textFont =
    { name = "test-font"
    , asciiFirst = Char.toCode 'A'
    , asciiLast = Char.toCode 'Z'
    , numGlyphs = 26
    , glyphWidth = 3
    , glyphHeight = 3
    , extraLineHeight = 1
    , bits = List.repeat (26 * 3) 0
    }


previewMouseEvent : ESP32 -> Int -> String -> Int -> Int -> Int -> ( String, Encode.Value )
previewMouseEvent esp32 zoom eventName videoX videoY button =
    let
        vc =
            ESP32.videoConstants esp32

        offsetX =
            (videoX - vc.xMin) * zoom

        offsetY =
            (videoY - vc.yMin) * zoom
    in
    ( eventName
    , Encode.object
        [ ( "offsetX", Encode.int offsetX )
        , ( "offsetY", Encode.int offsetY )
        , ( "clientX", Encode.int offsetX )
        , ( "clientY", Encode.int offsetY )
        , ( "pageX", Encode.int offsetX )
        , ( "pageY", Encode.int offsetY )
        , ( "button", Encode.int button )
        ]
    )


previewSurfaceSelector : List Test.Html.Selector.Selector
previewSurfaceSelector =
    [ Test.Html.Selector.id "preview-surface" ]


layerMenuSelector : List Test.Html.Selector.Selector
layerMenuSelector =
    [ Test.Html.Selector.id "preview-layer-menu" ]


selectedNodeSelector : String -> List Test.Html.Selector.Selector
selectedNodeSelector hash =
    [ Test.Html.Selector.tag "g"
    , Test.Html.Selector.attribute (Html.Attributes.attribute "data-selected" "true")
    , Test.Html.Selector.attribute (Html.Attributes.attribute "data-hash" hash)
    ]


previewSurface : Query.Single msg -> Query.Single msg
previewSurface view =
    view
        |> Query.find previewSurfaceSelector


previewClick :
    ESP32
    -> Int
    -> String
    -> Int
    -> Int
    -> Int
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
previewClick esp32 zoom eventName x y button =
    ProgramTest.simulateDomEvent
        (Query.find previewSurfaceSelector)
        (previewMouseEvent esp32 zoom eventName x y button)


leftClick :
    ESP32
    -> Int
    -> Int
    -> Int
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
leftClick esp32 zoom x y =
    previewClick esp32 zoom "click" x y 0


previewMouseDown :
    ESP32
    -> Int
    -> Int
    -> Int
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
previewMouseDown esp32 zoom x y =
    previewClick esp32 zoom "mousedown" x y 0


rightClick :
    ESP32
    -> Int
    -> Int
    -> Int
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
rightClick esp32 zoom x y =
    previewClick esp32 zoom "contextmenu" x y 2


isSelected : String -> Query.Single msg -> Expect.Expectation
isSelected hash view =
    view
        |> previewSurface
        |> Query.findAll (selectedNodeSelector hash)
        |> Query.first
        |> Query.has []


expectSelected :
    String
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> Expect.Expectation
expectSelected hash =
    ProgramTest.expectView
        (isSelected hash)


ensureNotSelected :
    String
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
ensureNotSelected hash =
    ProgramTest.ensureView
        (\view ->
            view
                |> previewSurface
                |> Query.findAll (selectedNodeSelector hash)
                |> Query.count (Expect.equal 0)
        )


expectSelectedCount :
    Int
    -> Query.Single msg
    -> Expect.Expectation
expectSelectedCount expectedCount =
    \view ->
        view
            |> previewSurface
            |> Query.findAll
                [ Test.Html.Selector.attribute (Html.Attributes.attribute "data-selected" "true") ]
            |> Query.count (Expect.equal expectedCount)


ensureLayerMenuOpen :
    ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
ensureLayerMenuOpen =
    ProgramTest.ensureViewHas layerMenuSelector


withinLayerMenu :
    (ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
     -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    )
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
withinLayerMenu action program =
    ProgramTest.within (Query.find layerMenuSelector) action program


expectWithinLayerMenu :
    (Query.Single Main.Msg -> Expect.Expectation)
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> Expect.Expectation
expectWithinLayerMenu assertion =
    ProgramTest.expectView
        (\view ->
            view
                |> Query.find layerMenuSelector
                |> assertion
        )


expectWithinLayerMenuHas :
    List Test.Html.Selector.Selector
    -> ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> Expect.Expectation
expectWithinLayerMenuHas selectors =
    expectWithinLayerMenu (\menu -> Query.has selectors menu)


expectLayerMenuOpen :
    ProgramTest.ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
    -> Expect.Expectation
expectLayerMenuOpen =
    ProgramTest.expectViewHas layerMenuSelector

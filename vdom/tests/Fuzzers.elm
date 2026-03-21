module Fuzzers exposing
    ( bbox
    , bitDepth
    , color
    , esp32
    , esp32AndFittingNode
    , leafNode
    , node
    , textNode
    , zoom
    )

import Bitmap exposing (BitDepth(..))
import BoundingBox exposing (BoundingBox)
import Color exposing (Color)
import ESP32 exposing (ESP32)
import Font exposing (Font)
import Fuzz exposing (Fuzzer)
import Node exposing (Node)


color : Fuzzer Color
color =
    Fuzz.intRange 0 255


bbox : Fuzzer BoundingBox
bbox =
    Fuzz.map4 (\x y w h -> { x = x, y = y, w = w, h = h })
        Fuzz.int
        Fuzz.int
        (Fuzz.intRange 1 128)
        (Fuzz.intRange 1 128)


zoom : Fuzzer Int
zoom =
    Fuzz.intRange 1 3


node : Fuzzer Node
node =
    node_ 4


leafNode : Fuzzer Node
leafNode =
    node_ 0


textNode : Fuzzer Node
textNode =
    Fuzz.map5 (\x y fontIndex c text -> Node.text (List.repeat 4 fittingFont) text { x = x, y = y, text = text, fontIndex = fontIndex, color = c })
        (Fuzz.intRange 0 512)
        (Fuzz.intRange 0 512)
        (Fuzz.intRange 0 3)
        color
        asciiString


asciiString : Fuzzer String
asciiString =
    Fuzz.asciiStringOfLengthBetween 0 24
        |> Fuzz.listOfLengthBetween 0 12
        |> Fuzz.map (String.join "\n")


bitDepth : Fuzzer BitDepth
bitDepth =
    Fuzz.oneOf
        [ Fuzz.constant BitDepth1
        , Fuzz.constant BitDepth2
        , Fuzz.constant BitDepth4
        , Fuzz.constant BitDepth8
        ]


font : Fuzzer Font
font =
    Fuzz.constant
        (\name ( asciiFirst, asciiLast, numGlyphs ) glyphWidth glyphHeight extraLineHeight ->
            { name = name
            , asciiFirst = asciiFirst
            , asciiLast = asciiLast
            , numGlyphs = numGlyphs
            , glyphWidth = glyphWidth
            , glyphHeight = glyphHeight
            , extraLineHeight = extraLineHeight
            , bits = []
            }
        )
        |> Fuzz.andMap (Fuzz.stringOfLengthBetween 0 6)
        |> Fuzz.andMap asciiRange
        |> Fuzz.andMap (Fuzz.intRange 1 12)
        |> Fuzz.andMap (Fuzz.intRange 1 12)
        |> Fuzz.andMap (Fuzz.intRange 0 4)
        |> Fuzz.andThen
            (\f ->
                Fuzz.listOfLength (f.numGlyphs * f.glyphHeight)
                    (Fuzz.intRange 0 255)
                    -- each byte is a row of pixels in the character
                    |> Fuzz.map (\bits -> { f | bits = bits })
            )


asciiRange : Fuzzer ( Int, Int, Int )
asciiRange =
    Fuzz.intRange 32 126
        |> Fuzz.andThen
            (\asciiFirst ->
                Fuzz.intRange asciiFirst 126
                    |> Fuzz.map
                        (\asciiLast ->
                            ( asciiFirst
                            , asciiLast
                            , asciiLast - asciiFirst + 1
                            )
                        )
            )


esp32 : Fuzzer ESP32
esp32 =
    Fuzz.constant
        (\w h maxTotalNodes nodeGroupMaxChildren tileSize fonts ->
            { videoWidth = w
            , videoHeight = h
            , crtPaddingLeft = -1
            , crtPaddingRight = -1
            , crtPaddingTop = -1
            , crtPaddingBottom = -1
            , maxTotalNodes = maxTotalNodes
            , nodeGroupMaxChildren = nodeGroupMaxChildren
            , tileSize = tileSize
            , fonts = fonts
            }
        )
        |> Fuzz.andMap (Fuzz.intRange 16 400)
        |> Fuzz.andMap (Fuzz.intRange 16 240)
        |> Fuzz.andMap (Fuzz.intRange 0 7 |> Fuzz.map (\e -> 2 ^ e))
        |> Fuzz.andMap (Fuzz.intRange 0 5 |> Fuzz.map (\e -> 2 ^ e))
        |> Fuzz.andMap (Fuzz.intRange 0 5 |> Fuzz.map (\e -> 2 ^ e))
        |> Fuzz.andMap (Fuzz.listOfLengthBetween 0 3 font)
        |> Fuzz.andThen
            (\e ->
                Fuzz.map2 Tuple.pair
                    (paddingPair e.videoWidth)
                    (paddingPair e.videoHeight)
                    |> Fuzz.map
                        (\( ( crtPaddingLeft, crtPaddingRight ), ( crtPaddingTop, crtPaddingBottom ) ) ->
                            { e
                                | crtPaddingLeft = crtPaddingLeft
                                , crtPaddingRight = crtPaddingRight
                                , crtPaddingTop = crtPaddingTop
                                , crtPaddingBottom = crtPaddingBottom
                            }
                        )
            )


esp32AndFittingNode : Fuzzer ( ESP32, Node )
esp32AndFittingNode =
    Fuzz.map2
        (\esp32_ nodeValue ->
            ( setLimitsToMakeNodeFit nodeValue esp32_
            , nodeValue
            )
        )
        esp32
        node


setLimitsToMakeNodeFit : Node -> ESP32 -> ESP32
setLimitsToMakeNodeFit nodeValue esp32_ =
    { esp32_
        | maxTotalNodes = Node.size nodeValue
        , nodeGroupMaxChildren = Node.maxGroupChildren nodeValue
        , fonts = List.repeat (Node.maxFontIndex nodeValue + 1) fittingFont
    }


fittingFont : Font
fittingFont =
    { name = "TestFont"
    , asciiFirst = 32
    , asciiLast = 126
    , numGlyphs = 95
    , glyphWidth = 6
    , glyphHeight = 8
    , extraLineHeight = 0
    , bits = []
    }


paddingPair : Int -> Fuzzer ( Int, Int )
paddingPair axisSize =
    Fuzz.intRange 0 (axisSize - 1)
        |> Fuzz.andThen
            (\paddingStart ->
                Fuzz.intRange 0 (axisSize - paddingStart - 1)
                    |> Fuzz.map (\paddingEnd -> ( paddingStart, paddingEnd ))
            )


node_ : Int -> Fuzzer Node
node_ maxDepth =
    [ Just <|
        Fuzz.map5 (\x y w h c -> Node.rect "r" { x = x, y = y, w = w, h = h, color = c })
            Fuzz.int
            Fuzz.int
            Fuzz.int
            Fuzz.int
            color
    , Just <|
        Fuzz.map5 (\x y w h c -> Node.rectFill "rf" { x = x, y = y, w = w, h = h, color = c })
            Fuzz.int
            Fuzz.int
            Fuzz.int
            Fuzz.int
            color
    , Just <|
        Fuzz.map4 (\x y len c -> Node.xLine "xl" { x = x, y = y, len = len, color = c })
            Fuzz.int
            Fuzz.int
            Fuzz.int
            color
    , Just <|
        Fuzz.map4 (\x y len c -> Node.yLine "yl" { x = x, y = y, len = len, color = c })
            Fuzz.int
            Fuzz.int
            Fuzz.int
            color
    , Just textNode
    , Just <|
        Fuzz.map5
            (\x y w h bd ->
                let
                    byteLen : Int
                    byteLen =
                        ((w * h * Bitmap.bitDepthToInt bd) + 7) // 8
                in
                Node.bitmap "bm"
                    { x = x
                    , y = y
                    , w = w
                    , h = h
                    , bitDepth = bd
                    , data = List.repeat byteLen 255
                    }
            )
            (Fuzz.intRange 0 64)
            (Fuzz.intRange 0 64)
            (Fuzz.intRange 0 32)
            (Fuzz.intRange 0 32)
            bitDepth
    , if maxDepth == 0 then
        Nothing

      else
        Just <|
            Fuzz.map (\children -> Node.group "g" children)
                (Fuzz.listOfLengthBetween 0 5 (node_ (maxDepth - 1)))
    ]
        |> List.filterMap identity
        |> Fuzz.oneOf

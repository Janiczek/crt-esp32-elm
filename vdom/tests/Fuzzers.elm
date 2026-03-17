module Fuzzers exposing (color, node, bbox)

import Color exposing (Color)
import BoundingBox exposing (BoundingBox)
import Fuzz exposing (Fuzzer)
import Node exposing (Node)


color : Fuzzer Color
color =
    Fuzz.oneOf
        [ Fuzz.constant Color.black
        , Fuzz.constant Color.gray
        , Fuzz.constant Color.white
        ]

bbox : Fuzzer BoundingBox
bbox =
    Fuzz.map4 (\x y w h -> { x = x, y = y, w = w, h = h })
        Fuzz.int
        Fuzz.int
        Fuzz.int
        Fuzz.int

node : Fuzzer Node
node =
    node_ 5


asciiString : Fuzzer String
asciiString =
    Fuzz.asciiChar
        |> Fuzz.listOfLengthBetween 0 60
        |> Fuzz.listOfLengthBetween 0 24
        |> Fuzz.map (List.map String.fromList >> String.join "\n")


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
    , Just <|
        Fuzz.map5 (\x y fontIndex c text -> Node.text [] text { x = x, y = y, text = text, fontIndex = fontIndex, color = c })
            Fuzz.int
            Fuzz.int
            Fuzz.int
            color
            asciiString
    , if maxDepth == 0 then
        Nothing

      else
        Just <|
            Fuzz.map (\children -> Node.group "g" children)
                (Fuzz.listOfLengthBetween 0 5 (node_ (maxDepth - 1)))
    ]
        |> List.filterMap identity
        |> Fuzz.oneOf
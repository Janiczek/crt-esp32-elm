module DirtyTests exposing (suite)

import BoundingBox exposing (BoundingBox)
import Color
import Dirty
import Expect
import Font exposing (Font)
import Fuzz exposing (Fuzzer)
import Fuzzers
import Node
import Set
import String.Extra
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Dirty"
        [ Test.describe "diffChildren"
            (let
                basicGrid =
                    { tileSize = 8
                    , tileCols = 4
                    , tileRows = 4
                    }

                color =
                    Color.black

                rect key bbox =
                    Node.rect key
                        { x = bbox.x
                        , y = bbox.y
                        , w = bbox.w
                        , h = bbox.h
                        , color = color
                        }

                bboxA : BoundingBox
                bboxA =
                    { x = 0, y = 0, w = 8, h = 8 }

                bboxB : BoundingBox
                bboxB =
                    { x = 8, y = 0, w = 8, h = 8 }

                bboxB2 : BoundingBox
                bboxB2 =
                    { x = 8, y = 0, w = 16, h = 8 }

                bboxC : BoundingBox
                bboxC =
                    { x = 0, y = 8, w = 8, h = 8 }

                nodeA =
                    rect "a" bboxA

                nodeB =
                    rect "b" bboxB

                nodeB2 =
                    rect "b" bboxB2

                nodeC =
                    rect "c" bboxC
             in
             [ Test.test "update only (same key): equals diff of matched nodes" <|
                \_ ->
                    Dirty.diffChildren_TEST basicGrid [] [ nodeB ] [ nodeB2 ]
                        |> Expect.equal (Dirty.diff basicGrid [] nodeB nodeB2)
             , Test.test "insert only: marks inserted bbox" <|
                \_ ->
                    Dirty.diffChildren_TEST basicGrid [] [] [ nodeC ]
                        |> Expect.equal (Dirty.markBbox_TEST basicGrid nodeC.bbox)
             , Test.test "delete only: marks deleted bbox" <|
                \_ ->
                    Dirty.diffChildren_TEST basicGrid [] [ nodeA ] []
                        |> Expect.equal (Dirty.markBbox_TEST basicGrid nodeA.bbox)
             , Test.test "mixed update+insert+delete: union of update diff + inserted bbox + deleted bbox (order independent)" <|
                \_ ->
                    let
                        expected =
                            Dirty.diff basicGrid [] nodeB nodeB2
                                |> Set.union (Dirty.markBbox_TEST basicGrid nodeA.bbox)
                                |> Set.union (Dirty.markBbox_TEST basicGrid nodeC.bbox)
                    in
                    Dirty.diffChildren_TEST basicGrid [] [ nodeA, nodeB ] [ nodeB2, nodeC ]
                        |> Expect.equal expected
             , Test.test "mixed update+insert+delete: still works if child order differs" <|
                \_ ->
                    let
                        expected =
                            Dirty.diff basicGrid [] nodeB nodeB2
                                |> Set.union (Dirty.markBbox_TEST basicGrid nodeA.bbox)
                                |> Set.union (Dirty.markBbox_TEST basicGrid nodeC.bbox)
                    in
                    Dirty.diffChildren_TEST basicGrid [] [ nodeB, nodeA ] [ nodeC, nodeB2 ]
                        |> Expect.equal expected
             ]
            )
        , Test.describe "changedTextCells"
            [ Test.describe "unit tests" <|
                let
                    testCases :
                        List
                            ( String
                            , ( String, String )
                            , List ( Int, Int )
                            )
                    testCases =
                        [ ( "change at beginning"
                          , ( "...\n...\n..."
                            , "...\nX..\n..."
                            )
                          , [ ( 1, 0 ) ]
                          )
                        , ( "change in middle"
                          , ( "...\n...\n..."
                            , "...\n.X.\n..."
                            )
                          , [ ( 1, 1 ) ]
                          )
                        , ( "change at end"
                          , ( "...\n...\n..."
                            , "...\n..X\n..."
                            )
                          , [ ( 1, 2 ) ]
                          )
                        , ( "addition at beginning"
                          , ( "...\n...\n..."
                            , "X...\n...\n..."
                            )
                          , [ ( 0, 0 ), ( 0, 3 ) ]
                          )
                        , ( "addition at beginning (unique chars)"
                          , ( "abc\n...\n..."
                            , "Xabc\n...\n..."
                            )
                          , [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
                          )
                        , ( "addition in middle"
                          , ( "...\n...\n..."
                            , "...\n.X..\n..."
                            )
                          , [ ( 1, 1 ), ( 1, 3 ) ]
                          )
                        , ( "addition in middle (unique chars)"
                          , ( "...\nabc\n..."
                            , "...\naXbc\n..."
                            )
                          , [ ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ]
                          )
                        , ( "addition at end"
                          , ( "...\n...\n..."
                            , "...X\n...\n..."
                            )
                          , [ ( 0, 3 ) ]
                          )
                        , ( "added line"
                          , ( "...\n..."
                            , "...\n...\n..."
                            )
                          , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                          )
                        , ( "added line with newline"
                          , ( "...\n..."
                            , "...\n...\n..."
                            )
                          , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                          )
                        , ( "leading newline: AAA to \\nAAA marks both lines"
                          , ( "AAA"
                            , "\nAAA"
                            )
                          , [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
                          )
                        , ( "leading newline removed: \\nAAA to AAA marks both lines"
                          , ( "\nAAA"
                            , "AAA"
                            )
                          , [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
                          )
                        ]

                    toTest :
                        ( String
                        , ( String, String )
                        , List ( Int, Int )
                        )
                        -> Test
                    toTest ( name, ( oldStr, newStr ), expected ) =
                        Test.test name <|
                            \_ ->
                                Dirty.changedTextCells_TEST oldStr newStr
                                    |> Expect.equalLists expected
                in
                List.map toTest testCases
            , Test.fuzz (Fuzz.pair Fuzz.string Fuzz.string) "if number of newlines differ, output is non-empty" <|
                \( a, b ) ->
                    let
                        numNewlines : String -> Int
                        numNewlines str =
                            String.Extra.countOccurrences "\n" (String.trim str)
                    in
                    if numNewlines a /= numNewlines b then
                        Dirty.changedTextCells_TEST a b
                            |> Expect.notEqual []

                    else
                        Expect.pass
            , let
                stringOfLength : Int -> Fuzzer String
                stringOfLength len =
                    Fuzz.listOfLength len Fuzz.asciiChar
                        |> Fuzz.map String.fromList
              in
              Test.fuzz
                (Fuzz.intRange 1 40
                    |> Fuzz.andThen
                        (\len ->
                            Fuzz.pair
                                (stringOfLength len)
                                (stringOfLength (len + 1))
                        )
                )
                "if number of characters differ, output is non-empty"
              <|
                \( a, b ) ->
                    Dirty.changedTextCells_TEST a b
                        |> Expect.notEqual []
            ]
        , Test.describe "allTextCells"
            [ Test.describe "unit tests" <|
                let
                    testCases :
                        List
                            ( String
                            , ( Int, Int, String )
                            , List ( Int, Int, Char )
                            )
                    testCases =
                        [ ( "empty string"
                          , ( 0, 0, "" )
                          , []
                          )
                        , ( "simple"
                          , ( 0, 0, "abc\ndef" )
                          , [ ( 1, 2, 'f' ), ( 1, 1, 'e' ), ( 1, 0, 'd' ), ( 0, 2, 'c' ), ( 0, 1, 'b' ), ( 0, 0, 'a' ) ]
                          )
                        , ( "non-zero coords"
                          , ( 3, 2, "abc\ndef" )
                          , [ ( 4, 2, 'f' ), ( 4, 1, 'e' ), ( 4, 0, 'd' ), ( 3, 4, 'c' ), ( 3, 3, 'b' ), ( 3, 2, 'a' ) ]
                          )
                        ]

                    toTest :
                        ( String
                        , ( Int, Int, String )
                        , List ( Int, Int, Char )
                        )
                        -> Test
                    toTest ( name, ( row, col, chars ), expected ) =
                        Test.test name <|
                            \_ ->
                                Dirty.allTextCells_TEST row col (String.toList chars)
                                    |> Expect.equalLists expected
                in
                List.map toTest testCases
            , Test.fuzz (Fuzz.triple Fuzz.int Fuzz.int Fuzz.string) "col only affects until first newline" <|
                \( row, col, chars ) ->
                    let
                        charsWithPrefix : String
                        charsWithPrefix =
                            "abc\n" ++ chars
                    in
                    Dirty.allTextCells_TEST row col (String.toList charsWithPrefix)
                        |> List.sort
                        |> List.drop 3
                        |> List.map (\( r, c, char ) -> ( r, c ))
                        |> Expect.equalLists
                            (Dirty.allTextCells_TEST row col (String.toList chars)
                                |> List.sort
                                |> List.map
                                    (\( r, c, _ ) ->
                                        if r == row then
                                            ( r + 1, c - col )

                                        else
                                            ( r + 1, c )
                                    )
                            )
            ]
        , Test.describe "textCellsUntilNewline"
            [ Test.describe "unit tests" <|
                let
                    testCases :
                        List
                            ( String
                            , ( Int, Int, String )
                            , { cells : List ( Int, Int )
                              , rest : List Char
                              , row : Int
                              , col : Int
                              }
                            )
                    testCases =
                        [ ( "empty"
                          , ( 0, 0, "" )
                          , { cells = []
                            , rest = []
                            , row = 0
                            , col = 0
                            }
                          )
                        , ( "simple case"
                          , ( 0, 0, "abc\ndef" )
                          , { cells = [ ( 0, 2 ), ( 0, 1 ), ( 0, 0 ) ]
                            , rest = [ 'd', 'e', 'f' ]
                            , row = 1
                            , col = 0
                            }
                          )
                        , ( "non-zero coords"
                          , ( 3, 2, "abc\ndef" )
                          , { cells = [ ( 3, 4 ), ( 3, 3 ), ( 3, 2 ) ]
                            , rest = [ 'd', 'e', 'f' ]
                            , row = 4
                            , col = 0
                            }
                          )
                        , ( "starting with newline"
                          , ( 3, 2, "\ndef" )
                          , { cells = []
                            , rest = [ 'd', 'e', 'f' ]
                            , row = 4
                            , col = 0
                            }
                          )
                        , ( "no newline"
                          , ( 0, 0, "abc" )
                          , { cells = [ ( 0, 2 ), ( 0, 1 ), ( 0, 0 ) ]
                            , rest = []
                            , row = 0
                            , col = 3
                            }
                          )
                        ]

                    toTest :
                        ( String
                        , ( Int, Int, String )
                        , { cells : List ( Int, Int )
                          , rest : List Char
                          , row : Int
                          , col : Int
                          }
                        )
                        -> Test
                    toTest ( name, ( row, col, chars ), expected ) =
                        Test.test name <|
                            \_ ->
                                Dirty.textCellsUntilNewline_TEST row col (String.toList chars)
                                    |> Expect.equal expected
                in
                List.map toTest testCases
            ]
        , Test.describe "textCellToGlyphBbox"
            [ Test.describe "unit tests" <|
                let
                    font8x8 : Font
                    font8x8 =
                        { name = "F8"
                        , asciiFirst = 32
                        , asciiLast = 126
                        , numGlyphs = 95
                        , glyphWidth = 8
                        , glyphHeight = 8
                        , extraLineHeight = 0
                        , bits = []
                        }

                    font5x7extra1 : Font
                    font5x7extra1 =
                        { name = "F5"
                        , asciiFirst = 32
                        , asciiLast = 126
                        , numGlyphs = 95
                        , glyphWidth = 5
                        , glyphHeight = 7
                        , extraLineHeight = 1
                        , bits = []
                        }

                    testCases :
                        List
                            { name : String
                            , textOrigin : ( Int, Int )
                            , font : Font
                            , cell : ( Int, Int )
                            , expected : BoundingBox
                            }
                    testCases =
                        [ { name = "origin cell (0,0), font 8×8"
                          , textOrigin = ( 0, 0 )
                          , font = font8x8
                          , cell = ( 0, 0 )
                          , expected = { x = 0, y = 0, w = 8, h = 8 }
                          }
                        , { name = "text at (1,0), cell (0,0), font 8×8: glyph spans two tiles"
                          , textOrigin = ( 1, 0 )
                          , font = font8x8
                          , cell = ( 0, 0 )
                          , expected = { x = 1, y = 0, w = 8, h = 8 }
                          }
                        , { name = "text at (0,0), cell (1,0), font 8×8: second row first char"
                          , textOrigin = ( 0, 0 )
                          , font = font8x8
                          , cell = ( 1, 0 )
                          , expected = { x = 0, y = 8, w = 8, h = 8 }
                          }
                        , { name = "text at (0,0), cell (0,0), font 5×7 extraLineHeight 1"
                          , textOrigin = ( 0, 0 )
                          , font = font5x7extra1
                          , cell = ( 0, 0 )
                          , expected = { x = 0, y = 1, w = 5, h = 7 }
                          }
                        , { name = "text at (10,4), cell (2,3), font 8×8"
                          , textOrigin = ( 10, 4 )
                          , font = font8x8
                          , cell = ( 2, 3 )
                          , expected = { x = 34, y = 20, w = 8, h = 8 }
                          }
                        ]

                    toTest c =
                        Test.test c.name <|
                            \_ ->
                                Dirty.textCellToGlyphBbox_TEST
                                    { x = Tuple.first c.textOrigin, y = Tuple.second c.textOrigin }
                                    c.font
                                    c.cell
                                    |> Expect.equal c.expected
                in
                List.map toTest testCases
            , Test.fuzz
                (Fuzz.map5
                    (\x y gw gh extra ->
                        ( ( x, y )
                        , { name = ""
                          , asciiFirst = 32
                          , asciiLast = 126
                          , numGlyphs = 95
                          , glyphWidth = gw
                          , glyphHeight = gh
                          , extraLineHeight = extra
                          , bits = []
                          }
                        )
                    )
                    Fuzz.int
                    Fuzz.int
                    (Fuzz.intRange 1 16)
                    (Fuzz.intRange 1 16)
                    (Fuzz.intRange 0 4)
                )
                "glyph bbox always has w = font.glyphWidth and h = font.glyphHeight"
              <|
                \( textOrigin, font ) ->
                    let
                        row =
                            0

                        col =
                            0

                        bbox =
                            Dirty.textCellToGlyphBbox_TEST
                                { x = Tuple.first textOrigin, y = Tuple.second textOrigin }
                                font
                                ( row, col )
                    in
                    ( bbox.w, bbox.h )
                        |> Expect.equal ( font.glyphWidth, font.glyphHeight )
            , Test.fuzz
                (Fuzz.intRange 4 16
                    |> Fuzz.andThen
                        (\tileSize ->
                            Fuzz.intRange 1 (tileSize - 1)
                                |> Fuzz.map (\textX -> ( tileSize, textX ))
                        )
                )
                "single char spanning tile boundary: glyph bbox marks at least 2 tiles"
              <|
                \( tileSize, textX ) ->
                    let
                        grid =
                            { tileSize = tileSize
                            , tileCols = 50
                            , tileRows = 50
                            }

                        font =
                            { name = ""
                            , asciiFirst = 32
                            , asciiLast = 126
                            , numGlyphs = 95
                            , glyphWidth = tileSize
                            , glyphHeight = tileSize
                            , extraLineHeight = 0
                            , bits = []
                            }

                        bbox =
                            Dirty.textCellToGlyphBbox_TEST { x = textX, y = 0 } font ( 0, 0 )
                    in
                    Dirty.markBbox_TEST grid bbox
                        |> Set.size
                        |> Expect.atLeast 2
            ]
        , Test.describe "markBbox"
            [ Test.describe "overlap cases" <|
                let
                    grid :
                        { tileSize : Int
                        , tileCols : Int
                        , tileRows : Int
                        }
                    grid =
                        { tileSize = 8
                        , tileCols = 4
                        , tileRows = 4
                        }

                    cases :
                        List
                            { name : String
                            , bbox : BoundingBox
                            , expected : List ( Int, Int )
                            }
                    cases =
                        [ { name = "center: bbox fully inside one tile"
                          , bbox = { x = 2, y = 2, w = 4, h = 4 }
                          , expected = [ ( 0, 0 ) ]
                          }
                        , { name = "left side: bbox crosses vertical boundary"
                          , bbox = { x = 4, y = 2, w = 8, h = 4 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ) ]
                          }
                        , { name = "right side: bbox crosses vertical boundary"
                          , bbox = { x = 6, y = 2, w = 5, h = 4 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ) ]
                          }
                        , { name = "top side: bbox crosses horizontal boundary"
                          , bbox = { x = 2, y = 4, w = 4, h = 8 }
                          , expected = [ ( 0, 0 ), ( 0, 1 ) ]
                          }
                        , { name = "bottom side: bbox crosses horizontal boundary"
                          , bbox = { x = 2, y = 6, w = 4, h = 5 }
                          , expected = [ ( 0, 0 ), ( 0, 1 ) ]
                          }
                        , { name = "top-left corner: 2×2 tiles"
                          , bbox = { x = 4, y = 4, w = 8, h = 8 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]
                          }
                        , { name = "top-right corner: 2×2 tiles"
                          , bbox = { x = 6, y = 4, w = 8, h = 8 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]
                          }
                        , { name = "bottom-left corner: 2×2 tiles"
                          , bbox = { x = 4, y = 6, w = 8, h = 8 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]
                          }
                        , { name = "bottom-right corner: 2×2 tiles"
                          , bbox = { x = 6, y = 6, w = 8, h = 8 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]
                          }
                        , { name = "center with large bbox: touches all neighbours"
                          , bbox = { x = 2, y = 2, w = 16, h = 16 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 0, 2 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                          }
                        ]

                    toTest :
                        { name : String
                        , bbox : BoundingBox
                        , expected : List ( Int, Int )
                        }
                        -> Test
                    toTest c =
                        Test.test c.name <|
                            \() ->
                                Dirty.markBbox_TEST grid c.bbox
                                    |> Expect.equal (Set.fromList c.expected)
                in
                List.map toTest cases
            ]
        , Test.describe "markRectBorder"
            [ Test.describe "border-only (excludes interior tiles)" <|
                let
                    grid =
                        { tileSize = 8
                        , tileCols = 4
                        , tileRows = 4
                        }

                    cases =
                        [ { name = "single-tile: bbox fully inside one tile"
                          , bbox = { x = 2, y = 2, w = 4, h = 4 }
                          , expected = [ ( 0, 0 ) ]
                          }
                        , { name = "2×2 tiles: all four tiles are on border"
                          , bbox = { x = 4, y = 4, w = 8, h = 8 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]
                          }
                        , { name = "3×3 tiles ring: center tile not included"
                          , bbox = { x = 2, y = 2, w = 16, h = 16 }
                          , expected = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 0, 1 ), ( 2, 1 ), ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
                          }
                        ]

                    toTest c =
                        Test.test c.name <|
                            \() ->
                                Dirty.markRectBorder_TEST grid c.bbox
                                    |> Expect.equal (Set.fromList c.expected)
                in
                List.map toTest cases
            ]
        , let
            basicGrid =
                { tileSize = 8
                , tileCols = 4
                , tileRows = 4
                }
          in
          Test.describe "diff"
            [ Test.describe "Rect (outline) scenes"
                [ Test.test "two rects sharing top-left, same color, second larger: dirty tiles include both old and new borders" <|
                    \_ ->
                        let
                            color =
                                Color.black

                            smallBbox =
                                { x = 0, y = 0, w = 8, h = 8 }

                            largeBbox =
                                { x = 0, y = 0, w = 16, h = 16 }

                            oldRoot =
                                Node.rect "r"
                                    { x = smallBbox.x
                                    , y = smallBbox.y
                                    , w = smallBbox.w
                                    , h = smallBbox.h
                                    , color = color
                                    }

                            newRoot =
                                Node.rect "r"
                                    { x = largeBbox.x
                                    , y = largeBbox.y
                                    , w = largeBbox.w
                                    , h = largeBbox.h
                                    , color = color
                                    }

                            expected =
                                Dirty.markRectBorder_TEST basicGrid smallBbox
                                    |> Set.union (Dirty.markRectBorder_TEST basicGrid largeBbox)
                        in
                        Dirty.diff basicGrid [] oldRoot newRoot
                            |> Expect.equal expected
                , Test.test "same-color rect moved left across a tile boundary repaints both old and new border tiles" <|
                    \_ ->
                        let
                            oldBbox =
                                { x = 8, y = 0, w = 8, h = 8 }

                            newBbox =
                                { x = 7, y = 0, w = 8, h = 8 }

                            oldRoot =
                                Node.rect "r"
                                    { x = oldBbox.x
                                    , y = oldBbox.y
                                    , w = oldBbox.w
                                    , h = oldBbox.h
                                    , color = Color.black
                                    }

                            newRoot =
                                Node.rect "r"
                                    { x = newBbox.x
                                    , y = newBbox.y
                                    , w = newBbox.w
                                    , h = newBbox.h
                                    , color = Color.black
                                    }

                            expected =
                                Dirty.markRectBorder_TEST basicGrid oldBbox
                                    |> Set.union (Dirty.markRectBorder_TEST basicGrid newBbox)
                        in
                        Dirty.diff basicGrid [] oldRoot newRoot
                            |> Expect.equal expected
                , Test.test "same-color rect geometry change within the same tile ring still repaints all touched tiles" <|
                    \_ ->
                        let
                            oldBbox =
                                { x = 10, y = 10, w = 14, h = 14 }

                            newBbox =
                                { x = 9, y = 10, w = 15, h = 14 }

                            oldRoot =
                                Node.rect "r"
                                    { x = oldBbox.x
                                    , y = oldBbox.y
                                    , w = oldBbox.w
                                    , h = oldBbox.h
                                    , color = Color.black
                                    }

                            newRoot =
                                Node.rect "r"
                                    { x = newBbox.x
                                    , y = newBbox.y
                                    , w = newBbox.w
                                    , h = newBbox.h
                                    , color = Color.black
                                    }

                            expected =
                                Dirty.markRectBorder_TEST basicGrid oldBbox
                                    |> Set.union (Dirty.markRectBorder_TEST basicGrid newBbox)
                        in
                        Dirty.diff basicGrid [] oldRoot newRoot
                            |> Expect.equal expected
                ]
            , Test.describe "Group-Group scenes"
                [ Test.test "update+insert+delete via key matching" <|
                    \_ ->
                        let
                            color =
                                Color.black

                            rect key bbox =
                                Node.rect key
                                    { x = bbox.x
                                    , y = bbox.y
                                    , w = bbox.w
                                    , h = bbox.h
                                    , color = color
                                    }

                            bboxA =
                                { x = 0, y = 0, w = 8, h = 8 }

                            bboxB =
                                { x = 8, y = 0, w = 8, h = 8 }

                            bboxB2 =
                                { x = 8, y = 0, w = 16, h = 8 }

                            bboxC =
                                { x = 0, y = 8, w = 8, h = 8 }

                            oldA =
                                rect "a" bboxA

                            oldB =
                                rect "b" bboxB

                            newB =
                                rect "b" bboxB2

                            newC =
                                rect "c" bboxC

                            oldRoot =
                                Node.group "g" [ oldA, oldB ]

                            newRoot =
                                Node.group "g" [ newB, newC ]

                            expected =
                                Dirty.diff basicGrid [] oldB newB
                                    |> Set.union (Dirty.markBbox_TEST basicGrid oldA.bbox)
                                    |> Set.union (Dirty.markBbox_TEST basicGrid newC.bbox)
                        in
                        Dirty.diff basicGrid [] oldRoot newRoot
                            |> Expect.equal expected
                ]
            , Test.fuzz Fuzzers.node "diff of two identical nodes is empty" <|
                \node ->
                    Dirty.diff basicGrid [] node node
                        |> Expect.equal Set.empty
            , Test.test "keyed sibling reorder: overlapping rects order swapped marks tiles" <|
                \_ ->
                    let
                        grid =
                            basicGrid

                        bboxA =
                            { x = 0, y = 0, w = 8, h = 8 }

                        bboxB =
                            { x = 4, y = 4, w = 8, h = 8 }

                        rect key b =
                            Node.rect key
                                { x = b.x, y = b.y, w = b.w, h = b.h, color = Color.black }

                        oldRoot =
                            Node.group "g" [ rect "a" bboxA, rect "b" bboxB ]

                        newRoot =
                            Node.group "g" [ rect "b" bboxB, rect "a" bboxA ]

                        expected =
                            Dirty.markBbox_TEST grid bboxA
                                |> Set.union (Dirty.markBbox_TEST grid bboxB)
                    in
                    Dirty.diff grid [] oldRoot newRoot
                        |> Expect.equal expected
            , Test.test "Rect color-only change: same bbox different color marks border tiles" <|
                \_ ->
                    let
                        grid =
                            basicGrid

                        bbox =
                            { x = 0, y = 0, w = 8, h = 8 }

                        oldRoot =
                            Node.rect "r"
                                { x = bbox.x, y = bbox.y, w = bbox.w, h = bbox.h, color = Color.black }

                        newRoot =
                            Node.rect "r"
                                { x = bbox.x, y = bbox.y, w = bbox.w, h = bbox.h, color = Color.white }

                        expected =
                            Dirty.markRectBorder_TEST grid bbox
                    in
                    Dirty.diff grid [] oldRoot newRoot
                        |> Expect.equal expected
            , Test.test "Text: changed glyph spanning two tiles marks both tiles" <|
                \_ ->
                    let
                        grid =
                            basicGrid

                        font =
                            { name = "Test"
                            , asciiFirst = 32
                            , asciiLast = 126
                            , numGlyphs = 95
                            , glyphWidth = 8
                            , glyphHeight = 8
                            , extraLineHeight = 0
                            , bits = []
                            }

                        oldRoot =
                            Node.text [ font ] "txt" { x = 1, y = 0, text = "a", fontIndex = 0, color = Color.white }

                        newRoot =
                            Node.text [ font ] "txt" { x = 1, y = 0, text = "b", fontIndex = 0, color = Color.white }

                        glyphBbox =
                            { x = 1, y = 0, w = 8, h = 8 }

                        expected =
                            Dirty.markBbox_TEST grid glyphBbox
                    in
                    Dirty.diff grid [ font ] oldRoot newRoot
                        |> Expect.equal expected
            , Test.test "Text: leading newline A to \\nA marks tile containing second line" <|
                \_ ->
                    let
                        grid =
                            basicGrid

                        font =
                            { name = "Test"
                            , asciiFirst = 32
                            , asciiLast = 126
                            , numGlyphs = 95
                            , glyphWidth = 8
                            , glyphHeight = 7
                            , extraLineHeight = 1
                            , bits = []
                            }

                        oldRoot =
                            Node.text [ font ] "txt" { x = 0, y = 0, text = "A", fontIndex = 0, color = Color.white }

                        newRoot =
                            Node.text [ font ] "txt" { x = 0, y = 0, text = "\nA", fontIndex = 0, color = Color.white }
                    in
                    Dirty.diff grid [ font ] oldRoot newRoot
                        |> Expect.equalSets (Set.fromList [ ( 0, 0 ), ( 0, 1 ) ])
            ]
        ]

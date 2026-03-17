module DirtyTests exposing (suite)

import BoundingBox exposing (BoundingBox)
import Dirty
import Expect
import Fuzz exposing (Fuzzer)
import Set
import String.Extra
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Dirty"
        [ Test.describe "changedTextCells"
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
                          , [ ( 0, 2 ), ( 0, 1 ), ( 0, 0 ), ( 0, 3 ) ]
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
                          , [ ( 1, 2 ), ( 1, 1 ), ( 1, 3 ) ]
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
                          , [ ( 2, 2 ), ( 2, 1 ), ( 2, 0 ) ]
                          )
                        , ( "added line with newline"
                          , ( "...\n..."
                            , "...\n...\n..."
                            )
                          , [ ( 2, 2 ), ( 2, 1 ), ( 2, 0 ) ]
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
                                (stringOfLength len)
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
                            , List ( Int, Int )
                            )
                    testCases =
                        [ ( "empty string"
                          , ( 0, 0, "" )
                          , []
                          )
                        , ( "simple"
                          , ( 0, 0, "abc\ndef" )
                          , [ ( 1, 2 ), ( 1, 1 ), ( 1, 0 ), ( 0, 2 ), ( 0, 1 ), ( 0, 0 ) ]
                          )
                        , ( "non-zero coords"
                          , ( 3, 2, "abc\ndef" )
                          , [ ( 4, 2 ), ( 4, 1 ), ( 4, 0 ), ( 3, 4 ), ( 3, 3 ), ( 3, 2 ) ]
                          )
                        ]

                    toTest :
                        ( String
                        , ( Int, Int, String )
                        , List ( Int, Int )
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
                        |> Expect.equalLists
                            (Dirty.allTextCells_TEST row col (String.toList chars)
                                |> List.sort
                                |> List.map
                                    (\( r, c ) ->
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
        ]

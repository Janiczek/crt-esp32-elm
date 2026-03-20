module BitmapTests exposing (suite)

import Bitmap exposing (BitDepth(..))
import Expect
import Fuzz exposing (Fuzzer)
import Fuzzers
import Test exposing (Test)


type alias BitmapRowInput =
    { bitDepth : BitDepth
    , width : Int
    , row : Int
    , data : List Int
    }


suite : Test
suite =
    Test.describe "Bitmap"
        [ Test.describe "rowGraysSequential"
            [ Test.describe "unit tests"
                (let
                    testCases : List ( String, ( ( BitDepth, Int, Int ), List Int ), List Int )
                    testCases =
                        [ ( "width <= 0 returns an empty row"
                          , ( ( BitDepth8, 0, 0 ), [ 10, 20, 30 ] )
                          , []
                          )
                        , ( "BitDepth8 reads the requested row exactly"
                          , ( ( BitDepth8, 3, 0 ), [ 10, 20, 30 ] )
                          , [ 10, 20, 30 ]
                          )
                        , ( "BitDepth8 pads missing bytes with zeros"
                          , ( ( BitDepth8, 4, 0 ), [ 10, 20 ] )
                          , [ 10, 20, 0, 0 ]
                          )
                        , ( "BitDepth8 clamps grayscale values into 0..255"
                          , ( ( BitDepth8, 3, 0 ), [ 300, -5, 128 ] )
                          , [ 255, 0, 128 ]
                          )
                        , ( "BitDepth4 respects nibble alignment across row boundaries"
                          , ( ( BitDepth4, 3, 1 ), [ 0x12, 0x34, 0x56 ] )
                          , [ 68, 85, 102 ]
                          )
                        , ( "BitDepth2 respects sample alignment across row boundaries"
                          , ( ( BitDepth2, 3, 1 ), [ 0x1B, 0xE4 ] )
                          , [ 255, 255, 170 ]
                          )
                        , ( "BitDepth1 expands bits into black and white pixels"
                          , ( ( BitDepth1, 5, 0 ), [ 0xB0 {- 0b10110000 -} ] )
                          , [ 255, 0, 255, 255, 0 ]
                          )
                        , ( "BitDepth1 pads missing source bytes with zeros"
                          , ( ( BitDepth1, 10, 0 ), [ 0xFF ] )
                          , [ 255, 255, 255, 255, 255, 255, 255, 255, 0, 0 ]
                          )
                        ]

                    toTest : ( String, ( ( BitDepth, Int, Int ), List Int ), List Int ) -> Test
                    toTest ( desc, ( ( bitDepth, width, row ), inputData ), expected ) =
                        Test.test desc <|
                            \_ ->
                                Bitmap.rowGraysSequential_TEST bitDepth width row inputData
                                    |> Expect.equal expected
                 in
                 List.map toTest testCases
                )
            , Test.fuzz rowInputFuzzer "always returns exactly width grayscale samples in range" <|
                \input ->
                    let
                        grays =
                            Bitmap.rowGraysSequential_TEST input.bitDepth input.width input.row input.data
                    in
                    if List.length grays /= input.width then
                        Expect.fail "rowGraysSequential returned the wrong number of pixels"

                    else if List.any (\gray -> gray < 0 || gray > 255) grays then
                        Expect.fail "rowGraysSequential returned a value outside 0..255"

                    else
                        Expect.pass
            , Test.fuzz bitDepth8ReferenceFuzzer "BitDepth8 row offsets match the raw byte layout" <|
                \( width, row, byte ) ->
                    let
                        grays =
                            Bitmap.rowGraysSequential_TEST BitDepth8 width row (List.repeat (row * width) 0 ++ List.repeat width byte)
                    in
                    grays
                        |> Expect.equal (List.repeat width byte)
            ]
        ]


rowInputFuzzer : Fuzzer BitmapRowInput
rowInputFuzzer =
    Fuzz.map3
        (\bitDepth width row ->
            let
                maxLen =
                    Bitmap.packedByteLength width (row + 1) bitDepth + 8
            in
            Fuzz.listOfLengthBetween 0 maxLen (Fuzz.intRange 0 255)
                |> Fuzz.map
                    (\data ->
                        { bitDepth = bitDepth
                        , width = width
                        , row = row
                        , data = data
                        }
                    )
        )
        Fuzzers.bitDepth
        (Fuzz.intRange 1 64)
        (Fuzz.intRange 0 16)
        |> Fuzz.andThen identity


bitDepth8ReferenceFuzzer : Fuzzer ( Int, Int, Int )
bitDepth8ReferenceFuzzer =
    Fuzz.map3 (\width row byte -> ( width, row, byte ))
        (Fuzz.intRange 1 64)
        (Fuzz.intRange 0 16)
        (Fuzz.intRange 0 255)

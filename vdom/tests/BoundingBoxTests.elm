module BoundingBoxTests exposing (suite)

import BoundingBox exposing (BoundingBox)
import Expect
import Fuzzers
import Test exposing (Test)


suite : Test
suite =
    Test.describe "BoundingBox"
        [ Test.test "union is computed from extents (regression)" <|
            \_ ->
                let
                    a : BoundingBox
                    a =
                        { x = 10, y = 10, w = 3, h = 4 }

                    b : BoundingBox
                    b =
                        { x = 100, y = 20, w = 5, h = 6 }

                    u : BoundingBox
                    u =
                        BoundingBox.union a b
                in
                -- Old buggy implementation would keep w/h as max(w/h),
                -- which fails whenever boxes are far apart.
                u
                    |> Expect.equal
                        { x = 10
                        , y = 10
                        , w = (100 + 5) - 10
                        , h = (20 + 6) - 10
                        }
        , Test.fuzz Fuzzers.bbox "union with empty returns the other bbox" <|
            \bbox ->
                BoundingBox.union BoundingBox.empty bbox
                    |> Expect.equal bbox
        ]


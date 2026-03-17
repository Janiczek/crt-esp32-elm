module BoundingBoxTests exposing (suite)

import BoundingBox exposing (BoundingBox)
import Expect
import Fuzz exposing (Fuzzer)
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
        , Test.fuzz2 Fuzzers.bbox Fuzzers.bbox "commutative" <|
            \a b ->
                BoundingBox.union a b
                    |> Expect.equal (BoundingBox.union b a)
        , Test.fuzz Fuzzers.bbox "union with empty returns the other bbox" <|
            \bbox ->
                BoundingBox.union BoundingBox.empty bbox
                    |> Expect.equal bbox
        , Test.fuzz2 Fuzzers.bbox Fuzzers.bbox "union of any two valid boxes has correct extent (min corner, max right/bottom)" <|
            \a b ->
                let
                    u =
                        BoundingBox.union a b

                    expectedX =
                        min a.x b.x

                    expectedY =
                        min a.y b.y

                    expectedRight =
                        max (a.x + a.w) (b.x + b.w)

                    expectedBottom =
                        max (a.y + a.h) (b.y + b.h)
                in
                Expect.all
                    [ \r -> Expect.equal expectedX r.x
                    , \r -> Expect.equal expectedY r.y
                    , \r -> Expect.equal (expectedRight - expectedX) r.w
                    , \r -> Expect.equal (expectedBottom - expectedY) r.h
                    ]
                    u
        , Test.fuzz bboxWithContained "union when one bbox fully contains the other equals the outer box" <|
            \( outer, inner ) ->
                BoundingBox.union outer inner
                    |> Expect.equal outer
        , Test.fuzz bboxSharingCorner "union of two boxes sharing a corner equals minimal enclosing box" <|
            \( a, b ) ->
                let
                    u =
                        BoundingBox.union a b
                in
                Expect.all
                    [ \r -> Expect.equal a.x r.x
                    , \r -> Expect.equal a.y r.y
                    , \r -> Expect.equal (a.w + b.w) r.w
                    , \r -> Expect.equal (a.h + b.h) r.h
                    ]
                    u
        , Test.fuzz disjointBboxes "union of two disjoint (non-touching) boxes still has correct extent" <|
            \( a, b ) ->
                let
                    u =
                        BoundingBox.union a b
                in
                Expect.all
                    [ \r -> Expect.equal (min a.x b.x) r.x
                    , \r -> Expect.equal (min a.y b.y) r.y
                    , \r -> Expect.equal (max (a.x + a.w) (b.x + b.w) - min a.x b.x) r.w
                    , \r -> Expect.equal (max (a.y + a.h) (b.y + b.h) - min a.y b.y) r.h
                    ]
                    u
        ]


bboxWithContained : Fuzzer ( BoundingBox, BoundingBox )
bboxWithContained =
    Fuzzers.bbox
        |> Fuzz.andThen
            (\inner ->
                Fuzz.map4
                    (\padL padT padR padB ->
                        ( { x = inner.x - padL
                          , y = inner.y - padT
                          , w = inner.w + padL + padR
                          , h = inner.h + padT + padB
                          }
                        , inner
                        )
                    )
                    (Fuzz.intRange 0 200)
                    (Fuzz.intRange 0 200)
                    (Fuzz.intRange 0 200)
                    (Fuzz.intRange 0 200)
            )


bboxSharingCorner : Fuzzer ( BoundingBox, BoundingBox )
bboxSharingCorner =
    Fuzzers.bbox
        |> Fuzz.andThen
            (\a ->
                Fuzz.map2
                    (\bw bh ->
                        ( a
                        , { x = a.x + a.w
                          , y = a.y + a.h
                          , w = bw
                          , h = bh
                          }
                        )
                    )
                    (Fuzz.intRange 1 200)
                    (Fuzz.intRange 1 200)
            )


disjointBboxes : Fuzzer ( BoundingBox, BoundingBox )
disjointBboxes =
    Fuzzers.bbox
        |> Fuzz.andThen
            (\a ->
                Fuzz.map4
                    (\gap bY bW bH ->
                        ( a
                        , { x = a.x + a.w + gap
                          , y = bY
                          , w = bW
                          , h = bH
                          }
                        )
                    )
                    (Fuzz.intRange 1 100)
                    Fuzz.int
                    (Fuzz.intRange 1 200)
                    (Fuzz.intRange 1 200)
            )

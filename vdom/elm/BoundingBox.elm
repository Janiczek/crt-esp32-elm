module BoundingBox exposing (BoundingBox, contains, empty, union)


type alias BoundingBox =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }


empty : BoundingBox
empty =
    { x = 0
    , y = 0
    , w = 0
    , h = 0
    }


{-| Half-open rectangle; false when width or height is non-positive.
-}
contains : Int -> Int -> BoundingBox -> Bool
contains x y box =
    (box.w > 0)
        && (box.h > 0)
        && (x >= box.x)
        && (x < box.x + box.w)
        && (y >= box.y)
        && (y < box.y + box.h)


union : BoundingBox -> BoundingBox -> BoundingBox
union a b =
    if a.w <= 0 || a.h <= 0 then
        b

    else if b.w <= 0 || b.h <= 0 then
        a

    else
        let
            x0 =
                min a.x b.x

            y0 =
                min a.y b.y

            x1 =
                max (a.x + a.w) (b.x + b.w)

            y1 =
                max (a.y + a.h) (b.y + b.h)
        in
        { x = x0
        , y = y0
        , w = x1 - x0
        , h = y1 - y0
        }

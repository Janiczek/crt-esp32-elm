module BoundingBox exposing (BoundingBox, empty, union)


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


union : BoundingBox -> BoundingBox -> BoundingBox
union a b =
    { x = min a.x b.x
    , y = min a.y b.y
    , w = max a.w b.w
    , h = max a.h b.h
    }

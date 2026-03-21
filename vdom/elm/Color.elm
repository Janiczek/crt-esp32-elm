module Color exposing (Color, black, gray, toCss, white)


type alias Color =
    Int


black : Color
black =
    0


gray : Color
gray =
    127


white : Color
white =
    255


toCss : Color -> String
toCss c =
    let
        n =
            clamp 0 255 c
    in
    "rgb(" ++ String.fromInt n ++ "," ++ String.fromInt n ++ "," ++ String.fromInt n ++ ")"

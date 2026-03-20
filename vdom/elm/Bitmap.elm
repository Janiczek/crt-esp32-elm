module Bitmap exposing
    ( BitDepth(..), bitDepthFromInt, bitDepthToInt
    , packedByteLength
    , rowGraysSequential
    , rowGraysSequential_TEST
    )

{-|

@docs BitDepth, bitDepthFromInt, bitDepthToInt
@docs packedByteLength
@docs rowGraysSequential
@docs rowGraysSequential_TEST

-}

import Bitwise


packedByteLength : Int -> Int -> BitDepth -> Int
packedByteLength w h bitDepth =
    ((max 0 w * max 0 h * bitDepthToInt bitDepth) + 7) // 8


type BitDepth
    = BitDepth1
    | BitDepth2
    | BitDepth4
    | BitDepth8


bitDepthToInt : BitDepth -> Int
bitDepthToInt d =
    case d of
        BitDepth1 ->
            1

        BitDepth2 ->
            2

        BitDepth4 ->
            4

        BitDepth8 ->
            8


bitDepthFromInt : Int -> Maybe BitDepth
bitDepthFromInt n =
    case n of
        1 ->
            Just BitDepth1

        2 ->
            Just BitDepth2

        4 ->
            Just BitDepth4

        8 ->
            Just BitDepth8

        _ ->
            Nothing


{-| One row of packed bitmap data as grayscale 0-255, left to right (single sequential pass over `List Int`).

TODO: is any of this generalizable / extractable? Seems like there's a bunch of similar code.

-}
rowGraysSequential : BitDepth -> Int -> Int -> List Int -> List Int
rowGraysSequential bitDepth width row data =
    let
        p0 =
            row * width

        padEndZeros : Int -> List Int -> List Int
        padEndZeros left acc =
            List.reverse acc ++ List.repeat left 0
    in
    if width <= 0 then
        []

    else
        case bitDepth of
            BitDepth8 ->
                List.drop p0 data
                    |> List.take width
                    |> (\xs -> xs ++ List.repeat (width - List.length xs) 0)
                    |> List.map (clamp 0 255)

            BitDepth4 ->
                let
                    go left highNibbleNext bs acc =
                        if left <= 0 then
                            List.reverse acc

                        else
                            case bs of
                                [] ->
                                    padEndZeros left acc

                                b :: rest ->
                                    let
                                        sample =
                                            if highNibbleNext then
                                                Bitwise.and (Bitwise.shiftRightZfBy 4 b) 0x0F

                                            else
                                                Bitwise.and b 0x0F

                                        gray =
                                            sample * 17
                                    in
                                    if highNibbleNext then
                                        go (left - 1) False (b :: rest) (gray :: acc)

                                    else
                                        go (left - 1) True rest (gray :: acc)
                in
                go width (modBy 2 p0 == 0) (List.drop (p0 // 2) data) []

            BitDepth2 ->
                let
                    go left posInByte bs acc =
                        if left <= 0 then
                            List.reverse acc

                        else
                            case bs of
                                [] ->
                                    padEndZeros left acc

                                b :: rest ->
                                    if posInByte >= 4 then
                                        go left 0 rest acc

                                    else
                                        let
                                            shift =
                                                6 - posInByte * 2

                                            sample =
                                                Bitwise.and (Bitwise.shiftRightZfBy shift b) 0x03

                                            gray =
                                                sample * 85

                                            nextPos =
                                                posInByte + 1
                                        in
                                        if nextPos >= 4 then
                                            go (left - 1) 0 rest (gray :: acc)

                                        else
                                            go (left - 1) nextPos (b :: rest) (gray :: acc)
                in
                go width (modBy 4 p0) (List.drop (p0 // 4) data) []

            BitDepth1 ->
                let
                    go left bitInByte bs acc =
                        if left <= 0 then
                            List.reverse acc

                        else
                            case bs of
                                [] ->
                                    padEndZeros left acc

                                b :: rest ->
                                    if bitInByte >= 8 then
                                        go left 0 rest acc

                                    else
                                        let
                                            shift =
                                                7 - bitInByte

                                            sample =
                                                Bitwise.and (Bitwise.shiftRightZfBy shift b) 0x01

                                            gray =
                                                if sample == 0 then
                                                    0

                                                else
                                                    255

                                            nextBit =
                                                bitInByte + 1
                                        in
                                        if nextBit >= 8 then
                                            go (left - 1) 0 rest (gray :: acc)

                                        else
                                            go (left - 1) nextBit (b :: rest) (gray :: acc)
                in
                go width (modBy 8 p0) (List.drop (p0 // 8) data) []


rowGraysSequential_TEST : BitDepth -> Int -> Int -> List Int -> List Int
rowGraysSequential_TEST =
    rowGraysSequential

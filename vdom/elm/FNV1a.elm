module FNV1a exposing
    ( initialSeed
    , updateInt16
    , updateInt32
    , updateInt8
    , updateString
    )

{-| 32-bit FNV-1a hash. Ported from C hash.h. ASCII strings (char codes 32..126) only;
one byte per character. No UTF-8/UTF-32. Also hashes integers in little-endian byte order.
-}

import Bitwise


prime : Int
prime =
    16777619


{-| Initial seed (2166136261). hash "" == initialSeed.
-}
initialSeed : Int
initialSeed =
    0x811C9DC5


step : Int -> Int -> Int
step h b =
    Bitwise.and 0xFFFFFFFF (Bitwise.xor h (Bitwise.and 0xFF b) * prime)


{-| Feed an ASCII string into the hash.
-}
updateString : String -> Int -> Int
updateString str seed =
    String.foldl (\c acc -> step acc (Char.toCode c)) seed str


{-| Feed a signed 8-bit integer (one byte, endianness doesn't matter) into the hash.
-}
updateInt8 : Int -> Int -> Int
updateInt8 value seed =
    step seed (Bitwise.and 0xFF value)


{-| Feed a signed 16-bit integer (two bytes, little-endian) into the hash.
-}
updateInt16 : Int -> Int -> Int
updateInt16 value seed =
    let
        v =
            Bitwise.and 0xFFFFFFFF value
    in
    seed
        |> step (Bitwise.and 0xFF v)
        |> step (Bitwise.and 0xFF (Bitwise.shiftRightZfBy 8 v))


{-| Feed a signed 32-bit integer (four bytes, little-endian) into the hash.
-}
updateInt32 : Int -> Int -> Int
updateInt32 value seed =
    let
        v =
            Bitwise.and 0xFFFFFFFF value
    in
    seed
        |> step (Bitwise.and 0xFF v)
        |> step (Bitwise.and 0xFF (Bitwise.shiftRightZfBy 8 v))
        |> step (Bitwise.and 0xFF (Bitwise.shiftRightZfBy 16 v))
        |> step (Bitwise.and 0xFF (Bitwise.shiftRightZfBy 24 v))

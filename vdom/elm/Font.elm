module Font exposing (Font, decoder, renderGlyphPixel)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode
import Bytes.Decode.Extra
import BytesExtraExtra


type alias Font =
    { name : String
    , asciiFirst : Int
    , asciiLast : Int
    , numGlyphs : Int
    , glyphWidth : Int
    , glyphHeight : Int
    , extraLineHeight : Int
    , bits : List Int -- 0..255 (uint8s), glyph bitmaps
    }


decoder : Bytes.Decode.Decoder Font
decoder =
    Bytes.Decode.succeed Font
        |> Bytes.Decode.Extra.andMap BytesExtraExtra.sizedStringDecoder
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        |> Bytes.Decode.Extra.andMap (BytesExtraExtra.sizedListDecoder Bytes.Decode.unsignedInt8)


renderGlyphPixel : Font -> Int -> Int -> Int -> Bool
renderGlyphPixel font g row col =
    let
        byteIdx : Int
        byteIdx =
            g * font.glyphHeight + row

        byte : Int
        byte =
            font.bits
                |> List.drop byteIdx
                |> List.head
                |> Maybe.withDefault 0

        bit : Int
        bit =
            Bitwise.and (Bitwise.shiftRightBy (7 - col) byte) 1
    in
    bit == 1

module BytesExtraExtra exposing
    ( compressedEncoder
    , sizedListDecoder
    , sizedListEncoder
    , sizedStringDecoder
    , sizedStringEncoder
    )

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode exposing (Decoder)
import Bytes.Decode.Extra
import Bytes.Encode
import Flate


sizedStringDecoder : Decoder String
sizedStringDecoder =
    Bytes.Decode.unsignedInt16 LE
        |> Bytes.Decode.andThen Bytes.Decode.string


sizedListDecoder : Decoder a -> Decoder (List a)
sizedListDecoder childDecoder =
    Bytes.Decode.unsignedInt16 LE
        |> Bytes.Decode.andThen (\len -> Bytes.Decode.Extra.list len childDecoder)


sizedStringEncoder : String -> Bytes.Encode.Encoder
sizedStringEncoder s =
    Bytes.Encode.sequence
        [ Bytes.Encode.unsignedInt16 LE (Bytes.Encode.getStringWidth s)
        , Bytes.Encode.string s
        ]


sizedListEncoder : (a -> Bytes.Encode.Encoder) -> List a -> Bytes.Encode.Encoder
sizedListEncoder childEncoder xs =
    Bytes.Encode.sequence
        [ Bytes.Encode.unsignedInt16 LE (List.length xs)
        , Bytes.Encode.sequence (List.map childEncoder xs)
        ]


compressedEncoder : Bytes.Encode.Encoder -> Bytes.Encode.Encoder
compressedEncoder encoder =
    let
        raw : Bytes
        raw =
            Bytes.Encode.encode encoder

        decompressedLen : Int
        decompressedLen =
            Bytes.width raw
    in
    if decompressedLen == 0 then
        Bytes.Encode.sequence
            [ Bytes.Encode.unsignedInt16 LE 0
            , Bytes.Encode.unsignedInt16 LE 0
            ]

    else
        let
            compressed : Bytes
            compressed =
                Flate.deflateZlib raw

            compressedLen : Int
            compressedLen =
                Bytes.width compressed
        in
        Bytes.Encode.sequence
            [ Bytes.Encode.unsignedInt16 LE decompressedLen
            , Bytes.Encode.unsignedInt16 LE compressedLen
            , Bytes.Encode.bytes compressed
            ]

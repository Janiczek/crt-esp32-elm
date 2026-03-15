module BytesExtraExtra exposing
    ( sizedListDecoder
    , sizedListEncoder
    , sizedStringDecoder
    , sizedStringEncoder
    )

import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (Decoder)
import Bytes.Decode.Extra
import Bytes.Encode


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

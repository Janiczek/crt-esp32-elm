module ESP32 exposing (ESP32, VideoConstants, decoder, videoConstants)

{-| TODO PERF: optimize by using smaller (16bit) ints?
-}

import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (Decoder)
import Bytes.Decode.Extra
import Font exposing (Font)
import BytesExtraExtra


type alias ESP32 =
    { videoWidth : Int
    , videoHeight : Int
    , crtPaddingLeft : Int
    , crtPaddingRight : Int
    , crtPaddingTop : Int
    , crtPaddingBottom : Int
    , maxTotalNodes : Int
    , nodeGroupMaxChildren : Int
    , fonts : List Font
    }


type alias VideoConstants =
    { xMin : Int
    , xMax : Int
    , yMin : Int
    , yMax : Int
    , usableW : Int
    , usableH : Int
    , xCenter : Int
    , yCenter : Int
    }


{-| Mirrors #defines in constants.h
-}
videoConstants : ESP32 -> VideoConstants
videoConstants esp32 =
    let
        xMin =
            esp32.crtPaddingLeft

        xMax =
            esp32.videoWidth - esp32.crtPaddingRight

        yMin =
            esp32.crtPaddingTop

        yMax =
            esp32.videoHeight - esp32.crtPaddingBottom

        usableW =
            xMax - xMin + 1

        usableH =
            yMax - yMin + 1

        xCenter =
            (usableW // 2) + xMin

        yCenter =
            (usableH // 2) + yMin
    in
    { xMin = xMin
    , xMax = xMax
    , yMin = yMin
    , yMax = yMax
    , usableW = usableW
    , usableH = usableH
    , xCenter = xCenter
    , yCenter = yCenter
    }


{-| Decodes ESP32 from 12 bytes: six big-endian uint16s in field order.
-}
decoder : Decoder ESP32
decoder =
    Bytes.Decode.succeed ESP32
        -- Video dimensions
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        -- CRT paddings
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt8)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt8)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt8)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt8)
        -- Limits
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        -- Fonts
        |> Bytes.Decode.Extra.andMap (BytesExtraExtra.sizedListDecoder Font.decoder)
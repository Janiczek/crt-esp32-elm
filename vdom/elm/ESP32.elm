module ESP32 exposing (ESP32, VideoConstants, decoder, growToAccommodate, videoConstants)

{-| TODO PERF: optimize by using smaller (16bit) ints?
-}

import BoundingBox exposing (BoundingBox)
import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (Decoder)
import Bytes.Decode.Extra
import BytesExtraExtra
import Font exposing (Font)


type alias ESP32 =
    { videoWidth : Int
    , videoHeight : Int
    , crtPaddingLeft : Int
    , crtPaddingRight : Int
    , crtPaddingTop : Int
    , crtPaddingBottom : Int
    , maxTotalNodes : Int
    , nodeGroupMaxChildren : Int
    , tileSize : Int
    , fonts : List Font
    }


type alias VideoConstants =
    { xMin : Int
    , xMax : Int
    , yMin : Int
    , yMax : Int
    , usableWidth : Int
    , usableHeight : Int
    , xCenter : Int
    , yCenter : Int
    , tileCols : Int
    , tileRows : Int
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

        usableWidth =
            xMax - xMin + 1

        usableHeight =
            yMax - yMin + 1

        xCenter =
            (usableWidth // 2) + xMin

        yCenter =
            (usableHeight // 2) + yMin

        tileCols =
            esp32.videoWidth // esp32.tileSize

        tileRows =
            esp32.videoHeight // esp32.tileSize
    in
    { xMin = xMin
    , xMax = xMax
    , yMin = yMin
    , yMax = yMax
    , usableWidth = usableWidth
    , usableHeight = usableHeight
    , xCenter = xCenter
    , yCenter = yCenter
    , tileCols = tileCols
    , tileRows = tileRows
    }


{-| Grow `videoWidth` / `videoHeight` only (CRT paddings are unchanged) so the usable
area extends far enough right/down to include every pixel in `bbox`, when that is
possible without moving the usable origin.

Uses the same half-open convention as `Node.pointInBounds`: valid `x` satisfies
`bbox.x <= x < bbox.x + bbox.w` (and similarly for `y`). Empty boxes (`w <= 0` or `h <= 0`) are a no-op.

Because `xMin` / `yMin` stay `crtPaddingLeft` / `crtPaddingTop`, a box with
`bbox.x < xMin` or `bbox.y < yMin` cannot be fully accommodated — this returns
`esp32` unchanged. Layouts anchored at the current usable origin (e.g. preview
`rectScenario`) always pass that check.

-}
growToAccommodate : BoundingBox -> ESP32 -> ESP32
growToAccommodate bbox esp32 =
    if bbox.w <= 0 || bbox.h <= 0 then
        esp32

    else
        let
            vc =
                videoConstants esp32
        in
        if bbox.x < vc.xMin || bbox.y < vc.yMin then
            esp32

        else
            let
                needXMax =
                    bbox.x + bbox.w - 1

                needYMax =
                    bbox.y + bbox.h - 1

                targetXMax =
                    max vc.xMax needXMax

                targetYMax =
                    max vc.yMax needYMax

                newVideoWidth =
                    max esp32.videoWidth (targetXMax + esp32.crtPaddingRight)

                newVideoHeight =
                    max esp32.videoHeight (targetYMax + esp32.crtPaddingBottom)
            in
            { esp32
                | videoWidth = newVideoWidth
                , videoHeight = newVideoHeight
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
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        -- Limits
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        |> Bytes.Decode.Extra.andMap (Bytes.Decode.unsignedInt16 LE)
        -- Tile
        |> Bytes.Decode.Extra.andMap Bytes.Decode.unsignedInt8
        -- Fonts
        |> Bytes.Decode.Extra.andMap (BytesExtraExtra.sizedListDecoder Font.decoder)

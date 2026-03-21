module Node exposing
    ( LimitError(..)
    , Node
    , Type(..)
    , bitmap
    , bytesEncoder
    , centerPosition
    , collectHitPaths
    , displayLabel
    , empty
    , fromKeyAndType
    , group
    , hitPathsAtPixel
    , jsonDecoder
    , jsonEncoder
    , label
    , limitErrors
    , maxFontIndex
    , maxGroupChildren
    , nodeDrawsPixel
    , pointInBounds
    , rect
    , rectFill
    , size
    , text
    , textDrawsPixel
    , xLine
    , yLine
    )

import Bitmap exposing (BitDepth)
import Bitwise
import BoundingBox exposing (BoundingBox)
import Bytes exposing (Endianness(..))
import Bytes.Encode
import BytesExtraExtra
import Color exposing (Color)
import ESP32 exposing (ESP32, VideoConstants)
import FNV1a
import Flate
import Font exposing (Font)
import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Encode
import List.Extra


type alias Node =
    { key : String
    , hash : Int
    , bbox : BoundingBox
    , type_ : Type
    }


type Type
    = Rect { x : Int, y : Int, w : Int, h : Int, color : Color }
    | RectFill { x : Int, y : Int, w : Int, h : Int, color : Color }
    | XLine { x : Int, y : Int, len : Int, color : Color }
    | YLine { x : Int, y : Int, len : Int, color : Color }
    | Text { x : Int, y : Int, text : String, fontIndex : Int, color : Color }
    | Bitmap { x : Int, y : Int, w : Int, h : Int, bitDepth : BitDepth, data : List Int }
    | Group { children : List Node }


type LimitError
    = MaxTotalNodesExceeded
        { maxTotalNodes : Int
        , actualTotalNodes : Int
        }
    | NodeGroupMaxChildrenExceeded
        { groupKey : String
        , maxChildren : Int
        , actualChildren : Int
        }


centerPosition : VideoConstants -> Node -> ( Int, Int )
centerPosition vc node_ =
    let
        centerBySize w h =
            ( vc.xCenter - (w // 2) |> max vc.xMin
            , vc.yCenter - (h // 2) |> max vc.yMin
            )
    in
    centerBySize
        (max 0 node_.bbox.w)
        (max 0 node_.bbox.h)


typeTag : Type -> Int
typeTag type_ =
    case type_ of
        Rect _ ->
            0

        RectFill _ ->
            1

        XLine _ ->
            2

        YLine _ ->
            3

        Text _ ->
            4

        Bitmap _ ->
            6

        Group _ ->
            5


label : Node -> String
label node_ =
    case node_.type_ of
        Rect _ ->
            "Rect"

        RectFill _ ->
            "RectFill"

        XLine _ ->
            "XLine"

        YLine _ ->
            "YLine"

        Text _ ->
            "Text"

        Bitmap _ ->
            "Bitmap"

        Group _ ->
            "Group"


displayLabel : Node -> String
displayLabel node_ =
    label node_ ++ " \"" ++ node_.key ++ "\""


pointInBounds : Int -> Int -> { a | x : Int, y : Int, w : Int, h : Int } -> Bool
pointInBounds x y bounds =
    (bounds.w > 0)
        && (bounds.h > 0)
        && (x >= bounds.x)
        && (x < bounds.x + bounds.w)
        && (y >= bounds.y)
        && (y < bounds.y + bounds.h)


textDrawsPixel : List Font -> Int -> Int -> { x : Int, y : Int, text : String, fontIndex : Int, color : Color } -> Bool
textDrawsPixel fonts px py r =
    case List.Extra.getAt r.fontIndex fonts of
        Nothing ->
            False

        Just font ->
            let
                lineHeight =
                    font.glyphHeight + font.extraLineHeight

                hasChar c =
                    let
                        code =
                            Char.toCode c
                    in
                    code >= font.asciiFirst && code <= font.asciiLast

                go curX curY chars =
                    case chars of
                        [] ->
                            False

                        '\n' :: rest ->
                            go r.x (curY + lineHeight) rest

                        c :: rest ->
                            let
                                nextX =
                                    curX
                                        + (if hasChar c then
                                            font.glyphWidth

                                           else
                                            0
                                          )
                            in
                            if
                                hasChar c
                                    && px
                                    >= curX
                                    && px
                                    < curX
                                    + font.glyphWidth
                                    && py
                                    >= curY
                                    && py
                                    < curY
                                    + font.glyphHeight
                            then
                                True

                            else
                                go nextX curY rest
            in
            go r.x r.y (String.toList r.text)


nodeDrawsPixel : List Font -> Int -> Int -> Node -> Bool
nodeDrawsPixel fonts px py node_ =
    if not (pointInBounds px py node_.bbox) then
        False

    else
        case node_.type_ of
            Rect { x, y, w, h } ->
                let
                    x2 =
                        x + w - 1

                    y2 =
                        y + h - 1
                in
                ((py == y || py == y2) && px >= x && px <= x2)
                    || ((px == x || px == x2) && py >= y && py <= y2)

            RectFill _ ->
                True

            XLine { x, y, len } ->
                len > 0 && py == y && px >= x && px < x + len

            YLine { x, y, len } ->
                len > 0 && px == x && py >= y && py < y + len

            Text r ->
                textDrawsPixel fonts px py r

            Bitmap _ ->
                True

            Group _ ->
                False


collectHitPaths : List Font -> Int -> Int -> List Int -> Node -> List (List Int)
collectHitPaths fonts px py path node_ =
    if not (pointInBounds px py node_.bbox) then
        []

    else
        case node_.type_ of
            Group { children } ->
                children
                    |> List.indexedMap
                        (\i child ->
                            collectHitPaths fonts px py (path ++ [ i ]) child
                        )
                    |> List.reverse
                    |> List.concat

            Text _ ->
                [ path ]

            Bitmap _ ->
                [ path ]

            Rect _ ->
                [ path ]

            RectFill _ ->
                [ path ]

            XLine _ ->
                [ path ]

            YLine _ ->
                [ path ]


hitPathsAtPixel : List Font -> Int -> Int -> Node -> List (List Int)
hitPathsAtPixel fonts px py root =
    collectHitPaths fonts px py [] root


text : List Font -> String -> { x : Int, y : Int, text : String, fontIndex : Int, color : Color } -> Node
text =
    node Text


rect : String -> { x : Int, y : Int, w : Int, h : Int, color : Color } -> Node
rect =
    node Rect []


rectFill : String -> { x : Int, y : Int, w : Int, h : Int, color : Color } -> Node
rectFill =
    node RectFill []


xLine : String -> { x : Int, y : Int, len : Int, color : Color } -> Node
xLine =
    node XLine []


yLine : String -> { x : Int, y : Int, len : Int, color : Color } -> Node
yLine =
    node YLine []


bitmap : String -> { x : Int, y : Int, w : Int, h : Int, bitDepth : BitDepth, data : List Int } -> Node
bitmap key config =
    let
        expectedLen =
            Bitmap.packedByteLength config.w config.h config.bitDepth
    in
    node Bitmap
        []
        key
        { config
            | data = normalizeBitmapData expectedLen config.data
        }


group : String -> List Node -> Node
group key children =
    node Group [] key { children = children }


empty : Node
empty =
    node Group [] "" { children = [] }


node : (a -> Type) -> List Font -> String -> a -> Node
node toType fonts key config =
    Node key 0 BoundingBox.empty (toType config)
        |> withBbox fonts
        |> withHash


textMultilineSize : Font -> String -> ( Int, Int )
textMultilineSize font s =
    let
        hasChar : Char -> Bool
        hasChar c =
            let
                code : Int
                code =
                    Char.toCode c
            in
            code >= font.asciiFirst && code <= font.asciiLast

        lineHeight : Int
        lineHeight =
            font.glyphHeight + font.extraLineHeight
    in
    String.foldl
        (\c ( maxW, lineW, lineCount ) ->
            if c == '\n' then
                ( max maxW lineW
                , 0
                , lineCount + 1
                )

            else
                ( maxW
                , lineW
                    + (if hasChar c then
                        font.glyphWidth

                       else
                        0
                      )
                , lineCount
                )
        )
        ( 0, 0, 1 )
        s
        |> (\( maxW, lineW, lineCount ) ->
                ( max maxW lineW, lineCount * lineHeight )
           )


normalizeBitmapData : Int -> List Int -> List Int
normalizeBitmapData expectedLen data =
    let
        normalized =
            data
                |> List.map (Bitwise.and 0xFF)
                |> List.take expectedLen
    in
    normalized
        ++ -- padding the rest of the data to the expected w * h
           List.repeat (expectedLen - List.length normalized) 0


{-| Bounding box for the node. Groups union children bboxes; text uses font metrics.
-}
bbox : List Font -> Node -> BoundingBox
bbox fonts node_ =
    case node_.type_ of
        Rect { x, y, w, h } ->
            { x = x
            , y = y
            , w = w
            , h = h
            }

        RectFill { x, y, w, h } ->
            { x = x
            , y = y
            , w = w
            , h = h
            }

        XLine { x, y, len } ->
            { x = x
            , y = y
            , w = len
            , h = 1
            }

        YLine { x, y, len } ->
            { x = x
            , y = y
            , w = 1
            , h = len
            }

        Text r ->
            case List.Extra.getAt r.fontIndex fonts of
                Nothing ->
                    -- Font not found, will not be shown
                    { x = r.x
                    , y = r.y
                    , w = 0
                    , h = 0
                    }

                Just font ->
                    let
                        ( w, h ) =
                            textMultilineSize font r.text
                    in
                    { x = r.x
                    , y = r.y
                    , w = w
                    , h = h
                    }

        Bitmap r ->
            { x = r.x
            , y = r.y
            , w = max 0 r.w
            , h = max 0 r.h
            }

        Group r ->
            List.foldl
                (\c acc -> BoundingBox.union c.bbox acc)
                BoundingBox.empty
                r.children


hash : Node -> Int
hash node_ =
    let
        seed : Int
        seed =
            FNV1a.initialSeed
                |> FNV1a.updateInt8 (typeTag node_.type_)
                |> FNV1a.updateString node_.key
    in
    case node_.type_ of
        Rect { x, y, w, h, color } ->
            seed
                |> FNV1a.updateInt16 x
                |> FNV1a.updateInt16 y
                |> FNV1a.updateInt16 w
                |> FNV1a.updateInt16 h
                |> FNV1a.updateInt8 color

        RectFill { x, y, w, h, color } ->
            seed
                |> FNV1a.updateInt16 x
                |> FNV1a.updateInt16 y
                |> FNV1a.updateInt16 w
                |> FNV1a.updateInt16 h
                |> FNV1a.updateInt8 color

        XLine { x, y, len, color } ->
            seed
                |> FNV1a.updateInt16 x
                |> FNV1a.updateInt16 y
                |> FNV1a.updateInt16 len
                |> FNV1a.updateInt8 color

        YLine { x, y, len, color } ->
            seed
                |> FNV1a.updateInt16 x
                |> FNV1a.updateInt16 y
                |> FNV1a.updateInt16 len
                |> FNV1a.updateInt8 color

        Text r ->
            seed
                |> FNV1a.updateInt16 r.x
                |> FNV1a.updateInt16 r.y
                |> FNV1a.updateInt8 r.color
                |> FNV1a.updateInt8 r.fontIndex
                |> FNV1a.updateString r.text

        Bitmap r ->
            seed
                |> FNV1a.updateInt16 r.x
                |> FNV1a.updateInt16 r.y
                |> FNV1a.updateInt16 r.w
                |> FNV1a.updateInt16 r.h
                |> FNV1a.updateInt8 (Bitmap.bitDepthToInt r.bitDepth)
                |> FNV1a.updateBytes r.data

        Group r ->
            List.foldl (\c acc -> FNV1a.updateInt32 (hash c) acc) seed r.children


withBbox : List Font -> Node -> Node
withBbox fonts node_ =
    { node_ | bbox = bbox fonts node_ }


withHash : Node -> Node
withHash node_ =
    { node_ | hash = hash node_ }


{-| Build a node from a key and type (e.g. when updating only type in an editor).
-}
fromKeyAndType : List Font -> String -> Type -> Node
fromKeyAndType fonts key type_ =
    case type_ of
        Rect r ->
            rect key r

        RectFill r ->
            rectFill key r

        XLine r ->
            xLine key r

        YLine r ->
            yLine key r

        Text r ->
            text fonts key r

        Bitmap r ->
            bitmap key r

        Group { children } ->
            group key children


jsonEncoder : Node -> Json.Encode.Value
jsonEncoder node_ =
    let
        base tag extra =
            Json.Encode.object <|
                List.concat
                    [ [ ( "type", Json.Encode.string tag )
                      , ( "key", Json.Encode.string node_.key )
                      ]
                    , extra
                    ]
    in
    case node_.type_ of
        Rect r ->
            base "Rect"
                [ ( "x", Json.Encode.int r.x )
                , ( "y", Json.Encode.int r.y )
                , ( "w", Json.Encode.int r.w )
                , ( "h", Json.Encode.int r.h )
                , ( "color", Json.Encode.int r.color )
                ]

        RectFill r ->
            base "RectFill"
                [ ( "x", Json.Encode.int r.x )
                , ( "y", Json.Encode.int r.y )
                , ( "w", Json.Encode.int r.w )
                , ( "h", Json.Encode.int r.h )
                , ( "color", Json.Encode.int r.color )
                ]

        XLine r ->
            base "XLine"
                [ ( "x", Json.Encode.int r.x )
                , ( "y", Json.Encode.int r.y )
                , ( "len", Json.Encode.int r.len )
                , ( "color", Json.Encode.int r.color )
                ]

        YLine r ->
            base "YLine"
                [ ( "x", Json.Encode.int r.x )
                , ( "y", Json.Encode.int r.y )
                , ( "len", Json.Encode.int r.len )
                , ( "color", Json.Encode.int r.color )
                ]

        Text r ->
            base "Text"
                [ ( "x", Json.Encode.int r.x )
                , ( "y", Json.Encode.int r.y )
                , ( "text", Json.Encode.string r.text )
                , ( "fontIndex", Json.Encode.int r.fontIndex )
                , ( "color", Json.Encode.int r.color )
                ]

        Bitmap r ->
            base "Bitmap"
                [ ( "x", Json.Encode.int r.x )
                , ( "y", Json.Encode.int r.y )
                , ( "w", Json.Encode.int r.w )
                , ( "h", Json.Encode.int r.h )
                , ( "bitDepth", Json.Encode.int (Bitmap.bitDepthToInt r.bitDepth) )
                , ( "data", Json.Encode.list Json.Encode.int r.data )
                ]

        Group r ->
            base "Group"
                [ ( "children", Json.Encode.list jsonEncoder r.children )
                ]


jsonDecoder : ESP32 -> Decoder Node
jsonDecoder esp32 =
    let
        fontCount =
            List.length esp32.fonts
    in
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                case tag of
                    "Rect" ->
                        Json.Decode.succeed
                            (\key x y w h color -> rect key { x = x, y = y, w = w, h = h, color = color })
                            |> Json.Decode.Extra.andMap (Json.Decode.field "key" Json.Decode.string)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "x" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "y" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "w" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "h" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "color" Json.Decode.int)

                    "RectFill" ->
                        Json.Decode.succeed
                            (\key x y w h color -> rectFill key { x = x, y = y, w = w, h = h, color = color })
                            |> Json.Decode.Extra.andMap (Json.Decode.field "key" Json.Decode.string)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "x" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "y" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "w" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "h" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "color" Json.Decode.int)

                    "XLine" ->
                        Json.Decode.succeed
                            (\key x y len color -> xLine key { x = x, y = y, len = len, color = color })
                            |> Json.Decode.Extra.andMap (Json.Decode.field "key" Json.Decode.string)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "x" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "y" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "len" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "color" Json.Decode.int)

                    "YLine" ->
                        Json.Decode.succeed
                            (\key x y len color -> yLine key { x = x, y = y, len = len, color = color })
                            |> Json.Decode.Extra.andMap (Json.Decode.field "key" Json.Decode.string)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "x" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "y" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "len" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "color" Json.Decode.int)

                    "Text" ->
                        Json.Decode.succeed
                            (\key x y t fontIndex color -> text esp32.fonts key { x = x, y = y, text = t, fontIndex = fontIndex, color = color })
                            |> Json.Decode.Extra.andMap (Json.Decode.field "key" Json.Decode.string)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "x" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "y" Json.Decode.int)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "text" Json.Decode.string)
                            |> Json.Decode.Extra.andMap
                                (Json.Decode.field "fontIndex"
                                    (Json.Decode.int
                                        |> Json.Decode.andThen
                                            (\i ->
                                                if i >= fontCount then
                                                    Json.Decode.fail ("fontIndex " ++ String.fromInt i ++ " was >= the font count " ++ String.fromInt fontCount)

                                                else
                                                    Json.Decode.succeed i
                                            )
                                    )
                                )
                            |> Json.Decode.Extra.andMap (Json.Decode.field "color" Json.Decode.int)

                    "Bitmap" ->
                        Json.Decode.succeed (\key x y w h bitDepth data -> bitmap key { x = x, y = y, w = w, h = h, bitDepth = bitDepth, data = data })
                            |> Json.Decode.Extra.andMap (Json.Decode.field "key" Json.Decode.string)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "x" (nonnegativeIntDecoder "x"))
                            |> Json.Decode.Extra.andMap (Json.Decode.field "y" (nonnegativeIntDecoder "y"))
                            |> Json.Decode.Extra.andMap (Json.Decode.field "w" (nonnegativeIntDecoder "w"))
                            |> Json.Decode.Extra.andMap (Json.Decode.field "h" (nonnegativeIntDecoder "h"))
                            |> Json.Decode.Extra.andMap (Json.Decode.field "bitDepth" bitDepthDecoder)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "data" (Json.Decode.list Json.Decode.int))

                    "Group" ->
                        Json.Decode.succeed group
                            |> Json.Decode.Extra.andMap (Json.Decode.field "key" Json.Decode.string)
                            |> Json.Decode.Extra.andMap (Json.Decode.field "children" (Json.Decode.list (Json.Decode.lazy (\_ -> jsonDecoder esp32))))

                    _ ->
                        Json.Decode.fail ("Unknown node type: " ++ tag)
            )
        |> Json.Decode.andThen
            (\decoded ->
                case List.head (limitErrors esp32 decoded) of
                    Just err ->
                        Json.Decode.fail (decoderLimitErrorToString err)

                    Nothing ->
                        Json.Decode.succeed decoded
            )


limitErrors : ESP32 -> Node -> List LimitError
limitErrors esp32 node_ =
    let
        totalNodes =
            size node_

        totalNodeError =
            if totalNodes > esp32.maxTotalNodes then
                [ MaxTotalNodesExceeded
                    { maxTotalNodes = esp32.maxTotalNodes
                    , actualTotalNodes = totalNodes
                    }
                ]

            else
                []
    in
    totalNodeError ++ collectGroupChildrenErrors esp32 node_


collectGroupChildrenErrors : ESP32 -> Node -> List LimitError
collectGroupChildrenErrors esp32 node_ =
    case node_.type_ of
        Group { children } ->
            let
                hereError =
                    if List.length children > esp32.nodeGroupMaxChildren then
                        [ NodeGroupMaxChildrenExceeded
                            { groupKey = node_.key
                            , maxChildren = esp32.nodeGroupMaxChildren
                            , actualChildren = List.length children
                            }
                        ]

                    else
                        []
            in
            hereError ++ List.concatMap (collectGroupChildrenErrors esp32) children

        Rect _ ->
            []

        RectFill _ ->
            []

        XLine _ ->
            []

        YLine _ ->
            []

        Text _ ->
            []

        Bitmap _ ->
            []


decoderLimitErrorToString : LimitError -> String
decoderLimitErrorToString err =
    case err of
        MaxTotalNodesExceeded { maxTotalNodes, actualTotalNodes } ->
            "Total node limit exceeded: max "
                ++ String.fromInt maxTotalNodes
                ++ ", got "
                ++ String.fromInt actualTotalNodes

        NodeGroupMaxChildrenExceeded { groupKey, maxChildren, actualChildren } ->
            "Group child limit exceeded for key \""
                ++ groupKey
                ++ "\": max "
                ++ String.fromInt maxChildren
                ++ ", got "
                ++ String.fromInt actualChildren


nonnegativeIntDecoder : String -> Decoder Int
nonnegativeIntDecoder fieldName =
    Json.Decode.int
        |> Json.Decode.andThen
            (\n ->
                if n < 0 then
                    Json.Decode.fail (fieldName ++ " must be nonnegative")

                else
                    Json.Decode.succeed n
            )


bitDepthDecoder : Decoder BitDepth
bitDepthDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\i ->
                case Bitmap.bitDepthFromInt i of
                    Just d ->
                        Json.Decode.succeed d

                    Nothing ->
                        Json.Decode.fail ("Unsupported bitmap bitDepth: " ++ String.fromInt i)
            )


bytesEncoder : Node -> Bytes.Encode.Encoder
bytesEncoder node_ =
    Bytes.Encode.sequence <|
        (Bytes.Encode.unsignedInt8 (typeTag node_.type_)
            :: (case node_.type_ of
                    Rect { x, y, w, h, color } ->
                        [ Bytes.Encode.signedInt32 LE x
                        , Bytes.Encode.signedInt32 LE y
                        , Bytes.Encode.signedInt32 LE w
                        , Bytes.Encode.signedInt32 LE h
                        , Bytes.Encode.unsignedInt8 color
                        ]

                    RectFill { x, y, w, h, color } ->
                        [ Bytes.Encode.signedInt32 LE x
                        , Bytes.Encode.signedInt32 LE y
                        , Bytes.Encode.signedInt32 LE w
                        , Bytes.Encode.signedInt32 LE h
                        , Bytes.Encode.unsignedInt8 color
                        ]

                    XLine { x, y, len, color } ->
                        [ Bytes.Encode.signedInt32 LE x
                        , Bytes.Encode.signedInt32 LE y
                        , Bytes.Encode.signedInt32 LE len
                        , Bytes.Encode.unsignedInt8 color
                        ]

                    YLine { x, y, len, color } ->
                        [ Bytes.Encode.signedInt32 LE x
                        , Bytes.Encode.signedInt32 LE y
                        , Bytes.Encode.signedInt32 LE len
                        , Bytes.Encode.unsignedInt8 color
                        ]

                    Text r ->
                        [ Bytes.Encode.signedInt32 LE r.x
                        , Bytes.Encode.signedInt32 LE r.y
                        , Bytes.Encode.signedInt32 LE r.fontIndex
                        , Bytes.Encode.unsignedInt8 r.color
                        , BytesExtraExtra.compressedEncoder (Bytes.Encode.string r.text)
                        ]

                    Bitmap r ->
                        [ Bytes.Encode.signedInt32 LE r.x
                        , Bytes.Encode.signedInt32 LE r.y
                        , Bytes.Encode.signedInt32 LE r.w
                        , Bytes.Encode.signedInt32 LE r.h
                        , Bytes.Encode.unsignedInt8 (Bitmap.bitDepthToInt r.bitDepth)
                        , BytesExtraExtra.compressedEncoder
                            (Bytes.Encode.sequence
                                (List.map Bytes.Encode.unsignedInt8 r.data)
                            )
                        ]

                    Group r ->
                        [ BytesExtraExtra.sizedListEncoder bytesEncoder r.children
                        ]
               )
        )


size : Node -> Int
size node_ =
    case node_.type_ of
        Group { children } ->
            1 + List.sum (List.map size children)

        _ ->
            1


maxGroupChildren : Node -> Int
maxGroupChildren node_ =
    case node_.type_ of
        Group { children } ->
            max (List.length children)
                (Maybe.withDefault 0
                    (List.maximum
                        (List.map maxGroupChildren children)
                    )
                )

        _ ->
            0


maxFontIndex : Node -> Int
maxFontIndex node_ =
    case node_.type_ of
        Text r ->
            r.fontIndex

        Group { children } ->
            Maybe.withDefault 0
                (List.maximum
                    (List.map maxFontIndex children)
                )

        _ ->
            0

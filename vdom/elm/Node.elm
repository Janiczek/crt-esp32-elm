module Node exposing
    ( LimitError(..)
    , Node
    , Type(..)
    , allKeys
    , bitmap
    , bytesEncoder
    , centerPosition
    , displayLabel
    , empty
    , fromKeyAndType
    , group
    , hitPathsAtPixel
    , jsonDecoder
    , jsonEncoder
    , limitErrors
    , maxFontIndex
    , maxGroupChildren
    , rect
    , rectFill
    , size
    , text
    , topLeftXY
    , typeWithNewXY
    , uniqueKeyAmong
    , validateLimits
    , xLine
    , yLine
    )

import Bitmap exposing (BitDepth)
import Bitwise
import BoundingBox exposing (BoundingBox, contains)
import Bytes exposing (Endianness(..))
import Bytes.Encode
import BytesExtraExtra
import Color exposing (Color)
import ESP32 exposing (ESP32, VideoConstants)
import FNV1a
import Font exposing (Font)
import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Encode
import List.Extra
import Set exposing (Set)


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
        centerBySize : Int -> Int -> ( Int, Int )
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


{-| Every `key` on this node and all descendants.
-}
allKeys : Node -> Set String
allKeys node_ =
    case node_.type_ of
        Rect _ ->
            Set.singleton node_.key

        RectFill _ ->
            Set.singleton node_.key

        XLine _ ->
            Set.singleton node_.key

        YLine _ ->
            Set.singleton node_.key

        Text _ ->
            Set.singleton node_.key

        Bitmap _ ->
            Set.singleton node_.key

        Group { children } ->
            List.foldl (\c acc -> Set.union (allKeys c) acc) (Set.singleton node_.key) children


{-| If `base` is already in `used`, return `base ++ "-2"`, `base ++ "-3"`, … until unused.
-}
uniqueKeyAmong : Set String -> String -> String
uniqueKeyAmong used base =
    if Set.member base used then
        uniqueKeyAmongSuffix used base 2

    else
        base


uniqueKeyAmongSuffix : Set String -> String -> Int -> String
uniqueKeyAmongSuffix used base n =
    let
        candidate : String
        candidate =
            base ++ "-" ++ String.fromInt n
    in
    if Set.member candidate used then
        uniqueKeyAmongSuffix used base (n + 1)

    else
        candidate


{-| The `(x, y)` stored on each leaf `Type` (same fields `typeWithNewXY` edits).

We use this instead of `Node.bbox` because bbox is derived (union of children, text metrics, etc.)
and is not what the editor moves when changing position — that is always the type’s own `x`/`y`.
`Group` has no such pair on `Type`, only children, so this returns `Nothing`.

-}
topLeftXY : Type -> Maybe ( Int, Int )
topLeftXY type_ =
    case type_ of
        Rect r ->
            Just ( r.x, r.y )

        RectFill r ->
            Just ( r.x, r.y )

        XLine r ->
            Just ( r.x, r.y )

        YLine r ->
            Just ( r.x, r.y )

        Text r ->
            Just ( r.x, r.y )

        Bitmap r ->
            Just ( r.x, r.y )

        Group _ ->
            Nothing


{-| Leaf types get new `x`/`y`; `Group` is returned unchanged.
-}
typeWithNewXY : Int -> Int -> Type -> Type
typeWithNewXY x y type_ =
    case type_ of
        Rect r ->
            Rect { r | x = x, y = y }

        RectFill r ->
            RectFill { r | x = x, y = y }

        XLine r ->
            XLine { r | x = x, y = y }

        YLine r ->
            YLine { r | x = x, y = y }

        Text r ->
            Text { r | x = x, y = y }

        Bitmap r ->
            Bitmap { r | x = x, y = y }

        Group _ ->
            type_


collectHitPaths : Int -> Int -> List Int -> Node -> List (List Int)
collectHitPaths px py path node_ =
    if not (contains px py node_.bbox) then
        []

    else
        case node_.type_ of
            Group { children } ->
                children
                    |> List.indexedMap
                        (\i child ->
                            collectHitPaths px py (path ++ [ i ]) child
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


hitPathsAtPixel : Int -> Int -> Node -> List (List Int)
hitPathsAtPixel px py root =
    collectHitPaths px py [] root


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
        expectedLen : Int
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
        normalized : List Int
        normalized =
            data
                |> List.take expectedLen
                |> List.map (Bitwise.and 0xFF)
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
        base : String -> List ( String, Json.Encode.Value ) -> Json.Encode.Value
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
        fontCount : Int
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
                case validateLimits esp32 decoded of
                    Ok okNode ->
                        Json.Decode.succeed okNode

                    Err errs ->
                        Json.Decode.fail
                            (List.head errs
                                |> Maybe.map decoderLimitErrorToString
                                |> Maybe.withDefault "Node exceeds ESP32 limits"
                            )
            )


{-| `Ok node` when `limitErrors` is empty; otherwise `Err` lists all limit violations.
-}
validateLimits : ESP32 -> Node -> Result (List LimitError) Node
validateLimits esp32 node_ =
    case limitErrors esp32 node_ of
        [] ->
            Ok node_

        errs ->
            Err errs


limitErrors : ESP32 -> Node -> List LimitError
limitErrors esp32 node_ =
    let
        totalNodes : Int
        totalNodes =
            size node_

        totalNodeError : List LimitError
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
                hereError : List LimitError
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

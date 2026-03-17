module Node exposing (Node, Type(..), encoder, group, rect, rectFill, text, xLine, yLine, empty)

import BoundingBox exposing (BoundingBox)
import Bytes exposing (Endianness(..))
import Bytes.Encode
import BytesExtraExtra
import Color exposing (Color)
import FNV1a
import Font exposing (Font)
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
    | Group { children : List Node }


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

        Group _ ->
            5


text : List Font -> String -> { x : Int, y : Int, text : String, fontIndex : Int, color : Color } -> Node
text =
    node Text


rect : String -> { x : Int, y : Int, w : Int, h : Int, color : Color } -> Node
rect =
    node Rect []


rectFill : String -> { x : Int, y : Int, w : Int, h : Int, color : Color } -> Node
rectFill =
    node Rect []


xLine : String -> { x : Int, y : Int, len : Int, color : Color } -> Node
xLine =
    node XLine []


yLine : String -> { x : Int, y : Int, len : Int, color : Color } -> Node
yLine =
    node YLine []


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

        Group r ->
            List.foldl (\c acc -> FNV1a.updateInt32 (hash c) acc) seed r.children


withBbox : List Font -> Node -> Node
withBbox fonts node_ =
    { node_ | bbox = bbox fonts node_ }


withHash : Node -> Node
withHash node_ =
    { node_ | hash = hash node_ }


encoder : Node -> Bytes.Encode.Encoder
encoder node_ =
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
                        , BytesExtraExtra.sizedStringEncoder r.text
                        ]

                    Group r ->
                        [ BytesExtraExtra.sizedListEncoder encoder r.children
                        ]
               )
        )

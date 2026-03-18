module Node exposing (Node, Type(..), empty, encoder, fromKeyAndType, group, jsonDecoder, jsonEncoder, rect, rectFill, text, xLine, yLine)

import BoundingBox exposing (BoundingBox)
import Bytes exposing (Endianness(..))
import Bytes.Encode
import BytesExtraExtra
import Color exposing (Color)
import FNV1a
import Flate
import Font exposing (Font)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
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
    node RectFill []


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

        Group { children } ->
            group key children


jsonEncoder : Node -> Encode.Value
jsonEncoder node_ =
    let
        base tag extra =
            Encode.object
                (( "type", Encode.string tag )
                    :: ( "key", Encode.string node_.key )
                    :: extra
                )
    in
    case node_.type_ of
        Rect r ->
            base "Rect"
                [ ( "x", Encode.int r.x )
                , ( "y", Encode.int r.y )
                , ( "w", Encode.int r.w )
                , ( "h", Encode.int r.h )
                , ( "color", Encode.int r.color )
                ]

        RectFill r ->
            base "RectFill"
                [ ( "x", Encode.int r.x )
                , ( "y", Encode.int r.y )
                , ( "w", Encode.int r.w )
                , ( "h", Encode.int r.h )
                , ( "color", Encode.int r.color )
                ]

        XLine r ->
            base "XLine"
                [ ( "x", Encode.int r.x )
                , ( "y", Encode.int r.y )
                , ( "len", Encode.int r.len )
                , ( "color", Encode.int r.color )
                ]

        YLine r ->
            base "YLine"
                [ ( "x", Encode.int r.x )
                , ( "y", Encode.int r.y )
                , ( "len", Encode.int r.len )
                , ( "color", Encode.int r.color )
                ]

        Text r ->
            base "Text"
                [ ( "x", Encode.int r.x )
                , ( "y", Encode.int r.y )
                , ( "text", Encode.string r.text )
                , ( "fontIndex", Encode.int r.fontIndex )
                , ( "color", Encode.int r.color )
                ]

        Group r ->
            base "Group"
                [ ( "children", Encode.list jsonEncoder r.children )
                ]


jsonDecoder : List Font -> Decoder Node
jsonDecoder fonts =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Rect" ->
                        Decode.map2 (\key r -> rect key r)
                            (Decode.field "key" Decode.string)
                            (Decode.map5 (\x y w h color -> { x = x, y = y, w = w, h = h, color = color })
                                (Decode.field "x" Decode.int)
                                (Decode.field "y" Decode.int)
                                (Decode.field "w" Decode.int)
                                (Decode.field "h" Decode.int)
                                (Decode.field "color" Decode.int)
                            )

                    "RectFill" ->
                        Decode.map2 (\key r -> rectFill key r)
                            (Decode.field "key" Decode.string)
                            (Decode.map5 (\x y w h color -> { x = x, y = y, w = w, h = h, color = color })
                                (Decode.field "x" Decode.int)
                                (Decode.field "y" Decode.int)
                                (Decode.field "w" Decode.int)
                                (Decode.field "h" Decode.int)
                                (Decode.field "color" Decode.int)
                            )

                    "XLine" ->
                        Decode.map2 (\key r -> xLine key r)
                            (Decode.field "key" Decode.string)
                            (Decode.map4 (\x y len color -> { x = x, y = y, len = len, color = color })
                                (Decode.field "x" Decode.int)
                                (Decode.field "y" Decode.int)
                                (Decode.field "len" Decode.int)
                                (Decode.field "color" Decode.int)
                            )

                    "YLine" ->
                        Decode.map2 (\key r -> yLine key r)
                            (Decode.field "key" Decode.string)
                            (Decode.map4 (\x y len color -> { x = x, y = y, len = len, color = color })
                                (Decode.field "x" Decode.int)
                                (Decode.field "y" Decode.int)
                                (Decode.field "len" Decode.int)
                                (Decode.field "color" Decode.int)
                            )

                    "Text" ->
                        Decode.map2 (\key r -> text fonts key r)
                            (Decode.field "key" Decode.string)
                            (Decode.map5 (\x y t fontIndex color -> { x = x, y = y, text = t, fontIndex = fontIndex, color = color })
                                (Decode.field "x" Decode.int)
                                (Decode.field "y" Decode.int)
                                (Decode.field "text" Decode.string)
                                (Decode.field "fontIndex" Decode.int)
                                (Decode.field "color" Decode.int)
                            )

                    "Group" ->
                        Decode.map2 (\key children -> group key children)
                            (Decode.field "key" Decode.string)
                            (Decode.field "children" (Decode.list (Decode.lazy (\_ -> jsonDecoder fonts))))

                    _ ->
                        Decode.fail ("Unknown node type: " ++ tag)
            )


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
                        let
                            raw =
                                Bytes.Encode.encode (Bytes.Encode.string r.text)

                            decompressedLen =
                                Bytes.width raw

                            textPayload =
                                if decompressedLen == 0 then
                                    [ Bytes.Encode.unsignedInt16 LE 0
                                    , Bytes.Encode.unsignedInt16 LE 0
                                    ]

                                else
                                    let
                                        compressed =
                                            Flate.deflateZlib raw

                                        compressedLen =
                                            Bytes.width compressed
                                    in
                                    [ Bytes.Encode.unsignedInt16 LE decompressedLen
                                    , Bytes.Encode.unsignedInt16 LE compressedLen
                                    , Bytes.Encode.bytes compressed
                                    ]
                        in
                        [ Bytes.Encode.signedInt32 LE r.x
                        , Bytes.Encode.signedInt32 LE r.y
                        , Bytes.Encode.signedInt32 LE r.fontIndex
                        , Bytes.Encode.unsignedInt8 r.color
                        ]
                            ++ textPayload

                    Group r ->
                        [ BytesExtraExtra.sizedListEncoder encoder r.children
                        ]
               )
        )

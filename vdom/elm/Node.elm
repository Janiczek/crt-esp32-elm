module Node exposing (Node, Type(..), encoder, group)

import Bytes exposing (Endianness(..))
import Bytes.Encode
import BytesExtraExtra
import Color exposing (Color)


type alias Node =
    { key : Int
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


group : Int -> List Type -> Node
group key children =
    Node key <| Group { children = List.indexedMap Node children }


encoder : Node -> Bytes.Encode.Encoder
encoder node =
    [ [ Bytes.Encode.unsignedInt8 (typeTag node.type_)
      , Bytes.Encode.unsignedInt8 node.key
      ]
    , case node.type_ of
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

        Text { x, y, text, fontIndex, color } ->
            [ Bytes.Encode.signedInt32 LE x
            , Bytes.Encode.signedInt32 LE y
            , Bytes.Encode.signedInt32 LE fontIndex
            , Bytes.Encode.unsignedInt8 color
            , BytesExtraExtra.sizedStringEncoder text
            ]

        Group { children } ->
            [ BytesExtraExtra.sizedListEncoder encoder children
            ]
    ]
        |> List.concat
        |> Bytes.Encode.sequence

port module Main exposing (Flags, Model, Msg, main)

{-| A web app to connect to and control an ESP32 via Web Serial.

The ESP32 is displaying VDOM scenes on a CRT display connected via the GPIO25
DAC pin (NTSC greyscale 400x240 signal).

The point of the web app is to edit the state of the ESP32 on the fly, instead
of changing the C source code and recompiling+reflashing. Also it's pretty cool.

TODO:

  - [ ] edit font data
  - [ ] scene editor

-}

import Bitwise
import Browser exposing (Document)
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Color
import Command
import Dirty
import ESP32 exposing (ESP32, VideoConstants, videoConstants)
import Font exposing (Font)
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import Html.Events
import List.Cartesian
import Node exposing (Node)
import Svg exposing (Svg)
import Svg.Attributes


type alias Flags =
    ()


type Model
    = NotConnected ModelNotConnected
    | Connected ModelConnected


type alias ModelNotConnected =
    { lastError : String
    }


type alias ModelConnected =
    { esp32 : ESP32
    , videoConstants : VideoConstants
    , lastError : String
    , textarea : String
    , fontIndex : Int
    , rootNode : Node
    }


type Msg
    = ConnectRequested
    | ConnectSuccessful ESP32
    | DisconnectRequested
    | DisconnectSuccessful
    | FailureOccurred String
    | MsgConnected MsgConnected


type MsgConnected
    = SetTextarea String
    | SetFontIndex Int


port connect : () -> Cmd msg


port disconnect : () -> Cmd msg


port onConnectSuccessful : (Bytes -> msg) -> Sub msg


port onDisconnectSuccessful : (() -> msg) -> Sub msg


port onFailure : (String -> msg) -> Sub msg


port sendCommand : ( Bytes, Bool ) -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( NotConnected { lastError = "" }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectRequested ->
            ( model, connect () )

        ConnectSuccessful esp32 ->
            let
                videoConstants_ =
                    videoConstants esp32

                textarea =
                    "Ready..."

                fontIndex_ =
                    if List.length esp32.fonts > 1 then
                        1

                    else
                        0

                node =
                    textScene esp32 videoConstants_ textarea fontIndex_
            in
            ( Connected
                { esp32 = esp32
                , videoConstants = videoConstants_
                , lastError = ""
                , textarea = textarea
                , fontIndex = fontIndex_
                , rootNode = node
                }
            , setRootNode esp32 videoConstants_ Node.empty node
                |> Cmd.map MsgConnected
            )

        DisconnectRequested ->
            ( model, disconnect () )

        DisconnectSuccessful ->
            ( NotConnected { lastError = "" }
            , Cmd.none
            )

        FailureOccurred error ->
            case model of
                NotConnected nc ->
                    ( NotConnected { nc | lastError = error }, Cmd.none )

                Connected c ->
                    ( Connected { c | lastError = error }, Cmd.none )

        MsgConnected msgConnected ->
            case model of
                NotConnected _ ->
                    ( model, Cmd.none )

                Connected modelConnected ->
                    let
                        ( newModelConnected, cmdMsgConnected ) =
                            updateConnected msgConnected modelConnected
                    in
                    ( Connected newModelConnected
                    , Cmd.map MsgConnected cmdMsgConnected
                    )


updateConnected : MsgConnected -> ModelConnected -> ( ModelConnected, Cmd MsgConnected )
updateConnected msgConnected modelConnected =
    case msgConnected of
        SetTextarea text ->
            let
                node =
                    textScene modelConnected.esp32 modelConnected.videoConstants text modelConnected.fontIndex
            in
            ( { modelConnected | textarea = text, rootNode = node }
            , setRootNode modelConnected.esp32 modelConnected.videoConstants modelConnected.rootNode node
            )

        SetFontIndex idx ->
            let
                maxIdx =
                    List.length modelConnected.esp32.fonts - 1

                fontIndex_ =
                    clamp 0 maxIdx idx

                node =
                    textScene modelConnected.esp32 modelConnected.videoConstants modelConnected.textarea fontIndex_
            in
            ( { modelConnected | fontIndex = fontIndex_, rootNode = node }
            , setRootNode modelConnected.esp32 modelConnected.videoConstants modelConnected.rootNode node
            )


setRootNode : ESP32 -> VideoConstants -> Node -> Node -> Cmd MsgConnected
setRootNode esp32 videoConstants previousNode node =
    if node.hash == previousNode.hash then
        Cmd.none

    else
        let
            command =
                Command.SetRootNode node
                    (Dirty.diff
                        { tileSize = esp32.tileSize
                        , tileCols = videoConstants.tileCols
                        , tileRows = videoConstants.tileRows
                        }
                        esp32.fonts
                        previousNode
                        node
                    )

            bytes =
                Command.encoder command |> Bytes.Encode.encode
        in
        sendCommand ( bytes, Command.needsAck command )


view : Model -> Document Msg
view model =
    { title = "ESP32 Command Centre"
    , body = [ view_ model ]
    }


view_ : Model -> Html Msg
view_ model =
    case model of
        NotConnected modelNotConnected ->
            viewNotConnected modelNotConnected

        Connected modelConnected ->
            viewConnected modelConnected


viewNotConnected : ModelNotConnected -> Html Msg
viewNotConnected model =
    Html.div []
        [ Html.div [] [ Html.text "Not connected." ]
        , viewLastError model.lastError
        , Html.button
            [ Html.Events.onClick ConnectRequested ]
            [ Html.text "Connect" ]
        ]


viewFontSelect : ModelConnected -> Html Msg
viewFontSelect model =
    Html.div [ Html.Attributes.style "margin-top" "0.5rem" ]
        [ Html.label
            [ Html.Attributes.style "margin-right" "0.5rem" ]
            [ Html.text "Font" ]
        , Html.select
            [ Html.Events.onInput
                (\s ->
                    MsgConnected (SetFontIndex (Maybe.withDefault 0 (String.toInt s)))
                )
            ]
            (model.esp32.fonts
                |> List.indexedMap
                    (\i font ->
                        Html.option
                            [ Html.Attributes.value (String.fromInt i)
                            , Html.Attributes.Extra.attributeIf (i == model.fontIndex) (Html.Attributes.attribute "selected" "")
                            ]
                            [ Html.text (String.fromInt i ++ ": " ++ font.name) ]
                    )
            )
        ]


viewConnected : ModelConnected -> Html Msg
viewConnected model =
    Html.div []
        [ Html.div [] [ Html.text "Connected." ]
        , viewLastError model.lastError
        , Html.button
            [ Html.Events.onClick DisconnectRequested ]
            [ Html.text "Disconnect" ]
        , Html.textarea
            [ Html.Attributes.cols 80
            , Html.Attributes.rows 25
            , Html.Events.onInput (MsgConnected << SetTextarea)
            ]
            [ Html.text model.textarea ]
        , viewFontSelect model
        , viewDeviceInfo model.esp32 model.videoConstants
        ]


viewDeviceInfo : ESP32 -> VideoConstants -> Html msg
viewDeviceInfo esp32 vc =
    let
        tableStyle : List (Html.Attribute msg)
        tableStyle =
            [ Html.Attributes.style "font-family" "ui-monospace, monospace"
            , Html.Attributes.style "font-size" "0.875rem"
            , Html.Attributes.style "border-collapse" "collapse"
            ]

        thStyle : List (Html.Attribute msg)
        thStyle =
            [ Html.Attributes.style "text-align" "left"
            , Html.Attributes.style "padding" "0.25rem 0.5rem 0.25rem 0"
            , Html.Attributes.style "color" "#666"
            ]

        tdStyle : List (Html.Attribute msg)
        tdStyle =
            [ Html.Attributes.style "padding" "0.25rem 0.5rem" ]

        tableRow : String -> Int -> Html msg
        tableRow name value =
            Html.tr []
                [ Html.th
                    (thStyle
                        ++ [ Html.Attributes.style "font-weight" "500" ]
                    )
                    [ Html.text name ]
                , Html.td tdStyle [ Html.text (String.fromInt value) ]
                ]

        esp32Table : Html msg
        esp32Table =
            Html.table
                (Html.Attributes.style "margin-top" "0.5rem"
                    :: tableStyle
                )
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th
                            (thStyle
                                ++ [ Html.Attributes.style "font-weight" "600" ]
                            )
                            [ Html.text "ESP32" ]
                        , Html.th tdStyle []
                        ]
                    ]
                , Html.tbody []
                    [ tableRow "videoWidth" esp32.videoWidth
                    , tableRow "videoHeight" esp32.videoHeight
                    , tableRow "crtPaddingLeft" esp32.crtPaddingLeft
                    , tableRow "crtPaddingRight" esp32.crtPaddingRight
                    , tableRow "crtPaddingTop" esp32.crtPaddingTop
                    , tableRow "crtPaddingBottom" esp32.crtPaddingBottom
                    , tableRow "maxTotalNodes" esp32.maxTotalNodes
                    , tableRow "nodeGroupMaxChildren" esp32.nodeGroupMaxChildren
                    , Html.tr []
                        [ Html.th
                            (thStyle
                                ++ [ Html.Attributes.style "font-weight" "500" ]
                            )
                            [ Html.text "fonts" ]
                        , Html.td tdStyle
                            [ Html.text (String.fromInt (List.length esp32.fonts)) ]
                        ]
                    ]
                ]

        vcTable : Html msg
        vcTable =
            Html.table
                (Html.Attributes.style "margin-top" "0.75rem"
                    :: tableStyle
                )
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th
                            (thStyle
                                ++ [ Html.Attributes.style "font-weight" "600" ]
                            )
                            [ Html.text "Video constants" ]
                        , Html.th tdStyle []
                        ]
                    ]
                , Html.tbody []
                    [ tableRow "xMin" vc.xMin
                    , tableRow "xMax" vc.xMax
                    , tableRow "yMin" vc.yMin
                    , tableRow "yMax" vc.yMax
                    , tableRow "usableWidth" vc.usableWidth
                    , tableRow "usableHeight" vc.usableHeight
                    , tableRow "xCenter" vc.xCenter
                    , tableRow "yCenter" vc.yCenter
                    ]
                ]

        fontsSection : Html msg
        fontsSection =
            Html.div [ Html.Attributes.style "margin-top" "1rem" ]
                [ Html.div
                    [ Html.Attributes.style "font-weight" "600"
                    , Html.Attributes.style "font-size" "0.875rem"
                    , Html.Attributes.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.text "Fonts" ]
                , Html.table (tableStyle ++ [ Html.Attributes.style "width" "100%" ])
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th thStyle [ Html.text "Name" ]
                            , Html.th thStyle [ Html.text "ASCII" ]
                            , Html.th thStyle [ Html.text "Glyphs" ]
                            , Html.th thStyle [ Html.text "Size" ]
                            , Html.th thStyle [ Html.text "Extra LH" ]
                            , Html.th thStyle [ Html.text "Bitmap" ]
                            ]
                        ]
                    , Html.tbody []
                        (List.map (viewFontRow tdStyle) esp32.fonts)
                    ]
                ]
    in
    Html.div
        [ Html.Attributes.style "border" "1px solid #ddd"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "padding" "0.75rem 1rem"
        , Html.Attributes.style "max-width" "100%"
        ]
        [ esp32Table, vcTable, fontsSection ]


viewFontRow :
    List (Html.Attribute msg)
    -> Font
    -> Html msg
viewFontRow tdStyle font =
    Html.tr []
        [ Html.td tdStyle [ Html.text font.name ]
        , Html.td tdStyle
            [ Html.text
                (String.fromInt font.asciiFirst
                    ++ "–"
                    ++ String.fromInt font.asciiLast
                )
            ]
        , Html.td tdStyle [ Html.text (String.fromInt font.numGlyphs) ]
        , Html.td tdStyle
            [ Html.text
                (String.fromInt font.glyphWidth
                    ++ "×"
                    ++ String.fromInt font.glyphHeight
                )
            ]
        , Html.td tdStyle [ Html.text (String.fromInt font.extraLineHeight) ]
        , Html.td tdStyle [ viewFontBitmap font ]
        ]


viewFontBitmap : Font -> Html msg
viewFontBitmap font =
    let
        scale : Int
        scale =
            3

        glyphsPerRow : Int
        glyphsPerRow =
            16

        rows : Int
        rows =
            (font.numGlyphs + glyphsPerRow - 1) // glyphsPerRow

        totalWidth : Int
        totalWidth =
            glyphsPerRow * font.glyphWidth * scale

        totalHeight : Int
        totalHeight =
            rows * font.glyphHeight * scale

        glyphPixel : Int -> Int -> Int -> Bool
        glyphPixel g row col =
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

        rects : List (Svg msg)
        rects =
            List.Cartesian.map3
                (\g r c ->
                    if glyphPixel g r c then
                        Just
                            (Svg.rect
                                [ Svg.Attributes.x (String.fromInt (scale * (modBy glyphsPerRow g * font.glyphWidth + c)))
                                , Svg.Attributes.y (String.fromInt (scale * (g // glyphsPerRow * font.glyphHeight + r)))
                                , Svg.Attributes.width (String.fromInt scale)
                                , Svg.Attributes.height (String.fromInt scale)
                                , Svg.Attributes.fill "currentColor"
                                ]
                                []
                            )

                    else
                        Nothing
                )
                (List.range 0 (font.numGlyphs - 1))
                (List.range 0 (font.glyphHeight - 1))
                (List.range 0 (font.glyphWidth - 1))
                |> List.filterMap identity
    in
    Html.div
        [ Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "line-height" "0"
        ]
        [ Svg.svg
            [ Svg.Attributes.width (String.fromInt totalWidth)
            , Svg.Attributes.height (String.fromInt totalHeight)
            , Svg.Attributes.viewBox
                ("0 0 "
                    ++ String.fromInt totalWidth
                    ++ " "
                    ++ String.fromInt totalHeight
                )
            ]
            rects
        ]


viewLastError : String -> Html msg
viewLastError lastError =
    if lastError /= "" then
        Html.div
            [ Html.Attributes.style "color" "red" ]
            [ Html.text lastError ]

    else
        Html.text ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onFailure FailureOccurred
        , case model of
            NotConnected _ ->
                onConnectSuccessful_ ConnectSuccessful FailureOccurred

            Connected _ ->
                onDisconnectSuccessful (\() -> DisconnectSuccessful)
        ]


onConnectSuccessful_ : (ESP32 -> msg) -> (String -> msg) -> Sub msg
onConnectSuccessful_ onSuccess onFail =
    onConnectSuccessful
        (\bytes ->
            case Bytes.Decode.decode ESP32.decoder bytes of
                Just esp32 ->
                    onSuccess esp32

                Nothing ->
                    onFail "Failed to decode ESP32 data"
        )


textScene : ESP32 -> VideoConstants -> String -> Int -> Node
textScene esp32 c text fontIndex_ =
    Node.group "main"
        [ Node.text esp32.fonts
            "text-scene"
            { x = c.xMin + 4
            , y = c.yMin + 2
            , text = text
            , fontIndex = fontIndex_
            , color = Color.white
            }
        , Node.rectFill "cross bg"
            { x = c.xMax - 30
            , y = c.yMax - 30
            , w = 25
            , h = 25
            , color = Color.white
            }
        , Node.xLine "cross horiz"
            { x = c.xMax - 29
            , y = c.yMax - 18
            , len = 23
            , color = Color.gray
            }
        , Node.yLine "cross vert"
            { x = c.xMax - 18
            , y = c.yMax - 29
            , len = 23
            , color = Color.gray
            }
        , Node.rect "border shadow"
            { x = c.xMin - 1
            , y = c.yMin - 1
            , w = c.usableWidth + 2
            , h = c.usableHeight + 2
            , color = Color.gray
            }
        , Node.rect "border"
            { x = c.xMin
            , y = c.yMin
            , w = c.usableWidth
            , h = c.usableHeight
            , color = Color.white
            }
        ]

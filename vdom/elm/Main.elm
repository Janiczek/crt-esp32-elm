port module Main exposing (main)

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
import Command
import ESP32 exposing (ESP32, VideoConstants, videoConstants)
import Example
import Font exposing (Font)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Node exposing (Node)
import Svg
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
    }


type Msg
    = ConnectRequested
    | ConnectSuccessful ESP32
    | DisconnectRequested
    | DisconnectSuccessful
    | FailureOccurred String
    | MsgConnected MsgConnected


type MsgConnected
    = SetExampleRoot Node


port connect : () -> Cmd msg


port disconnect : () -> Cmd msg


port onConnectSuccessful : (Bytes -> msg) -> Sub msg


port onDisconnectSuccessful : (() -> msg) -> Sub msg


port onFailure : (String -> msg) -> Sub msg


port sendCommand : Bytes -> Cmd msg


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
            ( Connected
                { esp32 = esp32
                , videoConstants = videoConstants esp32
                , lastError = ""
                }
            , Cmd.none
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
        SetExampleRoot node ->
            ( modelConnected
            , Command.SetRootNode node
                |> Command.encoder
                |> Bytes.Encode.encode
                |> sendCommand
            )


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
        [ Html.text "Not connected."
        , Html.button
            [ Html.Events.onClick ConnectRequested ]
            [ Html.text "Connect" ]
        , viewLastError model.lastError
        ]


viewConnected : ModelConnected -> Html Msg
viewConnected model =
    Html.div [] <|
        List.concat
            [ [ Html.text "Connected."
              , Html.button
                    [ Html.Events.onClick DisconnectRequested ]
                    [ Html.text "Disconnect" ]
              , viewDeviceInfo model.esp32 model.videoConstants
              ]
            , Example.all
                |> List.map
                    (\( name, toNode ) ->
                        let
                            node =
                                toNode model.esp32 model.videoConstants
                        in
                        Html.button
                            [ Html.Events.onClick (MsgConnected (SetExampleRoot node)) ]
                            [ Html.text name ]
                    )
            , [ viewLastError model.lastError ]
            ]


viewDeviceInfo : ESP32 -> VideoConstants -> Html Msg
viewDeviceInfo esp32 vc =
    let
        tableStyle =
            [ Html.Attributes.style "font-family" "ui-monospace, monospace"
            , Html.Attributes.style "font-size" "0.875rem"
            , Html.Attributes.style "border-collapse" "collapse"
            ]

        thStyle =
            [ Html.Attributes.style "text-align" "left"
            , Html.Attributes.style "padding" "0.25rem 0.5rem 0.25rem 0"
            , Html.Attributes.style "color" "#666"
            ]

        tdStyle =
            [ Html.Attributes.style "padding" "0.25rem 0.5rem" ]

        tableRow name value =
            Html.tr []
                [ Html.th (thStyle ++ [ Html.Attributes.style "font-weight" "500" ]) [ Html.text name ]
                , Html.td tdStyle [ Html.text (String.fromInt value) ]
                ]

        esp32Table =
            Html.table (Html.Attributes.style "margin-top" "0.5rem" :: tableStyle)
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th (thStyle ++ [ Html.Attributes.style "font-weight" "600" ]) [ Html.text "ESP32" ]
                        , Html.th tdStyle [ Html.text "" ]
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
                        [ Html.th (thStyle ++ [ Html.Attributes.style "font-weight" "500" ]) [ Html.text "fonts" ]
                        , Html.td tdStyle [ Html.text (String.fromInt (List.length esp32.fonts)) ]
                        ]
                    ]
                ]

        vcTable =
            Html.table (Html.Attributes.style "margin-top" "0.75rem" :: tableStyle)
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th (thStyle ++ [ Html.Attributes.style "font-weight" "600" ]) [ Html.text "Video constants" ]
                        , Html.th tdStyle [ Html.text "" ]
                        ]
                    ]
                , Html.tbody []
                    [ tableRow "xMin" vc.xMin
                    , tableRow "xMax" vc.xMax
                    , tableRow "yMin" vc.yMin
                    , tableRow "yMax" vc.yMax
                    , tableRow "usableW" vc.usableW
                    , tableRow "usableH" vc.usableH
                    , tableRow "xCenter" vc.xCenter
                    , tableRow "yCenter" vc.yCenter
                    ]
                ]

        fontsSection =
            Html.div [ Html.Attributes.style "margin-top" "1rem" ]
                [ Html.div [ Html.Attributes.style "font-weight" "600", Html.Attributes.style "font-size" "0.875rem", Html.Attributes.style "margin-bottom" "0.5rem" ]
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
                        (List.map (viewFontRow thStyle tdStyle) esp32.fonts)
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
    List (Html.Attribute Msg)
    -> List (Html.Attribute Msg)
    -> Font
    -> Html Msg
viewFontRow thStyle tdStyle font =
    Html.tr []
        [ Html.td tdStyle [ Html.text font.name ]
        , Html.td tdStyle
            [ Html.text (String.fromInt font.asciiFirst ++ "–" ++ String.fromInt font.asciiLast) ]
        , Html.td tdStyle [ Html.text (String.fromInt font.numGlyphs) ]
        , Html.td tdStyle
            [ Html.text (String.fromInt font.glyphW ++ "×" ++ String.fromInt font.glyphH) ]
        , Html.td tdStyle [ Html.text (String.fromInt font.extraLineHeight) ]
        , Html.td tdStyle [ viewFontBitmap font ]
        ]


viewFontBitmap : Font -> Html Msg
viewFontBitmap font =
    let
        scale =
            3

        glyphsPerRow =
            16

        rows =
            (font.numGlyphs + glyphsPerRow - 1) // glyphsPerRow

        totalW =
            glyphsPerRow * font.glyphW * scale

        totalH =
            rows * font.glyphH * scale

        glyphPixel g row col =
            let
                byteIdx =
                    g * font.glyphH + row

                byte =
                    List.drop byteIdx font.bits |> List.head |> Maybe.withDefault 0

                bit =
                    Bitwise.and (Bitwise.shiftRightBy (7 - col) byte) 1
            in
            bit == 1

        rects =
            List.concatMap
                (\g ->
                    List.concatMap
                        (\r ->
                            List.filterMap
                                (\c ->
                                    if glyphPixel g r c then
                                        Just
                                            (Svg.rect
                                                [ Svg.Attributes.x (String.fromInt (scale * (modBy glyphsPerRow g * font.glyphW + c)))
                                                , Svg.Attributes.y (String.fromInt (scale * (g // glyphsPerRow * font.glyphH + r)))
                                                , Svg.Attributes.width (String.fromInt scale)
                                                , Svg.Attributes.height (String.fromInt scale)
                                                , Svg.Attributes.fill "currentColor"
                                                ]
                                                []
                                            )

                                    else
                                        Nothing
                                )
                                (List.range 0 (font.glyphW - 1))
                        )
                        (List.range 0 (font.glyphH - 1))
                )
                (List.range 0 (font.numGlyphs - 1))
    in
    Html.div [ Html.Attributes.style "display" "inline-block", Html.Attributes.style "line-height" "0" ]
        [ Svg.svg
            [ Svg.Attributes.width (String.fromInt totalW)
            , Svg.Attributes.height (String.fromInt totalH)
            , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt totalW ++ " " ++ String.fromInt totalH)
            ]
            rects
        ]


viewLastError : String -> Html Msg
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
        [ onFailure (\error -> FailureOccurred error)
        , case model of
            NotConnected _ ->
                Sub.batch
                    [ onConnectSuccessful_ ConnectSuccessful FailureOccurred
                    ]

            Connected _ ->
                Sub.batch
                    [ onDisconnectSuccessful (\() -> DisconnectSuccessful)
                    ]
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

port module Main exposing (Flags, Model, Msg, main)

{-| A web app to connect to and control an ESP32 over WiFi/WebSocket.

The ESP32 is displaying VDOM scenes on a CRT display connected via the GPIO25
DAC pin (NTSC greyscale 400x240 signal).

The point of the web app is to edit the state of the ESP32 on the fly, instead
of changing the C source code and recompiling+reflashing. Also it's pretty cool.

TODO:

  - [ ] edit font data

-}

import Bitwise
import Browser exposing (Document)
import Browser.Events
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Bytes.Extra
import Color
import Dirty
import ESP32 exposing (ESP32, VideoConstants)
import Font exposing (Font)
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import List.Cartesian
import List.Extra
import Node exposing (Node, Type(..))
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Time


type alias Flags =
    ()


type Model
    = NotConnected ModelNotConnected
    | Connected ModelConnected


type alias LoadingProgress =
    { expectedChunkCount : Int
    , receivedChunkCount : Int
    , expectedTotalBytes : Int
    , receivedBytes : Int
    }


type alias InitialLoadStart =
    { expectedChunkCount : Int
    , expectedTotalBytes : Int
    }


type alias ModelNotConnected =
    { lastError : String
    , loadingProgress : Maybe LoadingProgress
    }


{-| Root node sync state between Elm and the ESP32.

The ESP32 applies root updates in order. Elm can edit the desired root
frequently, but it must debounce and it must not send a new root while an
earlier `SetRootNode` command is awaiting ACK.

Roles:

  - `desiredRoot`: the root Elm wants the ESP32 to render next. What Elm shows in the preview.
  - `ackedRoot`: the last root confirmed by an ACK; this is the diff base. What ESP32 is rendering.
  - `inFlightRoot`: a root currently sent to the ESP32 and awaiting ACK. What ESP32 will render next.

Debounce:

  - While debouncing (`RootDebouncing` / `RootAwaitingAckWithPending`), the
    `DebounceFrame` timestamp is captured and the send happens once the
    debounce window has elapsed.

-}
type RootNode
    = RootSynced
        { root : Node
        }
    | RootDebouncing
        { ackedRoot : Node
        , desiredRoot : Node
        , debounceStartedAtMs : Maybe Int
        }
    | RootAwaitingAck
        { ackedRoot : Node
        , inFlightRoot : Node
        }
    | RootAwaitingAckWithPending
        { ackedRoot : Node
        , inFlightRoot : Node
        , desiredRoot : Node
        , debounceStartedAtMs : Maybe Int
        }


desiredRoot : RootNode -> Node
desiredRoot rootSyncState =
    case rootSyncState of
        RootSynced { root } ->
            root

        RootDebouncing r ->
            r.desiredRoot

        RootAwaitingAck { inFlightRoot } ->
            inFlightRoot

        RootAwaitingAckWithPending r ->
            r.desiredRoot


type alias ModelConnected =
    { esp32 : ESP32
    , videoConstants : VideoConstants
    , lastError : String
    , rootNode : RootNode
    , rootNodeJsonText : String
    , rootNodeJsonError : Maybe String
    , selectedPath : List Int
    , previewZoom : Int
    }


type Msg
    = ConnectRequested
    | InitialLoadStarted InitialLoadStart
    | InitialLoadChunkReceived Int
    | ConnectSuccessful ESP32
    | DisconnectRequested
    | DisconnectSuccessful
    | FailureOccurred String
    | MsgConnected MsgConnected


type MsgConnected
    = SelectNode (List Int)
    | UpdateNodeAtPath (List Int) String Type
    | InsertChild (List Int) Int Type
    | RemoveNode (List Int)
    | SetPreviewZoom Int
    | SetRootNodeJsonText String
    | RestoreRootNodeJson
    | DebounceFrame Int
    | RootNodeAcked


port connect : () -> Cmd msg


port disconnect : () -> Cmd msg


port onInitialLoadChunkCount : (InitialLoadStart -> msg) -> Sub msg


port onInitialLoadChunkReceived : (Int -> msg) -> Sub msg


port onConnectSuccessful : (Bytes -> msg) -> Sub msg


port onDisconnectSuccessful : (() -> msg) -> Sub msg


port onFailure : (String -> msg) -> Sub msg


port onRootNodeAck : (() -> msg) -> Sub msg


port sendRootNode : Bytes -> Cmd msg


insertAt : Int -> a -> List a -> List a
insertAt i x list =
    -- TODO use List.Extra instead
    List.take i list ++ (x :: List.drop i list)


getNodeAtPath : List Int -> Node -> Maybe Node
getNodeAtPath path root =
    case path of
        [] ->
            Just root

        i :: rest ->
            case root.type_ of
                Node.Group { children } ->
                    List.Extra.getAt i children
                        |> Maybe.andThen (getNodeAtPath rest)

                _ ->
                    Nothing


setNodeAtPath : List Font -> List Int -> Node -> Node -> Node
setNodeAtPath fonts path newNode root =
    case path of
        [] ->
            newNode

        i :: rest ->
            case root.type_ of
                Node.Group { children } ->
                    case List.Extra.getAt i children of
                        Just child ->
                            let
                                newChild =
                                    setNodeAtPath fonts rest newNode child
                            in
                            List.Extra.updateAt i (always newChild) children
                                |> Node.group root.key

                        Nothing ->
                            root

                _ ->
                    root


removeNodeAtPath : List Font -> List Int -> Node -> Node
removeNodeAtPath fonts path root =
    case path of
        [] ->
            root

        [ i ] ->
            case root.type_ of
                Node.Group { children } ->
                    Node.group root.key (List.Extra.removeAt i children)

                _ ->
                    root

        i :: rest ->
            case root.type_ of
                Node.Group { children } ->
                    case List.Extra.getAt i children of
                        Just child ->
                            let
                                newChild =
                                    removeNodeAtPath fonts rest child
                            in
                            List.Extra.updateAt i (always newChild) children
                                |> Node.group root.key

                        Nothing ->
                            root

                _ ->
                    root


insertChildAtPath : List Font -> List Int -> Int -> Node -> Node -> Node
insertChildAtPath fonts parentPath index newChild root =
    case getNodeAtPath parentPath root of
        Just parent ->
            case parent.type_ of
                Node.Group { children } ->
                    let
                        inserted =
                            insertAt index newChild children
                    in
                    setNodeAtPath fonts parentPath (Node.group parent.key inserted) root

                _ ->
                    root

        Nothing ->
            root


defaultNodeForType : List Font -> VideoConstants -> Type -> Node
defaultNodeForType fonts vc type_ =
    let
        x =
            vc.xMin

        y =
            vc.yMin

        c =
            Color.white
    in
    case type_ of
        Rect _ ->
            Node.rect "rect" { x = x, y = y, w = 10, h = 10, color = c }

        RectFill _ ->
            Node.rectFill "rectFill" { x = x, y = y, w = 10, h = 10, color = c }

        XLine _ ->
            Node.xLine "xLine" { x = x, y = y, len = 20, color = c }

        YLine _ ->
            Node.yLine "yLine" { x = x, y = y, len = 20, color = c }

        Text _ ->
            Node.text fonts "text" { x = x, y = y, text = "Text", fontIndex = 0, color = c }

        Group _ ->
            Node.group "group" []


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


emptyModelNotConnected : ModelNotConnected
emptyModelNotConnected =
    { lastError = ""
    , loadingProgress = Nothing
    }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( NotConnected emptyModelNotConnected
    , Cmd.none
    )


encodeNodeJson : Node -> String
encodeNodeJson node =
    node
        |> Node.jsonEncoder
        |> Encode.encode 0


{-| The debounce logic in subscriptions will automatically pick this up and send a command.
-}
commitNewRootNode : Node -> ModelConnected -> ModelConnected
commitNewRootNode newRoot modelConnected =
    let
        json =
            encodeNodeJson newRoot

        newRootNode =
            case modelConnected.rootNode of
                RootSynced { root } ->
                    RootDebouncing
                        { ackedRoot = root
                        , desiredRoot = newRoot
                        , debounceStartedAtMs = Nothing
                        }

                RootDebouncing { ackedRoot } ->
                    RootDebouncing
                        { ackedRoot = ackedRoot
                        , desiredRoot = newRoot
                        , debounceStartedAtMs = Nothing
                        }

                RootAwaitingAck { ackedRoot, inFlightRoot } ->
                    RootAwaitingAckWithPending
                        { ackedRoot = ackedRoot
                        , inFlightRoot = inFlightRoot
                        , desiredRoot = newRoot
                        , debounceStartedAtMs = Nothing
                        }

                RootAwaitingAckWithPending { ackedRoot, inFlightRoot } ->
                    RootAwaitingAckWithPending
                        { ackedRoot = ackedRoot
                        , inFlightRoot = inFlightRoot
                        , desiredRoot = newRoot
                        , debounceStartedAtMs = Nothing
                        }
    in
    { modelConnected
        | rootNode = newRootNode
        , rootNodeJsonText = json
        , rootNodeJsonError = Nothing
    }


setRootNodeDebounceWaitMs : Int
setRootNodeDebounceWaitMs =
    16


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectRequested ->
            ( case model of
                NotConnected nc ->
                    NotConnected
                        { nc
                            | lastError = ""
                            , loadingProgress = Nothing
                        }

                Connected _ ->
                    model
            , connect ()
            )

        InitialLoadStarted initialLoadStart ->
            case model of
                NotConnected nc ->
                    let
                        sanitizedChunkCount =
                            max 0 initialLoadStart.expectedChunkCount

                        sanitizedTotalBytes =
                            max 0 initialLoadStart.expectedTotalBytes
                    in
                    ( NotConnected
                        { nc
                            | loadingProgress =
                                if sanitizedChunkCount > 0 then
                                    Just
                                        { expectedChunkCount = sanitizedChunkCount
                                        , receivedChunkCount = 0
                                        , expectedTotalBytes = sanitizedTotalBytes
                                        , receivedBytes = 0
                                        }

                                else
                                    Nothing
                        }
                    , Cmd.none
                    )

                Connected _ ->
                    ( model, Cmd.none )

        InitialLoadChunkReceived chunkBytes ->
            case model of
                NotConnected nc ->
                    let
                        sanitizedChunkBytes =
                            max 0 chunkBytes
                    in
                    ( NotConnected
                        { nc
                            | loadingProgress =
                                nc.loadingProgress
                                    |> Maybe.map
                                        (\progress ->
                                            { progress
                                                | receivedChunkCount =
                                                    min progress.expectedChunkCount (progress.receivedChunkCount + 1)
                                                , receivedBytes =
                                                    min progress.expectedTotalBytes (progress.receivedBytes + sanitizedChunkBytes)
                                            }
                                        )
                        }
                    , Cmd.none
                    )

                Connected _ ->
                    ( model, Cmd.none )

        ConnectSuccessful esp32 ->
            let
                videoConstants_ =
                    ESP32.videoConstants esp32

                node =
                    Node.empty

                json =
                    encodeNodeJson node
            in
            ( Connected
                { esp32 = esp32
                , videoConstants = videoConstants_
                , lastError = ""
                , rootNode = RootSynced { root = node }
                , rootNodeJsonText = json
                , rootNodeJsonError = Nothing
                , selectedPath = []
                , previewZoom = 3
                }
            , Cmd.none
            )

        DisconnectRequested ->
            ( model, disconnect () )

        DisconnectSuccessful ->
            ( NotConnected emptyModelNotConnected
            , Cmd.none
            )

        FailureOccurred error ->
            case model of
                NotConnected nc ->
                    ( NotConnected
                        { nc
                            | lastError = error
                            , loadingProgress = Nothing
                        }
                    , Cmd.none
                    )

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
        SelectNode path ->
            ( { modelConnected | selectedPath = path }
            , Cmd.none
            )

        UpdateNodeAtPath path key type_ ->
            let
                existing =
                    getNodeAtPath path (desiredRoot modelConnected.rootNode)

                newNode =
                    case type_ of
                        Group _ ->
                            case existing of
                                Just n ->
                                    case n.type_ of
                                        Node.Group { children } ->
                                            Node.group key children

                                        _ ->
                                            Node.group key []

                                Nothing ->
                                    Node.group key []

                        _ ->
                            case existing of
                                Just _ ->
                                    Node.fromKeyAndType modelConnected.esp32.fonts key type_

                                Nothing ->
                                    desiredRoot modelConnected.rootNode

                newRoot =
                    case existing of
                        Just _ ->
                            setNodeAtPath modelConnected.esp32.fonts path newNode (desiredRoot modelConnected.rootNode)

                        Nothing ->
                            desiredRoot modelConnected.rootNode
            in
            ( modelConnected
                |> commitNewRootNode newRoot
            , Cmd.none
            )

        InsertChild parentPath index type_ ->
            let
                newChild =
                    defaultNodeForType modelConnected.esp32.fonts modelConnected.videoConstants type_

                newRoot =
                    insertChildAtPath modelConnected.esp32.fonts parentPath index newChild (desiredRoot modelConnected.rootNode)
            in
            ( modelConnected
                |> commitNewRootNode newRoot
            , Cmd.none
            )

        RemoveNode path ->
            let
                newRoot =
                    removeNodeAtPath modelConnected.esp32.fonts path (desiredRoot modelConnected.rootNode)
            in
            ( { modelConnected
                | selectedPath =
                    if path == modelConnected.selectedPath then
                        []

                    else
                        modelConnected.selectedPath
              }
                |> commitNewRootNode newRoot
            , Cmd.none
            )

        SetPreviewZoom zoom ->
            ( { modelConnected | previewZoom = zoom }
            , Cmd.none
            )

        SetRootNodeJsonText text ->
            case Decode.decodeString (Node.jsonDecoder modelConnected.esp32.fonts) text of
                Ok parsed ->
                    ( modelConnected
                        |> commitNewRootNode parsed
                    , Cmd.none
                    )

                Err err ->
                    ( { modelConnected
                        | rootNodeJsonText = text
                        , rootNodeJsonError = Just (Decode.errorToString err)
                      }
                    , Cmd.none
                    )

        RestoreRootNodeJson ->
            ( { modelConnected
                | rootNodeJsonText = encodeNodeJson (desiredRoot modelConnected.rootNode)
                , rootNodeJsonError = Nothing
              }
            , Cmd.none
            )

        DebounceFrame nowMs ->
            case modelConnected.rootNode of
                RootSynced _ ->
                    ( modelConnected, Cmd.none )

                RootDebouncing r ->
                    case r.debounceStartedAtMs of
                        Nothing ->
                            ( { modelConnected
                                | rootNode =
                                    RootDebouncing
                                        { ackedRoot = r.ackedRoot
                                        , desiredRoot = r.desiredRoot
                                        , debounceStartedAtMs = Just nowMs
                                        }
                              }
                            , Cmd.none
                            )

                        Just startedAtMs ->
                            if nowMs - startedAtMs < setRootNodeDebounceWaitMs then
                                ( modelConnected, Cmd.none )

                            else if r.ackedRoot.hash == r.desiredRoot.hash then
                                ( { modelConnected | rootNode = RootSynced { root = r.desiredRoot } }
                                , Cmd.none
                                )

                            else
                                ( { modelConnected
                                    | rootNode =
                                        RootAwaitingAck
                                            { ackedRoot = r.ackedRoot
                                            , inFlightRoot = r.desiredRoot
                                            }
                                  }
                                , setRootNode modelConnected.esp32 modelConnected.videoConstants r.ackedRoot r.desiredRoot
                                )

                RootAwaitingAck _ ->
                    ( modelConnected, Cmd.none )

                RootAwaitingAckWithPending r ->
                    case r.debounceStartedAtMs of
                        Nothing ->
                            ( { modelConnected
                                | rootNode =
                                    RootAwaitingAckWithPending
                                        { ackedRoot = r.ackedRoot
                                        , inFlightRoot = r.inFlightRoot
                                        , desiredRoot = r.desiredRoot
                                        , debounceStartedAtMs = Just nowMs
                                        }
                              }
                            , Cmd.none
                            )

                        Just startedAtMs ->
                            -- Can't send while waiting for ACK; keep the debounce timer running.
                            if nowMs - startedAtMs < setRootNodeDebounceWaitMs then
                                ( modelConnected, Cmd.none )

                            else
                                ( modelConnected, Cmd.none )

        RootNodeAcked ->
            case modelConnected.rootNode of
                RootAwaitingAck { inFlightRoot } ->
                    ( { modelConnected | rootNode = RootSynced { root = inFlightRoot } }
                    , Cmd.none
                    )

                RootAwaitingAckWithPending r ->
                    let
                        acked =
                            r.inFlightRoot
                    in
                    if r.desiredRoot.hash == acked.hash then
                        ( { modelConnected | rootNode = RootSynced { root = r.desiredRoot } }
                        , Cmd.none
                        )

                    else
                        ( { modelConnected
                            | rootNode =
                                RootDebouncing
                                    { ackedRoot = acked
                                    , desiredRoot = r.desiredRoot
                                    , debounceStartedAtMs = r.debounceStartedAtMs
                                    }
                          }
                        , Cmd.none
                        )

                _ ->
                    ( modelConnected, Cmd.none )


setRootNode : ESP32 -> VideoConstants -> Node -> Node -> Cmd MsgConnected
setRootNode esp32 videoConstants previousNode node =
    if node.hash == previousNode.hash then
        Cmd.none

    else
        let
            dirtyTiles =
                Dirty.diff
                    { tileSize = esp32.tileSize
                    , tileCols = videoConstants.tileCols
                    , tileRows = videoConstants.tileRows
                    }
                    esp32.fonts
                    previousNode
                    node

            _ =
                Debug.log "sending dirty tiles" (Set.size dirtyTiles)

            bytes =
                Bytes.Encode.sequence
                    [ Node.encoder node
                    , Dirty.dirtyTilesEncoder dirtyTiles
                    ]
                    |> Bytes.Encode.encode
        in
        sendRootNode bytes


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
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "padding" "0.5rem"
        ]
        (List.concat
            [ [ Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "gap" "0.5rem"
                    , Html.Attributes.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.text "Not connected."
                    , Html.button
                        [ Html.Events.onClick ConnectRequested ]
                        [ Html.text "Connect" ]
                    , viewLastError model.lastError
                    ]
              ]
            , case model.loadingProgress of
                Just progress ->
                    [ viewInitialLoadProgress progress ]

                Nothing ->
                    []
            ]
        )


viewInitialLoadProgress : LoadingProgress -> Html Msg
viewInitialLoadProgress progress =
    let
        widthPercent =
            if progress.expectedChunkCount <= 0 then
                "0%"

            else
                String.fromFloat
                    (100
                        * toFloat progress.receivedChunkCount
                        / toFloat progress.expectedChunkCount
                    )
                    ++ "%"

        kilobytesText bytes =
            let
                tenths =
                    round ((toFloat bytes * 10) / 1024)

                whole =
                    tenths // 10

                fractional =
                    modBy 10 tenths
            in
            String.fromInt whole ++ "." ++ String.fromInt fractional ++ " KB"
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "0.35rem"
        , Html.Attributes.style "max-width" "20rem"
        ]
        [ Html.div []
            [ Html.text
                ("Loading ESP32 data: "
                    ++ String.fromInt progress.receivedChunkCount
                    ++ " / "
                    ++ String.fromInt progress.expectedChunkCount
                    ++ " chunks"
                )
            ]
        , Html.div []
            [ Html.text
                (kilobytesText progress.receivedBytes
                    ++ " / "
                    ++ kilobytesText progress.expectedTotalBytes
                )
            ]
        , Html.div
            [ Html.Attributes.style "height" "0.75rem"
            , Html.Attributes.style "background" "#222"
            , Html.Attributes.style "border" "1px solid #555"
            ]
            [ Html.div
                [ Html.Attributes.style "height" "100%"
                , Html.Attributes.style "width" widthPercent
                , Html.Attributes.style "background" "#7bd88f"
                ]
                []
            ]
        ]


viewPreviewColumn : VideoConstants -> Int -> Node -> List Font -> Html Msg
viewPreviewColumn vc zoom root fonts =
    let
        zoomedW =
            vc.usableWidth * zoom

        zoomedH =
            vc.usableHeight * zoom

        zoomButton z =
            Html.button
                [ Html.Events.onClick (MsgConnected (SetPreviewZoom z))
                , Html.Attributes.Extra.attributeIf (zoom == z)
                    (Html.Attributes.style "background" "var(--selection)")
                ]
                [ Html.text (String.fromInt z ++ "x") ]
    in
    Html.div
        [ Html.Attributes.style "flex" "0 0 auto"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "align-items" "flex-start"
        ]
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "gap" "0.5rem"
            , Html.Attributes.style "margin-bottom" "0.25rem"
            ]
            [ Html.div
                [ Html.Attributes.style "font-size" "0.875rem"
                ]
                [ Html.text "Preview" ]
            , zoomButton 1
            , zoomButton 2
            , zoomButton 3
            ]
        , Html.div
            [ Html.Attributes.style "width" (String.fromInt zoomedW ++ "px")
            , Html.Attributes.style "height" (String.fromInt zoomedH ++ "px")
            , Html.Attributes.style "flex-shrink" "0"
            , Html.Attributes.style "position" "relative"
            , Html.Attributes.style "overflow" "visible"
            ]
            [ Html.div
                [ Html.Attributes.style "width" (String.fromInt vc.usableWidth ++ "px")
                , Html.Attributes.style "height" (String.fromInt vc.usableHeight ++ "px")
                , Html.Attributes.style "transform" ("scale(" ++ String.fromInt zoom ++ ")")
                , Html.Attributes.style "transform-origin" "top left"
                , Html.Attributes.style "background" "black"
                , Html.Attributes.style "anchor-name" "--screen"
                , Html.Attributes.style "overflow" "hidden"
                , Html.Attributes.style "image-rendering" "pixelated"
                , Html.Attributes.style "image-rendering" "crisp-edges"
                ]
                [ Svg.svg
                    [ Svg.Attributes.width (String.fromInt vc.usableWidth)
                    , Svg.Attributes.height (String.fromInt vc.usableHeight)
                    , Svg.Attributes.viewBox
                        (String.fromInt vc.xMin
                            ++ " "
                            ++ String.fromInt vc.yMin
                            ++ " "
                            ++ String.fromInt vc.usableWidth
                            ++ " "
                            ++ String.fromInt vc.usableHeight
                        )
                    , Svg.Attributes.style "display:block"
                    ]
                    (renderNodeToSvg fonts root)
                ]
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "position-anchor" "--screen"
                , Html.Attributes.style "right" "anchor(--screen left)"
                , Html.Attributes.style "bottom" "anchor(--screen top)"
                , Html.Attributes.style "width" "8px"
                , Html.Attributes.style "height" "8px"
                , Html.Attributes.style "border-bottom" "1px solid var(--muted)"
                , Html.Attributes.style "border-right" "1px solid var(--muted)"
                , Html.Attributes.style "pointer-events" "none"
                ]
                []
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "position-anchor" "--screen"
                , Html.Attributes.style "left" "anchor(--screen right)"
                , Html.Attributes.style "bottom" "anchor(--screen top)"
                , Html.Attributes.style "width" "8px"
                , Html.Attributes.style "height" "8px"
                , Html.Attributes.style "border-bottom" "1px solid var(--muted)"
                , Html.Attributes.style "border-left" "1px solid var(--muted)"
                , Html.Attributes.style "pointer-events" "none"
                ]
                []
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "position-anchor" "--screen"
                , Html.Attributes.style "right" "anchor(--screen left)"
                , Html.Attributes.style "top" "anchor(--screen bottom)"
                , Html.Attributes.style "width" "8px"
                , Html.Attributes.style "height" "8px"
                , Html.Attributes.style "border-top" "1px solid var(--muted)"
                , Html.Attributes.style "border-right" "1px solid var(--muted)"
                , Html.Attributes.style "pointer-events" "none"
                ]
                []
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "position-anchor" "--screen"
                , Html.Attributes.style "left" "anchor(--screen right)"
                , Html.Attributes.style "top" "anchor(--screen bottom)"
                , Html.Attributes.style "width" "8px"
                , Html.Attributes.style "height" "8px"
                , Html.Attributes.style "border-top" "1px solid var(--muted)"
                , Html.Attributes.style "border-left" "1px solid var(--muted)"
                , Html.Attributes.style "pointer-events" "none"
                ]
                []
            ]
        ]


viewSidebarColumn : ModelConnected -> Html Msg
viewSidebarColumn model =
    Html.div
        [ Html.Attributes.style "flex" "0 0 350px"
        , Html.Attributes.style "width" "350px"
        , Html.Attributes.style "min-width" "350px"
        , Html.Attributes.style "max-width" "350px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "min-height" "0"
        , Html.Attributes.style "overflow" "auto"
        , Html.Attributes.style "gap" "0.5rem"
        ]
        [ viewTreeColumn model
        , viewDetailsColumn model
        , viewRootNodeJsonColumn model
        ]


viewRootNodeJsonColumn : ModelConnected -> Html Msg
viewRootNodeJsonColumn model =
    let
        hasError =
            model.rootNodeJsonError /= Nothing

        textareaBorder =
            if hasError then
                "1px solid #b91c1c"

            else
                "1px solid var(--border)"

        errorView =
            case model.rootNodeJsonError of
                Nothing ->
                    Html.text ""

                Just err ->
                    Html.div
                        [ Html.Attributes.style "margin-top" "0.35rem"
                        , Html.Attributes.style "color" "#b91c1c"
                        , Html.Attributes.style "font-size" "0.8rem"
                        , Html.Attributes.style "white-space" "pre-wrap"
                        ]
                        [ Html.text err ]
    in
    Html.div
        [ Html.Attributes.style "flex" "1 1 0"
        , Html.Attributes.style "min-height" "0"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "background" "var(--surface-2)"
        , Html.Attributes.style "border" "1px solid var(--border)"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "overflow" "hidden"
        ]
        [ Html.div
            [ Html.Attributes.style "padding" "0.25rem 0.5rem"
            , Html.Attributes.style "font-size" "0.875rem"
            , Html.Attributes.style "font-weight" "600"
            , Html.Attributes.style "background" "var(--surface-3)"
            , Html.Attributes.style "border-bottom" "1px solid var(--border)"
            ]
            [ Html.text "Node JSON" ]
        , Html.div
            [ Html.Attributes.style "flex" "1"
            , Html.Attributes.style "min-height" "0"
            , Html.Attributes.style "overflow" "auto"
            , Html.Attributes.style "padding" "0.5rem"
            ]
            [ Html.textarea
                [ Html.Attributes.value model.rootNodeJsonText
                , Html.Events.onInput (\t -> MsgConnected (SetRootNodeJsonText t))
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "min-height" "10rem"
                , Html.Attributes.style "box-sizing" "border-box"
                , Html.Attributes.style "font-family" "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
                , Html.Attributes.style "font-size" "0.8rem"
                , Html.Attributes.style "border" textareaBorder
                , Html.Attributes.style "border-radius" "4px"
                , Html.Attributes.style "padding" "0.5rem"
                ]
                []
            , Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "gap" "0.5rem"
                , Html.Attributes.style "margin-top" "0.5rem"
                ]
                (if hasError then
                    [ Html.button
                        [ Html.Events.onClick (MsgConnected RestoreRootNodeJson) ]
                        [ Html.text "Restore" ]
                    ]

                 else
                    []
                )
            , errorView
            ]
        ]


viewTreeColumn : ModelConnected -> Html Msg
viewTreeColumn model =
    Html.div
        [ Html.Attributes.style "flex" "0 0 auto"
        , Html.Attributes.style "max-height" "35vh"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "background" "var(--surface-2)"
        , Html.Attributes.style "border" "1px solid var(--border)"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "overflow" "hidden"
        ]
        [ Html.div
            [ Html.Attributes.style "padding" "0.25rem 0.5rem"
            , Html.Attributes.style "font-size" "0.875rem"
            , Html.Attributes.style "font-weight" "600"
            , Html.Attributes.style "background" "var(--surface-3)"
            , Html.Attributes.style "border-bottom" "1px solid var(--border)"
            ]
            [ Html.text "Node tree" ]
        , Html.div
            [ Html.Attributes.style "flex" "1"
            , Html.Attributes.style "min-height" "0"
            , Html.Attributes.style "overflow" "auto"
            , Html.Attributes.style "padding" "0.25rem"
            ]
            [ viewTreeNode model [] (desiredRoot model.rootNode)
            ]
        ]


viewTreeNode : ModelConnected -> List Int -> Node -> Html Msg
viewTreeNode model path node =
    let
        isSelected =
            model.selectedPath == path

        nodeLabel =
            case node.type_ of
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

                Node.Group _ ->
                    "Group"

        rowAttrs =
            [ Html.Attributes.style "padding" "0.2rem 0.4rem"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "border-radius" "2px"
            , Html.Attributes.style "margin-bottom" "1px"
            , Html.Events.onClick (MsgConnected (SelectNode path))
            ]
                ++ (if isSelected then
                        [ Html.Attributes.style "background" "var(--selection)" ]

                    else
                        []
                   )

        childrenView =
            case node.type_ of
                Node.Group { children } ->
                    Html.div
                        [ Html.Attributes.style "margin-left" "1rem" ]
                        (List.indexedMap
                            (\i child ->
                                viewTreeNode model (path ++ [ i ]) child
                            )
                            children
                        )

                _ ->
                    Html.text ""

        addRemove =
            Html.span
                [ Html.Attributes.style "margin-left" "0.5rem"
                , Html.Attributes.style "font-size" "0.75rem"
                ]
                (if path == [] then
                    [ viewAddChildButton model path ]

                 else
                    [ viewAddChildButton model path
                    , Html.text " "
                    , Html.button
                        [ Html.Events.onClick (MsgConnected (RemoveNode path))
                        , Html.Attributes.style "padding" "0 0.25rem"
                        ]
                        [ Html.text "Remove" ]
                    ]
                )
    in
    Html.div []
        [ Html.div rowAttrs
            [ Html.text (nodeLabel ++ " \"" ++ node.key ++ "\"")
            , addRemove
            ]
        , childrenView
        ]


viewAddChildButton : ModelConnected -> List Int -> Html Msg
viewAddChildButton model path =
    let
        canAdd =
            case getNodeAtPath path (desiredRoot model.rootNode) of
                Just n ->
                    case n.type_ of
                        Node.Group _ ->
                            True

                        _ ->
                            False

                Nothing ->
                    False
    in
    if canAdd then
        let
            insertIndex =
                getNodeAtPath path (desiredRoot model.rootNode)
                    |> Maybe.andThen
                        (\n ->
                            case n.type_ of
                                Node.Group { children } ->
                                    Just (List.length children)

                                _ ->
                                    Nothing
                        )
                    |> Maybe.withDefault 0

            vc =
                model.videoConstants

            addOption val label =
                Html.option [ Html.Attributes.value val ] [ Html.text label ]
        in
        Html.select
            [ Html.Events.on "change"
                (Decode.at [ "target", "value" ] Decode.string
                    |> Decode.map
                        (\v ->
                            case v of
                                "rect" ->
                                    MsgConnected (InsertChild path insertIndex (Rect { x = vc.xMin, y = vc.yMin, w = 10, h = 10, color = Color.white }))

                                "rectFill" ->
                                    MsgConnected (InsertChild path insertIndex (RectFill { x = vc.xMin, y = vc.yMin, w = 10, h = 10, color = Color.white }))

                                "xLine" ->
                                    MsgConnected (InsertChild path insertIndex (XLine { x = vc.xMin, y = vc.yMin, len = 20, color = Color.white }))

                                "yLine" ->
                                    MsgConnected (InsertChild path insertIndex (YLine { x = vc.xMin, y = vc.yMin, len = 20, color = Color.white }))

                                "text" ->
                                    MsgConnected (InsertChild path insertIndex (Text { x = vc.xMin, y = vc.yMin, text = "Text", fontIndex = 0, color = Color.white }))

                                "group" ->
                                    MsgConnected (InsertChild path insertIndex (Group { children = [] }))

                                _ ->
                                    MsgConnected (SelectNode path)
                        )
                )
            , Html.Attributes.style "padding" "0 0.25rem"
            ]
            [ Html.option [ Html.Attributes.value "" ] [ Html.text "Add…" ]
            , addOption "rect" "Rect"
            , addOption "rectFill" "RectFill"
            , addOption "xLine" "XLine"
            , addOption "yLine" "YLine"
            , addOption "text" "Text"
            , addOption "group" "Group"
            ]

    else
        Html.text ""


viewDetailsColumn : ModelConnected -> Html Msg
viewDetailsColumn model =
    Html.div
        [ Html.Attributes.style "flex" "1 1 0"
        , Html.Attributes.style "min-height" "0"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "background" "var(--surface-2)"
        , Html.Attributes.style "border" "1px solid var(--border)"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "overflow" "hidden"
        ]
        [ Html.div
            [ Html.Attributes.style "padding" "0.25rem 0.5rem"
            , Html.Attributes.style "font-size" "0.875rem"
            , Html.Attributes.style "font-weight" "600"
            , Html.Attributes.style "background" "var(--surface-3)"
            , Html.Attributes.style "border-bottom" "1px solid var(--border)"
            ]
            [ Html.text "Details" ]
        , Html.div
            [ Html.Attributes.style "flex" "1"
            , Html.Attributes.style "overflow" "auto"
            , Html.Attributes.style "padding" "0.5rem"
            ]
            (case getNodeAtPath model.selectedPath (desiredRoot model.rootNode) of
                Just node ->
                    viewNodeDetails model node model.selectedPath

                Nothing ->
                    [ Html.div [ Html.Attributes.style "color" "var(--muted)" ] [ Html.text "Select a node." ] ]
            )
        ]


viewNodeDetails : ModelConnected -> Node -> List Int -> List (Html Msg)
viewNodeDetails model node path =
    let
        fieldLabel s =
            Html.div
                [ Html.Attributes.style "font-size" "0.75rem"
                , Html.Attributes.style "margin-top" "0.5rem"
                , Html.Attributes.style "margin-bottom" "0.15rem"
                ]
                [ Html.text s ]

        keyField =
            [ fieldLabel "Key"
            , Html.input
                [ Html.Attributes.value node.key
                , Html.Events.onInput
                    (\k ->
                        MsgConnected (UpdateNodeAtPath path k node.type_)
                    )
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "box-sizing" "border-box"
                ]
                []
            ]

        typeFields =
            case node.type_ of
                Rect r ->
                    [ fieldLabel "x"
                    , intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> Rect { r | x = x }) r.x
                    , fieldLabel "y"
                    , intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> Rect { r | y = y }) r.y
                    , fieldLabel "w"
                    , intSliderInput 0 model.videoConstants.usableWidth path node.key (\w -> Rect { r | w = w }) r.w
                    , fieldLabel "h"
                    , intSliderInput 0 model.videoConstants.usableHeight path node.key (\h -> Rect { r | h = h }) r.h
                    , fieldLabel "color"
                    , colorInput path node.key (\color -> Rect { r | color = color }) r.color
                    ]

                RectFill r ->
                    [ fieldLabel "x"
                    , intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> RectFill { r | x = x }) r.x
                    , fieldLabel "y"
                    , intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> RectFill { r | y = y }) r.y
                    , fieldLabel "w"
                    , intSliderInput 0 model.videoConstants.usableWidth path node.key (\w -> RectFill { r | w = w }) r.w
                    , fieldLabel "h"
                    , intSliderInput 0 model.videoConstants.usableHeight path node.key (\h -> RectFill { r | h = h }) r.h
                    , fieldLabel "color"
                    , colorInput path node.key (\color -> RectFill { r | color = color }) r.color
                    ]

                XLine r ->
                    [ fieldLabel "x"
                    , intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> XLine { r | x = x }) r.x
                    , fieldLabel "y"
                    , intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> XLine { r | y = y }) r.y
                    , fieldLabel "len"
                    , intSliderInput 0 model.videoConstants.usableWidth path node.key (\len -> XLine { r | len = len }) r.len
                    , fieldLabel "color"
                    , colorInput path node.key (\color -> XLine { r | color = color }) r.color
                    ]

                YLine r ->
                    [ fieldLabel "x"
                    , intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> YLine { r | x = x }) r.x
                    , fieldLabel "y"
                    , intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> YLine { r | y = y }) r.y
                    , fieldLabel "len"
                    , intSliderInput 0 model.videoConstants.usableHeight path node.key (\len -> YLine { r | len = len }) r.len
                    , fieldLabel "color"
                    , colorInput path node.key (\color -> YLine { r | color = color }) r.color
                    ]

                Text r ->
                    [ fieldLabel "x"
                    , intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> Text { r | x = x }) r.x
                    , fieldLabel "y"
                    , intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> Text { r | y = y }) r.y
                    , fieldLabel "text"
                    , Html.textarea
                        [ Html.Attributes.value r.text
                        , Html.Events.onInput (\t -> MsgConnected (UpdateNodeAtPath path node.key (Text { r | text = t })))
                        , Html.Attributes.rows 4
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "box-sizing" "border-box"
                        ]
                        []
                    , fieldLabel "fontIndex"
                    , Html.select
                        [ Html.Events.onInput
                            (\s ->
                                String.toInt s
                                    |> Maybe.map (\fontIndex -> MsgConnected (UpdateNodeAtPath path node.key (Text { r | fontIndex = fontIndex })))
                                    |> Maybe.withDefault (MsgConnected (SelectNode path))
                            )
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "box-sizing" "border-box"
                        ]
                        (model.esp32.fonts
                            |> List.indexedMap
                                (\i font ->
                                    Html.option
                                        [ Html.Attributes.value (String.fromInt i)
                                        , Html.Attributes.Extra.attributeIf (i == r.fontIndex) (Html.Attributes.attribute "selected" "")
                                        ]
                                        [ Html.text (String.fromInt i ++ ": " ++ font.name) ]
                                )
                        )
                    , fieldLabel "color"
                    , colorInput path node.key (\color -> Text { r | color = color }) r.color
                    ]

                Node.Group _ ->
                    []
    in
    (keyField ++ typeFields)
        |> List.map (\e -> Html.div [] [ e ])


intInput : List Int -> String -> (Int -> Type) -> Int -> Html Msg
intInput path key toType current =
    Html.input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.value (String.fromInt current)
        , Html.Events.onInput
            (\s ->
                String.toInt s
                    |> Maybe.map (\v -> MsgConnected (UpdateNodeAtPath path key (toType v)))
                    |> Maybe.withDefault (MsgConnected (SelectNode path))
            )
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "box-sizing" "border-box"
        ]
        []


intSliderInput : Int -> Int -> List Int -> String -> (Int -> Type) -> Int -> Html Msg
intSliderInput min_ max_ path key toType current =
    let
        current_ =
            clamp min_ max_ current
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "gap" "0.5rem"
        ]
        [ Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min (String.fromInt min_)
            , Html.Attributes.max (String.fromInt max_)
            , Html.Attributes.step "1"
            , Html.Attributes.value (String.fromInt current_)
            , Html.Events.onInput
                (\s ->
                    String.toInt s
                        |> Maybe.map (clamp min_ max_)
                        |> Maybe.map (\v -> MsgConnected (UpdateNodeAtPath path key (toType v)))
                        |> Maybe.withDefault (MsgConnected (SelectNode path))
                )
            , Html.Attributes.style "flex" "1"
            ]
            []
        , Html.div
            [ Html.Attributes.style "width" "3rem"
            , Html.Attributes.style "text-align" "right"
            , Html.Attributes.style "font-family" "ui-monospace, monospace"
            , Html.Attributes.style "font-size" "0.875rem"
            ]
            [ Html.text (String.fromInt current_) ]
        ]


colorInput : List Int -> String -> (Int -> Type) -> Int -> Html Msg
colorInput path key toType current =
    let
        current_ =
            clamp 0 255 current
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "gap" "0.5rem"
        ]
        [ Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "255"
            , Html.Attributes.step "1"
            , Html.Attributes.value (String.fromInt current_)
            , Html.Events.onInput
                (\s ->
                    String.toInt s
                        |> Maybe.map (clamp 0 255)
                        |> Maybe.map (\v -> MsgConnected (UpdateNodeAtPath path key (toType v)))
                        |> Maybe.withDefault (MsgConnected (SelectNode path))
                )
            , Html.Attributes.style "flex" "1"
            ]
            []
        , Html.div
            [ Html.Attributes.style "width" "3rem"
            , Html.Attributes.style "text-align" "right"
            , Html.Attributes.style "font-family" "ui-monospace, monospace"
            , Html.Attributes.style "font-size" "0.875rem"
            ]
            [ Html.text (String.fromInt current_) ]
        ]


viewConnected : ModelConnected -> Html Msg
viewConnected model =
    let
        vc =
            model.videoConstants
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "padding" "0.5rem"
        , Html.Attributes.style "gap" "0.5rem"
        ]
        [ Html.div
            [ Html.Attributes.style "flex" "1 1 auto"
            , Html.Attributes.style "min-width" "0"
            , Html.Attributes.style "min-height" "0"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            ]
            [ Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "gap" "0.5rem"
                , Html.Attributes.style "margin-bottom" "0.5rem"
                ]
                [ Html.text "Connected."
                , Html.button
                    [ Html.Events.onClick DisconnectRequested ]
                    [ Html.text "Disconnect" ]
                , viewLastError model.lastError
                ]
            , Html.div
                [ Html.Attributes.style "flex" "1 1 auto"
                , Html.Attributes.style "min-height" "0"
                , Html.Attributes.style "overflow" "auto"
                ]
                [ viewPreviewColumn vc model.previewZoom (desiredRoot model.rootNode) model.esp32.fonts ]
            ]
        , viewSidebarColumn model
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
            , Html.Attributes.style "color" "var(--muted)"
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


colorToCss : Int -> String
colorToCss c =
    let
        n =
            clamp 0 255 c
    in
    "rgb(" ++ String.fromInt n ++ "," ++ String.fromInt n ++ "," ++ String.fromInt n ++ ")"


renderGlyphPixel : Font -> Int -> Int -> Int -> Bool
renderGlyphPixel font g row col =
    let
        byteIdx =
            g * font.glyphHeight + row

        byte =
            font.bits
                |> List.drop byteIdx
                |> List.head
                |> Maybe.withDefault 0

        bit =
            Bitwise.and (Bitwise.shiftRightBy (7 - col) byte) 1
    in
    bit == 1


renderNodeToSvg : List Font -> Node -> List (Svg msg)
renderNodeToSvg fonts node_ =
    case node_.type_ of
        Rect { x, y, w, h, color } ->
            if w <= 0 || h <= 0 then
                []

            else
                let
                    css =
                        colorToCss color

                    x2 =
                        x + w - 1

                    y2 =
                        y + h - 1
                in
                [ -- 1px inside outline (avoids half-clipped SVG strokes at viewport edges)
                  Svg.rect
                    [ Svg.Attributes.x (String.fromInt x)
                    , Svg.Attributes.y (String.fromInt y)
                    , Svg.Attributes.width (String.fromInt w)
                    , Svg.Attributes.height "1"
                    , Svg.Attributes.fill css
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.x (String.fromInt x)
                    , Svg.Attributes.y (String.fromInt y2)
                    , Svg.Attributes.width (String.fromInt w)
                    , Svg.Attributes.height "1"
                    , Svg.Attributes.fill css
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.x (String.fromInt x)
                    , Svg.Attributes.y (String.fromInt y)
                    , Svg.Attributes.width "1"
                    , Svg.Attributes.height (String.fromInt h)
                    , Svg.Attributes.fill css
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.x (String.fromInt x2)
                    , Svg.Attributes.y (String.fromInt y)
                    , Svg.Attributes.width "1"
                    , Svg.Attributes.height (String.fromInt h)
                    , Svg.Attributes.fill css
                    ]
                    []
                ]

        RectFill { x, y, w, h, color } ->
            [ Svg.rect
                [ Svg.Attributes.x (String.fromInt x)
                , Svg.Attributes.y (String.fromInt y)
                , Svg.Attributes.width (String.fromInt w)
                , Svg.Attributes.height (String.fromInt h)
                , Svg.Attributes.fill (colorToCss color)
                ]
                []
            ]

        XLine { x, y, len, color } ->
            [ Svg.rect
                [ Svg.Attributes.x (String.fromInt x)
                , Svg.Attributes.y (String.fromInt y)
                , Svg.Attributes.width (String.fromInt len)
                , Svg.Attributes.height "1"
                , Svg.Attributes.fill (colorToCss color)
                ]
                []
            ]

        YLine { x, y, len, color } ->
            [ Svg.rect
                [ Svg.Attributes.x (String.fromInt x)
                , Svg.Attributes.y (String.fromInt y)
                , Svg.Attributes.width "1"
                , Svg.Attributes.height (String.fromInt len)
                , Svg.Attributes.fill (colorToCss color)
                ]
                []
            ]

        Text { x, y, text, fontIndex, color } ->
            case List.Extra.getAt fontIndex fonts of
                Nothing ->
                    []

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

                        drawChar gx gy char =
                            let
                                code =
                                    Char.toCode char
                            in
                            if hasChar char then
                                let
                                    glyphIdx =
                                        code - font.asciiFirst
                                in
                                List.range 0 (font.glyphHeight - 1)
                                    |> List.concatMap
                                        (\r ->
                                            List.range 0 (font.glyphWidth - 1)
                                                |> List.filterMap
                                                    (\c ->
                                                        if renderGlyphPixel font glyphIdx r c then
                                                            Just
                                                                (Svg.rect
                                                                    [ Svg.Attributes.x (String.fromInt (gx + c))
                                                                    , Svg.Attributes.y (String.fromInt (gy + r))
                                                                    , Svg.Attributes.width "1"
                                                                    , Svg.Attributes.height "1"
                                                                    , Svg.Attributes.fill (colorToCss color)
                                                                    ]
                                                                    []
                                                                )

                                                        else
                                                            Nothing
                                                    )
                                        )

                            else
                                []

                        foldChars : Int -> Int -> Int -> List Char -> List (Svg msg)
                        foldChars startX curX curY chars =
                            case chars of
                                [] ->
                                    []

                                '\n' :: rest ->
                                    foldChars startX startX (curY + lineHeight) rest

                                c :: rest ->
                                    let
                                        glyphs =
                                            drawChar curX curY c
                                    in
                                    glyphs
                                        ++ foldChars startX
                                            (curX
                                                + (if hasChar c then
                                                    font.glyphWidth

                                                   else
                                                    0
                                                  )
                                            )
                                            curY
                                            rest
                    in
                    foldChars x x y (String.toList text)

        Group { children } ->
            List.concatMap (renderNodeToSvg fonts) children


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onFailure FailureOccurred
        , case model of
            NotConnected _ ->
                Sub.batch
                    [ onInitialLoadChunkCount InitialLoadStarted
                    , onInitialLoadChunkReceived InitialLoadChunkReceived
                    , onConnectSuccessful_ ConnectSuccessful FailureOccurred
                    ]

            Connected modelConnected ->
                Sub.batch
                    [ onDisconnectSuccessful (\() -> DisconnectSuccessful)
                    , onRootNodeAck (\() -> MsgConnected RootNodeAcked)
                    , case modelConnected.rootNode of
                        RootSynced _ ->
                            Sub.none

                        RootDebouncing _ ->
                            Browser.Events.onAnimationFrame
                                (\posix ->
                                    MsgConnected (DebounceFrame (Time.posixToMillis posix))
                                )

                        RootAwaitingAck _ ->
                            Sub.none

                        RootAwaitingAckWithPending _ ->
                            Browser.Events.onAnimationFrame
                                (\posix ->
                                    MsgConnected (DebounceFrame (Time.posixToMillis posix))
                                )
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


textScene : ESP32 -> VideoConstants -> String -> Int -> Node
textScene esp32 c text fontIndex_ =
    Node.group "main"
        [ Node.text esp32.fonts
            "text-scene"
            { x = c.xMin + 5
            , y = c.yMin + 3
            , text = text
            , fontIndex = fontIndex_
            , color = Color.white
            }
        , Node.rectFill "cross bg"
            { x = c.xMax - 31
            , y = c.yMax - 31
            , w = 25
            , h = 25
            , color = Color.white
            }
        , Node.xLine "cross horiz"
            { x = c.xMax - 30
            , y = c.yMax - 19
            , len = 23
            , color = Color.gray
            }
        , Node.yLine "cross vert"
            { x = c.xMax - 19
            , y = c.yMax - 30
            , len = 23
            , color = Color.gray
            }
        , Node.rect "border shadow"
            { x = c.xMin
            , y = c.yMin
            , w = c.usableWidth
            , h = c.usableHeight
            , color = Color.gray
            }
        , Node.rect "border"
            { x = c.xMin + 1
            , y = c.yMin + 1
            , w = c.usableWidth - 2
            , h = c.usableHeight - 2
            , color = Color.white
            }
        ]

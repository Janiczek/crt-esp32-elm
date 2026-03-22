port module Main exposing (Flags, LoadingProgress, Model(..), ModelConnected, ModelNotConnected, Msg, PreviewContextMenu, PreviewDragState, RootNode(..), desiredRoot, main, update, view)

{-| A web app to connect to and control an ESP32 over WiFi/WebSocket.

The ESP32 is displaying VDOM scenes on a CRT display connected via the GPIO25
DAC pin (NTSC greyscale 400x240 signal).

The point of the web app is to edit the state of the ESP32 on the fly, instead
of changing the C source code and recompiling+reflashing. Also it's pretty cool.

TODO:

  - [ ] edit font data
  - [ ] move nodes _into_ groups (change semantics of moveTreeNodeUp/Down)
  - [ ] upload images and convert them to greyscale bitmap
      - [ ] dithering formats? Applicable for non-BitDepth1?

-}

import Bitmap exposing (BitDepth)
import Bitmap.Bd1_256_16_TestStrip
import Bitmap.Bd1_64_64_Duke
import Bitmap.Bd2_256_16_TestStrip
import Bitmap.Bd2_64_64_Duke
import Bitmap.Bd4_256_16_TestStrip
import Bitmap.Bd4_64_64_Duke
import Bitmap.Bd8_256_16_TestStrip
import Bitmap.Bd8_64_64_Duke
import BoundingBox exposing (BoundingBox)
import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Color exposing (Color)
import Dirty
import ESP32 exposing (ESP32, VideoConstants)
import Font exposing (Font)
import Font.Fallback
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import Html.Events
import Json.Decode
import Json.Encode
import List.Extra
import Node exposing (Node, Type(..))
import Result
import Path
import PreviewDrag
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import Task


type alias Flags =
    ()


type Model
    = NotConnected ModelNotConnected
    | Connected ModelConnected
    | LocalConnected ModelConnected


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
frequently, but it must throttle and it must not send a new root while an
earlier `SetRootNode` command is awaiting ACK.

Roles:

  - `desiredRoot`: the root Elm wants the ESP32 to render next. What Elm shows in the preview.
  - `ackedRoot`: the last root confirmed by an ACK; this is the diff base. What ESP32 is rendering.
  - `inFlightRoot`: a root currently sent to the ESP32 and awaiting ACK. What ESP32 will render next.

Throttle:

  - While throttling (`RootThrottled` / `RootAwaitingAckWithPending`), Elm
    keeps the latest desired root and only flushes on the next animation
    frame.

-}
type RootNode
    = RootSynced
        { root : Node
        }
    | RootThrottled
        { ackedRoot : Node
        , desiredRoot : Node
        }
    | RootAwaitingAck
        { ackedRoot : Node
        , inFlightRoot : Node
        }
    | RootAwaitingAckWithPending
        { ackedRoot : Node
        , inFlightRoot : Node
        , desiredRoot : Node
        }


desiredRoot : RootNode -> Node
desiredRoot rootSyncState =
    case rootSyncState of
        RootSynced { root } ->
            root

        RootThrottled r ->
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
    , selectedPath : Maybe (List Int)
    , previewContextMenu : Maybe PreviewContextMenu
    , previewZoom : Int
    , previewDrag : Maybe PreviewDragState
    , {- the next PreviewClicked should not run normal click-to-select logic
         (we have _just_ dragged a node at least 1px but click will still happen
         right after that mouseup)
      -}
      previewIgnoreClick : Bool
    }


type alias PreviewDragState =
    { path : List Int
    , key : String
    , startClient : ( Float, Float )
    , startNodeXY : ( Int, Int )
    , moved : Bool
    }


type alias PreviewContextMenu =
    { x : Int
    , y : Int
    , paths : List (List Int)
    }


type Msg
    = ConnectRequested
    | ConnectLocalRequested
    | InitialLoadStarted InitialLoadStart
    | InitialLoadChunkReceived Int
    | ConnectSuccessful ESP32
    | DisconnectRequested
    | DisconnectSuccessful
    | FailureOccurred String
    | MsgConnected MsgConnected


type MsgConnected
    = SelectNode (Maybe (List Int))
    | PreviewClicked Int Int
    | PreviewContextMenuRequested Int Int
    | PreviewDragMouseDown { videoX : Int, videoY : Int, clientX : Float, clientY : Float }
    | PreviewDragMove { clientX : Float, clientY : Float }
    | PreviewDragEnd
    | UpdateNodeAtPath (List Int) String Type
    | UpdateNodeAtPathAndRefocusPreview (List Int) String Type
    | PreviewFocusAttempted
    | InsertChild (List Int) Int Type
    | RemoveNode (List Int)
    | MoveTreeNodeUp (List Int)
    | MoveTreeNodeDown (List Int)
    | SetPreviewZoom Int
    | SetRootNodeJsonText String
    | RestoreRootNodeJson
    | ThrottleFrame
    | RootNodeAcked
    | SelectedUnknown String String
    | -- done for StopPropagation
      InteractedWithSelect
    | NudgeSelectedNode Int Int


port connect : () -> Cmd msg


port disconnect : () -> Cmd msg


port onInitialLoadChunkCount : (InitialLoadStart -> msg) -> Sub msg


port onInitialLoadChunkReceived : (Int -> msg) -> Sub msg


port onConnectSuccessful : (Bytes -> msg) -> Sub msg


port onDisconnectSuccessful : (() -> msg) -> Sub msg


port onFailure : (String -> msg) -> Sub msg


port onRootNodeAck : (() -> msg) -> Sub msg


port sendRootNode : Bytes -> Cmd msg


addChildValidationError : ModelConnected -> List Int -> Type -> Maybe Node.LimitError
addChildValidationError model parentPath type_ =
    let
        root : Node
        root =
            desiredRoot model.rootNode

        insertIndex : Int
        insertIndex =
            Path.getNodeAtPath parentPath root
                |> Maybe.andThen
                    (\n ->
                        case n.type_ of
                            Node.Group { children } ->
                                Just (List.length children)

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault 0

        candidateRoot : Node
        candidateRoot =
            Path.insertChildAtPath
                parentPath
                insertIndex
                (newChildForInsert model.esp32.fonts model.videoConstants root type_)
                root
    in
    if candidateRoot.hash == root.hash then
        Nothing

    else
        case Node.validateLimits model.esp32 candidateRoot of
            Ok _ ->
                Nothing

            Err errs ->
                List.head errs


addChildLimitWarning : Node.LimitError -> String
addChildLimitWarning err =
    case err of
        Node.MaxTotalNodesExceeded { maxTotalNodes } ->
            "Add disabled: max total nodes reached (" ++ String.fromInt maxTotalNodes ++ ")."

        Node.NodeGroupMaxChildrenExceeded { maxChildren } ->
            "Add disabled: group child limit reached (" ++ String.fromInt maxChildren ++ ")."


type alias EmbeddedBitmap =
    { label : String
    , w : Int
    , h : Int
    , bitDepth : BitDepth
    , data : List Int
    }


embeddedFromTuple : String -> ( ( Int, Int ), BitDepth, List Int ) -> EmbeddedBitmap
embeddedFromTuple label ( ( w, h ), bitDepth, data ) =
    { label = label
    , w = w
    , h = h
    , bitDepth = bitDepth
    , data = data
    }


{-| Every `Bitmap.*` asset imported in this module; used by the bitmap preset picker.
Adding a new `import Bitmap.*` should add a matching entry here.
-}
allBitmaps : List EmbeddedBitmap
allBitmaps =
    [ embeddedFromTuple "Duke 64×64 · 1-bit" Bitmap.Bd1_64_64_Duke.bd1_64_64_duke
    , embeddedFromTuple "Duke 64×64 · 2-bit" Bitmap.Bd2_64_64_Duke.bd2_64_64_duke
    , embeddedFromTuple "Duke 64×64 · 4-bit" Bitmap.Bd4_64_64_Duke.bd4_64_64_duke
    , embeddedFromTuple "Duke 64×64 · 8-bit" Bitmap.Bd8_64_64_Duke.bd8_64_64_duke
    , embeddedFromTuple "Test strip 256×16 · 1-bit" Bitmap.Bd1_256_16_TestStrip.bd1_256_16_teststrip
    , embeddedFromTuple "Test strip 256×16 · 2-bit" Bitmap.Bd2_256_16_TestStrip.bd2_256_16_teststrip
    , embeddedFromTuple "Test strip 256×16 · 4-bit" Bitmap.Bd4_256_16_TestStrip.bd4_256_16_teststrip
    , embeddedFromTuple "Test strip 256×16 · 8-bit" Bitmap.Bd8_256_16_TestStrip.bd8_256_16_teststrip
    ]


embeddedBitmapMatchIndex : { a | w : Int, h : Int, bitDepth : BitDepth, data : List Int } -> Maybe Int
embeddedBitmapMatchIndex r =
    allBitmaps
        |> List.indexedMap Tuple.pair
        |> List.Extra.find
            (\( _, e ) ->
                e.w == r.w && e.h == r.h && e.bitDepth == r.bitDepth && e.data == r.data
            )
        |> Maybe.map Tuple.first


defaultBitmapConfig :
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    , bitDepth : BitDepth
    , data : List Int
    }
defaultBitmapConfig =
    let
        ( ( w, h ), bitDepth, data ) =
            Bitmap.Bd8_256_16_TestStrip.bd8_256_16_teststrip
    in
    { x = 0
    , y = 0
    , w = w
    , h = h
    , bitDepth = bitDepth
    , data = data
    }


defaultNodeForType : List Font -> VideoConstants -> Type -> Node
defaultNodeForType fonts vc type_ =
    let
        x : Int
        x =
            vc.xMin

        y : Int
        y =
            vc.yMin

        c : Color
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

        Bitmap _ ->
            Node.bitmap "bitmap" { defaultBitmapConfig | x = x, y = y }

        Group _ ->
            Node.group "group" []


{-| Default child for insert, with a key that does not collide with any key under `root`.
-}
newChildForInsert : List Font -> VideoConstants -> Node -> Type -> Node
newChildForInsert fonts vc root type_ =
    let
        prototype : Node
        prototype =
            defaultNodeForType fonts vc type_

        key : String
        key =
            Node.uniqueKeyAmong (Node.allKeys root) prototype.key
    in
    Node.fromKeyAndType fonts key prototype.type_


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
        |> Json.Encode.encode 0


{-| The throttle logic in subscriptions will automatically pick this up and send a command.
-}
commitNewRootNode : Node -> ModelConnected -> ModelConnected
commitNewRootNode newRoot modelConnected =
    let
        json : String
        json =
            encodeNodeJson newRoot

        newRootNode : RootNode
        newRootNode =
            case modelConnected.rootNode of
                RootSynced { root } ->
                    RootThrottled
                        { ackedRoot = root
                        , desiredRoot = newRoot
                        }

                RootThrottled { ackedRoot } ->
                    RootThrottled
                        { ackedRoot = ackedRoot
                        , desiredRoot = newRoot
                        }

                RootAwaitingAck { ackedRoot, inFlightRoot } ->
                    RootAwaitingAckWithPending
                        { ackedRoot = ackedRoot
                        , inFlightRoot = inFlightRoot
                        , desiredRoot = newRoot
                        }

                RootAwaitingAckWithPending { ackedRoot, inFlightRoot } ->
                    RootAwaitingAckWithPending
                        { ackedRoot = ackedRoot
                        , inFlightRoot = inFlightRoot
                        , desiredRoot = newRoot
                        }
    in
    { modelConnected
        | rootNode = newRootNode
        , rootNodeJsonText = json
        , rootNodeJsonError = Nothing
    }


commitNewRootNodeLocal : Node -> ModelConnected -> ModelConnected
commitNewRootNodeLocal newRoot modelConnected =
    { modelConnected
        | rootNode = RootSynced { root = newRoot }
        , rootNodeJsonText = encodeNodeJson newRoot
        , rootNodeJsonError = Nothing
    }


type alias ConnectedUpdateConfig =
    { commitRoot : Node -> ModelConnected -> ModelConnected
    , syncToDevice : Bool
    }


commitUpdateNodeAtPath : ConnectedUpdateConfig -> ModelConnected -> List Int -> String -> Type -> ModelConnected
commitUpdateNodeAtPath cfg modelConnected path key type_ =
    let
        existing : Maybe Node
        existing =
            Path.getNodeAtPath path (desiredRoot modelConnected.rootNode)

        newRoot : Node
        newRoot =
            case existing of
                Just _ ->
                    let
                        newNode : Node
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
                    in
                    Path.setNodeAtPath path newNode (desiredRoot modelConnected.rootNode)

                Nothing ->
                    desiredRoot modelConnected.rootNode
    in
    { modelConnected | previewContextMenu = Nothing }
        |> cfg.commitRoot newRoot


previewSurfaceDomId : String
previewSurfaceDomId =
    "preview-surface"


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

                LocalConnected _ ->
                    model
            , connect ()
            )

        ConnectLocalRequested ->
            let
                ( ( dukeW, dukeH ), dukeBitDepth, dukeData ) =
                    Bitmap.Bd4_64_64_Duke.bd4_64_64_duke

                localEsp32 : ESP32
                localEsp32 =
                    { videoWidth = 400
                    , videoHeight = 240
                    , crtPaddingLeft = 0
                    , crtPaddingRight = 0
                    , crtPaddingTop = 0
                    , crtPaddingBottom = 0
                    , maxTotalNodes = 1024
                    , nodeGroupMaxChildren = 128
                    , tileSize = 8
                    , fonts = [ Font.Fallback.fallback ]
                    }

                root : Node
                root =
                    Node.group "root"
                        [ Node.rect "frame"
                            { x = 12
                            , y = 12
                            , w = 120
                            , h = 68
                            , color = Color.white
                            }
                        , Node.bitmap "duke"
                            { x = 24
                            , y = 24
                            , w = dukeW
                            , h = dukeH
                            , bitDepth = dukeBitDepth
                            , data = dukeData
                            }
                        ]
            in
            ( LocalConnected
                { esp32 = localEsp32
                , videoConstants = ESP32.videoConstants localEsp32
                , lastError = ""
                , rootNode = RootSynced { root = root }
                , rootNodeJsonText = encodeNodeJson root
                , rootNodeJsonError = Nothing
                , selectedPath = Nothing
                , previewContextMenu = Nothing
                , previewZoom = 3
                , previewDrag = Nothing
                , previewIgnoreClick = False
                }
            , Cmd.none
            )

        InitialLoadStarted initialLoadStart ->
            case model of
                NotConnected nc ->
                    let
                        sanitizedChunkCount : Int
                        sanitizedChunkCount =
                            max 0 initialLoadStart.expectedChunkCount
                    in
                    ( NotConnected
                        { nc
                            | loadingProgress =
                                if sanitizedChunkCount > 0 then
                                    let
                                        sanitizedTotalBytes : Int
                                        sanitizedTotalBytes =
                                            max 0 initialLoadStart.expectedTotalBytes
                                    in
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

                LocalConnected _ ->
                    ( model, Cmd.none )

        InitialLoadChunkReceived chunkBytes ->
            case model of
                NotConnected nc ->
                    ( NotConnected
                        { nc
                            | loadingProgress =
                                nc.loadingProgress
                                    |> Maybe.map
                                        (\progress ->
                                            let
                                                sanitizedChunkBytes : Int
                                                sanitizedChunkBytes =
                                                    max 0 chunkBytes
                                            in
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

                LocalConnected _ ->
                    ( model, Cmd.none )

        ConnectSuccessful esp32 ->
            let
                videoConstants_ : VideoConstants
                videoConstants_ =
                    ESP32.videoConstants esp32

                node : Node
                node =
                    Node.empty

                json : String
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
                , selectedPath = Nothing
                , previewContextMenu = Nothing
                , previewZoom = 3
                , previewDrag = Nothing
                , previewIgnoreClick = False
                }
            , Cmd.none
            )

        DisconnectRequested ->
            case model of
                Connected _ ->
                    ( model, disconnect () )

                LocalConnected _ ->
                    ( NotConnected emptyModelNotConnected
                    , Cmd.none
                    )

                NotConnected _ ->
                    ( model, Cmd.none )

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

                LocalConnected c ->
                    ( LocalConnected { c | lastError = error }, Cmd.none )

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

                LocalConnected modelConnected ->
                    let
                        ( newModelConnected, cmdMsgConnected ) =
                            updateConnectedLocal msgConnected modelConnected
                    in
                    ( LocalConnected newModelConnected
                    , Cmd.map MsgConnected cmdMsgConnected
                    )


updateConnected : MsgConnected -> ModelConnected -> ( ModelConnected, Cmd MsgConnected )
updateConnected =
    updateConnected_
        { commitRoot = commitNewRootNode
        , syncToDevice = True
        }


updateConnectedLocal : MsgConnected -> ModelConnected -> ( ModelConnected, Cmd MsgConnected )
updateConnectedLocal =
    updateConnected_
        { commitRoot = commitNewRootNodeLocal
        , syncToDevice = False
        }


updateConnected_ :
    ConnectedUpdateConfig
    -> MsgConnected
    -> ModelConnected
    -> ( ModelConnected, Cmd MsgConnected )
updateConnected_ cfg msgConnected modelConnected =
    case msgConnected of
        SelectNode path ->
            ( { modelConnected
                | selectedPath = path
                , previewContextMenu = Nothing
                , previewDrag = Nothing
                , previewIgnoreClick = False
              }
            , case path of
                Just _ ->
                    Browser.Dom.focus previewSurfaceDomId
                        |> Task.attempt (\_ -> PreviewFocusAttempted)

                Nothing ->
                    Cmd.none
            )

        PreviewClicked x y ->
            if modelConnected.previewIgnoreClick then
                ( { modelConnected | previewIgnoreClick = False }
                , Cmd.none
                )

            else
                let
                    hits : List (List Int)
                    hits =
                        Node.hitPathsAtPixel
                            x
                            y
                            (desiredRoot modelConnected.rootNode)
                in
                ( { modelConnected
                    | selectedPath = List.head hits
                    , previewContextMenu = Nothing
                  }
                , Cmd.none
                )

        PreviewContextMenuRequested x y ->
            let
                hits : List (List Int)
                hits =
                    Node.hitPathsAtPixel
                        x
                        y
                        (desiredRoot modelConnected.rootNode)
            in
            if List.isEmpty hits then
                ( { modelConnected
                    | selectedPath = Nothing
                    , previewContextMenu = Nothing
                  }
                , Cmd.none
                )

            else
                ( { modelConnected
                    | previewContextMenu =
                        Just
                            { x = x
                            , y = y
                            , paths = hits
                            }
                  }
                , Cmd.none
                )

        PreviewDragMouseDown { videoX, videoY, clientX, clientY } ->
            ( tryStartPreviewDrag videoX videoY clientX clientY modelConnected
            , Cmd.none
            )

        PreviewDragMove { clientX, clientY } ->
            ( applyPreviewDragMove cfg.commitRoot clientX clientY modelConnected
            , Cmd.none
            )

        PreviewDragEnd ->
            ( applyPreviewDragEnd modelConnected
            , Cmd.none
            )

        UpdateNodeAtPath path key type_ ->
            ( commitUpdateNodeAtPath cfg modelConnected path key type_
            , Cmd.none
            )

        UpdateNodeAtPathAndRefocusPreview path key type_ ->
            ( commitUpdateNodeAtPath cfg modelConnected path key type_
            , Browser.Dom.focus previewSurfaceDomId
                |> Task.attempt (\_ -> PreviewFocusAttempted)
            )

        PreviewFocusAttempted ->
            ( modelConnected, Cmd.none )

        InsertChild parentPath index type_ ->
            let
                root : Node
                root =
                    desiredRoot modelConnected.rootNode

                newChild : Node
                newChild =
                    newChildForInsert modelConnected.esp32.fonts modelConnected.videoConstants root type_

                newRoot : Node
                newRoot =
                    Path.insertChildAtPath parentPath index newChild root
            in
            newRoot
                |> Node.validateLimits modelConnected.esp32
                |> Result.toMaybe
                |> Maybe.map
                    (\validRoot ->
                        ( { modelConnected
                            | previewContextMenu = Nothing
                            , previewDrag = Nothing
                            , selectedPath = Just (parentPath ++ [ index ])
                          }
                            |> cfg.commitRoot validRoot
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault
                    ( { modelConnected | previewContextMenu = Nothing }, Cmd.none )

        RemoveNode path ->
            let
                newRoot : Node
                newRoot =
                    Path.removeNodeAtPath path (desiredRoot modelConnected.rootNode)
            in
            ( { modelConnected
                | selectedPath =
                    modelConnected.selectedPath
                        |> Maybe.andThen
                            (\selectedPath ->
                                if path |> List.Extra.isPrefixOf selectedPath then
                                    Nothing

                                else
                                    Just selectedPath
                            )
                , previewContextMenu = Nothing
                , previewDrag = Nothing
              }
                |> cfg.commitRoot newRoot
            , Cmd.none
            )

        MoveTreeNodeUp path ->
            let
                root : Node
                root =
                    desiredRoot modelConnected.rootNode
            in
            root
                |> Path.moveTreeNodeUp path
                |> Maybe.andThen
                    (\candidate ->
                        Node.validateLimits modelConnected.esp32 candidate
                            |> Result.toMaybe
                    )
                |> Maybe.map
                    (\validRoot ->
                        ( { modelConnected
                            | previewContextMenu = Nothing
                            , previewDrag = Nothing
                            , selectedPath =
                                modelConnected.selectedPath
                                    |> Maybe.andThen
                                        (\p ->
                                            Path.getNodeAtPath p root
                                                |> Maybe.andThen (\n -> Path.findPathByKey n.key validRoot)
                                        )
                          }
                            |> cfg.commitRoot validRoot
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault
                    ( { modelConnected | previewContextMenu = Nothing }, Cmd.none )

        MoveTreeNodeDown path ->
            let
                root : Node
                root =
                    desiredRoot modelConnected.rootNode
            in
            root
                |> Path.moveTreeNodeDown path
                |> Maybe.andThen
                    (\candidate ->
                        Node.validateLimits modelConnected.esp32 candidate
                            |> Result.toMaybe
                    )
                |> Maybe.map
                    (\validRoot ->
                        ( { modelConnected
                            | previewContextMenu = Nothing
                            , previewDrag = Nothing
                            , selectedPath =
                                modelConnected.selectedPath
                                    |> Maybe.andThen
                                        (\p ->
                                            Path.getNodeAtPath p root
                                                |> Maybe.andThen (\n -> Path.findPathByKey n.key validRoot)
                                        )
                          }
                            |> cfg.commitRoot validRoot
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault
                    ( { modelConnected | previewContextMenu = Nothing }, Cmd.none )

        SetPreviewZoom zoom ->
            ( { modelConnected
                | previewZoom = zoom
                , previewContextMenu = Nothing
                , previewDrag = Nothing
              }
            , Cmd.none
            )

        SetRootNodeJsonText text ->
            case Json.Decode.decodeString (Node.jsonDecoder modelConnected.esp32) text of
                Ok parsed ->
                    ( { modelConnected | previewContextMenu = Nothing, previewDrag = Nothing }
                        |> cfg.commitRoot parsed
                    , Cmd.none
                    )

                Err err ->
                    ( { modelConnected
                        | rootNodeJsonText = text
                        , rootNodeJsonError = Just (Json.Decode.errorToString err)
                        , previewContextMenu = Nothing
                      }
                    , Cmd.none
                    )

        RestoreRootNodeJson ->
            ( { modelConnected
                | rootNodeJsonText = encodeNodeJson (desiredRoot modelConnected.rootNode)
                , rootNodeJsonError = Nothing
                , previewContextMenu = Nothing
                , previewDrag = Nothing
              }
            , Cmd.none
            )

        ThrottleFrame ->
            if cfg.syncToDevice then
                case modelConnected.rootNode of
                    RootSynced _ ->
                        ( modelConnected, Cmd.none )

                    RootThrottled r ->
                        if r.ackedRoot.hash == r.desiredRoot.hash then
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

                    RootAwaitingAckWithPending _ ->
                        -- Can't send while waiting for ACK; keep the latest desired root queued.
                        ( modelConnected, Cmd.none )

            else
                ( modelConnected, Cmd.none )

        RootNodeAcked ->
            if cfg.syncToDevice then
                case modelConnected.rootNode of
                    RootAwaitingAck { inFlightRoot } ->
                        ( { modelConnected | rootNode = RootSynced { root = inFlightRoot } }
                        , Cmd.none
                        )

                    RootAwaitingAckWithPending r ->
                        let
                            acked : Node
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
                                    RootThrottled
                                        { ackedRoot = acked
                                        , desiredRoot = r.desiredRoot
                                        }
                              }
                            , Cmd.none
                            )

                    _ ->
                        ( modelConnected, Cmd.none )

            else
                ( modelConnected, Cmd.none )

        InteractedWithSelect ->
            ( modelConnected, Cmd.none )

        NudgeSelectedNode dx dy ->
            case modelConnected.selectedPath of
                Nothing ->
                    ( modelConnected, Cmd.none )

                Just path ->
                    let
                        root : Node
                        root =
                            desiredRoot modelConnected.rootNode
                    in
                    case Path.getNodeAtPath path root of
                        Nothing ->
                            ( modelConnected, Cmd.none )

                        Just node ->
                            let
                                doNudge : () -> ModelConnected
                                doNudge () =
                                    nudgePositionedLeafAtPath cfg.commitRoot dx dy modelConnected path node root
                            in
                            ( case node.type_ of
                                Group _ ->
                                    modelConnected

                                Rect _ ->
                                    doNudge ()

                                RectFill _ ->
                                    doNudge ()

                                XLine _ ->
                                    doNudge ()

                                YLine _ ->
                                    doNudge ()

                                Text _ ->
                                    doNudge ()

                                Bitmap _ ->
                                    doNudge ()
                            , Cmd.none
                            )

        SelectedUnknown s v ->
            Debug.todo ("SelectedUnknown " ++ s ++ " " ++ v)


setRootNode : ESP32 -> VideoConstants -> Node -> Node -> Cmd MsgConnected
setRootNode esp32 videoConstants previousNode node =
    if node.hash == previousNode.hash then
        Cmd.none

    else
        let
            dirtyTiles : Set ( Int, Int )
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

            bytes : Bytes
            bytes =
                Bytes.Encode.sequence
                    [ Node.bytesEncoder node
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
            viewConnected { isLocal = False } modelConnected

        LocalConnected modelConnected ->
            viewConnected { isLocal = True } modelConnected


viewNotConnected : ModelNotConnected -> Html Msg
viewNotConnected model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "padding" "0.35rem"
        ]
        (List.concat
            [ [ Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "gap" "0.4rem"
                    , Html.Attributes.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.text "Not connected."
                    , Html.button
                        [ Html.Events.onClick ConnectRequested ]
                        [ Html.text "Connect" ]
                    , Html.button
                        [ Html.Events.onClick ConnectLocalRequested ]
                        [ Html.text "Local" ]
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
        widthPercent : String
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

        kilobytesText : Int -> String
        kilobytesText bytes =
            let
                tenths : Int
                tenths =
                    round ((toFloat bytes * 10) / 1024)

                whole : Int
                whole =
                    tenths // 10

                fractional : Int
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


previewPointDecoder : VideoConstants -> Int -> Json.Decode.Decoder ( Int, Int )
previewPointDecoder vc zoom =
    let
        zoom_ : Int
        zoom_ =
            max 1 zoom

        coordinateDecoder : String -> Int -> Int -> Json.Decode.Decoder Int
        coordinateDecoder field min_ max_ =
            Json.Decode.oneOf
                [ Json.Decode.field field Json.Decode.float
                , Json.Decode.field field Json.Decode.int |> Json.Decode.map toFloat
                ]
                |> Json.Decode.map
                    (\offset ->
                        clamp min_ max_ (min_ + (floor offset // zoom_))
                    )
    in
    Json.Decode.map2 Tuple.pair
        (coordinateDecoder "offsetX" vc.xMin vc.xMax)
        (coordinateDecoder "offsetY" vc.yMin vc.yMax)


previewDragMouseDownDecoder : VideoConstants -> Int -> Json.Decode.Decoder Msg
previewDragMouseDownDecoder vc zoom =
    previewPointDecoder vc zoom
        |> Json.Decode.andThen
            (\( vx, vy ) ->
                Json.Decode.map2 Tuple.pair
                    (Json.Decode.map2 Tuple.pair
                        (Json.Decode.field "clientX" Json.Decode.float)
                        (Json.Decode.field "clientY" Json.Decode.float)
                    )
                    (Json.Decode.field "button" Json.Decode.int)
                    |> Json.Decode.andThen
                        (\( ( cx, cy ), btn ) ->
                            if btn == 0 then
                                Json.Decode.succeed
                                    (MsgConnected
                                        (PreviewDragMouseDown
                                            { videoX = vx, videoY = vy, clientX = cx, clientY = cy }
                                        )
                                    )

                            else
                                Json.Decode.fail "not primary button"
                        )
            )


previewDragMoveDecoder : Json.Decode.Decoder Msg
previewDragMoveDecoder =
    Json.Decode.map2 (\cx cy -> MsgConnected (PreviewDragMove { clientX = cx, clientY = cy }))
        (Json.Decode.oneOf
            [ Json.Decode.field "clientX" Json.Decode.float
            , Json.Decode.field "clientX" Json.Decode.int |> Json.Decode.map toFloat
            ]
        )
        (Json.Decode.oneOf
            [ Json.Decode.field "clientY" Json.Decode.float
            , Json.Decode.field "clientY" Json.Decode.int |> Json.Decode.map toFloat
            ]
        )


previewArrowKeyDecoder : Json.Decode.Decoder ( Msg, Bool )
previewArrowKeyDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "shiftKey" Json.Decode.bool)
        |> Json.Decode.andThen
            (\( key, shift ) ->
                let
                    step : Int
                    step =
                        if shift then
                            4

                        else
                            1
                in
                case key of
                    "ArrowLeft" ->
                        Json.Decode.succeed ( MsgConnected (NudgeSelectedNode -step 0), True )

                    "ArrowRight" ->
                        Json.Decode.succeed ( MsgConnected (NudgeSelectedNode step 0), True )

                    "ArrowUp" ->
                        Json.Decode.succeed ( MsgConnected (NudgeSelectedNode 0 -step), True )

                    "ArrowDown" ->
                        Json.Decode.succeed ( MsgConnected (NudgeSelectedNode 0 step), True )

                    _ ->
                        Json.Decode.fail "not an arrow nudge key"
            )


tryStartPreviewDrag : Int -> Int -> Float -> Float -> ModelConnected -> ModelConnected
tryStartPreviewDrag videoX videoY clientX clientY modelConnected =
    let
        root : Node
        root =
            desiredRoot modelConnected.rootNode

        hits : List (List Int)
        hits =
            Node.hitPathsAtPixel videoX videoY root
    in
    case List.head hits of
        Nothing ->
            modelConnected

        Just path ->
            case Path.getNodeAtPath path root of
                Nothing ->
                    modelConnected

                Just node ->
                    case node.type_ of
                        Group _ ->
                            modelConnected

                        _ ->
                            case Node.topLeftXY node.type_ of
                                Nothing ->
                                    modelConnected

                                Just startXY ->
                                    { modelConnected
                                        | selectedPath = Just path
                                        , previewContextMenu = Nothing
                                        , previewDrag =
                                            Just
                                                { path = path
                                                , key = node.key
                                                , startClient = ( clientX, clientY )
                                                , startNodeXY = startXY
                                                , moved = False
                                                }
                                    }


applyPreviewDragMove :
    (Node -> ModelConnected -> ModelConnected)
    -> Float
    -> Float
    -> ModelConnected
    -> ModelConnected
applyPreviewDragMove commitRoot clientX clientY modelConnected =
    case modelConnected.previewDrag of
        Nothing ->
            modelConnected

        Just drag ->
            let
                root : Node
                root =
                    desiredRoot modelConnected.rootNode
            in
            case Path.getNodeAtPath drag.path root of
                Nothing ->
                    { modelConnected | previewDrag = Nothing }

                Just node ->
                    if node.key /= drag.key then
                        { modelConnected | previewDrag = Nothing }

                    else
                        case node.type_ of
                            Group _ ->
                                { modelConnected | previewDrag = Nothing }

                            _ ->
                                case Node.topLeftXY node.type_ of
                                    Nothing ->
                                        { modelConnected | previewDrag = Nothing }

                                    Just ( curX, curY ) ->
                                        let
                                            ( newX, newY ) =
                                                PreviewDrag.clampedNodeXYFromClientDrag
                                                    modelConnected.videoConstants
                                                    modelConnected.previewZoom
                                                    drag.startClient
                                                    ( clientX, clientY )
                                                    drag.startNodeXY

                                            newMoved : Bool
                                            newMoved =
                                                drag.moved
                                                    || (( newX, newY ) /= drag.startNodeXY)

                                            nextDrag : Maybe PreviewDragState
                                            nextDrag =
                                                Just { drag | moved = newMoved }
                                        in
                                        if newX == curX && newY == curY then
                                            { modelConnected | previewDrag = nextDrag }

                                        else
                                            let
                                                newType : Type
                                                newType =
                                                    Node.typeWithNewXY newX newY node.type_

                                                newNode : Node
                                                newNode =
                                                    Node.fromKeyAndType modelConnected.esp32.fonts node.key newType

                                                newRoot : Node
                                                newRoot =
                                                    Path.setNodeAtPath drag.path newNode root
                                            in
                                            { modelConnected
                                                | previewContextMenu = Nothing
                                                , previewDrag = nextDrag
                                            }
                                                |> commitRoot newRoot


applyPreviewDragEnd : ModelConnected -> ModelConnected
applyPreviewDragEnd modelConnected =
    case modelConnected.previewDrag of
        Nothing ->
            modelConnected

        Just drag ->
            { modelConnected
                | previewDrag = Nothing
                , previewIgnoreClick = drag.moved
            }


nudgePositionedLeafAtPath :
    (Node -> ModelConnected -> ModelConnected)
    -> Int
    -> Int
    -> ModelConnected
    -> List Int
    -> Node
    -> Node
    -> ModelConnected
nudgePositionedLeafAtPath commitRoot dx dy modelConnected path node root =
    case Node.topLeftXY node.type_ of
        Nothing ->
            modelConnected

        Just ( x, y ) ->
            let
                ( newX, newY ) =
                    PreviewDrag.clampedNodeNudge modelConnected.videoConstants dx dy ( x, y )
            in
            if newX == x && newY == y then
                modelConnected

            else
                let
                    newType : Type
                    newType =
                        Node.typeWithNewXY newX newY node.type_

                    newNode : Node
                    newNode =
                        Node.fromKeyAndType modelConnected.esp32.fonts node.key newType

                    newRoot : Node
                    newRoot =
                        Path.setNodeAtPath path newNode root
                in
                { modelConnected
                    | previewContextMenu = Nothing
                    , previewDrag = Nothing
                }
                    |> commitRoot newRoot


previewDragSubscriptions : Maybe PreviewDragState -> Sub Msg
previewDragSubscriptions drag =
    case drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove previewDragMoveDecoder
                , Browser.Events.onMouseUp (Json.Decode.succeed (MsgConnected PreviewDragEnd))
                ]


selectionBorderView : VideoConstants -> Int -> Maybe Node -> Html msg
selectionBorderView vc zoom maybeNode =
    case maybeNode of
        Nothing ->
            Html.text ""

        Just node ->
            let
                bbox : BoundingBox
                bbox =
                    node.bbox
            in
            if bbox.w <= 0 || bbox.h <= 0 then
                Html.text ""

            else
                let
                    leftPx : Int
                    leftPx =
                        (bbox.x - vc.xMin) * zoom

                    topPx : Int
                    topPx =
                        (bbox.y - vc.yMin) * zoom

                    widthPx : Int
                    widthPx =
                        max 1 (bbox.w * zoom)

                    heightPx : Int
                    heightPx =
                        max 1 (bbox.h * zoom)

                    borderStyle : String
                    borderStyle =
                        case node.type_ of
                            Group _ ->
                                "dashed"

                            Rect _ ->
                                "solid"

                            RectFill _ ->
                                "solid"

                            XLine _ ->
                                "solid"

                            YLine _ ->
                                "solid"

                            Text _ ->
                                "solid"

                            Bitmap _ ->
                                "solid"
                in
                Html.div
                    [ Html.Attributes.id "preview-selection-border"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "left" (String.fromInt leftPx ++ "px")
                    , Html.Attributes.style "top" (String.fromInt topPx ++ "px")
                    , Html.Attributes.style "width" (String.fromInt widthPx ++ "px")
                    , Html.Attributes.style "height" (String.fromInt heightPx ++ "px")
                    , Html.Attributes.style "border" "1px solid #ffd60a"
                    , Html.Attributes.style "border-style" borderStyle
                    , Html.Attributes.style "box-sizing" "border-box"
                    , Html.Attributes.style "pointer-events" "none"
                    , Html.Attributes.style "z-index" "1"
                    ]
                    []


previewContextMenuView : VideoConstants -> Int -> Node -> PreviewContextMenu -> Html Msg
previewContextMenuView vc zoom root previewContextMenu =
    let
        menuButton : List Int -> Node -> Html Msg
        menuButton path node =
            Html.button
                [ Html.Events.onClick (MsgConnected (SelectNode (Just path)))
                , Html.Attributes.style "display" "block"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "text-align" "left"
                , Html.Attributes.style "border-radius" "6px"
                ]
                [ Html.text (Node.displayLabel node) ]

        items : List (Html Msg)
        items =
            previewContextMenu.paths
                |> List.filterMap
                    (\path ->
                        Path.getNodeAtPath path root
                            |> Maybe.map (\node -> menuButton path node)
                    )
    in
    Html.div
        [ Html.Attributes.id "preview-layer-menu"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" (String.fromInt ((previewContextMenu.x - vc.xMin) * zoom) ++ "px")
        , Html.Attributes.style "top" (String.fromInt ((previewContextMenu.y - vc.yMin) * zoom) ++ "px")
        , Html.Attributes.style "z-index" "2"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "0.25rem"
        , Html.Attributes.style "min-width" "12rem"
        , Html.Attributes.style "padding" "0.35rem"
        , Html.Attributes.style "background" "var(--surface-2)"
        , Html.Attributes.style "border" "1px solid var(--border)"
        , Html.Attributes.style "border-radius" "8px"
        , Html.Attributes.style "box-shadow" "var(--shadow)"
        ]
        items


viewPreviewHeader : ModelConnected -> Html Msg
viewPreviewHeader model =
    let
        zoom : Int
        zoom =
            model.previewZoom

        zoomButton : Int -> Html Msg
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
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "gap" "0.4rem"
        ]
        [ Html.div
            [ Html.Attributes.style "font-size" "0.8125rem"
            ]
            [ Html.text "Preview" ]
        , zoomButton 1
        , zoomButton 2
        , zoomButton 3
        ]


viewPreviewSurface : ModelConnected -> Html Msg
viewPreviewSurface model =
    let
        vc : VideoConstants
        vc =
            model.videoConstants

        zoom : Int
        zoom =
            model.previewZoom

        root : Node
        root =
            desiredRoot model.rootNode

        selectedNode : Maybe Node
        selectedNode =
            model.selectedPath
                |> Maybe.andThen (\path -> Path.getNodeAtPath path root)

        zoomedW : Int
        zoomedW =
            vc.usableWidth * zoom

        zoomedH : Int
        zoomedH =
            vc.usableHeight * zoom

        clickDecoder : Json.Decode.Decoder Msg
        clickDecoder =
            previewPointDecoder vc zoom
                |> Json.Decode.map (\( x, y ) -> MsgConnected (PreviewClicked x y))

        contextMenuDecoder : Json.Decode.Decoder ( Msg, Bool )
        contextMenuDecoder =
            previewPointDecoder vc zoom
                |> Json.Decode.map
                    (\( x, y ) ->
                        ( MsgConnected (PreviewContextMenuRequested x y)
                        , True
                        )
                    )
    in
    Html.div
        [ Html.Attributes.style "width" (String.fromInt zoomedW ++ "px")
        , Html.Attributes.style "height" (String.fromInt zoomedH ++ "px")
        , Html.Attributes.style "flex-shrink" "0"
        , Html.Attributes.style "position" "relative"
        , Html.Attributes.style "overflow" "visible"
        ]
        [ Html.div
            [ Html.Attributes.id previewSurfaceDomId
            , Html.Attributes.tabindex 0
            , Html.Attributes.style "outline" "none"
            , Html.Attributes.style "width" (String.fromInt zoomedW ++ "px")
            , Html.Attributes.style "height" (String.fromInt zoomedH ++ "px")
            , Html.Events.on "click" clickDecoder
            , Html.Events.on "mousedown" (previewDragMouseDownDecoder vc zoom)
            , Html.Events.preventDefaultOn "contextmenu" contextMenuDecoder
            , Html.Events.preventDefaultOn "keydown" previewArrowKeyDecoder
            ]
            [ Html.div
                [ Html.Attributes.style "width" (String.fromInt vc.usableWidth ++ "px")
                , Html.Attributes.style "height" (String.fromInt vc.usableHeight ++ "px")
                , Html.Attributes.style "transform" ("scale(" ++ String.fromInt zoom ++ ")")
                , Html.Attributes.style "transform-origin" "top left"
                , Html.Attributes.style "background" "black"
                , Html.Attributes.style "overflow" "hidden"
                , Html.Attributes.style "image-rendering" "pixelated"
                , Html.Attributes.style "image-rendering" "crisp-edges"
                , Html.Attributes.style "pointer-events" "none"
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
                    , Svg.Attributes.style "display:block;pointer-events:none"
                    ]
                    (renderNodeToSvg model.esp32.fonts model.selectedPath [] root)
                ]
            ]
        , selectionBorderView vc zoom selectedNode
        , case model.previewContextMenu of
            Just previewContextMenu ->
                previewContextMenuView vc zoom root previewContextMenu

            Nothing ->
                Html.text ""
        ]


viewSidebarColumn : ModelConnected -> Html Msg
viewSidebarColumn model =
    Html.div
        [ Html.Attributes.style "flex" "0 0 260px"
        , Html.Attributes.style "width" "260px"
        , Html.Attributes.style "min-width" "260px"
        , Html.Attributes.style "max-width" "260px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "min-height" "0"
        , Html.Attributes.style "overflow" "hidden"
        , Html.Attributes.style "gap" "0.4rem"
        ]
        [ viewTreeColumn model
        , viewDetailsColumn model
        , viewRootNodeJsonColumn model
        ]


viewRootNodeJsonColumn : ModelConnected -> Html Msg
viewRootNodeJsonColumn model =
    let
        hasError : Bool
        hasError =
            model.rootNodeJsonError /= Nothing

        textareaBorder : String
        textareaBorder =
            if hasError then
                "1px solid #b91c1c"

            else
                "1px solid var(--border)"

        errorView : Html Msg
        errorView =
            case model.rootNodeJsonError of
                Nothing ->
                    Html.text ""

                Just err ->
                    Html.div
                        [ Html.Attributes.style "margin-top" "0.35rem"
                        , Html.Attributes.style "color" "#b91c1c"
                        , Html.Attributes.style "font-size" "0.6875rem"
                        , Html.Attributes.style "white-space" "pre-wrap"
                        ]
                        [ Html.text err ]
    in
    Html.div
        [ Html.Attributes.style "flex" "0 0 auto"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "background" "var(--surface-2)"
        , Html.Attributes.style "border" "1px solid var(--border)"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "overflow" "hidden"
        ]
        [ Html.div
            [ Html.Attributes.style "padding" "0.2rem 0.4rem"
            , Html.Attributes.style "font-size" "0.8125rem"
            , Html.Attributes.style "font-weight" "600"
            , Html.Attributes.style "background" "var(--surface-3)"
            , Html.Attributes.style "border-bottom" "1px solid var(--border)"
            ]
            [ Html.text "Node JSON" ]
        , Html.div
            [ Html.Attributes.style "padding" "0.35rem"
            ]
            [ Html.textarea
                [ Html.Attributes.value model.rootNodeJsonText
                , Html.Events.onInput (\t -> MsgConnected (SetRootNodeJsonText t))
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "10rem"
                , Html.Attributes.style "min-height" "10rem"
                , Html.Attributes.style "max-height" "10rem"
                , Html.Attributes.style "resize" "none"
                , Html.Attributes.style "box-sizing" "border-box"
                , Html.Attributes.style "font-family" "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
                , Html.Attributes.style "font-size" "0.6875rem"
                , Html.Attributes.style "border" textareaBorder
                , Html.Attributes.style "border-radius" "4px"
                , Html.Attributes.style "padding" "0.35rem"
                ]
                []
            , Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "gap" "0.4rem"
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
            [ Html.Attributes.style "padding" "0.2rem 0.4rem"
            , Html.Attributes.style "font-size" "0.8125rem"
            , Html.Attributes.style "font-weight" "600"
            , Html.Attributes.style "background" "var(--surface-3)"
            , Html.Attributes.style "border-bottom" "1px solid var(--border)"
            ]
            [ Html.text "Node tree" ]
        , Html.div
            [ Html.Attributes.style "flex" "1"
            , Html.Attributes.style "min-height" "0"
            , Html.Attributes.style "overflow" "auto"
            , Html.Attributes.style "padding" "0.2rem"
            ]
            [ viewTreeNode model [] (desiredRoot model.rootNode)
            ]
        ]


viewTreeNode : ModelConnected -> List Int -> Node -> Html Msg
viewTreeNode model path node =
    let
        isSelected : Bool
        isSelected =
            model.selectedPath == Just path

        rowAttrs : List (Html.Attribute Msg)
        rowAttrs =
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "row"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "space-between"
            , Html.Attributes.style "gap" "0.35rem"
            , Html.Attributes.style "padding" "0.15rem 0.35rem"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "border-radius" "2px"
            , Html.Attributes.style "margin-bottom" "1px"
            , Html.Events.onClick (MsgConnected (SelectNode (Just path)))
            ]
                ++ (if isSelected then
                        [ Html.Attributes.style "background" "var(--selection)" ]

                    else
                        []
                   )

        childrenView : Html Msg
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

        treeRoot : Node
        treeRoot =
            desiredRoot model.rootNode

        canMoveUp : Bool
        canMoveUp =
            Path.canMoveTreeNodeUp path treeRoot

        canMoveDown : Bool
        canMoveDown =
            Path.canMoveTreeNodeDown path treeRoot

        addRemove : Html Msg
        addRemove =
            Html.span
                [ Html.Attributes.style "flex-shrink" "0"
                , Html.Attributes.style "display" "inline-flex"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "gap" "0.25rem"
                , Html.Attributes.style "font-size" "0.6875rem"
                ]
                (if List.isEmpty path then
                    [ viewAddChildButton model path ]

                 else
                    [ viewAddChildButton model path
                    , Html.button
                        [ Html.Events.stopPropagationOn "click"
                            (Json.Decode.succeed ( MsgConnected (MoveTreeNodeUp path), True ))
                        , Html.Attributes.disabled (not canMoveUp)
                        , Html.Attributes.style "padding" "0 0.25rem"
                        ]
                        [ Html.text "↑" ]
                    , Html.button
                        [ Html.Events.stopPropagationOn "click"
                            (Json.Decode.succeed ( MsgConnected (MoveTreeNodeDown path), True ))
                        , Html.Attributes.disabled (not canMoveDown)
                        , Html.Attributes.style "padding" "0 0.25rem"
                        ]
                        [ Html.text "↓" ]
                    , Html.button
                        [ Html.Events.stopPropagationOn "click"
                            (Json.Decode.succeed ( MsgConnected (RemoveNode path), True ))
                        , Html.Attributes.style "padding" "0 0.25rem"
                        ]
                        [ Html.text "✘" ]
                    ]
                )
    in
    Html.div []
        [ Html.div rowAttrs
            [ Html.span
                [ Html.Attributes.style "flex" "1 1 auto"
                , Html.Attributes.style "min-width" "0"
                , Html.Attributes.style "overflow" "hidden"
                , Html.Attributes.style "text-overflow" "ellipsis"
                , Html.Attributes.style "white-space" "nowrap"
                ]
                [ Html.text (Node.displayLabel node) ]
            , addRemove
            ]
        , childrenView
        ]


viewAddChildButton : ModelConnected -> List Int -> Html Msg
viewAddChildButton model path =
    let
        canAdd : Bool
        canAdd =
            case Path.getNodeAtPath path (desiredRoot model.rootNode) of
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
            insertIndex : Int
            insertIndex =
                Path.getNodeAtPath path (desiredRoot model.rootNode)
                    |> Maybe.andThen
                        (\n ->
                            case n.type_ of
                                Node.Group { children } ->
                                    Just (List.length children)

                                _ ->
                                    Nothing
                        )
                    |> Maybe.withDefault 0

            vc : VideoConstants
            vc =
                model.videoConstants

            limitWarning : Maybe String
            limitWarning =
                addChildValidationError model path (Group { children = [] })
                    |> Maybe.map addChildLimitWarning

            addOption : String -> String -> Html Msg
            addOption val label =
                Html.option [ Html.Attributes.value val ] [ Html.text label ]
        in
        Html.span []
            [ Html.select
                [ Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( MsgConnected InteractedWithSelect, True ))
                , Html.Events.on "change"
                    (Json.Decode.at [ "target", "value" ] Json.Decode.string
                        |> Json.Decode.map
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

                                    "bitmap" ->
                                        MsgConnected
                                            (InsertChild path
                                                insertIndex
                                                (Bitmap { defaultBitmapConfig | x = vc.xMin, y = vc.yMin })
                                            )

                                    "group" ->
                                        MsgConnected (InsertChild path insertIndex (Group { children = [] }))

                                    _ ->
                                        MsgConnected (SelectedUnknown "add child option" v)
                            )
                    )
                , Html.Attributes.style "padding" "0 0.25rem"
                , Html.Attributes.id ("add-child-select-" ++ String.join "-" (List.map String.fromInt path))
                , Html.Attributes.disabled (limitWarning /= Nothing)
                , Html.Attributes.value ""
                , Html.Attributes.placeholder "Add…"
                ]
                [ Html.option
                    [ Html.Attributes.value ""
                    , Html.Attributes.selected True
                    , Html.Attributes.disabled True
                    ]
                    [ Html.text "Add…" ]
                , addOption "rect" "Rect"
                , addOption "rectFill" "RectFill"
                , addOption "xLine" "XLine"
                , addOption "yLine" "YLine"
                , addOption "text" "Text"
                , addOption "bitmap" "Bitmap"
                , addOption "group" "Group"
                ]
            , case limitWarning of
                Just warning ->
                    Html.span
                        [ Html.Attributes.style "margin-left" "0.35rem"
                        , Html.Attributes.style "color" "var(--muted)"
                        ]
                        [ Html.text warning ]

                Nothing ->
                    Html.text ""
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
            [ Html.Attributes.style "padding" "0.2rem 0.4rem"
            , Html.Attributes.style "font-size" "0.8125rem"
            , Html.Attributes.style "font-weight" "600"
            , Html.Attributes.style "background" "var(--surface-3)"
            , Html.Attributes.style "border-bottom" "1px solid var(--border)"
            ]
            [ Html.text "Details" ]
        , Html.div
            [ Html.Attributes.style "flex" "1"
            , Html.Attributes.style "overflow" "auto"
            , Html.Attributes.style "padding" "0.35rem"
            ]
            (case model.selectedPath of
                Just path ->
                    case Path.getNodeAtPath path (desiredRoot model.rootNode) of
                        Just node ->
                            [ viewNodeDetails model node path ]

                        Nothing ->
                            [ Html.div [ Html.Attributes.style "color" "var(--muted)" ] [ Html.text "Select a node." ] ]

                Nothing ->
                    [ Html.div [ Html.Attributes.style "color" "var(--muted)" ] [ Html.text "Select a node." ] ]
            )
        ]


detailsFieldGrid : List (Html Msg) -> Html Msg
detailsFieldGrid rows =
    Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" "max-content minmax(0, 1fr)"
        , Html.Attributes.style "column-gap" "0.45rem"
        , Html.Attributes.style "row-gap" "0.35rem"
        , Html.Attributes.style "align-items" "center"
        ]
        rows


detailsLabelCell : String -> Bool -> Html Msg
detailsLabelCell labelText alignTop =
    Html.div
        ([ Html.Attributes.style "font-size" "0.6875rem"
         , Html.Attributes.style "justify-self" "end"
         , Html.Attributes.style "text-align" "end"
         ]
            ++ (if alignTop then
                    [ Html.Attributes.style "align-self" "start"
                    , Html.Attributes.style "padding-top" "0.3rem"
                    ]

                else
                    [ Html.Attributes.style "align-self" "center" ]
               )
        )
        [ Html.text labelText ]


detailsControlCell : Bool -> Html Msg -> Html Msg
detailsControlCell alignTop control =
    Html.div
        ([ Html.Attributes.style "min-width" "0"
         , Html.Attributes.style "width" "100%"
         ]
            ++ (if alignTop then
                    [ Html.Attributes.style "align-self" "start" ]

                else
                    [ Html.Attributes.style "align-self" "center" ]
               )
        )
        [ control ]


detailsGridRow : String -> Html Msg -> Html Msg
detailsGridRow labelText control =
    Html.div
        [ Html.Attributes.style "display" "contents" ]
        [ detailsLabelCell labelText False
        , detailsControlCell False control
        ]


detailsGridRowTop : String -> Html Msg -> Html Msg
detailsGridRowTop labelText control =
    Html.div
        [ Html.Attributes.style "display" "contents" ]
        [ detailsLabelCell labelText True
        , detailsControlCell True control
        ]


viewNodeDetails : ModelConnected -> Node -> List Int -> Html Msg
viewNodeDetails model node path =
    let
        keyInput : Html Msg
        keyInput =
            Html.input
                [ Html.Attributes.value node.key
                , Html.Events.onInput
                    (\k ->
                        MsgConnected (UpdateNodeAtPath path k node.type_)
                    )
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "box-sizing" "border-box"
                ]
                []

        centerInUsableAreaButton : Int -> Int -> (Int -> Int -> Type) -> Html Msg
        centerInUsableAreaButton wantedX wantedY toType =
            Html.button
                [ Html.Events.onClick
                    (MsgConnected
                        (UpdateNodeAtPathAndRefocusPreview path
                            node.key
                            (toType
                                (max model.videoConstants.xMin wantedX)
                                (max model.videoConstants.yMin wantedY)
                            )
                        )
                    )
                ]
                [ Html.text "Center in usable area" ]

        centerInUsableAreaButtonForNode : (Int -> Int -> Type) -> Html Msg
        centerInUsableAreaButtonForNode toType =
            let
                ( x, y ) =
                    Node.centerPosition model.videoConstants node
            in
            centerInUsableAreaButton x y toType

        typeFields : List (Html Msg)
        typeFields =
            case node.type_ of
                Rect r ->
                    [ detailsGridRow "x" (intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> Rect { r | x = x }) r.x)
                    , detailsGridRow "y" (intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> Rect { r | y = y }) r.y)
                    , detailsGridRow "center" (centerInUsableAreaButtonForNode (\x y -> Rect { r | x = x, y = y }))
                    , detailsGridRow "w" (intSliderInput 0 model.videoConstants.usableWidth path node.key (\w -> Rect { r | w = w }) r.w)
                    , detailsGridRow "h" (intSliderInput 0 model.videoConstants.usableHeight path node.key (\h -> Rect { r | h = h }) r.h)
                    , detailsGridRow "color" (colorInput path node.key (\color -> Rect { r | color = color }) r.color)
                    ]

                RectFill r ->
                    [ detailsGridRow "x" (intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> RectFill { r | x = x }) r.x)
                    , detailsGridRow "y" (intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> RectFill { r | y = y }) r.y)
                    , detailsGridRow "center" (centerInUsableAreaButtonForNode (\x y -> RectFill { r | x = x, y = y }))
                    , detailsGridRow "w" (intSliderInput 0 model.videoConstants.usableWidth path node.key (\w -> RectFill { r | w = w }) r.w)
                    , detailsGridRow "h" (intSliderInput 0 model.videoConstants.usableHeight path node.key (\h -> RectFill { r | h = h }) r.h)
                    , detailsGridRow "color" (colorInput path node.key (\color -> RectFill { r | color = color }) r.color)
                    ]

                XLine r ->
                    [ detailsGridRow "x" (intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> XLine { r | x = x }) r.x)
                    , detailsGridRow "y" (intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> XLine { r | y = y }) r.y)
                    , detailsGridRow "center" (centerInUsableAreaButtonForNode (\x y -> XLine { r | x = x, y = y }))
                    , detailsGridRow "len" (intSliderInput 0 model.videoConstants.usableWidth path node.key (\len -> XLine { r | len = len }) r.len)
                    , detailsGridRow "color" (colorInput path node.key (\color -> XLine { r | color = color }) r.color)
                    ]

                YLine r ->
                    [ detailsGridRow "x" (intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> YLine { r | x = x }) r.x)
                    , detailsGridRow "y" (intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> YLine { r | y = y }) r.y)
                    , detailsGridRow "center" (centerInUsableAreaButtonForNode (\x y -> YLine { r | x = x, y = y }))
                    , detailsGridRow "len" (intSliderInput 0 model.videoConstants.usableHeight path node.key (\len -> YLine { r | len = len }) r.len)
                    , detailsGridRow "color" (colorInput path node.key (\color -> YLine { r | color = color }) r.color)
                    ]

                Text r ->
                    [ detailsGridRow "x" (intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> Text { r | x = x }) r.x)
                    , detailsGridRow "y" (intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> Text { r | y = y }) r.y)
                    , detailsGridRow "center" (centerInUsableAreaButtonForNode (\x y -> Text { r | x = x, y = y }))
                    , detailsGridRowTop "text"
                        (Html.textarea
                            [ Html.Attributes.value r.text
                            , Html.Events.onInput (\t -> MsgConnected (UpdateNodeAtPath path node.key (Text { r | text = t })))
                            , Html.Attributes.rows 4
                            , Html.Attributes.style "width" "100%"
                            , Html.Attributes.style "box-sizing" "border-box"
                            ]
                            []
                        )
                    , detailsGridRow "fontIndex"
                        (Html.select
                            [ Html.Events.onInput
                                (\s ->
                                    String.toInt s
                                        |> Maybe.map (\fontIndex -> MsgConnected (UpdateNodeAtPath path node.key (Text { r | fontIndex = fontIndex })))
                                        |> Maybe.withDefault (MsgConnected (SelectedUnknown "font" s))
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
                        )
                    , detailsGridRow "color" (colorInput path node.key (\color -> Text { r | color = color }) r.color)
                    ]

                Bitmap r ->
                    [ detailsGridRow "x" (intSliderInput model.videoConstants.xMin model.videoConstants.xMax path node.key (\x -> Bitmap { r | x = x }) r.x)
                    , detailsGridRow "y" (intSliderInput model.videoConstants.yMin model.videoConstants.yMax path node.key (\y -> Bitmap { r | y = y }) r.y)
                    , detailsGridRow "center" (centerInUsableAreaButtonForNode (\x y -> Bitmap { r | x = x, y = y }))
                    , detailsGridRow "preset"
                        (Html.select
                            [ Html.Events.onInput
                                (\s ->
                                    String.toInt s
                                        |> Maybe.andThen (\i -> List.Extra.getAt i allBitmaps)
                                        |> Maybe.map
                                            (\e ->
                                                MsgConnected
                                                    (UpdateNodeAtPath path
                                                        node.key
                                                        (Bitmap
                                                            { r
                                                                | w = e.w
                                                                , h = e.h
                                                                , bitDepth = e.bitDepth
                                                                , data = e.data
                                                            }
                                                        )
                                                    )
                                            )
                                        |> Maybe.withDefault (MsgConnected (SelectedUnknown "bitmap" s))
                                )
                            , Html.Attributes.style "width" "100%"
                            , Html.Attributes.style "box-sizing" "border-box"
                            ]
                            (allBitmaps
                                |> List.indexedMap
                                    (\i e ->
                                        Html.option
                                            [ Html.Attributes.value (String.fromInt i)
                                            , Html.Attributes.Extra.attributeIf (embeddedBitmapMatchIndex r == Just i) (Html.Attributes.attribute "selected" "")
                                            ]
                                            [ Html.text e.label ]
                                    )
                            )
                        )
                    , detailsGridRowTop "data"
                        (Html.div
                            [ Html.Attributes.style "font-size" "0.6875rem"
                            , Html.Attributes.style "color" "var(--muted)"
                            , Html.Attributes.style "white-space" "pre-wrap"
                            ]
                            [ Html.text
                                ("w: "
                                    ++ String.fromInt r.w
                                    ++ "\nh: "
                                    ++ String.fromInt r.h
                                    ++ "\nbitDepth: "
                                    ++ String.fromInt (Bitmap.bitDepthToInt r.bitDepth)
                                    ++ "\nStored bytes: "
                                    ++ String.fromInt (List.length r.data)
                                    ++ "\nPreset replaces w, h, bitDepth, and packed data (one-way). JSON pane can still overwrite data."
                                )
                            ]
                        )
                    ]

                Node.Group _ ->
                    []
    in
    detailsFieldGrid (detailsGridRow "Key" keyInput :: typeFields)


intSliderInput : Int -> Int -> List Int -> String -> (Int -> Type) -> Int -> Html Msg
intSliderInput min_ max_ path key toType current =
    let
        current_ : Int
        current_ =
            clamp min_ max_ current
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "gap" "0.4rem"
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
                        |> Maybe.withDefault (MsgConnected (SelectedUnknown "non-int value from a slider" s))
                )
            , Html.Attributes.style "flex" "1"
            ]
            []
        , Html.div
            [ Html.Attributes.style "width" "3rem"
            , Html.Attributes.style "text-align" "right"
            , Html.Attributes.style "font-family" "ui-monospace, monospace"
            , Html.Attributes.style "font-size" "0.8125rem"
            ]
            [ Html.text (String.fromInt current_) ]
        ]


colorInput : List Int -> String -> (Int -> Type) -> Int -> Html Msg
colorInput path key toType current =
    let
        current_ : Int
        current_ =
            clamp 0 255 current
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "gap" "0.4rem"
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
                        |> Maybe.withDefault (MsgConnected (SelectedUnknown "non-int color value from a slider" s))
                )
            , Html.Attributes.style "flex" "1"
            ]
            []
        , Html.div
            [ Html.Attributes.style "width" "3rem"
            , Html.Attributes.style "text-align" "right"
            , Html.Attributes.style "font-family" "ui-monospace, monospace"
            , Html.Attributes.style "font-size" "0.8125rem"
            ]
            [ Html.text (String.fromInt current_) ]
        ]


viewConnected : { isLocal : Bool } -> ModelConnected -> Html Msg
viewConnected { isLocal } model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "padding" "0.35rem"
        , Html.Attributes.style "gap" "0.4rem"
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
                , Html.Attributes.style "flex-wrap" "wrap"
                , Html.Attributes.style "justify-content" "space-between"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "gap" "0.4rem"
                , Html.Attributes.style "margin-bottom" "0.5rem"
                ]
                [ Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex" "1 1 auto"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "gap" "0.4rem"
                    , Html.Attributes.style "min-width" "0"
                    ]
                    [ Html.text "Connected."
                    , Html.button
                        [ Html.Events.onClick DisconnectRequested ]
                        [ Html.text "Disconnect" ]
                    , if isLocal then
                        Html.text "Local-only (not connected to an ESP32)."

                      else
                        Html.text ""
                    , viewLastError model.lastError
                    ]
                , viewPreviewHeader model
                ]
            , Html.div
                [ Html.Attributes.style "flex" "1 1 auto"
                , Html.Attributes.style "min-height" "0"
                , Html.Attributes.style "min-width" "0"
                , Html.Attributes.style "overflow" "auto"
                ]
                [ viewPreviewSurface model ]
            ]
        , viewSidebarColumn model
        ]


viewLastError : String -> Html msg
viewLastError lastError =
    if lastError /= "" then
        Html.div
            [ Html.Attributes.style "color" "red" ]
            [ Html.text lastError ]

    else
        Html.text ""


renderNodeToSvg : List Font -> Maybe (List Int) -> List Int -> Node -> List (Svg msg)
renderNodeToSvg fonts selectedPath path node_ =
    let
        inner : List (Svg msg)
        inner =
            case node_.type_ of
                Node.Group { children } ->
                    children
                        |> List.indexedMap
                            (\i child ->
                                renderNodeToSvg fonts selectedPath (path ++ [ i ]) child
                            )
                        |> List.concat

                _ ->
                    renderNodeToSvgLeaf fonts node_
    in
    [ Svg.g
        [ Html.Attributes.attribute "data-hash" (String.fromInt node_.hash)
        , Html.Attributes.attribute "data-index-path" (Path.toString path)
        , Html.Attributes.attribute "data-selected"
            (if selectedPath == Just path then
                "true"

             else
                "false"
            )
        ]
        inner
    ]


renderNodeToSvgLeaf : List Font -> Node -> List (Svg msg)
renderNodeToSvgLeaf fonts node_ =
    case node_.type_ of
        Rect { x, y, w, h, color } ->
            if w <= 0 || h <= 0 then
                []

            else
                let
                    css : String
                    css =
                        Color.toCss color

                    x2 : Int
                    x2 =
                        x + w - 1

                    y2 : Int
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
                , Svg.Attributes.fill (Color.toCss color)
                ]
                []
            ]

        XLine { x, y, len, color } ->
            [ Svg.rect
                [ Svg.Attributes.x (String.fromInt x)
                , Svg.Attributes.y (String.fromInt y)
                , Svg.Attributes.width (String.fromInt len)
                , Svg.Attributes.height "1"
                , Svg.Attributes.fill (Color.toCss color)
                ]
                []
            ]

        YLine { x, y, len, color } ->
            [ Svg.rect
                [ Svg.Attributes.x (String.fromInt x)
                , Svg.Attributes.y (String.fromInt y)
                , Svg.Attributes.width "1"
                , Svg.Attributes.height (String.fromInt len)
                , Svg.Attributes.fill (Color.toCss color)
                ]
                []
            ]

        Text { x, y, text, fontIndex, color } ->
            case List.Extra.getAt fontIndex fonts of
                Nothing ->
                    []

                Just font ->
                    let
                        lineHeight : Int
                        lineHeight =
                            font.glyphHeight + font.extraLineHeight

                        hasChar : Char -> Bool
                        hasChar c =
                            let
                                code : Int
                                code =
                                    Char.toCode c
                            in
                            code >= font.asciiFirst && code <= font.asciiLast

                        drawChar : Int -> Int -> Char -> List (Svg msg)
                        drawChar gx gy char =
                            if hasChar char then
                                let
                                    code : Int
                                    code =
                                        Char.toCode char

                                    glyphIdx : Int
                                    glyphIdx =
                                        code - font.asciiFirst
                                in
                                List.range 0 (font.glyphHeight - 1)
                                    |> List.concatMap
                                        (\r ->
                                            List.range 0 (font.glyphWidth - 1)
                                                |> List.filterMap
                                                    (\c ->
                                                        if Font.renderGlyphPixel font glyphIdx r c then
                                                            Just
                                                                (Svg.rect
                                                                    [ Svg.Attributes.x (String.fromInt (gx + c))
                                                                    , Svg.Attributes.y (String.fromInt (gy + r))
                                                                    , Svg.Attributes.width "1"
                                                                    , Svg.Attributes.height "1"
                                                                    , Svg.Attributes.fill (Color.toCss color)
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
                                        glyphs : List (Svg msg)
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

        Bitmap { x, y, w, h, bitDepth, data } ->
            List.range 0 (h - 1)
                |> List.concatMap
                    (\row ->
                        Bitmap.rowGraysSequential bitDepth w row data
                            |> List.indexedMap
                                (\column gray ->
                                    Svg.rect
                                        [ Svg.Attributes.x (String.fromInt (x + column))
                                        , Svg.Attributes.y (String.fromInt (y + row))
                                        , Svg.Attributes.width "1"
                                        , Svg.Attributes.height "1"
                                        , Svg.Attributes.fill (Color.toCss gray)
                                        ]
                                        []
                                )
                    )

        Node.Group _ ->
            []


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
                    , previewDragSubscriptions modelConnected.previewDrag
                    , case modelConnected.rootNode of
                        RootSynced _ ->
                            Sub.none

                        RootThrottled _ ->
                            Browser.Events.onAnimationFrame
                                (\_ -> MsgConnected ThrottleFrame)

                        RootAwaitingAck _ ->
                            Sub.none

                        RootAwaitingAckWithPending _ ->
                            Browser.Events.onAnimationFrame
                                (\_ -> MsgConnected ThrottleFrame)
                    ]

            LocalConnected modelConnected ->
                previewDragSubscriptions modelConnected.previewDrag
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

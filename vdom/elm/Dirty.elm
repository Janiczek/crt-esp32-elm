module Dirty exposing
    ( TileGrid
    , diff, dirtyTilesEncoder
    , markBbox_TEST, markRectBorder_TEST, textCellsUntilNewline_TEST, changedTextCells_TEST, allTextCells_TEST
    )

{-|

@docs TileGrid
@docs diff, dirtyTilesEncoder
@docs markBbox_TEST, markRectBorder_TEST, textCellsUntilNewline_TEST, changedTextCells_TEST, allTextCells_TEST

-}

import BoundingBox exposing (BoundingBox)
import Bytes exposing (Endianness(..))
import Bytes.Encode
import BytesExtraExtra
import Dict exposing (Dict)
import Font exposing (Font)
import List.Cartesian
import List.Extra
import Node exposing (Node, Type(..))
import Set exposing (Set)


type alias TileGrid =
    { tileSize : Int
    , tileCols : Int
    , tileRows : Int
    }


dirtyTilesEncoder : Set ( Int, Int ) -> Bytes.Encode.Encoder
dirtyTilesEncoder tiles =
    let
        pairs =
            Set.toList tiles

        encodePair ( tx, ty ) =
            Bytes.Encode.sequence
                [ Bytes.Encode.unsignedInt8 tx
                , Bytes.Encode.unsignedInt8 ty
                ]
    in
    BytesExtraExtra.sizedListEncoder encodePair pairs


diff : TileGrid -> List Font -> Node -> Node -> Set ( Int, Int )
diff grid fonts oldRoot newRoot =
    if oldRoot.hash == newRoot.hash then
        Set.empty

    else
        case ( oldRoot.type_, newRoot.type_ ) of
            ( Text oldT, Text newT ) ->
                if oldT.x == newT.x && oldT.y == newT.y && oldT.fontIndex == newT.fontIndex && oldT.color == newT.color then
                    case List.Extra.getAt newT.fontIndex fonts of
                        Nothing ->
                            Debug.todo ("font not found: " ++ String.fromInt newT.fontIndex)

                        Just font ->
                            changedTextCells oldT.text newT.text
                                |> Set.fromList
                                |> Set.map (textCellToTile grid oldT font)

                else
                    Set.union (markBbox grid oldRoot.bbox) (markBbox grid newRoot.bbox)

            ( Rect _, Rect _ ) ->
                Set.union (markRectBorder grid oldRoot.bbox) (markRectBorder grid newRoot.bbox)

            _ ->
                Set.union (markBbox grid oldRoot.bbox) (markBbox grid newRoot.bbox)


{-| Diff contents of the two groups.
Match old and new children by key (only on this level),
diff matched pairs, mark bbox for deletions and insertions.
-}
diffChildren : TileGrid -> List Font -> List Node -> List Node -> Set ( Int, Int )
diffChildren grid fonts oldChildren newChildren =
    let
        old : Dict String Node
        old =
            oldChildren
                |> List.map (\n -> ( n.key, n ))
                |> Dict.fromList

        new : Dict String Node
        new =
            newChildren
                |> List.map (\n -> ( n.key, n ))
                |> Dict.fromList

        oldKeys : Set String
        oldKeys =
            old
                |> Dict.keys
                |> Set.fromList

        newKeys : Set String
        newKeys =
            new
                |> Dict.keys
                |> Set.fromList

        updateKeys : List String
        updateKeys =
            Set.intersect oldKeys newKeys
                |> Set.toList

        insertKeys : List String
        insertKeys =
            Set.diff newKeys oldKeys
                |> Set.toList

        deleteKeys : List String
        deleteKeys =
            Set.diff oldKeys newKeys
                |> Set.toList

        updateTiles =
            updateKeys
                |> List.map
                    (\key ->
                        case ( Dict.get key old, Dict.get key new ) of
                            ( Just oldNode, Just newNode ) ->
                                diff grid fonts oldNode newNode

                            _ ->
                                Set.empty
                    )
                |> List.foldl Set.union Set.empty

        insertionTiles =
            insertKeys
                |> List.filterMap (\key -> Dict.get key new)
                |> List.map (\n -> markBbox grid n.bbox)
                |> List.foldl Set.union Set.empty

        deletionTiles =
            deleteKeys
                |> List.filterMap (\key -> Dict.get key old)
                |> List.map (\n -> markBbox grid n.bbox)
                |> List.foldl Set.union Set.empty
    in
    updateTiles
        |> Set.union insertionTiles
        |> Set.union deletionTiles


{-| Mark all tiles overlapping a bounding box. Clamps to valid tile range.

This is a bit of a blunt weapon - it's definitely correct, but a knowledge of
the nodes' types might allow marking only specific relevant tiles instead of all
of them. For example diagonal lines wouldn't mark tiles in the two unrelated corners.

-}
markBbox : TileGrid -> BoundingBox -> Set ( Int, Int )
markBbox grid bbox =
    let
        tx0 =
            bbox.x // grid.tileSize

        ty0 =
            bbox.y // grid.tileSize

        tx1 =
            (bbox.x + bbox.w - 1) // grid.tileSize

        ty1 =
            (bbox.y + bbox.h - 1) // grid.tileSize

        clampTx tx =
            clamp 0 (grid.tileCols - 1) tx

        clampTy ty =
            clamp 0 (grid.tileRows - 1) ty
    in
    if tx1 < 0 || ty1 < 0 || tx0 >= grid.tileCols || ty0 >= grid.tileRows then
        Set.empty

    else
        List.Cartesian.map2 Tuple.pair
            (List.range (clampTx tx0) (clampTx tx1))
            (List.range (clampTy ty0) (clampTy ty1))
            |> Set.fromList


markRectBorder : TileGrid -> BoundingBox -> Set ( Int, Int )
markRectBorder grid bbox =
    if bbox.w <= 0 || bbox.h <= 0 then
        Set.empty

    else
        let
            top =
                { bbox | h = 1 }

            bottom =
                { bbox | y = bbox.y + bbox.h - 1, h = 1 }

            left =
                { bbox | w = 1 }

            right =
                { bbox | x = bbox.x + bbox.w - 1, w = 1 }
        in
        [ top, bottom, left, right ]
            |> List.map (markBbox grid)
            |> List.foldl Set.union Set.empty


{-| Finds character cells (not tiles!) that differ between old and new text.
This will later be used to mark tiles as dirty, with the knowledge of the font dimensions.
-}
changedTextCells : String -> String -> List ( Int, Int )
changedTextCells oldStr newStr =
    let
        step : List Char -> List Char -> Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
        step accOld accNew row col acc =
            case ( accOld, accNew ) of
                ( [], [] ) ->
                    acc

                ( [], _ ) ->
                    -- old string is empty, add all cells from new
                    acc ++ allTextCells row col accNew

                ( _, [] ) ->
                    -- new string is empty, add all cells from old
                    acc ++ allTextCells row col accOld

                ( '\n' :: restOld, '\n' :: restNew ) ->
                    step restOld restNew (row + 1) 0 acc

                ( '\n' :: restOld, _ ) ->
                    -- old line has ended, add all cells from current line in `new` then continue
                    let
                        res =
                            textCellsUntilNewline row col accNew
                    in
                    step restOld res.rest res.row res.col (acc ++ res.cells)

                ( _, '\n' :: restNew ) ->
                    let
                        res =
                            textCellsUntilNewline row col accOld
                    in
                    step res.rest restNew res.row res.col (acc ++ res.cells)

                ( a :: restOld, b :: restNew ) ->
                    if a == b then
                        step restOld restNew row (col + 1) acc

                    else
                        step restOld restNew row (col + 1) (( row, col ) :: acc)
    in
    step (String.toList oldStr) (String.toList newStr) 0 0 []


{-| Produces all text cells in a string.
-}
allTextCells : Int -> Int -> List Char -> List ( Int, Int )
allTextCells row col chars =
    let
        go acc lst r c =
            case lst of
                [] ->
                    acc

                '\n' :: rest ->
                    go acc rest (r + 1) 0

                _ :: rest ->
                    go (( r, c ) :: acc) rest r (c + 1)
    in
    go [] chars row col


{-| Produces text cells until the next newline, plus the state needed for the outer loop.
-}
textCellsUntilNewline :
    Int
    -> Int
    -> List Char
    ->
        { cells : List ( Int, Int )
        , rest : List Char
        , row : Int
        , col : Int
        }
textCellsUntilNewline row col chars =
    let
        go acc lst r c =
            case lst of
                [] ->
                    { cells = acc
                    , rest = []
                    , row = r
                    , col = c
                    }

                '\n' :: rest ->
                    { cells = acc
                    , rest = rest
                    , row = r + 1
                    , col = 0
                    }

                _ :: rest ->
                    go (( r, c ) :: acc) rest r (c + 1)
    in
    go [] chars row col


textCellToTile : TileGrid -> { text | x : Int, y : Int } -> Font -> ( Int, Int ) -> ( Int, Int )
textCellToTile grid { x, y } font ( row, col ) =
    let
        screenX =
            x + col * font.glyphWidth

        screenY =
            y + row * font.glyphHeight + font.extraLineHeight

        tileX =
            screenX // grid.tileSize

        tileY =
            screenY // grid.tileSize
    in
    ( tileX, tileY )


markBbox_TEST : TileGrid -> BoundingBox -> Set ( Int, Int )
markBbox_TEST =
    markBbox


markRectBorder_TEST : TileGrid -> BoundingBox -> Set ( Int, Int )
markRectBorder_TEST =
    markRectBorder


textCellsUntilNewline_TEST : Int -> Int -> List Char -> { cells : List ( Int, Int ), rest : List Char, row : Int, col : Int }
textCellsUntilNewline_TEST =
    textCellsUntilNewline


changedTextCells_TEST : String -> String -> List ( Int, Int )
changedTextCells_TEST =
    changedTextCells


allTextCells_TEST : Int -> Int -> List Char -> List ( Int, Int )
allTextCells_TEST =
    allTextCells

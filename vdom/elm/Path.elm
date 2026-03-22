module Path exposing
    ( canMoveTreeNodeDown
    , canMoveTreeNodeUp
    , findPathByKey
    , flattenedPathsPreorder
    , getNodeAtPath
    , insertChildAtPath
    , moveTreeNodeDown
    , moveTreeNodeUp
    , removeNodeAtPath
    , setNodeAtPath
    , toString
    )

import List.Extra
import ListExtraExtra
import Node exposing (Node, Type(..))


pathParentAndIndex : List Int -> Maybe ( List Int, Int )
pathParentAndIndex path =
    case List.reverse path of
        [] ->
            Nothing

        lastIdx :: revInit ->
            Just ( List.reverse revInit, lastIdx )


{-| First path to a node with the given key (preorder DFS).
-}
findPathByKey : String -> Node -> Maybe (List Int)
findPathByKey key node_ =
    if key == node_.key then
        Just []

    else
        case node_.type_ of
            Group { children } ->
                findPathByKeyInChildren key children 0

            Rect _ ->
                Nothing

            RectFill _ ->
                Nothing

            XLine _ ->
                Nothing

            YLine _ ->
                Nothing

            Text _ ->
                Nothing

            Bitmap _ ->
                Nothing


findPathByKeyInChildren : String -> List Node -> Int -> Maybe (List Int)
findPathByKeyInChildren key children i =
    case children of
        [] ->
            Nothing

        c :: cs ->
            case findPathByKey key c of
                Just rest ->
                    Just (i :: rest)

                Nothing ->
                    findPathByKeyInChildren key cs (i + 1)


{-| Paths in the same order as rows in the sidebar node tree (preorder: node, then children left-to-right).
Root is `[]`.
-}
flattenedPathsPreorder : Node -> List (List Int)
flattenedPathsPreorder root =
    flattenFromPath [] root


flattenFromPath : List Int -> Node -> List (List Int)
flattenFromPath path node_ =
    path
        :: (case node_.type_ of
                Group { children } ->
                    List.concat
                        (List.indexedMap
                            (\i child -> flattenFromPath (path ++ [ i ]) child)
                            children
                        )

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
           )


canMoveTreeNodeUp : List Int -> Node -> Bool
canMoveTreeNodeUp path root =
    case pathParentAndIndex path of
        Nothing ->
            False

        Just ( parentPath, idx ) ->
            case getNodeAtPath parentPath root of
                Just parent ->
                    case parent.type_ of
                        Group { children } ->
                            let
                                len : Int
                                len =
                                    List.length children
                            in
                            if idx < 0 || idx >= len then
                                False

                            else if idx > 0 then
                                True

                            else
                                not (List.isEmpty parentPath)

                        Rect _ ->
                            False

                        RectFill _ ->
                            False

                        XLine _ ->
                            False

                        YLine _ ->
                            False

                        Text _ ->
                            False

                        Bitmap _ ->
                            False

                Nothing ->
                    False


canMoveTreeNodeDown : List Int -> Node -> Bool
canMoveTreeNodeDown path root =
    case pathParentAndIndex path of
        Nothing ->
            False

        Just ( parentPath, idx ) ->
            case getNodeAtPath parentPath root of
                Just parent ->
                    case parent.type_ of
                        Group { children } ->
                            let
                                len : Int
                                len =
                                    List.length children

                                last : Int
                                last =
                                    len - 1
                            in
                            if len == 0 || idx < 0 || idx > last then
                                False

                            else if idx < last then
                                True

                            else
                                not (List.isEmpty parentPath)

                        Rect _ ->
                            False

                        RectFill _ ->
                            False

                        XLine _ ->
                            False

                        YLine _ ->
                            False

                        Text _ ->
                            False

                        Bitmap _ ->
                            False

                Nothing ->
                    False


moveTreeNodeUp : List Int -> Node -> Maybe Node
moveTreeNodeUp path root =
    case pathParentAndIndex path of
        Nothing ->
            Nothing

        Just ( parentPath, idx ) ->
            case getNodeAtPath parentPath root of
                Just parent ->
                    case parent.type_ of
                        Group { children } ->
                            let
                                len : Int
                                len =
                                    List.length children
                            in
                            if len == 0 || idx < 0 || idx >= len then
                                Nothing

                            else if idx > 0 then
                                let
                                    newChildren : List Node
                                    newChildren =
                                        List.Extra.swapAt (idx - 1) idx children
                                in
                                Just (setNodeAtPath parentPath (Node.group parent.key newChildren) root)

                            else if List.isEmpty parentPath then
                                Nothing

                            else
                                case getNodeAtPath path root of
                                    Just node ->
                                        let
                                            without : Node
                                            without =
                                                removeNodeAtPath path root

                                            grandparentPath : List Int
                                            grandparentPath =
                                                List.take (List.length parentPath - 1) parentPath

                                            parentIdx : Int
                                            parentIdx =
                                                case List.reverse parentPath of
                                                    p :: _ ->
                                                        p

                                                    [] ->
                                                        0
                                        in
                                        Just (insertChildAtPath grandparentPath parentIdx node without)

                                    Nothing ->
                                        Nothing

                        Rect _ ->
                            Nothing

                        RectFill _ ->
                            Nothing

                        XLine _ ->
                            Nothing

                        YLine _ ->
                            Nothing

                        Text _ ->
                            Nothing

                        Bitmap _ ->
                            Nothing

                Nothing ->
                    Nothing


moveTreeNodeDown : List Int -> Node -> Maybe Node
moveTreeNodeDown path root =
    case pathParentAndIndex path of
        Nothing ->
            Nothing

        Just ( parentPath, idx ) ->
            case getNodeAtPath parentPath root of
                Just parent ->
                    case parent.type_ of
                        Group { children } ->
                            let
                                len : Int
                                len =
                                    List.length children

                                last : Int
                                last =
                                    len - 1
                            in
                            if len == 0 || idx < 0 || idx > last then
                                Nothing

                            else if idx < last then
                                let
                                    newChildren : List Node
                                    newChildren =
                                        List.Extra.swapAt idx (idx + 1) children
                                in
                                Just (setNodeAtPath parentPath (Node.group parent.key newChildren) root)

                            else if List.isEmpty parentPath then
                                Nothing

                            else
                                case getNodeAtPath path root of
                                    Just node ->
                                        let
                                            without : Node
                                            without =
                                                removeNodeAtPath path root

                                            grandparentPath : List Int
                                            grandparentPath =
                                                List.take (List.length parentPath - 1) parentPath

                                            parentIdx : Int
                                            parentIdx =
                                                case List.reverse parentPath of
                                                    p :: _ ->
                                                        p

                                                    [] ->
                                                        0
                                        in
                                        Just (insertChildAtPath grandparentPath (parentIdx + 1) node without)

                                    Nothing ->
                                        Nothing

                        Rect _ ->
                            Nothing

                        RectFill _ ->
                            Nothing

                        XLine _ ->
                            Nothing

                        YLine _ ->
                            Nothing

                        Text _ ->
                            Nothing

                        Bitmap _ ->
                            Nothing

                Nothing ->
                    Nothing


getNodeAtPath : List Int -> Node -> Maybe Node
getNodeAtPath path root =
    case path of
        [] ->
            Just root

        i :: rest ->
            case root.type_ of
                Group { children } ->
                    List.Extra.getAt i children
                        |> Maybe.andThen (getNodeAtPath rest)

                Rect _ ->
                    Nothing

                RectFill _ ->
                    Nothing

                XLine _ ->
                    Nothing

                YLine _ ->
                    Nothing

                Text _ ->
                    Nothing

                Bitmap _ ->
                    Nothing


setNodeAtPath : List Int -> Node -> Node -> Node
setNodeAtPath path newNode root =
    case path of
        [] ->
            newNode

        i :: rest ->
            case root.type_ of
                Group { children } ->
                    case List.Extra.getAt i children of
                        Just child ->
                            let
                                newChild : Node
                                newChild =
                                    setNodeAtPath rest newNode child
                            in
                            List.Extra.updateAt i (always newChild) children
                                |> Node.group root.key

                        Nothing ->
                            root

                Rect _ ->
                    root

                RectFill _ ->
                    root

                XLine _ ->
                    root

                YLine _ ->
                    root

                Text _ ->
                    root

                Bitmap _ ->
                    root


removeNodeAtPath : List Int -> Node -> Node
removeNodeAtPath path root =
    case path of
        [] ->
            root

        [ i ] ->
            case root.type_ of
                Group { children } ->
                    Node.group root.key (List.Extra.removeAt i children)

                Rect _ ->
                    root

                RectFill _ ->
                    root

                XLine _ ->
                    root

                YLine _ ->
                    root

                Text _ ->
                    root

                Bitmap _ ->
                    root

        i :: rest ->
            case root.type_ of
                Group { children } ->
                    case List.Extra.getAt i children of
                        Just child ->
                            let
                                newChild : Node
                                newChild =
                                    removeNodeAtPath rest child
                            in
                            List.Extra.updateAt i (always newChild) children
                                |> Node.group root.key

                        Nothing ->
                            root

                Rect _ ->
                    root

                RectFill _ ->
                    root

                XLine _ ->
                    root

                YLine _ ->
                    root

                Text _ ->
                    root

                Bitmap _ ->
                    root


insertChildAtPath : List Int -> Int -> Node -> Node -> Node
insertChildAtPath parentPath index newChild root =
    case getNodeAtPath parentPath root of
        Just parent ->
            case parent.type_ of
                Group { children } ->
                    let
                        inserted : List Node
                        inserted =
                            ListExtraExtra.insertAt index newChild children
                    in
                    setNodeAtPath parentPath (Node.group parent.key inserted) root

                Rect _ ->
                    root

                RectFill _ ->
                    root

                XLine _ ->
                    root

                YLine _ ->
                    root

                Text _ ->
                    root

                Bitmap _ ->
                    root

        Nothing ->
            root


toString : List Int -> String
toString path =
    path
        |> List.map String.fromInt
        |> String.join "."

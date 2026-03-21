module Path exposing
    ( getNodeAtPath
    , insertChildAtPath
    , removeNodeAtPath
    , setNodeAtPath
    , toString
    )

import List.Extra
import ListExtraExtra
import Node exposing (Node, Type(..))


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

module PathTests exposing (suite)

import Color
import Expect
import Fuzzers
import Node exposing (Node, Type(..))
import Path
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Path"
        [ Test.describe "moveTreeNodeUp"
            [ Test.fuzz Fuzzers.node "does not change Node.size when the move applies" <|
                \root ->
                    Path.flattenedPathsPreorder root
                        |> List.filter (not << List.isEmpty)
                        |> List.all
                            (\path ->
                                case Path.moveTreeNodeUp path root of
                                    Nothing ->
                                        True

                                    Just newRoot ->
                                        Node.size newRoot == Node.size root
                            )
                        |> Expect.equal True
            , Test.fuzz Fuzzers.node "if canMoveTreeNodeUp is true, moveTreeNodeUp yields a different node" <|
                \root ->
                    Path.flattenedPathsPreorder root
                        |> List.filter (not << List.isEmpty)
                        |> List.all
                            (\path ->
                                if Path.canMoveTreeNodeUp path root then
                                    case Path.moveTreeNodeUp path root of
                                        Just newRoot ->
                                            newRoot /= root

                                        Nothing ->
                                            False

                                else
                                    True
                            )
                        |> Expect.equal True
            , Test.test "swaps with previous sibling" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "a" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "b" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]

                        newRoot : Node
                        newRoot =
                            Path.moveTreeNodeUp [ 1 ] root
                                |> Maybe.withDefault root
                    in
                    Path.getNodeAtPath [ 0 ] newRoot
                        |> Maybe.map .key
                        |> Expect.equal (Just "b")
            , Test.test "promotes first child of inner group before that group among outer siblings" <|
                \() ->
                    let
                        inner : Node
                        inner =
                            Node.group "inner"
                                [ Node.rect "first" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "mid" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]

                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "before" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , inner
                                ]

                        newRoot : Node
                        newRoot =
                            Path.moveTreeNodeUp [ 1, 0 ] root
                                |> Maybe.withDefault root
                    in
                    ( Path.findPathByKey "first" newRoot
                    , Path.getNodeAtPath [ 2, 0 ] newRoot |> Maybe.map .key
                    )
                        |> Expect.equal ( Just [ 1 ], Just "mid" )
            , Test.test "no-op when first child under root" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "only" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "second" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]
                    in
                    Path.moveTreeNodeUp [ 0 ] root
                        |> Expect.equal Nothing
            ]
        , Test.describe "moveTreeNodeDown"
            [ Test.fuzz Fuzzers.node "does not change Node.size when the move applies" <|
                \root ->
                    Path.flattenedPathsPreorder root
                        |> List.filter (not << List.isEmpty)
                        |> List.all
                            (\path ->
                                case Path.moveTreeNodeDown path root of
                                    Nothing ->
                                        True

                                    Just newRoot ->
                                        Node.size newRoot == Node.size root
                            )
                        |> Expect.equal True
            , Test.fuzz Fuzzers.node "if canMoveTreeNodeDown is true, moveTreeNodeDown yields a different node" <|
                \root ->
                    Path.flattenedPathsPreorder root
                        |> List.filter (not << List.isEmpty)
                        |> List.all
                            (\path ->
                                if Path.canMoveTreeNodeDown path root then
                                    case Path.moveTreeNodeDown path root of
                                        Just newRoot ->
                                            newRoot /= root

                                        Nothing ->
                                            False

                                else
                                    True
                            )
                        |> Expect.equal True
            , Test.test "swaps with next sibling" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "a" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "b" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]

                        newRoot : Node
                        newRoot =
                            Path.moveTreeNodeDown [ 0 ] root
                                |> Maybe.withDefault root
                    in
                    Path.getNodeAtPath [ 0 ] newRoot
                        |> Maybe.map .key
                        |> Expect.equal (Just "b")
            , Test.test "promotes last child of inner group after that group among outer siblings" <|
                \() ->
                    let
                        inner : Node
                        inner =
                            Node.group "inner"
                                [ Node.rect "mid" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "last" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]

                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "before" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , inner
                                ]

                        newRoot : Node
                        newRoot =
                            Path.moveTreeNodeDown [ 1, 1 ] root
                                |> Maybe.withDefault root
                    in
                    ( Path.findPathByKey "last" newRoot
                    , Path.getNodeAtPath [ 1, 0 ] newRoot |> Maybe.map .key
                    )
                        |> Expect.equal ( Just [ 2 ], Just "mid" )
            , Test.test "no-op when last child under root" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "first" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "last" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]
                    in
                    Path.moveTreeNodeDown [ 1 ] root
                        |> Expect.equal Nothing
            ]
        , Test.describe "findPathByKey"
            [ Test.test "finds nested node" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.group "mid"
                                    [ Node.rect "deep" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                    ]
                                ]
                    in
                    Path.findPathByKey "deep" root
                        |> Expect.equal (Just [ 0, 0 ])
            ]
        , Test.describe "flattenedPathsPreorder"
            [ Test.test "matches sidebar row order (root, first child, nested, then next root child)" <|
                \() ->
                    let
                        inner : Node
                        inner =
                            Node.group "inner"
                                [ Node.rect "leaf" { x = 0, y = 0, w = 1, h = 1, color = Color.white } ]

                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "a" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , inner
                                , Node.rect "c" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]
                    in
                    Path.flattenedPathsPreorder root
                        |> Expect.equal [ [], [ 0 ], [ 1 ], [ 1, 0 ], [ 2 ] ]
            ]
        , Test.describe "canMoveTreeNodeUp"
            [ Test.test "False for first child under root" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "first" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "second" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]
                    in
                    Path.canMoveTreeNodeUp [ 0 ] root
                        |> Expect.equal False
            , Test.test "True for second child under root" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "first" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "second" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]
                    in
                    Path.canMoveTreeNodeUp [ 1 ] root
                        |> Expect.equal True
            , Test.test "True for first child of nested group (promote)" <|
                \() ->
                    let
                        inner : Node
                        inner =
                            Node.group "inner"
                                [ Node.rect "only" { x = 0, y = 0, w = 1, h = 1, color = Color.white } ]

                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "before" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , inner
                                ]
                    in
                    Path.canMoveTreeNodeUp [ 1, 0 ] root
                        |> Expect.equal True
            ]
        , Test.describe "canMoveTreeNodeDown"
            [ Test.test "True for first child under root when a sibling follows" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "first" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "last" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]
                    in
                    Path.canMoveTreeNodeDown [ 0 ] root
                        |> Expect.equal True
            , Test.test "False for last child under root" <|
                \() ->
                    let
                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "first" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "last" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]
                    in
                    Path.canMoveTreeNodeDown [ 1 ] root
                        |> Expect.equal False
            , Test.test "True for last child of nested group (promote)" <|
                \() ->
                    let
                        inner : Node
                        inner =
                            Node.group "inner"
                                [ Node.rect "mid" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , Node.rect "last" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                ]

                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "before" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , inner
                                ]
                    in
                    Path.canMoveTreeNodeDown [ 1, 1 ] root
                        |> Expect.equal True
            , Test.test "True for sole leaf of nested group (last in flattened preorder, can promote to root)" <|
                \() ->
                    let
                        inner : Node
                        inner =
                            Node.group "inner"
                                [ Node.rect "onlyLeaf" { x = 0, y = 0, w = 1, h = 1, color = Color.white } ]

                        root : Node
                        root =
                            Node.group "root"
                                [ Node.rect "before" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                , inner
                                ]
                    in
                    Path.canMoveTreeNodeDown [ 1, 0 ] root
                        |> Expect.equal True
            ]
        ]

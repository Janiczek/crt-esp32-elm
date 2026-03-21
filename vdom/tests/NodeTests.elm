module NodeTests exposing (suite)

import Color
import Expect
import Fuzzers
import Node exposing (Node, Type(..))
import Set
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Node"
        [ Test.describe "size"
            [ Test.fuzz Fuzzers.leafNode "leaf nodes have size 1" <|
                \leafNode ->
                    Node.size leafNode
                        |> Expect.equal 1
            , Test.test "nested groups count all descendants" <|
                \() ->
                    let
                        tree : Node
                        tree =
                            Node.group "root"
                                [ Node.rect "a" { x = 0, y = 0, w = 1, h = 1, color = Color.black }
                                , Node.group "mid"
                                    [ Node.rect "b" { x = 0, y = 0, w = 1, h = 1, color = Color.gray }
                                    , Node.rect "c" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                    , Node.group "deep"
                                        [ Node.rect "d" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                        ]
                                    ]
                                ]
                    in
                    Node.size tree
                        |> Expect.equal 7
            , Test.fuzz Fuzzers.node "wrapping in a group increases size by one" <|
                \node_ ->
                    Node.size (Node.group "wrap" [ node_ ])
                        |> Expect.equal (Node.size node_ + 1)
            , Test.fuzz Fuzzers.node "wrapping in a group and adding a sibling increases size by two" <|
                \node_ ->
                    Node.size
                        (Node.group "wrap"
                            [ node_
                            , Node.rect "sibling" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                            ]
                        )
                        |> Expect.equal (Node.size node_ + 2)
            ]
        , Test.describe "maxGroupChildren"
            [ Test.test "leaf nodes have maxGroupChildren 0" <|
                \() ->
                    Node.maxGroupChildren (Node.rect "leaf" { x = 0, y = 0, w = 1, h = 1, color = Color.white })
                        |> Expect.equal 0
            , Test.test "nested groups track widest group" <|
                \() ->
                    let
                        tree : Node
                        tree =
                            Node.group "root"
                                [ Node.rect "a" { x = 0, y = 0, w = 1, h = 1, color = Color.black }
                                , Node.group "mid"
                                    [ Node.rect "b" { x = 0, y = 0, w = 1, h = 1, color = Color.gray }
                                    , Node.rect "c" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                    , Node.group "deep"
                                        [ Node.rect "d" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                        ]
                                    ]
                                ]
                    in
                    Node.maxGroupChildren tree
                        |> Expect.equal 3
            , Test.fuzz Fuzzers.node "wrapping in a group with same number of siblings increases maxGroupChildren by one" <|
                \node_ ->
                    let
                        maxChildren : Int
                        maxChildren =
                            Node.maxGroupChildren node_

                        sibling : Node
                        sibling =
                            Node.rect "sib" { x = 0, y = 0, w = 1, h = 1, color = Color.gray }

                        siblings : List Node
                        siblings =
                            List.repeat maxChildren sibling

                        wrapped : Node
                        wrapped =
                            Node.group "wrap" (node_ :: siblings)
                    in
                    Node.maxGroupChildren wrapped
                        |> Expect.equal (maxChildren + 1)
            ]
        , Test.describe "maxFontIndex"
            [ Test.fuzz Fuzzers.leafNode "leaf nodes other than text have maxFontIndex 0" <|
                \leafNode ->
                    case leafNode.type_ of
                        Text _ ->
                            Expect.pass

                        Group _ ->
                            Debug.todo "shouldn't happen"

                        _ ->
                            Node.maxFontIndex leafNode
                                |> Expect.equal 0
            , Test.fuzz Fuzzers.textNode "text node: maxFontIndex == .fontIndex" <|
                \textNode ->
                    case textNode.type_ of
                        Text r ->
                            Node.maxFontIndex textNode
                                |> Expect.equal r.fontIndex

                        _ ->
                            Debug.todo "shouldn't happen"
            , Test.test "Goes into groups" <|
                \() ->
                    Node.maxFontIndex
                        (Node.group "root"
                            [ Node.text [] "text" { x = 0, y = 0, text = "text", fontIndex = 3, color = Color.white }
                            ]
                        )
                        |> Expect.equal 3
            ]
        , Test.describe "allKeys"
            [ Test.test "collects every key in the tree" <|
                \() ->
                    let
                        tree : Node
                        tree =
                            Node.group "root"
                                [ Node.rect "a" { x = 0, y = 0, w = 1, h = 1, color = Color.black }
                                , Node.group "mid"
                                    [ Node.rect "b" { x = 0, y = 0, w = 1, h = 1, color = Color.gray }
                                    , Node.rect "c" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                    , Node.group "deep"
                                        [ Node.rect "d" { x = 0, y = 0, w = 1, h = 1, color = Color.white }
                                        ]
                                    ]
                                ]
                    in
                    Node.allKeys tree
                        |> Expect.equal (Set.fromList [ "root", "a", "mid", "b", "c", "deep", "d" ])
            ]
        , Test.describe "uniqueKeyAmong"
            [ Test.test "returns base when unused" <|
                \() ->
                    Node.uniqueKeyAmong (Set.singleton "other") "rect"
                        |> Expect.equal "rect"
            , Test.test "appends -2 when base is taken" <|
                \() ->
                    Node.uniqueKeyAmong (Set.singleton "rect") "rect"
                        |> Expect.equal "rect-2"
            , Test.test "skips -2 when that key is also taken" <|
                \() ->
                    Node.uniqueKeyAmong (Set.fromList [ "rect", "rect-2" ]) "rect"
                        |> Expect.equal "rect-3"
            , Test.test "doesn't skip -3 even though -4 exists" <|
                \() ->
                    Node.uniqueKeyAmong (Set.fromList [ "rect", "rect-2", "rect-4" ]) "rect"
                        |> Expect.equal "rect-3"
            ]
        ]

module Tree.Transform exposing (..)

import List.Extra
import Tree exposing (Tree)
import Tree.Graph exposing (Edge, Graph, Node)
import Tree.Lib as Lib


toGraph : (a -> String) -> Tree a -> Graph
toGraph nodeToString tree =
    let
        edges : List ( String, String )
        edges =
            Lib.edges tree |> List.map (\( a, b ) -> ( nodeToString a, nodeToString b ))

        edgeGroups : List (List ( String, String ))
        edgeGroups =
            List.Extra.groupWhile (\a b -> Tuple.first a == Tuple.first b) edges
                |> List.map (\( a, b ) -> a :: b)
                |> Debug.log "EDGE GROUPS"

        root =
            { name = Tree.label tree, x = 0, y = 0, r = 15, color = "red" }
    in
    []


xCoordinate : Float -> Int -> Int -> Float -> Float
xCoordinate origin i n halfWidth =
    origin - halfWidth + 2 * (toFloat i / toFloat (n // 2)) * halfWidth


treeRoot : String -> Node
treeRoot name =
    { name = name, x = 0, y = 0, r = 15, color = "red" }


groupToNodes : Node -> Float -> List ( String, String ) -> List Node
groupToNodes rootNode halfWidth edges =
    let
        n =
            List.length edges

        y =
            rootNode.y + 1

        endPoints : List String
        endPoints =
            List.map Tuple.second edges
    in
    List.indexedMap (\i na -> { name = na, x = xCoordinate rootNode.x i n halfWidth, y = y, r = 15, color = "red" }) endPoints


groupToEdges : Node -> Float -> List ( String, String ) -> List Edge
groupToEdges rootNode halfWidth edges =
    let
        nodes =
            groupToNodes rootNode halfWidth edges
    in
    List.map (\n -> { from = rootNode, to = n, color = "gray" }) nodes

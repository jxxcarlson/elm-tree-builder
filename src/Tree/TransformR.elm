module Tree.TransformR exposing (State, Step(..), groupToEdges, groupToNodes, init, loop, nextStep, toGraph, treeRoot, xCoordinate)

import Dict exposing (Dict)
import HTree
import List.Extra
import Tree exposing (Tree)
import Tree.Graph exposing (Edge, Graph, Node)
import Tree.Lib as Lib


toGraph : Float -> (a -> String) -> Tree a -> Graph
toGraph halfWidth labelToString tree =
    loop (init halfWidth labelToString tree) nextStep


type alias State =
    { dict : Dict String Node
    , depths : Dict String Int
    , groups : List ( ( String, String ), List ( String, String ) )
    , graph : List Edge
    , halfWidth : Float
    }


init : Float -> (a -> String) -> Tree a -> State
init halfWidth renderLabel tree =
    let
        depths : Dict String Int
        depths =
            tree |> Tree.map renderLabel |> HTree.tagWithDepth |> Tree.flatten |> Dict.fromList

        edges : List ( String, String )
        edges =
            Lib.edges tree |> List.map (\( a, b ) -> ( renderLabel a, renderLabel b ))

        edgeGroups : List ( ( String, String ), List ( String, String ) )
        edgeGroups =
            List.Extra.groupWhile (\a b -> Tuple.first a == Tuple.first b) edges

        root : Node
        root =
            { name = Tree.label tree |> renderLabel, x = 0, y = 0, r = 12, color = "blue" }
    in
    { dict = Dict.fromList [ ( root.name, root ) ]
    , depths = depths
    , groups = edgeGroups
    , graph = []
    , halfWidth = halfWidth
    }


nextStep : State -> Step State Graph
nextStep state =
    case List.head state.groups of
        Nothing ->
            Done state.graph

        Just ( ( a, b ), rest ) ->
            case Dict.get a state.dict of
                Nothing ->
                    Done state.graph

                Just root ->
                    let
                        level_ =
                            Dict.get root.name state.depths |> Maybe.withDefault 1 |> (\x -> x + 1)

                        level =
                            level_ |> toFloat

                        factor =
                            0.8 ^ level

                        halfWidth =
                            factor * state.halfWidth

                        newEdges =
                            groupToEdges level_ root halfWidth (( a, b ) :: rest)

                        newNodes =
                            List.map .to newEdges

                        newDict =
                            List.foldl (\n runningDict -> Dict.insert n.name n runningDict) state.dict newNodes
                    in
                    Loop
                        { state
                            | dict = newDict
                            , groups = List.drop 1 state.groups
                            , graph = state.graph ++ newEdges
                        }


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b


xCoordinate : Int -> Float -> Int -> Int -> Float -> Float
xCoordinate level origin i n halfWidth =
    let
        nn =
            toFloat n
    in
    if n == 1 then
        origin

    else
        let
            f =
                nn / (nn - 1)

            fi =
                f * toFloat i - nn / 2 - 0.0 * toFloat (level + 1) * (toFloat i - nn / 2)
        in
        origin + 2 * halfWidth * fi / (nn ^ 2)


treeRoot : String -> Node
treeRoot name =
    { name = name, x = 0, y = 0, r = 15, color = "blue" }


groupToNodes : Int -> Node -> Float -> List ( String, String ) -> List Node
groupToNodes level rootNode halfWidth edges =
    let
        n =
            List.length edges

        y =
            rootNode.y + 1

        endPoints : List String
        endPoints =
            List.map Tuple.second edges
    in
    List.indexedMap (\i na -> { name = na, x = xCoordinate level rootNode.x i n halfWidth, y = y, r = 12, color = "blue" }) endPoints


groupToEdges : Int -> Node -> Float -> List ( String, String ) -> List Edge
groupToEdges level rootNode halfWidth edges =
    let
        nodes =
            groupToNodes level rootNode halfWidth edges
    in
    List.map (\n -> { from = rootNode, to = n, color = "gray" }) nodes

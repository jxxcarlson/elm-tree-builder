module Tree.Transform exposing (..)

import Dict exposing (Dict)
import List.Extra
import Tree exposing (Tree)
import Tree.Graph exposing (Edge, Graph, Node)
import Tree.Lib as Lib


toGraph : Float -> (a -> String) -> Tree a -> Graph
toGraph halfWidth labelToString tree =
    loop (init halfWidth labelToString tree) nextStep


type alias State =
    { dict : Dict String Node
    , groups : List ( ( String, String ), List ( String, String ) )
    , graph : List Edge
    , halfWidth : Float
    }


init : Float -> (a -> String) -> Tree a -> State
init halfWidth renderLabel tree =
    let
        edges : List ( String, String )
        edges =
            Lib.edges tree |> List.map (\( a, b ) -> ( renderLabel a, renderLabel b ))

        edgeGroups : List ( ( String, String ), List ( String, String ) )
        edgeGroups =
            List.Extra.groupWhile (\a b -> Tuple.first a == Tuple.first b) edges
                -- |> List.map (\( a, b ) -> a :: b)
                |> Debug.log "EDGE GROUPS"

        root : Node
        root =
            { name = Tree.label tree |> renderLabel, x = 0, y = 0, r = 15, color = "red" }
    in
    { dict = Dict.fromList [ ( root.name, root ) ]
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
                        halfWidth =
                            0.7 * state.halfWidth

                        newEdges =
                            groupToEdges root halfWidth (( a, b ) :: rest)

                        newNodes =
                            List.map .to newEdges

                        newDict =
                            List.foldl (\n runningDict -> Dict.insert n.name n runningDict) state.dict newNodes
                    in
                    Loop
                        { dict = newDict
                        , groups = List.drop 1 state.groups
                        , graph = state.graph ++ newEdges
                        , halfWidth = halfWidth
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

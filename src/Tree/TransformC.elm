module Tree.TransformC exposing (..)

import Dict exposing (Dict)
import HTree
import List.Extra
import Tree exposing (Tree)
import Tree.Graph exposing (Edge, Graph, Node)
import Tree.Lib as Lib


type alias Preferences =
    { halfAngle : Float
    , initialEdgeLength : Float
    , scaleFactor : Float
    , rScale : Float
    , ballRadius : Float
    , ballColor : String
    }


toGraph : Preferences -> (a -> String) -> Tree a -> Graph
toGraph preferences labelToString tree =
    loop (init preferences labelToString tree) nextStep


type alias State =
    { edgeDict : Dict String Edge
    , depths : Dict String Int
    , groups : List ( ( String, String ), List ( String, String ) )
    , graph : List Edge
    , preferences : Preferences
    }


init : Preferences -> (a -> String) -> Tree a -> State
init preferences renderLabel tree =
    let
        depths : Dict String Int
        depths =
            tree |> Tree.map renderLabel |> HTree.tagWithDepth |> Tree.flatten |> Debug.log "DEPTHS" |> Dict.fromList

        edges : List ( String, String )
        edges =
            Lib.edges tree |> List.map (\( a, b ) -> ( renderLabel a, renderLabel b ))

        edgeGroups : List ( ( String, String ), List ( String, String ) )
        edgeGroups =
            List.Extra.groupWhile (\a b -> Tuple.first a == Tuple.first b) edges
                |> Debug.log "EDGE GROUPS"

        root : Node
        root =
            { name = Tree.label tree |> renderLabel, x = 0, y = 0, r = preferences.ballRadius, color = preferences.ballColor }

        origin =
            { name = "origin", x = 0, y = -preferences.initialEdgeLength, r = preferences.ballRadius, color = preferences.ballColor }
    in
    { edgeDict = Dict.fromList [ ( root.name, { from = origin, to = root, color = "gray" } ) ]
    , depths = depths
    , groups = edgeGroups
    , graph = []
    , preferences = preferences
    }


nextStep : State -> Step State Graph
nextStep state =
    case List.head state.groups of
        Nothing ->
            Done state.graph

        Just ( ( a, b ), rest ) ->
            case Dict.get a state.edgeDict of
                Nothing ->
                    Done state.graph

                Just edge ->
                    let
                        _ =
                            Debug.log "EDGE" ( ( edge.from.name, edge.from.r ), ( edge.to.name, edge.to.r ) )

                        endPointNames : List String
                        endPointNames =
                            b :: List.map Tuple.second rest

                        namePairs : List ( String, String )
                        namePairs =
                            ( a, b ) :: rest

                        level_ =
                            Dict.get edge.to.name state.depths |> Maybe.withDefault 1 |> (\x -> x + 1)

                        newEdges : List Edge
                        newEdges =
                            groupToEdges state.preferences level_ edge namePairs

                        _ =
                            Debug.log "NORMS (1)" (List.map (edgeToVector >> norm) newEdges)

                        newDict =
                            List.foldl (\e runningDict -> Dict.insert e.to.name e runningDict) state.edgeDict newEdges
                    in
                    Loop
                        { state
                            | edgeDict = newDict
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


pi =
    3.1416


theta halfAngle i n =
    (if n < 2 then
        0

     else
        -halfAngle + (2 * toFloat i * halfAngle) / (toFloat n - 1)
    )
        |> Debug.log "THETA"


norm : Vector -> Float
norm v =
    sqrt (v.x * v.x + v.y * v.y)


subtract : Vector -> Vector -> Vector
subtract a b =
    { x = a.x - b.x, y = a.y - b.y }


add : Vector -> Vector -> Vector
add a b =
    { x = a.x + b.x, y = a.y - b.y }


scalarMul : Float -> Vector -> Vector
scalarMul c v =
    { x = c * v.x, y = c * v.y }


type alias Vector =
    { x : Float, y : Float }


edgeToVector : Edge -> Vector
edgeToVector edge =
    { x = edge.to.x - edge.from.x, y = edge.to.y - edge.from.y }


nodeToVector : Node -> Vector
nodeToVector node =
    { x = node.x, y = node.y }


vectorToNode : String -> String -> Float -> Vector -> Node
vectorToNode name color radius vec =
    { name = name, color = color, r = radius, x = vec.x, y = vec.y }


displaceEdge : Vector -> Edge -> Edge
displaceEdge vector edge =
    { edge | from = displaceNode vector edge.from, to = displaceNode vector edge.to }


displaceNode : Vector -> Node -> Node
displaceNode vector node =
    { node | x = node.x + vector.x, y = node.y + vector.y }


rotateEdge : Float -> Edge -> Edge
rotateEdge theta_ edge =
    let
        dx =
            edge.to.x - edge.from.x

        dy =
            edge.to.y - edge.from.y

        co =
            cos theta_

        si =
            sin theta_

        xx =
            edge.from.x + co * dx - si * dy

        yy =
            edge.from.y + si * dx + co * dy

        to =
            edge.to
    in
    { edge | to = { to | x = xx, y = yy } }


rotateAndRescaleEdge : Float -> Float -> Edge -> Edge
rotateAndRescaleEdge r theta_ edge =
    let
        dx =
            edge.to.x - edge.from.x

        dy =
            edge.to.y - edge.from.y

        co =
            cos theta_

        si =
            sin theta_

        xx =
            edge.from.x + r * (co * dx - si * dy)

        yy =
            edge.from.y + r * (si * dx + co * dy)

        to =
            edge.to
    in
    { edge | to = { to | x = xx, y = yy } }


groupToEdges : Preferences -> Int -> Edge -> List ( String, String ) -> List Edge
groupToEdges preferences level rootEdge edgesAsLabelPairs =
    let
        n =
            List.length edgesAsLabelPairs

        dr =
            edgeToVector rootEdge

        start =
            displaceEdge dr rootEdge

        _ =
            norm (edgeToVector start) |> Debug.log "START, NORM"

        endPoints =
            List.map Tuple.second edgesAsLabelPairs

        newEdges =
            List.indexedMap (\i na -> satellite preferences level i n na start) endPoints

        _ =
            Debug.log "NORMS" (List.map (edgeToVector >> norm) newEdges)

        _ =
            Debug.log "NAMES" (List.map (.to >> .name) newEdges)
    in
    newEdges


satellite : Preferences -> Int -> Int -> Int -> String -> Edge -> Edge
satellite preferences level i n name edge =
    let
        r =
            preferences.scaleFactor
    in
    rotateAndRescaleEdge r (theta preferences.halfAngle i n) edge |> renameEndPoint name


renameEndPoint name edge =
    let
        to =
            edge.to
    in
    { edge | to = { to | name = name } }

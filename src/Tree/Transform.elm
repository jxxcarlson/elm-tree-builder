module Tree.Transform exposing (Preferences, defaults, toGraph)

{-|

@docs Preferences, LabelStyle, defaults, toGraph

-}

import Dict exposing (Dict)
import HTree
import List.Extra
import Tree exposing (Tree)
import Tree.Graph exposing (Edge, Graph, Node)
import Tree.Lib as Lib


{-| Preferences for function toGraph.
-}
type alias Preferences =
    { halfAngle : Float
    , initialEdgeLength : Float
    , scaleFactor : Float
    , ballRadius : Float
    , ballColor : String
    }


{-| Default Preferences
-}
defaults : Preferences
defaults =
    { halfAngle = 0.25 * pi
    , initialEdgeLength = 2
    , scaleFactor = 0.8
    , ballRadius = 0
    , ballColor = "blue"
    }


{-| Transform a Tree to a Graph: a data structure that can be rendered to SVG.
-}
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
            tree |> Tree.map renderLabel |> HTree.tagWithDepth |> Tree.flatten |> Dict.fromList

        edges : List ( String, String )
        edges =
            Lib.edges tree |> List.map (\( a, b ) -> ( renderLabel a, renderLabel b ))

        edgeGroups : List ( ( String, String ), List ( String, String ) )
        edgeGroups =
            List.Extra.groupWhile (\a b -> Tuple.first a == Tuple.first b) edges

        root : Node
        root =
            { name = Tree.label tree |> renderLabel, x = 0, y = 0, r = preferences.ballRadius, color = preferences.ballColor }

        origin =
            { name = root.name, x = 0, y = -preferences.initialEdgeLength, r = preferences.ballRadius, color = preferences.ballColor }
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
                        namePairs : List ( String, String )
                        namePairs =
                            ( a, b ) :: rest

                        newEdges : List Edge
                        newEdges =
                            groupToEdges state.preferences edge namePairs

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
    if n < 2 then
        0

    else
        -halfAngle + (2 * toFloat i * halfAngle) / (toFloat n - 1)


type alias Vector =
    { x : Float, y : Float }


edgeToVector : Edge -> Vector
edgeToVector edge =
    { x = edge.to.x - edge.from.x, y = edge.to.y - edge.from.y }


displaceEdge : Vector -> Edge -> Edge
displaceEdge vector edge =
    { edge | from = displaceNode vector edge.from, to = displaceNode vector edge.to }


displaceNode : Vector -> Node -> Node
displaceNode vector node =
    { node | x = node.x + vector.x, y = node.y + vector.y }


rotateAndRescaleEdge : Float -> Float -> Edge -> Edge
rotateAndRescaleEdge scaleFacgtor theta_ edge =
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
            edge.from.x + scaleFacgtor * (co * dx - si * dy)

        yy =
            edge.from.y + scaleFacgtor * (si * dx + co * dy)

        to =
            edge.to
    in
    { edge | to = { to | x = xx, y = yy } }


groupToEdges : Preferences -> Edge -> List ( String, String ) -> List Edge
groupToEdges preferences rootEdge edgesAsLabelPairs =
    let
        n =
            List.length edgesAsLabelPairs

        dr =
            edgeToVector rootEdge

        start =
            displaceEdge dr rootEdge

        endPoints =
            List.map Tuple.second edgesAsLabelPairs |> List.reverse

        newEdges =
            List.indexedMap (\i na -> satellite preferences i n na start) endPoints
    in
    newEdges


satellite : Preferences -> Int -> Int -> String -> Edge -> Edge
satellite preferences i n name edge =
    rotateAndRescaleEdge preferences.scaleFactor (theta preferences.halfAngle i n) edge |> renameEndPoint name


renameEndPoint name edge =
    let
        to =
            edge.to
    in
    { edge | to = { to | name = name } }

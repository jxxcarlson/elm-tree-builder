module Tree.Svg exposing (render, transform)

{-| Helper functions for rendering trees:

    import Tree.Build
    import Tree.Transform

    tree =
        Tree.Build.fromString "?" identity "1\n 2\n 3\n 4\n"

    graph =
        Result.map (Tree.Transform.toGraph preferences identity) tree |> Result.withDefault []


    Tree.Svg.render (Tree.Svg.transform 280 100 60 60 0.5 graph)

@docs render, transform

-}

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tree.Graph exposing (Edge, Graph, Node)


transform : Float -> Float -> Float -> Float -> Float -> Graph -> Graph
transform dx dy sx sy sr graph =
    List.map (transformEdge dx dy sx sy sr) graph


render : Graph -> List (Svg msg)
render graph =
    List.map renderEdge graph |> List.concat


transformEdge : Float -> Float -> Float -> Float -> Float -> Edge -> Edge
transformEdge dx dy sx sy sr edge =
    { edge | from = transformNode dx dy sx sy sr edge.from, to = transformNode dx dy sx sy sr edge.to }


transformNode : Float -> Float -> Float -> Float -> Float -> Node -> Node
transformNode dx dy sx sy sr node =
    { node | x = sx * node.x + dx, y = sy * node.y + dy, r = sr * node.r }


renderEdge : Edge -> List (Svg msg)
renderEdge edge =
    [ svgLine edge.from.x edge.from.y edge.to.x edge.to.y edge.color
    , renderNode edge.from
    , renderNode edge.to
    ]


renderNode : Node -> Svg msg
renderNode node =
    svgCircle node.x node.y node.r node.color


svgCircle : Float -> Float -> Float -> String -> Svg msg
svgCircle x y rr color =
    circle
        [ cx (String.fromFloat x)
        , cy (String.fromFloat y)
        , r (String.fromFloat rr)
        , fill color
        ]
        []


svgLine : Float -> Float -> Float -> Float -> String -> Svg msg
svgLine x1_ y1_ x2_ y2_ color =
    line
        [ x1 (String.fromFloat x1_)
        , y1 (String.fromFloat y1_)
        , x2 (String.fromFloat x2_)
        , y2 (String.fromFloat y2_)
        , stroke color
        ]
        []

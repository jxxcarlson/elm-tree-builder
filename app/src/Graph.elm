module Graph exposing (Edge, Graph, Node, render, transform)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Graph =
    List Edge


type alias Edge =
    { from : Node, to : Node, color : String }


type alias Node =
    { name : String, x : Float, y : Float, r : Float, color : String }


transform : Float -> Float -> Float -> Float -> Float -> Graph -> Graph
transform dx dy sx sy sr graph =
    List.map (transformEdge dx dy sx sy sr) graph


transformEdge : Float -> Float -> Float -> Float -> Float -> Edge -> Edge
transformEdge dx dy sx sy sr edge =
    { edge | from = transformNode dx dy sx sy sr edge.from, to = transformNode dx dy sx sy sr edge.to }


transformNode : Float -> Float -> Float -> Float -> Float -> Node -> Node
transformNode dx dy sx sy sr node =
    { node | x = sx * node.x + dx, y = sy * node.y + dy, r = sr * node.r }


render : Graph -> List (Svg msg)
render graph =
    List.map renderEdge graph |> List.concat


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

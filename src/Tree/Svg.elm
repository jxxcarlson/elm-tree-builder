module Tree.Svg exposing (LabelStyle(..), render, transform)

{-| Helper functions for rendering trees:

    import Tree.Build
    import Tree.Render

    -- Build the tree from text
    tree =
        Tree.Build.fromString "?" .content "1\n 2\n 3\n 4\n"

    -- Build the graph
    graph =
        Result.map (Tree.Render.toGraph preferences identity)
          tree |> Result.withDefault []


    -- Render the graph.  The first argument is the label style.
    Tree.Svg.render NoLabel (Tree.Svg.transform 280 100 60 60 0.5 graph)

@docs LabelStyle, render, transform

-}

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tree.Graph exposing (Edge, Graph, Node)


{-| -}
type LabelStyle
    = NoLabel
    | FullLabel
    | FirstWord


{-| Transform: shift by dx, dy and rescale by sx,sy,sr where the
arguments are dx, dy, sx, sy, sr, graph
-}
transform : Float -> Float -> Float -> Float -> Float -> Graph -> Graph
transform dx dy sx sy sr graph =
    List.map (transformEdge dx dy sx sy sr) graph


{-| Render a graph to SVG
-}
render : LabelStyle -> Graph -> List (Svg msg)
render labelStyle graph =
    List.map (renderFirstEdge labelStyle) (List.take 1 graph) ++ List.map (renderEdge labelStyle) graph |> List.concat


{-| Translate and rescale and edge.
-}
transformEdge : Float -> Float -> Float -> Float -> Float -> Edge -> Edge
transformEdge dx dy sx sy sr edge =
    { edge | from = transformNode dx dy sx sy sr edge.from, to = transformNode dx dy sx sy sr edge.to }


transformNode : Float -> Float -> Float -> Float -> Float -> Node -> Node
transformNode dx dy sx sy sr node =
    { node | x = sx * node.x + dx, y = sy * node.y + dy, r = sr * node.r }


renderFirstEdge : LabelStyle -> Edge -> List (Svg msg)
renderFirstEdge labelStyle edge =
    [ svgLine edge.from.x edge.from.y edge.to.x edge.to.y edge.color
    , renderNodeWithLabel labelStyle edge.from
    , renderNode edge.to
    ]


renderEdge : LabelStyle -> Edge -> List (Svg msg)
renderEdge labelStyle edge =
    [ svgLine edge.from.x edge.from.y edge.to.x edge.to.y edge.color
    , renderNode edge.from
    , renderNodeWithLabel labelStyle edge.to
    ]


renderNodeWithLabel : LabelStyle -> Node -> Svg msg
renderNodeWithLabel labelStyle node =
    let
        label =
            case labelStyle of
                NoLabel ->
                    ""

                FullLabel ->
                    node.name

                FirstWord ->
                    node.name |> String.split " " |> List.head |> Maybe.withDefault ""
    in
    svg []
        [ svgCircle node.x node.y node.r node.color
        , text_
            [ x (String.fromFloat (node.x + 12))
            , y (String.fromFloat (node.y + 0))
            , fill "gray"
            ]
            [ text label ]
        ]


renderNode : Node -> Svg msg
renderNode node =
    svg []
        [ svgCircle node.x node.y node.r node.color
        ]


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

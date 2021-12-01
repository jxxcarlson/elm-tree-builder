module Main exposing (..)

import Example
import Graph
import Svg exposing (..)
import Svg.Attributes exposing (..)


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


main =
    svg
        [ width "420"
        , height "420"
        , viewBox "0 0 420 420"
        ]
        ([ rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            , fill "white"
            ]
            []
         ]
            ++ Graph.render (Graph.transform 280 100 40 60 0.5 Example.grr)
        )


main1 =
    svg
        [ width "420"
        , height "420"
        , viewBox "0 0 420 420"
        ]
        [ rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            , fill "white"
            ]
            []
        , svgLine 50 50 150 150 "gray"
        , svgCircle 50 50 15 "red"
        , svgCircle 150 150 15 "red"
        ]

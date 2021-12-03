module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tree.Build as Build
import Tree.Svg
import Tree.Transform exposing (defaults)


tree =
    Build.fromString "?" identity "1\n 2\n  4\n  u\n  5\n   a\n  6\n 3\n  7\n   9\n   10\n  8\n   x\n   y\n    z1\n    z2\n    z3"


graph =
    Result.map (Tree.Transform.toGraph preferences identity) tree |> Result.withDefault []


preferences =
    { defaults | ballRadius = 10 }


main =
    svg
        [ width "900"
        , height "900"
        , viewBox "0 0 900 900"
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
            ++ Tree.Svg.render (Tree.Svg.transform 280 100 60 60 0.5 graph)
        )

module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tree.Build as Build
import Tree.Svg
import Tree.Transform exposing (defaults)


tree1 =
    Build.fromString "?" identity "1 Intro\n 2 Home\n  3 Pets\n  4 Garden\n   5 Trees\n   6 Shrubs\n   7 Flowers"


tree2 =
    Build.fromString "?" identity "1\n 2\n  3\n  4\n"



--\n 3\n  6\n   10\n   11"


graph =
    Result.map (Tree.Transform.toGraph preferences identity) tree2 |> Result.withDefault []


preferences =
    { defaults | ballRadius = 10, halfAngle = 0.23 * pi, scaleFactor = 1 }


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
            ++ Tree.Svg.render Tree.Svg.FirstWord (Tree.Svg.transform 280 100 60 60 0.5 graph)
        )

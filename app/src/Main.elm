module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tree.Build as Build
import Tree.Svg
import Tree.Transform exposing (defaults)
import Tree.TransformR as TR


tree1 =
    Build.fromString "?" identity forestData


forestData =
    """
*
  1
    2
    3
  4
    5
    6
"""


forestData1 =
    """
*
 1
  2
  3
 4
  5
  6
"""


str =
    """
1
 2
  3
   4
   5
  6
 7
"""


str2 =
    """
1
 2
  3
   4
   5
  6
 7
  8
   9
   10
   11
"""


outline =
    """
1 Home
  2 Important things (according to Cicero)
    3 Library
    4 Garden
      5 Pond
      6 Grasses
      7 Flowers
"""


tree2 =
    Build.fromString "?" identity "1\n 2\n  3\n  4\n"



--\n 3\n  6\n   10\n   11"


graph =
    Result.map (Tree.Transform.toGraph preferences identity) tree1 |> Result.withDefault []


graph1 =
    Result.map (TR.toGraph 3 identity) tree1 |> Result.withDefault []


preferences =
    { defaults | ballRadius = 10, halfAngle = 0.1 * pi, scaleFactor = 0.85 }


main =
    svg
        [ width "900"
        , height "900"
        , viewBox "0 0 1000 1000"
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
            ++ Tree.Svg.render Tree.Svg.FullLabel (Tree.Svg.transform 480 100 60 60 0.5 graph1)
        )

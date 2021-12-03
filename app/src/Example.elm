module Example exposing (graph)

import Graph exposing (Edge, Graph, Node)
import Tree.Build as Build
import Tree.Transform
import Tree.TransformC


tree0 =
    Build.fromString "?" identity "1\n  2\n  3\n  4"


tree0a =
    Build.fromString "?" identity "1\n 2\n 3\n 4\n  5\n  6\n  7b\n  8"


tree1 =
    Build.fromString "?" identity "1\n 2\n  4\n  5\n 3\n  6\n   8\n   9\n  7"


tree2 =
    Build.fromString "?" identity "1\n 2\n  4\n  5\n   a\n  6\n 3\n  7\n   9\n   10\n  8\n   x\n   y"


tree3 =
    Build.fromString "?" identity "1\n 2\n  4\n  u\n  5\n   a\n  6\n 3\n  7\n   9\n   10\n  8\n   x\n   y\n    z1\n    z2\n    z3"


pi =
    3.1416


preferences =
    { halfAngle = pi / 4, radius = 1.2, rScale = 1.0 }


graph =
    Result.map (Tree.TransformC.toGraph preferences identity) tree3 |> Result.withDefault []

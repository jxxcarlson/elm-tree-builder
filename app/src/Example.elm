module Example exposing (graph)

import Graph exposing (Edge, Graph, Node)
import Tree.Build as Build
import Tree.Transform


tree1 =
    Build.fromString "?" identity "1\n 2\n  4\n  5\n 3\n  6\n   8\n   9\n  7"


tree2 =
    Build.fromString "?" identity "1\n 2\n  4\n  5\n   a\n  6\n 3\n  7\n   9\n   10\n  8\n   x\n   y"


tree3 =
    Build.fromString "?" identity "1\n 2\n  4\n  u\n  5\n   a\n  6\n 3\n  7\n   9\n   10\n  8\n   x\n   y\n    z1\n    z2\n    z3"


graph =
    Result.map (Tree.Transform.toGraph 6 identity) tree3 |> Result.withDefault []

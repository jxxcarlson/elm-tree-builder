module Tree.Example.String exposing (..)

{-|

    > import Tree.Exampl.String exposing(..)

    > data
      "1\n 2\n  3\n  4\n  5\n 6\n7\n8\n 9\n 10" : String

    > tree
      Tree "root" [Tree "1" [Tree "2" [Tree "3" [],Tree "4" [],Tree "5" []],Tree "6" []],Tree "7" [],Tree "8" [Tree "9" [],Tree "10" []]]

    > import Tree.Render as R
    > R.render identity tree
      "1\n 2\n  3\n  4\n  5\n 6\n7\n8\n 9\n 10" : String

    > testResult
    "Ok" : String

-}

import Tree.Build as Build exposing (InitialData)
import Tree.Render as Render


initialData : InitialData String
initialData =
    { quant = 1
    , defaultNode = "?"
    , rootNode = "root"
    , makeNode = identity
    }


{-|

> R.render T.renderNode T.t
> "\\n1\\n2\\n 2\\n3\\n 4\\n5" : String

-}
tree =
    Build.fromString initialData data


testResult =
    Render.test 1 identity initialData data


data =
    """1
 2
  3
  4
  5
 6
7
8
 9
 10"""

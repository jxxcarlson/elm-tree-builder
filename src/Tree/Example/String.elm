module Tree.Example.String exposing (..)

{-|

    > import Tree.Exampl.String exposing(..)

    > data
      "1\n 2\n  3\n  4\n  5\n 6\n7\n8\n 9\n 10" : String

    > tree
      Tree "root" [Tree "1" [Tree "2" [Tree "3" [],Tree "4" [],Tree "5" []],Tree "6" []],Tree "7" [],Tree "8" [Tree "9" [],Tree "10" []]]

    > import Tree.Lib as R
    > R.render identity tree
      "1\n 2\n  3\n  4\n  5\n 6\n7\n8\n 9\n 10" : String

    > testResult
    "Ok" : String

-}

import Tree.Blocks as Blocks
import Tree.Build as Build exposing (InitialData)
import Tree.Lib as Lib
import Tree.Random


r maxCount seed =
    Lib.test 1 identity initialData (Tree.Random.generateOutline maxCount seed)


initialData : InitialData String
initialData =
    { defaultNode = "?"
    , makeNode = identity
    }


b stuff =
    Build.fromString initialData stuff


{-|

> R.render T.renderNode T.t
> "\\n1\\n2\\n 2\\n3\\n 4\\n5" : String

-}
tree =
    Build.fromString initialData data


f maxCount seed =
    let
        outline =
            Tree.Random.generateOutline maxCount seed

        ( quantum, _ ) =
            outline |> Blocks.fromString |> Blocks.wellFormed
    in
    Lib.test quantum identity initialData outline


testResult =
    Lib.test 1 identity initialData data


data =
    """
1
 2
  3
  4
"""


data2 =
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

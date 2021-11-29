module Tree.Example.List exposing (..)

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

import Tree.Build as Build exposing (InitialData)
import Tree.Lib as Lib


makeNode : String -> List Int
makeNode content =
    content
        |> String.split ","
        |> List.map (\s -> String.toInt s |> Maybe.withDefault 0)


renderNode : List Int -> String
renderNode ks =
    List.map String.fromInt ks |> String.join ","


initialData quant =
    { , defaultNode = []
    , makeNode = makeNode
    }


tree =
    Build.fromString (initialData 1) data2


test k str =
    Lib.test k renderNode (initialData k) str


testResult =
    test 1 data2


data =
    """1,1
  2,2
  3,3
4,4"""


data2 =
    """1,1
 2,2
 3,3
4,4"""


data3 =
    """1
  2,3
  4,5
  6
    7,8
    9,10
  11,12,13
    14
    15
    16"""

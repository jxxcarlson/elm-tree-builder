module StringTreeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tree.Build as Build exposing (InitialData)
import Tree.Render as Render


testItem : String -> String -> Test
testItem label str =
    test label <|
        \_ ->
            Render.test 1 identity initialData str |> Expect.equal "Ok"


initialData : InitialData String
initialData =
    { quant = 1
    , defaultNode = "?"
    , rootNode = "root"
    , makeNode = identity
    }


suite : Test
suite =
    describe "The Build module"
        [ testItem "A" a
        , testItem "B" b
        , testItem "C" c
        , testItem "D" d
        ]


a =
    """
1
 2
 3
"""


b =
    """
1
 2
 3
4
"""


c =
    """
1
 2
  3
4
"""


d =
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
"""

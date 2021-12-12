module ListTreeTest exposing (a, b, makeNode, renderNode, suite, testItem)

import Expect
import Test exposing (Test, describe, test)
import Tree.Test as T


stringToBlock str =
    { content = str, indent = 0, lineNumber = 0, numberOfLines = 0 }


blockToString block =
    block.content


testItem : String -> String -> Test
testItem label str =
    test label <|
        \_ ->
            T.test 1 "?" stringToBlock blockToString (String.trim str) |> Expect.equal "Ok"


renderNode : List Int -> String
renderNode js =
    List.map String.fromInt js |> String.join ","


makeNode : String -> List Int
makeNode str =
    str |> String.split "," |> List.map (String.toInt >> Maybe.withDefault 0)


suite : Test
suite =
    describe "The Build module"
        [ testItem "A" a
        , testItem "B" b

        --, testItem "C" c
        --, testItem "D" d
        ]


a =
    """1,1
 2,2
 3,3
"""


b =
    """1,1
 2,2
  3,3
 4,4"""

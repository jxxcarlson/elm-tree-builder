module StringTreeTest exposing (a, b, c, d, fuzzSuite, fuzzTest, fuzzTestAux, suite, testItem)

import Expect
import Fuzz
import Test exposing (Test, describe, fuzz2, test)
import Tree.Blocks as Blocks
import Tree.BlocksV as BlocksV
import Tree.Random
import Tree.Test as T


stringToBlock str =
    { content = str, indent = 0, lineNumber = 0, numberOfLines = 0 }


blockToString block =
    block.content


testItem : String -> String -> Test
testItem label str =
    test label <|
        \_ ->
            T.test 1 "?" stringToBlock blockToString str |> Expect.equal "Ok"


fuzzTest nodes label =
    fuzz2 (Fuzz.intRange 3 nodes) (Fuzz.intRange 0 10000) label <|
        \maxCount seed ->
            fuzzTestAux maxCount seed |> Expect.equal "Ok"


fuzzTestAux maxCount seed =
    let
        outline =
            Tree.Random.generateOutline maxCount seed

        blocks =
            Blocks.fromStringAsLines outline
    in
    T.test (BlocksV.quantumOfBlocks blocks) "?" stringToBlock blockToString outline


suite : Test
suite =
    describe "The Build module"
        [ testItem "A" a
        , testItem "B" b
        , testItem "C" c
        , testItem "D" d
        , testItem "E" e
        ]


fuzzSuite : Test
fuzzSuite =
    describe "Fuzz test for building trees"
        [ fuzzTest 5 "5 nodes"
        , fuzzTest 10 "10 nodes"
        , fuzzTest 20 "20 nodes"
        , fuzzTest 40 "40 nodes"
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


e =
    """
# Bad tree
1
 2
  5
   p
   q
  6
 3
 4
  7
   r
   s
   t
  8
"""

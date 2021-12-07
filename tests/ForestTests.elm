module ForestTests exposing (a, aa, b, c, suite, testForestNumber, testItem, x)

import Expect
import Test exposing (Test, describe, test)
import Tree.Build as Build
import Tree.Test as T


stringToBlock str =
    { content = str, indent = 0 }


blockToString block =
    block.content


testItem : Int -> String -> String -> Test
testItem quantum label str =
    test label <|
        \_ ->
            T.forestTest quantum "?" stringToBlock blockToString str |> Expect.equal "Ok"


testForestNumber : Int -> String -> String -> Test
testForestNumber expectedNumber label str =
    test label <|
        \_ ->
            Build.forestFromString "?" blockToString stringToBlock str |> Result.map List.length |> Expect.equal (Ok expectedNumber)


suite : Test
suite =
    describe "building and rendering forests"
        [ testItem 1 "X" x
        , testForestNumber 2 "X'" x
        , testItem 1 "A" a
        , testItem 1 "B" b
        , testForestNumber 2 "B'" b
        , testItem 1 "C" c
        , testForestNumber 2 "C'" c
        , testItem 2 "AA" aa
        , testForestNumber 2 "AA'" aa
        ]


x =
    """
1
 a
 b
  i
  j
  k
2
 c
 d
"""


a =
    """
1
 2
 3
4
 5
 7
"""


b =
    """
1
 2
  3
   4
   5
6
 7
 8
"""


c =
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


aa =
    """1
  2
  3
4
  5
  6"""



--
--fuzzTest nodes label =
--    fuzz2 (Fuzz.intRange 3 nodes) (Fuzz.intRange 0 10000) label <|
--        \maxCount seed ->
--            fuzzTestAux maxCount seed |> Expect.equal "Ok"
--
--
--fuzzTestAux maxCount seed =
--    let
--        outline : String
--        outline =
--            Tree.Random.generateOutline maxCount seed
--
--        blocks =
--            outline |> Blocks.fromString |> List.map (\b -> { b | indent = b.indent + 1 })
--
--        blocks2 =
--            { content = "root", indent = 0 } :: blocks
--
--        forest =
--            Build.forestFromBlocks "?" identity identity blocks
--
--        ( quantum, _ ) =
--            outline |> Blocks.fromString |> Blocks.wellFormed
--    in
--    Lib.test quantum "?" identity identity outline

module ForestTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Tree.Blocks as Blocks
import Tree.Build as Build
import Tree.Lib as Lib
import Tree.Random


testItem : Int -> String -> String -> Test
testItem quantum label str =
    test label <|
        \_ ->
            Lib.forestTest quantum "?" identity identity str |> Expect.equal "Ok"


suite : Test
suite =
    Test.only <|
        describe "The Build module"
            [ testItem 1 "A" a
            , testItem 1 "B" b

            --, testItem "C" c
            --, testItem "D" d
            ]


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

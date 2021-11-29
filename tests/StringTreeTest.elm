module StringTreeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Tree.Blocks as Blocks
import Tree.Build exposing (InitialData)
import Tree.Random
import Tree.Render as Render


testItem : String -> String -> Test
testItem label str =
    test label <|
        \_ ->
            Render.test 1 identity initialData str |> Expect.equal "Ok"


fuzzTest nodes label =
    fuzz2 (Fuzz.intRange 3 nodes) (Fuzz.intRange 0 10000) label <|
        \maxCount seed ->
            fuzzTestAux maxCount seed |> Expect.equal "Ok"


fuzzTestAux maxCount seed =
    let
        outline =
            Tree.Random.generateOutline maxCount seed

        ( quantum, _ ) =
            outline |> Blocks.fromString |> Blocks.wellFormed
    in
    Render.test quantum identity initialData outline


initialData : InitialData String
initialData =
    { defaultNode = "?"
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


fuzzSuite : Test
fuzzSuite =
    Test.only <|
        describe "Fuzz test for building trees"
            [ fuzzTest 5 "5 nodes"
            , fuzzTest 10 "10 nodes"
            , fuzzTest 20 "20 nodes"

            --, fuzzTest 40 "40 nodes"
            --, fuzzTest 80 "80 nodes"
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

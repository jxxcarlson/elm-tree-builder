module ListTreeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Tree.Lib as Lib
import Tree.Random
import Tree.Test as T


testItem : String -> String -> Test
testItem label str =
    test label <|
        \_ ->
            T.test 1 "?" identity identity (String.trim str) |> Expect.equal "Ok"


fuzzTest nodes label =
    fuzz2 (Fuzz.intRange 3 nodes) (Fuzz.intRange 0 10000) label <|
        \maxCount seed ->
            T.test 1 [] renderNode makeNode (Tree.Random.generateOutline maxCount seed)
                |> Expect.equal "Ok"


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



--fuzzSuite : Test
--fuzzSuite =
--   Test.only <|
--        describe "Fuzz test for building trees"
--            [ fuzzTest 5 "5 nodes"
--            , fuzzTest 10 "10 nodes"
--            , fuzzTest 20 "20 nodes"
--            , fuzzTest 40 "40 nodes"
--            , fuzzTest 80 "80 nodes"
--            ]

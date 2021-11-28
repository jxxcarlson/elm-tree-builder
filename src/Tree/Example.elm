module Tree.Example exposing (..)

import Tree exposing (Tree)
import Tree.Block as Block exposing (Block)
import Tree.Build as Build exposing (InitialData)
import Tree.Render as Unbuild


{-|

> R.render T.renderNode T.t
> "\\n1\\n2\\n 2\\n3\\n 4\\n5" : String

-}
t =
    build 1 blocks


build : Int -> List Block -> Tree (List Int)
build quant blocks_ =
    Build.fromBlocks (Unbuild.initialData (testData quant)) blocks_


makeNode : String -> List Int
makeNode content =
    content
        |> String.lines
        |> List.map (\s -> String.toInt s |> Maybe.withDefault 0)


renderNode : List Int -> String
renderNode ks =
    List.map String.fromInt ks |> String.join "\n"


testData quant =
    { quant = quant
    , defaultNode = []
    , rootNode = []
    , makeNode = makeNode
    , renderNode = renderNode
    }


blocks =
    Block.make data1


str1 =
    String.join "\n" data1


data1 =
    [ "1"
    , "2"
    , ""
    , " 2"
    , " 3"
    , ""
    , " 4"
    , " 5"
    ]


data2 =
    [ "1"
    , "2"
    , ""
    , " 2"
    , " 3"
    , ""
    , " 4"
    , " 5"
    , ""
    , "6"
    , "7"
    ]

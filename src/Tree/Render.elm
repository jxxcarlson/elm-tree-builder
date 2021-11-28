module Tree.Render exposing (initialData, render, test)

import Tree exposing (Tree)
import Tree.Block as Block
import Tree.Build exposing (InitialData)


type alias TestData node =
    { quant : Int
    , defaultNode : node
    , rootNode : node
    , makeNode : String -> node
    , renderNode : node -> String
    }


initialData : TestData node -> InitialData node
initialData testData =
    { quant = testData.quant
    , defaultNode = testData.defaultNode
    , rootNode = testData.rootNode
    , makeNode = testData.makeNode
    }


render : (a -> String) -> Tree a -> String
render renderNode tree =
    renderAux renderNode ( tree, -1 ) |> String.replace "root\n" "" |> String.dropRight 1


test : TestData node -> String -> String
test testData str =
    let
        str2 =
            Tree.Build.fromBlocks (initialData testData) (Block.make (String.lines str)) |> render testData.renderNode
    in
    if str == str2 then
        "Ok"

    else
        str ++ " ≠ " ++ str2


renderAux : (node -> String) -> ( Tree node, Int ) -> String
renderAux renderNode ( tree, level ) =
    let
        children =
            List.map (\c -> ( c, level + 1 )) (Tree.children tree)
    in
    prefix level
        ++ renderNode (Tree.label tree)
        ++ "\n"
        ++ String.join "" (List.map (renderAux renderNode) children)


prefix : Int -> String
prefix level =
    String.repeat level " "

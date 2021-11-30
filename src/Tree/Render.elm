module Tree.Render exposing (..)

import Tree exposing (Tree)


toString : Int -> (a -> String) -> Tree a -> String
toString quantum renderNode tree =
    renderAux quantum renderNode ( tree, 0 ) |> String.dropRight 1


forestToString : Int -> (a -> String) -> List (Tree a) -> String
forestToString quantum renderNode forest =
    List.map (toString quantum renderNode) forest |> String.join "\n"


renderAux : Int -> (node -> String) -> ( Tree node, Int ) -> String
renderAux quantum renderNode ( tree, level ) =
    let
        children =
            List.map (\c -> ( c, level + 1 )) (Tree.children tree)
    in
    prefix quantum level
        ++ renderNode (Tree.label tree)
        ++ "\n"
        ++ String.join "" (List.map (renderAux quantum renderNode) children)


prefix : Int -> Int -> String
prefix quantum level =
    String.repeat (quantum * level) " "

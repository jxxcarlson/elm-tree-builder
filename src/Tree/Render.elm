module Tree.Render exposing (test, toString)

import Tree exposing (Tree)
import Tree.Build exposing (InitialData)


type alias TestData node =
    { quant : Int
    , defaultNode : node
    , makeNode : String -> node
    , renderNode : node -> String
    }


toString : Int -> (a -> String) -> Tree a -> String
toString quantum renderNode tree =
    renderAux quantum renderNode ( tree, 0 ) |> String.dropRight 1


test : Int -> (node -> String) -> InitialData node -> String -> String
test quantum renderNode initialData str =
    let
        maybeStr2 =
            Tree.Build.fromString initialData str |> Result.toMaybe |> Maybe.map (toString quantum renderNode)
    in
    if Just (String.trim str) == maybeStr2 then
        "Ok"

    else
        case maybeStr2 of
            Nothing ->
                "Failed to build tree or render it to string"

            Just str2 ->
                String.trim str ++ " ≠ " ++ str2


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

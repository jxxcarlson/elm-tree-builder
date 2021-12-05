module Tree.Test exposing (forestTest, test)

import Tree.Build
import Tree.Render as Render


forestTest : Int -> node -> (node -> String) -> (String -> node) -> String -> String
forestTest quantum defaultNode renderNode makeNode str =
    let
        maybeStr2 =
            Tree.Build.forestFromString defaultNode makeNode renderNode str
                |> Result.toMaybe
                |> Maybe.map (Render.forestToString quantum renderNode)
    in
    if Just (String.trim str) == maybeStr2 then
        "Ok"

    else
        case maybeStr2 of
            Nothing ->
                "Failed to build forest or render it to string"

            Just str2 ->
                String.trim str ++ " ≠ " ++ str2


removeComments : String -> String
removeComments str =
    str
        |> String.lines
        |> List.filter (\line -> String.left 1 line /= "#")
        |> String.join "\n"


test : Int -> node -> (node -> String) -> (String -> node) -> String -> String
test quantum defaultNode renderNode makeNode str_ =
    let
        str =
            removeComments str_

        maybeStr2 =
            Tree.Build.fromString defaultNode makeNode str |> Result.toMaybe |> Maybe.map (Render.toString quantum renderNode)
    in
    if Just (String.trim str) == maybeStr2 then
        "Ok"

    else
        case maybeStr2 of
            Nothing ->
                "Failed to build tree or render it to string"

            Just str2 ->
                String.trim str ++ " ≠ " ++ str2

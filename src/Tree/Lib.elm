module Tree.Lib exposing (preorder, test, toString)

import Tree exposing (Tree)
import Tree.Build exposing (InitialData)
import Tree.Zipper as Zipper exposing (Zipper)


type alias State a =
    { zipper : Maybe (Zipper a)
    , nodes : List a
    }


preorder : Tree a -> List a
preorder tree =
    loop (init tree) nextStep
        |> List.reverse


init : Tree a -> State a
init tree =
    let
        initialZipper : Zipper a
        initialZipper =
            Zipper.fromTree tree

        firstNode : a
        firstNode =
            Zipper.label initialZipper
    in
    { zipper = Just initialZipper
    , nodes = [ firstNode ]
    }


nextStep : State a -> Step (State a) (List a)
nextStep state =
    case state.zipper of
        Nothing ->
            Done state.nodes

        Just z ->
            let
                maybeNewZipper =
                    Zipper.forward z
            in
            case maybeNewZipper of
                Nothing ->
                    Done state.nodes

                Just newZipper ->
                    Loop { zipper = Just newZipper, nodes = Zipper.label newZipper :: state.nodes }


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


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b

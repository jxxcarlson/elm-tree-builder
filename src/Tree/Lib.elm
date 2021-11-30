module Tree.Lib exposing (edges, forestToString, levelOrder, preorder, preorderF, repeatF, toString)

import Tree exposing (Tree(..))
import Tree.Build
import Tree.Zipper as Zipper exposing (Zipper)


type alias State a =
    { zipper : Maybe (Zipper a)
    , nodes : List a
    }


type alias StateF a b =
    { zipper : Maybe (Zipper a)
    , nodes : List b
    }


preorder : Tree a -> List a
preorder tree =
    loop (init tree) nextStep
        |> List.reverse


{-|

    Given a tree, return a list of pairs of node labels
    representing edges of the tree.

-}
edges : Tree a -> List ( a, a )
edges tree =
    let
        f a t_ =
            ( a, List.length (Tree.children t_) )

        t1 : List ( a, Int )
        t1 =
            (\tt -> preorderF f tt) tree |> Debug.log "T1"

        l1 : List a
        l1 =
            repeatF t1 |> Debug.log "L1"

        l2 =
            levelOrder tree |> Debug.log "L2"
    in
    List.map2 (\a b -> ( a, b )) l1 (List.drop 1 l2)


repeatF : List ( a, Int ) -> List a
repeatF list =
    List.foldl (\( a, n ) acc -> List.repeat n a ++ acc) [] list |> List.reverse


preorderF : (a -> Tree a -> b) -> Tree a -> List b
preorderF f tree =
    loop (initF f tree) (nextStepF f)
        |> List.reverse


initF : (a -> Tree a -> b) -> Tree a -> StateF a b
initF f tree =
    let
        initialZipper =
            Zipper.fromTree tree

        firstNode =
            Zipper.label initialZipper
    in
    { zipper = Just initialZipper
    , nodes = [ f firstNode tree ]
    }


init : Tree a -> State a
init tree =
    let
        initialZipper =
            Zipper.fromTree tree

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


nextStepF : (a -> Tree a -> b) -> StateF a b -> Step (StateF a b) (List b)
nextStepF f state =
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
                    let
                        currentTree =
                            Zipper.tree newZipper

                        newNode =
                            f (Zipper.label newZipper) currentTree
                    in
                    Loop { zipper = Just newZipper, nodes = newNode :: state.nodes }


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


levelOrder : Tree a -> List a
levelOrder tree =
    lox [ tree ]


lox : List (Tree a) -> List a
lox list =
    case list of
        [] ->
            []

        first :: rest ->
            Tree.label first :: lox (rest ++ Tree.children first)



-- LOOP


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

module Tree.Lib exposing (preOrder, levelOrder, edges)

{-| Utility functions. preOrder and levelOrder transform
Tree a to List a, where the nodes are listed in the
given order. Given Tree a, the edges function returns
List (a,a), where a pair (p,q) is in the returned list
if and only if q is a child of p.

@docs preOrder, levelOrder, edges

-}

import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


{-| -}
preOrder : Tree a -> List a
preOrder tree =
    loop (init tree) nextStep
        |> List.reverse


{-| -}
levelOrder : Tree a -> List a
levelOrder tree =
    lox [ tree ]


{-|

    Return a list of pairs (x,y) where

        - x and y are of type a
        - (x,y) is in the list if and only if y is a chiild of x.

-}
edges : Tree a -> List ( a, a )
edges tree =
    let
        f a t_ =
            ( a, List.length (Tree.children t_) )

        t1 : List ( a, Int )
        t1 =
            (\tt -> preorderF f tt) tree

        l1 : List a
        l1 =
            repeatF t1

        l2 =
            levelOrder tree
    in
    List.map2 (\a b -> ( a, b )) l1 (List.drop 1 l2)



--FOR PREORDER


type alias State a =
    { zipper : Maybe (Zipper a)
    , nodes : List a
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



-- FOR EDGES


type alias StateF a b =
    { zipper : Maybe (Zipper a)
    , nodes : List b
    }


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



-- FOR LEVEL ORDER


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

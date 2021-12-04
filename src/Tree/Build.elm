module Tree.Build exposing (fromString, fromBlocks, forestFromString, forestFromBlocks, Error(..))

{-| Tools for building
a tree from a string or a list of blocks. A tree
is represented in text as an outline:

    Example:

        > data = "1\n 2\n 3\n 4\n5\n 6\n 7"

        > fromString "?" identity data
          Tree "0" [
                Tree "1" [Tree "2" [],Tree "3" [], Tree "4" []]
              , Tree "5" [Tree "6" [],Tree "7" []]
              ]

The first argument of fromString is a label for a default node.
The second argument, of type (String -> node), tells how to build a node from a string
representation of that node. Here we use the representation of rose trees of
[elm/rose-tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/).

@docs fromString, fromBlocks, forestFromString, forestFromBlocks, Error

-}

import Tree exposing (Tree)
import Tree.Blocks as Blocks exposing (Block)
import Tree.Zipper as Zipper exposing (Zipper)


{-| -}
type Error
    = EmptyBlocks
    | BlocksNotWellFormed


init : node -> (String -> node) -> List Block -> Result Error (State node)
init defaultNode makeNode blocks =
    case List.head blocks of
        Nothing ->
            Err EmptyBlocks

        Just rootBlock ->
            let
                ( quantum, wellFormed ) =
                    Blocks.wellFormed blocks
            in
            if wellFormed == False then
                Err BlocksNotWellFormed

            else
                Ok
                    { blocks = List.drop 1 blocks
                    , zipper = Zipper.fromTree <| Tree.tree (makeNode rootBlock.content) []
                    , indentationQuantum = quantum
                    , indent = 0
                    , level = 0
                    , make = makeNode
                    , default = Zipper.fromTree (Tree.tree defaultNode [])
                    }


type alias State node =
    { blocks : List Block
    , zipper : Zipper node
    , indentationQuantum : Int
    , indent : Int
    , level : Int
    , make : String -> node
    , default : Zipper node
    }


{-| -}
fromBlocks : node -> (String -> node) -> List Block -> Result Error (Tree node)
fromBlocks defaultNode makeNode blocks =
    case init defaultNode makeNode blocks of
        Err error ->
            Err error

        Ok initialState ->
            Ok <| loop initialState nextStep


{-| -}
fromString : node -> (String -> node) -> String -> Result Error (Tree node)
fromString defaultNode makeNode str =
    str
        |> Blocks.fromStringAsLines
        |> fromBlocks defaultNode makeNode


{-| Example:

    > forestFromString "?" identity identity "1\n 2\n 3"
      Ok [Tree "1" [Tree "2" [],Tree "3" []]]

    > forestFromString "?" identity identity "1\n 2\n 3\n4\n 5\n 6"
    Ok [Tree "1" [Tree "2" [],Tree "3" []],Tree "4" [Tree "5" [],Tree "6" []]]

-}
forestFromString : a -> (String -> a) -> (a -> String) -> String -> Result Error (List (Tree a))
forestFromString defaultNode makeNode renderNode str =
    str
        |> Blocks.fromStringAsLines
        |> forestFromBlocks defaultNode makeNode renderNode


{-| -}
forestFromBlocks : a -> (String -> a) -> (a -> String) -> List Block -> Result Error (List (Tree a))
forestFromBlocks defaultNode makeNode renderNode blocks =
    let
        quantum =
            Blocks.quantumOfBlocks blocks

        blocks1 : List Block
        blocks1 =
            blocks |> List.map (\b -> { b | indent = b.indent + quantum })

        blocks2 : List Block
        blocks2 =
            { content = renderNode defaultNode, indent = 0 } :: blocks1
    in
    fromBlocks defaultNode makeNode blocks2 |> Result.map Tree.children



-- FUNCTIONAL LOOP


nextStep : State node -> Step (State node) (Tree node)
nextStep state =
    case List.head state.blocks of
        Nothing ->
            Done (Zipper.toTree state.zipper)

        Just block ->
            case compare block.indent state.indent of
                GT ->
                    Loop <| handleGT block.indent block.content state

                EQ ->
                    Loop <| handleEQ block.indent block.content state

                LT ->
                    Loop <| handleLT block.indent block.content state


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



-- HANDLERS


handleEQ indent content state =
    let
        newTree =
            Tree.tree (state.make content) []
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , zipper = attachAtFocus newTree state.zipper
    }


handleGT indent content state =
    let
        newTree =
            Tree.tree (state.make content) []
    in
    case Zipper.lastChild state.zipper of
        Nothing ->
            -- This is the case for the first node of the tree after the root
            { state
                | blocks = List.drop 1 state.blocks
                , indent = indent
                , level = state.level + 1
                , zipper = attachAtFocus newTree state.zipper
            }

        Just newZipper ->
            { state
                | blocks = List.drop 1 state.blocks
                , indent = indent
                , level = state.level + 1
                , zipper = attachAtFocus newTree newZipper
            }


handleLT indent content state =
    let
        newTree =
            Tree.tree (state.make content) []

        deltaLevel =
            state.indent - indent // state.indentationQuantum
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , zipper = attachAtFocus newTree (repeat deltaLevel Zipper.parent state.zipper)
    }



-- HELPERS I


attachAtFocus : Tree.Tree a -> Zipper a -> Zipper a
attachAtFocus t z =
    Zipper.replaceTree (appendChild t z) z


appendChild : Tree a -> Zipper a -> Tree a
appendChild t z =
    Tree.appendChild t (Zipper.tree z)



-- HELPERS II


{-|

    Apply f to x n times

-}
repeatM : Int -> (a -> Maybe a) -> Maybe a -> Maybe a
repeatM n f x =
    if n == 0 then
        x

    else
        repeatM (n - 1) f (Maybe.andThen f x)


{-|

    Apply f to x n times

-}
repeat : Int -> (a -> Maybe a) -> a -> a
repeat n f x =
    case repeatM n f (Just x) of
        Nothing ->
            x

        Just y ->
            y

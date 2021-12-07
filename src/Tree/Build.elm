module Tree.Build exposing
    ( fromString, fromBlocks, forestFromString, forestFromBlocks, Error(..)
    , popUntil
    )

{-| This module provides tools for building
a tree from a string or a list of blocks. As noted
in the README, a tree
is represented in text as an outline:

     > data = "1\n 2\n 3\n 4\n5\n 6\n 7"

To build a tree from it, we apply the function fromString:

    fromString :
        node
        -> (Block -> a)
        -> String
        -> Result Error (Tree a)


    > fromString "?" .content data
      Tree "0" [
            Tree "1" [Tree "2" [],Tree "3" [], Tree "4" []]
          , Tree "5" [Tree "6" [],Tree "7" []]
      ]

The first argument of fromString is a label for a default node.
The second argument tells how to build a node from a Block.
In the example, we are building a tree with string labels,
so we need a function of type (Block -> String). Recall that

        type alias Block = { indent : Int, content: String }

Therefore

        .content : Block -> String

has the correct type. Here we use the representation of rose trees found in
[elm/rose-tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/).

**Note.** The build functions described here here make a guess as to what the "quantum of indentation"
is. The quantum of indentation is the largest integer Q such that all lines are indented
by some multiple of Q. The guess used is either 1 or the number of leading spaces in the first
line that has leading spaces.

@docs fromString, fromBlocks, forestFromString, forestFromBlocks, Error

-}

import Tree exposing (Tree)
import Tree.Blocks as Blocks exposing (Block)
import Tree.Zipper as Zipper exposing (Zipper)


{-| -}
type Error
    = EmptyBlocks
    | BlocksNotWellFormed


init : node -> (Block -> node) -> List Block -> Result Error (State node)
init defaultNode makeNode blocks =
    case List.head blocks of
        Nothing ->
            Err EmptyBlocks

        Just rootBlock ->
            Ok
                { blocks = List.drop 1 blocks
                , zipper = Zipper.fromTree <| Tree.tree (makeNode rootBlock) []
                , indentationQuantum = ProvisionalQuantum 1
                , indent = 0
                , level = 0
                , indentationChanges = []
                , make = makeNode
                , default = Zipper.fromTree (Tree.tree defaultNode [])
                }


type alias State node =
    { blocks : List Block
    , zipper : Zipper node
    , indentationQuantum : Quantum
    , indent : Int
    , indentationChanges : List Int
    , level : Int
    , make : Block -> node
    , default : Zipper node
    }


type Quantum
    = ProvisionalQuantum Int
    | ConfirmedQuantum Int


quantumValue : Quantum -> Int
quantumValue quantum =
    case quantum of
        ProvisionalQuantum k ->
            k

        ConfirmedQuantum k ->
            k


{-| -}
fromBlocks : a -> (Block -> a) -> List Block -> Result Error (Tree a)
fromBlocks defaultNode makeNode blocks =
    case init defaultNode makeNode blocks of
        Err error ->
            Err error

        Ok initialState ->
            Ok <| loop initialState nextStep


{-|

    > Build.fromString "?" .content "1\n 2\n 3"
      Ok (Tree "1" [Tree "2" [],Tree "3" []])

-}
fromString : a -> (Block -> a) -> String -> Result Error (Tree a)
fromString defaultNode makeNode str =
    str
        |> Blocks.fromStringAsLines
        |> fromBlocks defaultNode makeNode


{-| Example:

    > Build.forestFromString
          "?"
          .content
          (\str -> {indent = 0, content = str})
          "1\n 2\n 3\na\n b\n c"
      Ok [  Tree "1" [Tree "2" [],Tree "3" []]
           ,Tree "a" [Tree "b" [],Tree "c" []]
         ]

-}
forestFromString : a -> (Block -> a) -> (a -> Block) -> String -> Result Error (List (Tree a))
forestFromString defaultNode makeNode renderNode str =
    str
        |> Blocks.fromStringAsLines
        |> forestFromBlocks defaultNode makeNode renderNode


{-| -}
forestFromBlocks : a -> (Block -> a) -> (a -> Block) -> List Block -> Result Error (List (Tree a))
forestFromBlocks defaultNode makeNode renderNode blocks =
    let
        quantum =
            Blocks.quantumOfBlocks2 blocks

        blocks1 : List Block
        blocks1 =
            blocks |> List.map (\b -> { b | indent = b.indent + quantum })

        blocks2 : List Block
        blocks2 =
            renderNode defaultNode :: blocks1
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
                    Loop <| handleGT block.indent block state

                EQ ->
                    Loop <| handleEQ block.indent block state

                LT ->
                    Loop <| handleLT block.indent block state


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


getIndentationQuantum currentIndentatioQuantum blockIndentation =
    case currentIndentatioQuantum of
        ProvisionalQuantum _ ->
            if blockIndentation > 0 then
                ConfirmedQuantum blockIndentation

            else
                currentIndentatioQuantum

        ConfirmedQuantum k ->
            --if blockIndentation == 0 then
            --    ProvisionalQuantum 1
            --
            --else
            ConfirmedQuantum k


handleEQ : Int -> Block -> State node -> State node
handleEQ indent block state =
    let
        newTree =
            Tree.tree (state.make block) []
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , zipper = attachAtFocus newTree state.zipper

        --, indentationQuantum = getIndentationQuantum state.indentationQuantum block.indent
    }


handleGT : Int -> Block -> State node -> State node
handleGT indent block state =
    let
        newTree =
            Tree.tree (state.make block) []
    in
    case Zipper.lastChild state.zipper of
        Nothing ->
            -- This is the case for the first node of the tree after the root
            { state
                | blocks = List.drop 1 state.blocks
                , indent = indent
                , level = state.level + 1
                , indentationChanges = pushIndentationChange block.indent state.indentationChanges
                , zipper = attachAtFocus newTree state.zipper

                --, indentationQuantum = getIndentationQuantum state.indentationQuantum block.indent
            }

        Just newZipper ->
            { state
                | blocks = List.drop 1 state.blocks
                , indent = indent
                , level = state.level + 1
                , indentationChanges = pushIndentationChange block.indent state.indentationChanges
                , zipper = attachAtFocus newTree newZipper

                --, indentationQuantum = getIndentationQuantum state.indentationQuantum block.indent
            }


pushIndentationChange : Int -> List Int -> List Int
pushIndentationChange k ks =
    (k - List.sum ks) :: ks


handleLT : Int -> Block -> State node -> State node
handleLT indent block state =
    let
        newTree =
            Tree.tree (state.make block) []

        deltaInfo =
            popUntil (state.indent - indent) state.indentationChanges

        deltaLevel =
            deltaInfo.popped
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , level = state.level - deltaLevel
        , indentationChanges = deltaInfo.remaining
        , zipper = attachAtFocus newTree (repeat deltaLevel Zipper.parent state.zipper)

        -- , indentationQuantum = getIndentationQuantum state.indentationQuantum block.indent
    }


{-|

    > popUntil 5 [1,2,2,2]
      { popped = 3, remaining = [2], sum = 5 }

-}
popUntil : Int -> List Int -> { sum : Int, popped : Int, remaining : List Int }
popUntil goal input =
    popUntilAux goal { sum = 0, popped = 0, remaining = input }


popUntilAux : Int -> { sum : Int, popped : Int, remaining : List Int } -> { sum : Int, popped : Int, remaining : List Int }
popUntilAux goal { sum, popped, remaining } =
    case List.head remaining of
        Nothing ->
            { sum = sum, popped = popped, remaining = remaining }

        Just k ->
            let
                newSum =
                    sum + k
            in
            if newSum < goal then
                popUntilAux goal { sum = newSum, popped = popped + 1, remaining = List.drop 1 remaining }

            else
                { sum = newSum, popped = popped + 1, remaining = List.drop 1 remaining }



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

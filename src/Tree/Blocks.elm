module Tree.Blocks exposing
    ( Block, fromStringAsLines, fromStringAsParagraphs, make, quantumOfBlocks, wellFormed
    , quantumOfBlocks2
    )

{-| This module provides tools for converting text into a list of blocks, where

        type alias Block =
            { indent : Int
            , content : String
            }

Lists of blocks are used by the functions in Tree.Build to construct trees
form text. Here are some examples:

        > fromStringAsLines "one\n  two\n    three"
          [   { content = "one", indent = 0 }
            , { content = "two", indent = 2 }
            , { content = "three", indent = 4 }
          ]

        > fromStringAsParagraphs "one\nho ho ho!\n  two\n  ha ha ha!"
          [   { content = "one\nho ho ho!", indent = 0 }
            , { content = "two\nha ha ha!", indent = 2 }
          ]

@docs Block, fromStringAsLines, fromStringAsParagraphs, make, quantumOfBlocks, wellFormed

-}

import Tree.Line as Line
import Tree.Math as Math


{-| -}
type alias Block =
    { indent : Int
    , content : String
    }


differences : List Int -> List Int
differences xs =
    let
        n =
            List.length xs

        us =
            List.drop 1 xs

        vs =
            List.take (n - 1) xs
    in
    List.map2 (-) us vs


{-| Determine the "quantum of indentation." In a well-formed
list of blocks, when the indentation level increases, it increases
by one quantum. When it decreases, it decreases by one or
more quanta.

    > "1\n 2\n  3\n    4" |> fromString |> quantumOfBlocks
      1

    > "1\n  2\n    3\n      4" |> fromString |> quantumOfBlocks
       2

    > "1\n  2\n   3" |> fromString |> quantumOfBlocks

-}
quantumOfBlocks : List Block -> Int
quantumOfBlocks blocks =
    blocks |> List.map .indent |> differences |> List.filter (\n -> n > 0) |> Math.gcdList


quantumOfBlocks2 : List Block -> Int
quantumOfBlocks2 blocks =
    blocks |> List.map .indent |> differences |> List.filter (\n -> n > 0) |> List.head |> Maybe.withDefault 1


{-| Determine if a list of blocks is well-formed, meaning that when
the indentation level increases, it increases by one quantum
and no more. The return value is a pair consisting of the
quantum of indentation and True or False, depending on
whethe the list is well-formed.

    > "1\n 2\n 3\n  4\n 5" |> fromString |> wellFormed
      (1, True)

    > "1\n  2\n    3\n     4\n 5" |> fromString |> wellFormed
       (1, False)

    > "1\n  2\n  3\n    4\n5" |> fromString |> wellFormed
      (2, True)

-}
wellFormed : List Block -> ( Int, Bool )
wellFormed blocks =
    let
        quantum =
            quantumOfBlocks blocks

        outliers =
            blocks
                |> List.map .indent
                |> differences
                |> List.map (\k -> k // quantum)
                |> List.filter (\n -> n > 1)
    in
    ( quantum, List.length outliers == 0 )


{-| -}
fromStringAsLines : String -> List Block
fromStringAsLines str =
    str
        |> String.trim
        |> String.lines
        |> List.filter (\line -> String.left 1 line /= "#")
        |> List.map (\line -> Line.classify line)


{-| -}
fromStringAsParagraphs : String -> List Block
fromStringAsParagraphs str =
    str |> String.lines |> List.filter (\line -> String.left 1 line /= "#") |> make


{-| -}
make : List String -> List Block
make lines =
    loop (init lines) nextStep


init : List String -> State
init lines =
    { blocks = []
    , currentBlock = List.head lines |> Maybe.map Line.classify
    , lines = List.drop 1 lines
    , indent = 0
    }


type alias State =
    { blocks : List Block
    , currentBlock : Maybe Block
    , lines : List String
    , indent : Int
    }


nextStep : State -> Step State (List Block)
nextStep state =
    case List.head state.lines of
        Nothing ->
            case state.currentBlock of
                Nothing ->
                    Done (List.reverse state.blocks |> List.filter (\b -> b.content /= ""))

                Just block ->
                    Done (List.reverse (block :: state.blocks |> List.filter (\b -> b.content /= "")))

        Just rawLine ->
            let
                currentLine =
                    Line.classify rawLine
            in
            case compare currentLine.indent state.indent of
                GT ->
                    Loop <| handleGT currentLine state

                EQ ->
                    Loop <| handleEQ currentLine state

                LT ->
                    Loop <| handleLT currentLine state


handleGT currentLine state =
    case state.currentBlock of
        Nothing ->
            { state | lines = List.drop 1 state.lines, indent = currentLine.indent }

        Just block ->
            { state
                | lines = List.drop 1 state.lines
                , indent = currentLine.indent
                , blocks = block :: state.blocks
                , currentBlock = Just currentLine
            }


handleEQ currentLine state =
    case state.currentBlock of
        Nothing ->
            { state | lines = List.drop 1 state.lines }

        Just block ->
            if currentLine.content == "" then
                { state
                    | lines = List.drop 1 state.lines
                    , indent = currentLine.indent
                    , blocks = block :: state.blocks
                    , currentBlock = Just currentLine
                }

            else
                { state
                    | lines = List.drop 1 state.lines
                    , indent = currentLine.indent
                    , currentBlock = Just { block | content = block.content ++ "\n" ++ currentLine.content }
                }


handleLT currentLine state =
    case state.currentBlock of
        Nothing ->
            { state
                | lines = List.drop 1 state.lines
                , indent = currentLine.indent
            }

        Just block ->
            { state
                | lines = List.drop 1 state.lines
                , indent = currentLine.indent
                , blocks = block :: state.blocks
                , currentBlock = Just currentLine
            }


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

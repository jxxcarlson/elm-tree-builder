module Tree.BlocksV exposing (Block, fromStringAsLines, fromStringAsParagraphs, quantumOfBlocks)

{-| This module is like Tree.Blocks, except that if the first line of a block
is deemed to signal the beginning of a "verbatim block," all succeeding lines will be
incorporated in it, so long as their indentation level is greater than or equal to the
indentation level of the first line. To make this work, function fromStringAsParagraphs requires an additional argument:

    fromStringAsParagraphs :
        (String -> Bool)
        -> String
        -> List Block

The additional argument is a predicate which determines whether a line to be
considered the first line of a verbatim blck.

@docs Block, fromStringAsLines, fromStringAsParagraphs, quantumOfBlocks

-}

import Tree.Line as Line exposing (Line)
import Tree.Math as Math


{-| -}
type alias Block =
    { indent : Int
    , lineNumber : Int
    , numberOfLines : Int
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


{-| Used by Tree.Build
-}
quantumOfBlocks : List Block -> Int
quantumOfBlocks blocks =
    blocks |> List.map .indent |> differences |> List.filter (\n -> n > 0) |> Math.gcdList


{-| -}
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
        -- TODO: Big error below
        |> List.indexedMap (\k line -> Line.classify k line |> lineToBlock 1)


{-| -}
fromStringAsParagraphs : (String -> Bool) -> String -> List Block
fromStringAsParagraphs isVerbatimLine str =
    str |> String.lines |> List.filter (\line -> String.left 1 line /= "#") |> make isVerbatimLine


{-| -}
make : (String -> Bool) -> List String -> List Block
make isVerbatimLine lines =
    loop (init isVerbatimLine lines) nextStep


init : (String -> Bool) -> List String -> State
init isVerbatimLine lines =
    { blocks = []
    , currentBlock = List.head lines |> Maybe.map (Line.classify 0 >> lineToBlock 0)
    , lines = List.drop 1 lines
    , indent = 0
    , lineNumber = 0
    , inVerbatim = False
    , isVerbatimLine = isVerbatimLine
    }


lineToBlock : Int -> Line -> Block
lineToBlock numberOfLines { indent, content, lineNumber } =
    { indent = indent, content = content, lineNumber = lineNumber, numberOfLines = numberOfLines }


type alias State =
    { blocks : List Block
    , currentBlock : Maybe Block
    , lines : List String
    , indent : Int
    , lineNumber : Int
    , inVerbatim : Bool
    , isVerbatimLine : String -> Bool
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
                    -- TODO: the below is wrong
                    Line.classify (state.lineNumber + 1) rawLine
            in
            case compare currentLine.indent state.indent of
                GT ->
                    Loop <| handleGT (currentLine |> lineToBlock state.lineNumber) state

                EQ ->
                    Loop <| handleEQ (currentLine |> lineToBlock state.lineNumber) state

                LT ->
                    Loop <| handleLT (currentLine |> lineToBlock state.lineNumber) state


indentationOf k =
    String.repeat k " "


handleGT currentLine state =
    case state.currentBlock of
        Nothing ->
            { state | lines = List.drop 1 state.lines, indent = currentLine.indent }

        Just block ->
            if state.inVerbatim then
                -- add line to current block
                let
                    leadingSpaces =
                        indentationOf (currentLine.indent - state.indent)
                in
                { state
                    | lines = List.drop 1 state.lines
                    , lineNumber = state.lineNumber + 1
                    , currentBlock =
                        Just
                            { block
                                | content = block.content ++ "\n" ++ leadingSpaces ++ currentLine.content
                                , numberOfLines = state.lineNumber + 2 - block.lineNumber
                            }
                }

            else
                -- make new block
                { state
                    | lines = List.drop 1 state.lines
                    , lineNumber = state.lineNumber + 1
                    , indent = currentLine.indent
                    , blocks = block :: state.blocks
                    , currentBlock = Just currentLine |> Debug.log "GT, currentBLOCK"
                }


handleEQ currentLine state =
    case state.currentBlock of
        Nothing ->
            { state | lines = List.drop 1 state.lines }

        Just block ->
            if currentLine.content == "" then
                -- make new block and reset inVerbatim
                { state
                    | lines = List.drop 1 state.lines
                    , lineNumber = state.lineNumber + 1
                    , indent = currentLine.indent
                    , blocks = block :: state.blocks
                    , inVerbatim = state.isVerbatimLine currentLine.content
                    , currentBlock = Just currentLine
                }

            else if state.isVerbatimLine currentLine.content then
                -- add the current line to the block and keep the indentation level
                { state
                    | lines = List.drop 1 state.lines
                    , lineNumber = state.lineNumber + 1
                    , inVerbatim = True

                    --, indent = currentLine.indent
                    , currentBlock =
                        Just
                            { block
                                | content = block.content ++ "\n" ++ currentLine.content
                                , numberOfLines = state.lineNumber + 2 - block.lineNumber
                            }
                }

            else
                -- add the current line to the block
                { state
                    | lines = List.drop 1 state.lines
                    , lineNumber = state.lineNumber + 1
                    , indent = currentLine.indent
                    , currentBlock =
                        Just
                            { block
                                | content = block.content ++ "\n" ++ currentLine.content
                                , numberOfLines = state.lineNumber + 2 - block.lineNumber
                            }
                }


handleLT currentLine state =
    case state.currentBlock of
        Nothing ->
            { state
                | lines = List.drop 1 state.lines
                , lineNumber = state.lineNumber + 1
                , indent = currentLine.indent
            }

        Just block ->
            { state
                | lines = List.drop 1 state.lines
                , lineNumber = state.lineNumber + 1
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

module Tree.Blocks exposing (Block, fromString, make)

import Tree.Line as Line


type alias Block =
    { indent : Int
    , content : String
    }


fromString : String -> List Block
fromString str =
    str
        |> String.lines
        |> List.map (\line -> Line.classify line)


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

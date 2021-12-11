module Tree.Line exposing (Line, classify, prefixLength, prefixLengths)

import Parser exposing ((|.), (|=), Parser)


type alias Line =
    { indent : Int, content : String, lineNumber : Int }


classify : Int -> String -> Line
classify lineNumber str =
    case Parser.run (prefixParser lineNumber) str of
        Err _ ->
            { indent = 0, content = "!!ERROR", lineNumber = lineNumber }

        Ok result ->
            result


prefixLength : Int -> String -> Int
prefixLength lineNumber str =
    classify lineNumber str |> .indent


prefixLengths : Int -> List String -> List Int
prefixLengths lineNumber strs =
    strs |> List.map (prefixLength lineNumber) |> List.filter (\n -> n /= 0)


prefixParser : Int -> Parser Line
prefixParser lineNumber =
    Parser.succeed (\prefixStart prefixEnd end content -> { indent = prefixEnd - prefixStart, content = String.slice prefixEnd end content, lineNumber = lineNumber })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '\n')
        |= Parser.getOffset
        |= Parser.getSource

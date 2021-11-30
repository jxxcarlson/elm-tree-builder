module Tree.Line exposing (Line, classify, prefixLength, prefixLengths)

import Parser exposing ((|.), (|=), Parser)


type alias Line =
    { indent : Int, content : String }


classify : String -> Line
classify str =
    case Parser.run prefixParser str of
        Err _ ->
            { indent = 0, content = "!!ERROR" }

        Ok result ->
            result


prefixLength : String -> Int
prefixLength str =
    classify str |> .indent


prefixLengths : List String -> List Int
prefixLengths strs =
    strs |> List.map prefixLength |> List.filter (\n -> n /= 0)


prefixParser : Parser Line
prefixParser =
    Parser.succeed (\prefixStart prefixEnd end content -> { indent = prefixEnd - prefixStart, content = String.slice prefixEnd end content })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '\n')
        |= Parser.getOffset
        |= Parser.getSource

# Elm-Tree-Builder

This package provides tools for building a rose tree from a string or 
list of blocks.

    Example:

        > data = "1\n 2\n 3\n 4\n5\n 6\n 7"

        > fromString "?" identity data
          Tree "0" [
                Tree "1" [Tree "2" [],Tree "3" [], Tree "4" []]
              , Tree "5" [Tree "6" [],Tree "7" []]
              ]

The first argument of fromString is a label for a default node.
The second argument, of type (String -> node), tells how to build a node from a string
representation of that node.

A block is defined by 

        type alias Block =
        { indent : Int
        , content : String
        }





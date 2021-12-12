module Tree.Blocks exposing (Block, fromStringAsLines, fromStringAsParagraphs)

{-| This module provides tools for converting text into a list of blocks, where

        type alias Block =
            { indent : Int
            , content : String
            , lineNumber : Int
            }

Lists of blocks are used by the functions in Tree.Build to construct trees
form text. Here are some examples:

        > fromStringAsLines "one\n  two\n    three"
         [ { content = "one\nho ho ho!"
            , indent = 0, lineNumber = 0 }
          ,{ content = "two\nha ha ha!"
            , indent = 2, lineNumber = 1 }
         ]

        > fromStringAsParagraphs "one\n\two\nthree\n\n  ha ha ha!"
          [{ content = "one\n\two\nthree"
            , indent = 0, lineNumber = 0 }
          ,{ content = "ha ha ha!"
           , indent = 2, lineNumber = 3 }
          ]

The line number is the position of the first line of
the text of the block in the source text.

@docs Block, fromStringAsLines, fromStringAsParagraphs

-}

import Tree.BlocksV as BlocksV


{-| Aliased from Tree.BlocksV
-}
type alias Block =
    BlocksV.Block


{-| -}
fromStringAsLines : String -> List Block
fromStringAsLines =
    BlocksV.fromStringAsLines


{-| -}
fromStringAsParagraphs : String -> List Block
fromStringAsParagraphs =
    BlocksV.fromStringAsParagraphs (\line -> False)

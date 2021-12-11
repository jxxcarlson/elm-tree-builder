module Tree.Blocks exposing (Block, fromStringAsLines, fromStringAsParagraphs)

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

@docs Block, fromStringAsLines, fromStringAsParagraphs

-}

import Tree.BlocksV as BlocksV


{-| -}
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

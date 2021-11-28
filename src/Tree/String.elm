module Tree.String exposing (..)

import Tree exposing (Tree)
import Tree.Block as Block
import Tree.Build as Build
import Tree.Line as Line
import Tree.Math as Math


foo =
    1



--
--build : Int -> String -> Tree String
--build quant str =
--    let
--        blocks =
--            Block.blocksAsLines str
--
--        default =
--            "??"
--
--        root =
--            "root"
--    in
--    Build.fromBlocks stringInitialData blocks
--
--
--initalData =
--    Block.initialData
--
--
--quantum lines =
--    Math.gcdList (Line.prefixLengths lines)

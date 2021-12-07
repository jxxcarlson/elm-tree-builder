module Tree.Example.Block exposing (..)

import Tree.Blocks as Blocks
import Tree.BlocksV as BlocksV


data =
    """
par 1
ho ho ho
ha ha ha!
-- Santa

par 2
This is a test.
I repeat: a test.
-- Who knows?
"""


myVerbatimLine : String -> Bool
myVerbatimLine str =
    String.left 2 str == "||"


data2 =
    """
one
two

|| code
aaa
  bbb
    ccc
    ddd


  More stuff
  and still more
"""


blocks =
    BlocksV.fromStringAsParagraphs myVerbatimLine data2

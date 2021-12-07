module Tree.Example.Block exposing (..)

import Tree.Blocks as Blocks


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


blocks =
    Blocks.fromStringAsParagraphs data

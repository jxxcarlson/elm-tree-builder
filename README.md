# Tree Builder

Tools for building
a tree from a string or a list of blocks. A tree
is represented in text as an outline:

    The Book of Monsters
      Under the Earth
         Extinct Monsters
         Living Monsters
      On Dry Land
         Extinct Monsters
         Living Monsters
         Nice Monsters
      Imaginary Monsters

This root node is "The Book of Monsters." It has three child nodes,
"Under the Earth," "On Dry Land," and "Imaginary Monsters." A tree like
this where nodes can have more than two children is called a "rose tree."

    Example:

        > Import Tree.Build as Build

        > data = "1\n 2\n 3\n 4\n5\n 6\n 7"

        > Build.fromString "?" identity data
          Tree "0" [
                Tree "1" [Tree "2" [],Tree "3" [], Tree "4" []]
              , Tree "5" [Tree "6" [],Tree "7" []]
              ]

The first argument of fromString is a label for a default node.
The second argument, of type (String -> node), tells how to build a node from a string
representation of that node. Here we use the representation of rose trees of
[elm/rose-tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/).

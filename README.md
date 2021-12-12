# Tree Builder

This package provides tools for building a tree from a string or a list of blocks
as well as some other tools:

- Tree.Blocks: transform a string into a list of blocks

- Tree.Build: the main module; build a tree from a string or a list  
    of blocks.

- Tree.Lib: preOrder, levelOrder, edges.  The latter transforms 
  Tree a to List (a, a) where the List (a, a) represents a list of 
  edges of the tree

- Tree.Random: generate random trees

- Tree.Render: render trees to strings or graphs.  A graph
  is a data structure which represents graphical image of a tree.
  It is used to render trees to Svg.

- Tree.Svg: functions for rendering a Graph to SVG.



See the code in `app/src/Main.elm` for an example of building a
tree from text and rendering it to Svg.  You can also use this
app to generate random trees.  Trees can be read from a text
file and saved to a text file.

[Live Demo](https://jxxcarlson.github.io/app/treebuilder/)


## Building Trees

A tree
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

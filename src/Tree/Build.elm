module Tree.Build exposing (InitialData, fromBlocks)

{-|

    > s = "a\n b\n  c\n  d\n x"
    "a\n b\n  c\n  d\n x" : String

    > t = build 1 s
    Tree "root" [Tree "a" [Tree "b" [Tree "c" [],Tree "d" []],Tree "x" []]] : Tree.Tree String

    > render t
    "a\n b\n  c\n  d\n x" : String

-}

import Tree exposing (Tree(..))
import Tree.Block exposing (Block)
import Tree.Zipper as Zipper exposing (Zipper(..))


type alias InitialData node =
    { quant : Int
    , defaultNode : node
    , rootNode : node
    , makeNode : String -> node
    }


init : InitialData node -> List Block -> State node
init initialData blocks =
    { blocks = blocks
    , zipper = Zipper.fromTree <| Tree.tree initialData.rootNode []
    , indent = 0
    , level = 0
    , indentationQuantum = initialData.quant
    , make = initialData.makeNode
    , default = Zipper.fromTree (Tree.tree initialData.defaultNode [])
    }


fromBlocks : InitialData node -> List Block -> Tree node
fromBlocks initialData blocks =
    loop (init initialData blocks) nextStep



-- FUNCTIONAL LOOP


type alias State node =
    { blocks : List Block
    , zipper : Zipper node
    , indent : Int
    , level : Int
    , indentationQuantum : Int
    , make : String -> node
    , default : Zipper node
    }


nextStep : State node -> Step (State node) (Tree node)
nextStep state =
    case List.head state.blocks of
        Nothing ->
            Done (Zipper.toTree state.zipper)

        Just block ->
            case compare block.indent state.indent of
                GT ->
                    Loop <| handleGT block.indent block.content state

                EQ ->
                    Loop <| handleEQ block.indent block.content state

                LT ->
                    Loop <| handleLT block.indent block.content state


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b



-- HANDLERS


handleEQ indent content state =
    let
        newTree =
            Tree.tree (state.make content) []
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , zipper = attachAtFocus newTree state.zipper
    }


handleGT indent content state =
    let
        newTree =
            Tree.tree (state.make content) []
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , level = state.level + 1
        , zipper = attachAtFocus newTree (Zipper.lastChild state.zipper |> Maybe.withDefault state.default)
    }


handleLT indent content state =
    let
        newTree =
            Tree.tree (state.make content) []

        deltaLevel =
            state.indent - indent // state.indentationQuantum
    in
    { state
        | blocks = List.drop 1 state.blocks
        , indent = indent
        , zipper = attachAtFocus newTree (repeat deltaLevel Zipper.parent state.zipper)
    }



-- HELPERS I


attachAtFocus : Tree.Tree a -> Zipper a -> Zipper a
attachAtFocus t z =
    Zipper.replaceTree (appendChild t z) z


appendChild : Tree a -> Zipper a -> Tree a
appendChild t z =
    Tree.appendChild t (Zipper.tree z)



-- HELPERS II


{-|

    Apply f to x n times

-}
repeatM : Int -> (a -> Maybe a) -> Maybe a -> Maybe a
repeatM n f x =
    if n == 0 then
        x

    else
        repeatM (n - 1) f (Maybe.andThen f x)


{-|

    Apply f to x n times

-}
repeat : Int -> (a -> Maybe a) -> a -> a
repeat n f x =
    case repeatM n f (Just x) of
        Nothing ->
            x

        Just y ->
            y

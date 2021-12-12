module Tree.TransformExample exposing (..)

import Tree exposing (Tree)
import Tree.Build as Build
import Tree.Extra
import Tree.Zipper as Zipper exposing (Zipper)


data1 =
    """
1
 2
  3
   4
   5
  6
 7
  8
  9
 """


data2 =
    """
1
 2
  3
   4
   5
 6
 """


data3 =
    """
1
 2
  3
   4
    5
  6
 7
  8
   9
   10
 """


data =
    """
1
 2
  3
   4
    5
  6
 7
  8
"""


t_ =
    Build.fromString 0 (\rec -> rec.content |> String.toInt |> Maybe.withDefault 0) data |> Result.withDefault zeroTree


zeroTree =
    Tree.singleton 0


ex : Tree.Tree Int
ex =
    -- transform { sum = 0 } (\a acc -> { acc | sum = acc.sum + a }) (\acc a -> acc.sum + a) t_
    transform { sum = 0 } (\a acc -> { acc | sum = acc.sum + a }) (\acc a -> a) t_


type alias State a b acc =
    { inZipper : Zipper ( a, Int ), outZipper : Zipper b, acc : acc, currentDepth : Int }


transform : acc -> (a -> acc -> acc) -> (acc -> a -> b) -> Tree a -> Tree b
transform initialAcc updateAcc evaluateWithAcc tree =
    loop (init initialAcc updateAcc evaluateWithAcc tree) (nextStep updateAcc evaluateWithAcc)


init : acc -> (a -> acc -> acc) -> (acc -> a -> b) -> Tree a -> State a b acc
init initialAcc updateAcc evaluateWithAcc tree =
    let
        initialZipper =
            Zipper.fromTree (Tree.Extra.tagWithDepth tree)

        root =
            Zipper.label initialZipper |> Debug.log "NODE"

        newRoot =
            evaluateWithAcc initialAcc (Tuple.first root)

        acc =
            updateAcc (Tuple.first root) initialAcc |> Debug.log "ACC"

        outZipper =
            Zipper.fromTree (Tree.singleton newRoot)
    in
    { inZipper = initialZipper
    , outZipper = outZipper
    , acc = acc
    , currentDepth = 0
    }


nextStep : (a -> acc -> acc) -> (acc -> a -> b) -> State a b acc -> Step (State a b acc) (Tree b)
nextStep updateAcc evaluateWithAcc state =
    case Zipper.forward state.inZipper of
        Nothing ->
            Done (Zipper.toTree state.outZipper)

        Just zipper ->
            let
                currentRoot_ =
                    Zipper.label zipper

                currentDepth =
                    Tuple.second currentRoot_

                deltaDepth =
                    currentDepth - state.currentDepth

                currentRoot =
                    currentRoot_ |> Tuple.first

                acc =
                    updateAcc currentRoot state.acc |> Debug.log "ACC"

                newRoot =
                    evaluateWithAcc state.acc currentRoot |> Debug.log "newRoot"

                outZipper_ =
                    if deltaDepth > 0 then
                        Zipper.forward state.outZipper |> Maybe.withDefault state.outZipper |> Debug.log "ZIPPER (GT)\n                        \n                   "

                    else if deltaDepth < 0 then
                        repeat -deltaDepth Zipper.parent state.outZipper
                            |> Debug.log "PARENT (LT)"

                    else
                        state.outZipper |> Debug.log "PARENT (EQ)"

                --|> Maybe.withDefault state.outZipper
                outZipper =
                    -- attachAtFocus (Tree.singleton newRoot) state.outZipper
                    attachAtFocus (Tree.singleton newRoot) outZipper_

                _ =
                    Debug.log "NODE: ((IN, OUT), (newRoot, delta))" ( ( currentRoot_, Zipper.label outZipper ), ( newRoot, deltaDepth ) )
            in
            Loop
                { inZipper = zipper
                , outZipper = outZipper
                , acc = acc
                , currentDepth = currentDepth
                }


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


attachAtFocus : Tree.Tree a -> Zipper a -> Zipper a
attachAtFocus t z =
    Zipper.replaceTree (appendChild t z) z


appendChild : Tree a -> Zipper a -> Tree a
appendChild t z =
    Tree.appendChild t (Zipper.tree z)



-- LOOP


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

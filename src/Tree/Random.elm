module Tree.Random exposing (generate, generateOutline, depths)

{-| Utilities for generating and collecting statistics on random trees.

@docs generate, generateOutline, depths

-}

import List.Extra
import Random
import Tree exposing (Tree)
import Tree.Build as Build exposing (Error)
import Tree.Extra


{-| Generate a random rose tree with the given number nodes
and with given seed.
-}
generate : Int -> Int -> Result Error (Tree String)
generate maxNodes seed =
    generateOutline maxNodes seed
        |> Build.fromString "?" .content


{-| Generate a random outline.
-}
generateOutline : Int -> Int -> String
generateOutline maxEntries seed =
    loop (init maxEntries seed) nextStep


type alias State =
    { seed : Random.Seed
    , count : Int
    , indent : Int
    , maxCount : Int
    , output : String
    }


init : Int -> Int -> State
init maxCount seed =
    let
        ( _, newSeed ) =
            rng (Random.initialSeed seed)
    in
    { seed = newSeed
    , count = 1
    , indent = 0
    , maxCount = maxCount
    , output = ""
    }


type Move
    = Stay
    | Forward
    | Back
    | BackTwice


valueOf : Move -> Int
valueOf move =
    case move of
        Stay ->
            0

        Forward ->
            1

        Back ->
            -1

        BackTwice ->
            -2


rng : Random.Seed -> ( Move, Random.Seed )
rng seed =
    let
        ( move, s1 ) =
            Random.step (Random.weighted ( 20, Forward ) [ ( 45, Stay ), ( 20, Forward ), ( 25, Back ), ( 10, BackTwice ) ]) seed
    in
    ( move, s1 )


nextStep : State -> Step State String
nextStep state =
    if state.count == state.maxCount then
        Done state.output

    else
        let
            ( move, seed ) =
                rng state.seed

            indentaAfterRoot =
                -- After the root, indentation must be at least 1 and cannot be one more
                -- than the previous indentation
                state.indent + valueOf move |> (\x -> clamp 1 (state.indent + 1) x)

            indent =
                if state.count == 1 then
                    0

                else
                    indentaAfterRoot

            str =
                String.repeat indent " " ++ String.fromInt state.count
        in
        Loop <|
            { state
                | seed = seed
                , count = state.count + 1
                , indent = indent
                , output = state.output ++ "\n" ++ str
            }


{-|

    depths k n seed:  generate n random trees of k nodes using the given seed
    and collect statistics on their depths

    > depths 10 10 0
    [(2,1),(3,2),(4,3),(5,2),(8,2)] -- one tree of depth two, two trees of depth three, etc.

    > depths 10 10 1
    [(2,1),(3,1),(4,3),(5,2),(7,1),(8,2)]

    > depths 10 10 2
    [(2,1),(3,2),(4,3),(5,2),(7,1),(8,1)]

-}
depths : Int -> Int -> Int -> List ( Int, Int )
depths numberOfNodes numberOfTrees seed =
    List.foldl (\n acc -> (generate numberOfNodes (n + seed) |> Result.toMaybe |> Maybe.map Tree.Extra.depth |> Maybe.withDefault 0) :: acc) [] (List.range 0 (numberOfTrees - 1))
        |> List.sort
        |> List.Extra.group
        |> List.map (\( a, b ) -> ( a, List.length b + 1 ))


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

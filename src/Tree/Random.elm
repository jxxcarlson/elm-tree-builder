module Tree.Random exposing (depths, generate, generateOutline)

import List.Extra
import Random
import Tree exposing (Tree)
import Tree.Build as Build exposing (InitialData)
import Tree.Extra


{-| -}
generate : Int -> Int -> Maybe (Tree String)
generate maxCount seed =
    generateOutline maxCount seed
        |> Build.fromString initialData


generateOutline : Int -> Int -> String
generateOutline maxCount seed =
    loop (init maxCount seed) nextStep


initialData =
    { quant = 1
    , defaultNode = "?"
    , makeNode = identity
    }


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
        ( k, newSeed ) =
            rng 0 (Random.initialSeed seed)
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


rng : Int -> Random.Seed -> ( Move, Random.Seed )
rng indent seed =
    let
        ( move, s1 ) =
            Random.step (Random.weighted ( 40, Forward ) [ ( 25, Stay ), ( 40, Forward ), ( 25, Back ), ( 10, BackTwice ) ]) seed
    in
    ( move, s1 )


nextStep : State -> Step State String
nextStep state =
    if state.count == state.maxCount then
        Done state.output

    else
        let
            ( move, seed ) =
                rng state.indent state.seed

            indent =
                state.indent + valueOf move |> (\x -> clamp 0 (state.indent + 1) x)

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


depths nodes steps =
    List.foldl (\n acc -> (generate nodes n |> Maybe.map Tree.Extra.depth |> Maybe.withDefault 0) :: acc) [] (List.range 0 steps)
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

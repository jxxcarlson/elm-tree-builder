module Tree.Math exposing (gcd, gcdList)


gcd : Int -> Int -> Int
gcd a b =
    if a == 0 then
        b

    else
        gcd (modBy a b) a


gcdList : List Int -> Int
gcdList is =
    case List.head is of
        Nothing ->
            1

        Just n ->
            List.foldl (\k acc -> gcd k acc) n (List.drop 1 is)

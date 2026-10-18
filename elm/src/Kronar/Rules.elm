module Kronar.Rules exposing
    ( Score
    , etherBonus
    , isNegativePair
    , isValidJump
    , pathValues
    , sapphirePenalty
    , scorePath
    , valueAt
    )


type alias Score =
    { raw : Int
    , penalty : Int
    , bonus : Int
    , final : Int
    }


valueAt : List Int -> Int -> Int
valueAt board position =
    board
        |> List.drop position
        |> List.head
        |> Maybe.withDefault 0


pathValues : List Int -> List Int -> List Int
pathValues board path =
    List.map (valueAt board) path


isNegativePair : Int -> Int -> Bool
isNegativePair a b =
    a < 0 && b < 0


sapphirePenalty : List Int -> Int
sapphirePenalty values =
    case values of
        a :: b :: rest ->
            if isNegativePair a b then
                5 + sapphirePenalty (b :: rest)

            else
                sapphirePenalty (b :: rest)

        _ ->
            0


etherBonus : Int -> Int
etherBonus raw =
    if modBy 2 raw == 0 then
        10

    else
        0


scorePath : List Int -> List Int -> Score
scorePath board path =
    let
        values =
            pathValues board path

        raw =
            List.sum values

        penalty =
            sapphirePenalty values

        bonus =
            etherBonus raw
    in
    { raw = raw
    , penalty = penalty
    , bonus = bonus
    , final = raw - penalty + bonus
    }


isValidJump : Int -> Int -> Int -> Bool
isValidJump goal from to =
    let
        distance =
            to - from
    in
    distance >= 1 && distance <= 3 && to <= goal
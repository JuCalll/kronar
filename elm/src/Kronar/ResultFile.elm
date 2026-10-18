module Kronar.ResultFile exposing (GameResult, decoder, parse)

import Json.Decode as Decode exposing (Decoder)


type alias GameResult =
    { board : List Int
    , goal : Int
    , voidApplied : Bool
    , optimalPath : List Int
    , rawScore : Int
    , penalty : Int
    , bonus : Int
    , finalScore : Int
    , evaluatedPaths : Int
    }


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)


decoder : Decoder GameResult
decoder =
    Decode.succeed GameResult
        |> andMap (Decode.field "tablero" (Decode.list Decode.int))
        |> andMap (Decode.field "casilla_meta" Decode.int)
        |> andMap (Decode.field "regla_vacio_aplicada" Decode.bool)
        |> andMap (Decode.field "camino_optimo" (Decode.list Decode.int))
        |> andMap (Decode.field "puntaje_bruto" Decode.int)
        |> andMap (Decode.field "penalizacion_zafiro" Decode.int)
        |> andMap (Decode.field "bono_eter" Decode.int)
        |> andMap (Decode.field "puntaje_final" Decode.int)
        |> andMap (Decode.field "caminos_evaluados" Decode.int)


checkConsistency : GameResult -> Result String GameResult
checkConsistency result =
    let
        size =
            List.length result.board
    in
    if size < 2 || size > 15 then
        Err ("The board must have between 2 and 15 squares, but it has " ++ String.fromInt size ++ ".")

    else if result.goal < 0 || result.goal >= size then
        Err ("The goal (" ++ String.fromInt result.goal ++ ") is not a position on the board.")

    else if List.head result.optimalPath /= Just 0 then
        Err "The optimal path must start on the first square (position 0)."

    else if List.head (List.reverse result.optimalPath) /= Just result.goal then
        Err "The optimal path must end on the goal."

    else if not (validJumps result.optimalPath) then
        Err "Every jump in the optimal path must move 1, 2 or 3 squares forward."

    else
        Ok result


validJumps : List Int -> Bool
validJumps path =
    case path of
        a :: b :: rest ->
            b - a >= 1 && b - a <= 3 && validJumps (b :: rest)

        _ ->
            True


parse : String -> Result String GameResult
parse text =
    case Decode.decodeString decoder text of
        Err error ->
            Err ("The file is not a valid resultado.json.\n\n" ++ Decode.errorToString error)

        Ok result ->
            checkConsistency result
module View.Report exposing (routeText, view)

import Html exposing (Html, div, h2, p, strong, table, td, text, th, tr)
import Kronar.ResultFile exposing (GameResult)
import Kronar.Rules as Rules
import View.Style as Style


view : GameResult -> Html msg
view result =
    div Style.card
        [ h2 [] [ text "Report" ]
        , boardTable result.board
        , p []
            [ strong [] [ text "Goal: " ]
            , text ("square " ++ String.fromInt (result.goal + 1))
            ]
        , p []
            [ strong [] [ text "Void Rule: " ]
            , text (voidText result.voidApplied)
            ]
        , p []
            [ strong [] [ text "Optimal path: " ]
            , text (routeText result.optimalPath)
            ]
        , scoreTable result
        , p [] [ text ("Paths evaluated by Haskell: " ++ String.fromInt result.evaluatedPaths) ]
        ]


voidText : Bool -> String
voidText applied =
    if applied then
        "applied (the last square is empty, so the goal moved back)"

    else
        "not applied (the goal is the last square)"


routeText : List Int -> String
routeText path =
    let
        squares =
            List.map (\position -> String.fromInt (position + 1)) path
    in
    case List.reverse squares of
        [] ->
            "no squares"

        [ only ] ->
            "square " ++ only

        last :: restReversed ->
            "squares " ++ String.join ", " (List.reverse restReversed) ++ " and " ++ last


boardTable : List Int -> Html msg
boardTable board =
    table Style.table
        [ tr [] (th Style.headerCell [ text "Square" ] :: List.indexedMap squareNumber board)
        , tr [] (th Style.headerCell [ text "Value" ] :: List.map squareValue board)
        ]


squareNumber : Int -> Int -> Html msg
squareNumber position _ =
    th Style.headerCell [ text (String.fromInt (position + 1)) ]


squareValue : Int -> Html msg
squareValue value =
    td Style.cell [ text (String.fromInt value) ]


scoreTable : GameResult -> Html msg
scoreTable result =
    let
        mine =
            Rules.scorePath result.board result.optimalPath
    in
    table Style.table
        [ tr []
            [ th Style.headerCell [ text "" ]
            , th Style.headerCell [ text "resultado.json" ]
            , th Style.headerCell [ text "Recalculated in Elm" ]
            , th Style.headerCell [ text "Check" ]
            ]
        , scoreRow "Raw score" result.rawScore mine.raw
        , scoreRow "Sapphire penalty" result.penalty mine.penalty
        , scoreRow "Ether bonus" result.bonus mine.bonus
        , scoreRow "Final score" result.finalScore mine.final
        ]


scoreRow : String -> Int -> Int -> Html msg
scoreRow label fromJson fromElm =
    tr []
        [ th Style.headerCell [ text label ]
        , td Style.cell [ text (String.fromInt fromJson) ]
        , td Style.cell [ text (String.fromInt fromElm) ]
        , td Style.cell [ text (checkMark (fromJson == fromElm)) ]
        ]


checkMark : Bool -> String
checkMark ok =
    if ok then
        "OK"

    else
        "Mismatch"
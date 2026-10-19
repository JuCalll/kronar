module View.Player exposing (view)

import Html exposing (Html, button, div, h2, p, strong, text)
import Html.Events exposing (onClick)
import Kronar.ResultFile exposing (GameResult)
import Kronar.Rules as Rules
import View.Report as Report
import View.Style as Style


view : GameResult -> List Int -> String -> msg -> Html msg
view result playerPath note reset =
    let
        finished =
            List.head (List.reverse playerPath) == Just result.goal
    in
    div Style.card
        [ h2 [] [ text "Player mode" ]
        , p [] [ text "Click the squares to move Dravek. Each jump must be 1, 2 or 3 squares and must not go past the goal." ]
        , p [] [ strong [] [ text "Your path: " ], text (Report.routeText playerPath) ]
        , p [] [ text note ]
        , if finished then
            outcome result playerPath

          else
            text ""
        , button [ onClick reset ] [ text "Start again" ]
        ]


outcome : GameResult -> List Int -> Html msg
outcome result playerPath =
    let
        score =
            Rules.scorePath result.board playerPath

        difference =
            result.finalScore - score.final

        comparison =
            if difference == 0 then
                "You matched the optimum"

            else
                "You were " ++ String.fromInt difference ++ " point(s) away from the optimum (" ++ String.fromInt result.finalScore ++ ")."
    in
    div []
        [ p [] [ strong [] [ text "You reached the goal." ] ]
        , p []
            [ text
                ("Raw "
                    ++ String.fromInt score.raw
                    ++ ", Sapphire -"
                    ++ String.fromInt score.penalty
                    ++ ", Ether +"
                    ++ String.fromInt score.bonus
                    ++ ", final "
                    ++ String.fromInt score.final
                    ++ "."
                )
            ]
        , p [] [ text comparison ]
        ]

module View.Playback exposing (view)

import Html exposing (Html, button, div, h2, li, p, strong, text, ul)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Kronar.ResultFile exposing (GameResult)
import Kronar.Rules as Rules
import View.Style as Style


view : GameResult -> Int -> msg -> msg -> Html msg
view result step previous next =
    let
        lastStep =
            List.length result.optimalPath - 1

        visited =
            List.take (step + 1) result.optimalPath

        values =
            Rules.pathValues result.board visited

        energy =
            List.sum values

        penaltySoFar =
            Rules.sapphirePenalty values
    in
    div Style.card
        [ h2 [] [ text "Step-by-step playback" ]
        , button [ onClick previous, disabled (step == 0) ] [ text "Previous" ]
        , text " "
        , button [ onClick next, disabled (step >= lastStep) ] [ text "Next" ]
        , p [] [ strong [] [ text ("Turn " ++ String.fromInt step ++ " of " ++ String.fromInt lastStep) ] ]
        , ul [] (List.map (\line -> li [] [ text line ]) (events result step))
        , p [] [ text ("Energy collected: " ++ String.fromInt energy) ]
        , p [] [ text ("Sapphire penalty so far: " ++ String.fromInt penaltySoFar) ]
        ]


events : GameResult -> Int -> List String
events result step =
    let
        path =
            result.optimalPath

        position =
            positionAt path step

        value =
            Rules.valueAt result.board position

        lastStep =
            List.length path - 1
    in
    if step == 0 then
        [ "Dravek starts on square 1 and collects " ++ String.fromInt value ++ "." ]

    else
        let
            previousPosition =
                positionAt path (step - 1)

            previousValue =
                Rules.valueAt result.board previousPosition

            move =
                "He jumps "
                    ++ String.fromInt (position - previousPosition)
                    ++ " square(s) to square "
                    ++ String.fromInt (position + 1)
                    ++ " and collects "
                    ++ String.fromInt value
                    ++ "."

            sapphire =
                if Rules.isNegativePair previousValue value then
                    [ "Sapphire Rule: squares "
                        ++ String.fromInt (previousPosition + 1)
                        ++ " and "
                        ++ String.fromInt (position + 1)
                        ++ " are both negative, 5 points lost."
                    ]

                else
                    []

            ending =
                if step == lastStep then
                    finalLines result

                else
                    []
        in
        move :: sapphire ++ ending


finalLines : GameResult -> List String
finalLines result =
    let
        score =
            Rules.scorePath result.board result.optimalPath

        ether =
            if score.bonus > 0 then
                "Ether Rule: the raw score is even (" ++ String.fromInt score.raw ++ "), 10 points added."

            else
                "Ether Rule not triggered: the raw score is odd (" ++ String.fromInt score.raw ++ ")."
    in
    [ "Dravek reaches the goal.", ether, "Final score: " ++ String.fromInt score.final ++ "." ]


positionAt : List Int -> Int -> Int
positionAt path step =
    path
        |> List.drop step
        |> List.head
        |> Maybe.withDefault 0

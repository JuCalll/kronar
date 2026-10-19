module View.Board exposing (Config, view)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Config msg =
    { board : List Int
    , goal : Int
    , marked : List Int
    , dravek : Maybe Int
    , onSquareClick : Maybe (Int -> msg)
    }


type Kind
    = Positive
    | Negative
    | Empty
    | OutOfPlay


kindOf : Int -> Int -> Int -> Kind
kindOf goal position value =
    if position > goal then
        OutOfPlay

    else if value > 0 then
        Positive

    else if value < 0 then
        Negative

    else
        Empty


background : Kind -> String
background kind =
    case kind of
        Positive ->
            "#cdeccf"

        Negative ->
            "#f6c9c9"

        Empty ->
            "#e4e6eb"

        OutOfPlay ->
            "#ffffff"


view : Config msg -> Html msg
view config =
    div []
        [ div [ style "display" "flex", style "gap" "8px", style "flex-wrap" "wrap" ]
            (List.indexedMap (viewSquare config) config.board)
        , legend
        ]


viewSquare : Config msg -> Int -> Int -> Html msg
viewSquare config position value =
    let
        kind =
            kindOf config.goal position value

        isMarked =
            List.member position config.marked

        border =
            if kind == OutOfPlay then
                "2px dashed #9aa0a6"

            else if isMarked then
                "4px solid #d4a017"

            else
                "2px solid #9aa0a6"

        clickAttributes =
            case config.onSquareClick of
                Just toMsg ->
                    [ onClick (toMsg position), style "cursor" "pointer" ]

                Nothing ->
                    []
    in
    div
        ([ style "width" "64px"
         , style "height" "76px"
         , style "border" border
         , style "border-radius" "8px"
         , style "background" (background kind)
         , style "display" "flex"
         , style "flex-direction" "column"
         , style "align-items" "center"
         , style "justify-content" "center"
         , style "opacity"
            (if kind == OutOfPlay then
                "0.6"

             else
                "1"
            )
         ]
            ++ clickAttributes
        )
        [ span [ style "font-size" "12px", style "color" "#555" ] [ text ("Square " ++ String.fromInt (position + 1)) ]
        , span [ style "font-size" "22px", style "font-weight" "bold" ] [ text (String.fromInt value) ]
        , span [ style "font-size" "12px" ] [ text (dravekMark config.dravek position) ]
        ]


dravekMark : Maybe Int -> Int -> String
dravekMark dravek position =
    if dravek == Just position then
        "Dravek"

    else
        ""


legend : Html msg
legend =
    div [ style "font-size" "13px", style "margin-top" "8px", style "color" "#444" ]
        [ text "Green: positive. Red: negative. Gray: empty. Dashed: out of play. Gold border: path." ]
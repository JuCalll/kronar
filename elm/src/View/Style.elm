module View.Style exposing (card, cell, headerCell, table)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


card : List (Attribute msg)
card =
    [ style "border" "1px solid #d0d4dc"
    , style "border-radius" "8px"
    , style "padding" "16px"
    , style "margin" "16px 0"
    , style "background" "#ffffff"
    ]


table : List (Attribute msg)
table =
    [ style "border-collapse" "collapse"
    , style "margin" "8px 0"
    ]


cell : List (Attribute msg)
cell =
    [ style "border" "1px solid #d0d4dc"
    , style "padding" "4px 10px"
    , style "text-align" "center"
    ]


headerCell : List (Attribute msg)
headerCell =
    cell ++ [ style "background" "#eef1f6", style "font-weight" "bold" ]
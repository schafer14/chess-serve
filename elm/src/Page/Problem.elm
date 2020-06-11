module Page.Problem exposing (notFound, styles)

import Html exposing (..)
import Html.Attributes exposing (..)


notFound : List (Html msg)
notFound =
    [ div [ style "font-size" "12em" ] [ text "404" ]
    , div [ style "font-size" "3em" ] [ text "I cannot find this page!" ]
    , div [ style "font-size" "3em" ] [ a [ href "/home" ] [ text "Back to home" ] ]
    ]


styles : List (Attribute msg)
styles =
    [ style "text-align" "center"
    , style "color" "#9A9A9A"
    , style "padding" "6em 0"
    ]

module Skeleton exposing (Details, view)

import Browser
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, fill, height, layout, link, maximum, padding, rgb, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Html as Html exposing (Html)
import Html.Attributes exposing (style)
import Session exposing (Session, isLoggedIn)


type alias Details msg =
    { title : String
    , content : Element msg
    , session : Session
    }


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title ++ " | " ++ "Chessly"
    , body = [ layout [] <| body (Element.map toMsg details.content) details.session ]
    }


body : Element msg -> Session -> Element msg
body content session =
    column [ width fill, height fill ]
        [ navBar session
        , content
        ]


navBar : Session -> Element msg
navBar session =
    let
        item =
            if Session.isLoggedIn session then
                link [ alignRight ] { url = "/logout", label = text "Logout" }

            else
                link [ spacing 20, alignRight ] { url = "/login", label = text "Enter" }
    in
    el
        [ padding 25
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color <| rgb 0.5 0.5 0.5
        , width fill
        ]
    <|
        row [ width <| maximum 600 fill, spacing 25, centerX ]
            [ link [ alignLeft ] { url = "/home", label = text "Chess" }

            -- , item
            ]

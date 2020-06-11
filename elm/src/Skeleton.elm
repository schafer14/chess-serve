module Skeleton exposing (Details, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as D


type alias Details msg =
    { title : String
    , content : Html msg
    }


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title ++ " | " ++ "Chessly"
    , body = [ Html.map toMsg details.content ]
    }

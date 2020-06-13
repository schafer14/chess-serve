module Page.Logout exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (Element, centerX, centerY, column, el, fill, padding, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Http as Http
import Json.Decode as D
import Json.Encode as Encode
import Session exposing (Session, logout)
import Skeleton exposing (Details)


type alias Model =
    { session : Session
    }


type Msg
    = GotLogout (Result Http.Error ())


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Http.get
        { url = "/v1/auth/logout"
        , expect = Http.expectWhatever GotLogout
        }
    )


view : Model -> Details Msg
view model =
    { title = "Logout"
    , content = el [] (text "Logout")
    , session = model.session
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLogout (Result.Ok _) ->
            ( model
            , Nav.pushUrl (Session.navKey (logout model.session)) "/home"
            )

        GotLogout (Result.Err _) ->
            ( model
            , Nav.pushUrl (Session.navKey (logout model.session)) "/home"
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

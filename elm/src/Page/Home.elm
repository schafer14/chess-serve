module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (Attribute, Element, centerX, centerY, column, el, layout, link, padding, pointer, rgb, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder, field, string)
import Session exposing (Session)
import Skeleton exposing (Details)


type alias Model =
    { isJoining : Bool
    , gameToJoin : String
    , session : Session
    }


type Msg
    = RequestGame
    | JoinGame
    | Join String
    | Update Model
    | GotGame (Result Http.Error String)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, isJoining = False, gameToJoin = "" }, Cmd.none )


view : Model -> Details Msg
view model =
    { title = "Home"
    , content = layout [] <| renderHome model
    }


renderHome : Model -> Element Msg
renderHome model =
    let
        r =
            if model.isJoining then
                column [ spacing 10 ]
                    [ row [ centerX ] [ text "Join a game" ]
                    , row []
                        [ Input.text []
                            { onChange = \new -> Update { model | gameToJoin = new }
                            , text = model.gameToJoin
                            , placeholder = Nothing
                            , label = Input.labelAbove [ Font.size 14 ] (text "Game Id")
                            }
                        ]
                    , row [ centerX, spacing 10 ]
                        [ Input.button [ padding 10, Border.width 1, Border.color (rgb 0 0 0) ] { onPress = Just <| Join model.gameToJoin, label = text "Join" }
                        , Input.button [ padding 10, Border.width 1, Border.color (rgb 0 0 0) ] { onPress = Just <| Update { model | isJoining = False }, label = text "Cancel" }
                        ]
                    ]

            else
                row [ spacing 10 ]
                    [ Input.button [ padding 10, Border.width 1, Border.color (rgb 0 0 0) ] { onPress = Just RequestGame, label = text "Create game" }
                    , Input.button [ padding 10, Border.width 1, Border.color (rgb 0 0 0) ] { onPress = Just JoinGame, label = text "Join game" }
                    ]
    in
    el [ centerX, centerY ]
        (column
            []
            [ el [ centerX, padding 20 ] (text "Chess")
            , r
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update" ( msg, model )
    in
    case msg of
        JoinGame ->
            ( { model | isJoining = True }, Cmd.none )

        Update new ->
            ( new, Cmd.none )

        GotGame result ->
            case result of
                Ok id ->
                    ( model
                    , Nav.pushUrl (Session.navKey model.session)
                        ("/game/" ++ id)
                    )

                Err _ ->
                    ( model, Cmd.none )

        Join gameId ->
            ( model
            , Http.request
                { method = "PUT"
                , headers = []
                , url = "http://localhost:3000/v1/games/" ++ gameId ++ "/join"
                , expect = Http.expectJson GotGame gameDecoder
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        RequestGame ->
            ( model
            , Http.post
                { url = "http://localhost:3000/v1/games"
                , expect = Http.expectJson GotGame gameDecoder
                , body = Http.emptyBody
                }
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


gameDecoder : Decoder String
gameDecoder =
    field "id" string

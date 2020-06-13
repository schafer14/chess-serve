module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Page.Game as Game
import Page.Home as Home
import Page.Login as Login
import Page.Logout as Logout
import Page.Problem as Problem
import Session as Session exposing (Session)
import Skeleton
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string, top)


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { page : Page
    , session : Session
    }


type Page
    = NotFound
    | Home Home.Model
    | Login Login.Model
    | Logout Logout.Model
    | Game Game.Model


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | GameMsg Game.Msg
    | LoginMsg Login.Msg
    | LogoutMsg Logout.Msg


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    parseUrl url
        { session = Session.new key
        , page = NotFound
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.navKey model.session) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( UrlChanged url, _ ) ->
            parseUrl url model

        ( HomeMsg homeMsg, Home home ) ->
            Home.update homeMsg home |> updateWith Home HomeMsg model

        ( GameMsg gameMsg, Game game ) ->
            Game.update gameMsg game |> updateWith Game GameMsg model

        ( LoginMsg loginMsg, Login login ) ->
            Login.update loginMsg login |> updateWith Login LoginMsg model

        ( LogoutMsg logoutMsg, Logout logout ) ->
            Logout.update logoutMsg logout |> updateWith Logout LogoutMsg model

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Game gameModel ->
            Sub.map GameMsg (Game.subscriptions gameModel)

        _ ->
            Sub.none


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            { title = "Not Found", body = Problem.notFound }

        Home home ->
            Skeleton.view HomeMsg (Home.view home)

        Game game ->
            Skeleton.view GameMsg (Game.view game)

        Login login ->
            Skeleton.view LoginMsg (Login.view login)

        Logout logout ->
            Skeleton.view LogoutMsg (Logout.view logout)


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )


parseUrl : Url.Url -> Model -> ( Model, Cmd Msg )
parseUrl url model =
    let
        wrapInit : (model -> Page) -> (msg -> Msg) -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
        wrapInit toModel toMsg ( m, msg ) =
            ( { model | page = toModel m }, Cmd.map toMsg msg )

        parser =
            oneOf
                [ route top (wrapInit Home HomeMsg <| Home.init model.session)
                , route (s "home") (wrapInit Home HomeMsg <| Home.init model.session)
                , route (s "login") (wrapInit Login LoginMsg <| Login.init model.session)
                , route (s "logout") (wrapInit Logout LogoutMsg <| Logout.init model.session)
                , route (s "game" </> string)
                    (\gameId -> wrapInit Game GameMsg (Game.init model.session gameId))
                ]

        parsed =
            Url.Parser.parse parser url
    in
    case parsed of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Url.Parser.map handler parser

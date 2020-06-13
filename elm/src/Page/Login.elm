module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (Element, centerX, centerY, column, el, fill, padding, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Http as Http
import Json.Decode as D
import Json.Encode as Encode
import Session exposing (Session, login)
import Skeleton exposing (Details)


type alias Model =
    { signUp : SignUpForm
    , login : LoginForm
    , showing : Form
    , session : Session
    , pending : Bool
    }


type Form
    = SignUp
    | Login


type alias SignUpForm =
    { email : String
    , password : String
    , name : String
    , confirmPassword : String
    }


type alias LoginForm =
    { email : String
    , password : String
    }


type Msg
    = NoOp
    | SwitchForm
    | UpdateSignUp SignUpForm
    | UpdateLogin LoginForm
    | StartLogin
    | GotLogin (Result Http.Error String)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , signUp = { name = "", email = "", password = "", confirmPassword = "" }
      , login = { email = "", password = "" }
      , showing = SignUp
      , pending = False
      }
    , Cmd.none
    )


view : Model -> Details Msg
view model =
    { title = "Login"
    , content = renderLogin model
    , session = model.session
    }


renderLogin : Model -> Element Msg
renderLogin model =
    if model.showing == SignUp then
        el [ centerY, centerX ] <| signUpBox model

    else
        el [ centerY, centerX ] <| loginBox model.login


loginBox : LoginForm -> Element Msg
loginBox login =
    el [ Border.width 1, padding 50, width fill ] <|
        column [ spacing 50, width fill ]
            [ el [ centerX ] (text "Log In")
            , column [ spacing 25 ]
                [ Input.email [ width fill ]
                    { onChange = \e -> UpdateLogin { login | email = e }
                    , text = login.email
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text "Email Address"
                    }
                , Input.currentPassword [ width fill ]
                    { onChange = \pwd -> UpdateLogin { login | password = pwd }
                    , text = login.password
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text "Password"
                    , show = False
                    }
                ]
            , row [ centerX, spacing 5 ]
                [ Input.button [ centerX, padding 10, Border.width 1 ] { onPress = Just StartLogin, label = text "Login" }
                , Input.button [ centerX, padding 10 ] { onPress = Just SwitchForm, label = text "Sign Up" }
                ]
            ]


signUpBox : Model -> Element Msg
signUpBox model =
    el [ Border.width 1, padding 50, width fill ] <|
        column [ spacing 50, width fill ]
            [ el [ centerX ] (text "Sign Up")
            , column [ spacing 25 ]
                [ Input.text [ width fill ]
                    { onChange = \_ -> NoOp
                    , text = ""
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text "Name"
                    }
                , Input.email [ width fill ]
                    { onChange = \_ -> NoOp
                    , text = ""
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text "Email Address"
                    }
                , Input.newPassword [ width fill ]
                    { onChange = \_ -> NoOp
                    , text = ""
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text "Password"
                    , show = False
                    }
                , Input.newPassword [ width fill ]
                    { onChange = \_ -> NoOp
                    , text = ""
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text "Confirm Password"
                    , show = False
                    }
                ]
            , row [ centerX, spacing 5 ]
                [ Input.button [ centerX, padding 10, Border.width 1 ] { onPress = Nothing, label = text "Sign Up" }
                , Input.button [ centerX, padding 10 ] { onPress = Just SwitchForm, label = text "Login" }
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SwitchForm ->
            if model.showing == SignUp then
                ( { model | showing = Login }, Cmd.none )

            else
                ( { model | showing = SignUp }, Cmd.none )

        UpdateLogin loginForm ->
            ( { model | login = loginForm }, Cmd.none )

        UpdateSignUp signUpForm ->
            ( { model | signUp = signUpForm }, Cmd.none )

        StartLogin ->
            ( { model | pending = True }
            , Http.post
                { url = "/v1/auth/login"
                , expect = Http.expectJson GotLogin (D.field "status" D.string)
                , body = Http.jsonBody <| loginJson model.login
                }
            )

        GotLogin (Result.Ok _) ->
            let
                session =
                    login model.session
            in
            ( { model | session = session }
            , Nav.pushUrl (Session.navKey model.session) "/home"
            )

        GotLogin res ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


loginJson : LoginForm -> Encode.Value
loginJson login =
    Encode.object
        [ ( "email", Encode.string login.email )
        , ( "password", Encode.string login.password )
        ]


gray =
    rgb 0.5 0.5 0.5

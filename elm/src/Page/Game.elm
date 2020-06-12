port module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

import Chess exposing (..)
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Element exposing (Element, centerX, centerY, el, html, layout)
import Html exposing (Html, button, div)
import Html.Events exposing (..)
import Http as Http
import Json.Decode as Decode exposing (Decoder, field)
import Json.Encode as Encode exposing (Value)
import Maybe exposing (Maybe(..))
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..))
import Session exposing (Session)
import Skeleton
import Svg exposing (..)
import Svg.Attributes exposing (..)


port joinGame : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


type alias Model =
    { gameId : String
    , state : State
    , perspective : Color
    , paintings : List Painting
    , pieceMoving : Maybe Piece
    , session : Session
    }


type Msg
    = ClearBoard
    | BoardClick Coordinate
    | Flip
    | Recv String
    | Sent (Result Http.Error ())
    | GotGame (Result Http.Error String)


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearBoard ->
            clearBoard model |> withNoCmd

        BoardClick coord ->
            case handleBoardEvent model coord of
                ( newModel, Nothing ) ->
                    newModel |> withNoCmd

                ( newModel, Just move ) ->
                    case move2Str move of
                        Just moveString ->
                            ( newModel
                            , Http.request
                                { method = "PUT"
                                , headers = []
                                , url = "/v1/games/" ++ model.gameId ++ "/move"
                                , expect = Http.expectWhatever Sent
                                , body = Http.jsonBody <| encodeMove moveString
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                            )

                        Nothing ->
                            model |> withNoCmd

        Flip ->
            flip model |> withNoCmd

        Recv fen ->
            { model | state = fen2State fen } |> withNoCmd

        Sent _ ->
            model |> withNoCmd

        GotGame (Ok fen) ->
            { model | state = fen2State fen } |> withNoCmd

        GotGame _ ->
            model |> withNoCmd


flip : Model -> Model
flip model =
    let
        c =
            if model.perspective == White then
                Black

            else
                White
    in
    { model | perspective = c }


clearBoard : Model -> Model
clearBoard gs =
    { gs | paintings = [] }


highlightSquare : Model -> Coordinate -> Model
highlightSquare gameState coordinate =
    let
        painting =
            Highlight coordinate Red

        alreadyExists =
            List.member painting gameState.paintings

        newPaintings =
            if alreadyExists then
                List.filter (\x -> x /= painting) gameState.paintings

            else
                painting :: gameState.paintings
    in
    { gameState | paintings = newPaintings }


handleBoardEvent : Model -> Coordinate -> ( Model, Maybe Move )
handleBoardEvent state coordinate =
    case state.pieceMoving of
        Nothing ->
            ( startPieceMove state coordinate, Nothing )

        Just p ->
            endPieceMove state coordinate p


startPieceMove : Model -> Coordinate -> Model
startPieceMove boardState coordinate =
    let
        piece =
            boardState.state.pieces
                |> List.filter (isOn coordinate)
                |> List.filter (\p -> p.color == boardState.state.onMove)
                |> List.head

        canMove =
            0 < (List.length <| movesFrom boardState.state coordinate)
    in
    if not canMove then
        boardState

    else
        { boardState | pieceMoving = piece }


endPieceMove : Model -> Coordinate -> Piece -> ( Model, Maybe Move )
endPieceMove boardState dest oldPiece =
    let
        move =
            { src = oldPiece.coordinate, dest = dest, promotion = Nothing }

        legal =
            isLegal boardState.state move

        newState =
            applyMove boardState.state move
    in
    if legal then
        ( { boardState | state = newState, pieceMoving = Nothing }, Just move )

    else
        ( { boardState | pieceMoving = Nothing }, Nothing )


type PaintColor
    = Red
    | LightBlue


type Painting
    = Highlight Coordinate PaintColor


view : Model -> Skeleton.Details Msg
view boardState =
    { title = "Game"
    , content =
        layout [] <| el [ centerX, centerY ] (html <| renderBoard boardState)
    }


renderBoard : Model -> Html Msg
renderBoard boardState =
    svg
        [ viewBox "-3 -3 86 86"
        , width "640"
        , height "640"
        , onRightClick ClearBoard
        ]
        [ renderBg
        , renderGrid boardState.perspective
        , renderCoords boardState.perspective
        , renderTurnIndicator boardState.state.onMove
        , renderPaintings boardState.perspective boardState.paintings
        , renderPieces boardState.perspective boardState.state.pieces
        , renderDots boardState.perspective boardState
        ]


coordToLoc : Color -> Coordinate -> ( Int, Int )
coordToLoc perspective coord =
    let
        whitePerspective =
            perspective == White

        xPlace =
            if not whitePerspective then
                7 - coord.x

            else
                coord.x

        xVal =
            xPlace * 10

        yPlace =
            if whitePerspective then
                7 - coord.y

            else
                coord.y

        yVal =
            yPlace * 10
    in
    ( xVal, yVal )


pieceName : PieceType -> String
pieceName p =
    case p of
        King ->
            "king"

        Queen ->
            "queen"

        Rook ->
            "rook"

        Bishop ->
            "bishop"

        Knight ->
            "knight"

        Pawn ->
            "pawn"


colorName : Color -> String
colorName c =
    case c of
        White ->
            "white"

        Black ->
            "black"


renderPaintings : Color -> List Painting -> Html Msg
renderPaintings perspective paintings =
    g [] <| List.map (renderPainting perspective) paintings


renderPaintColor : PaintColor -> String
renderPaintColor p =
    case p of
        Red ->
            red

        LightBlue ->
            lightBlue


renderPainting : Color -> Painting -> Html Msg
renderPainting perspective (Highlight coord color) =
    let
        ( xVal, yVal ) =
            coordToLoc perspective coord

        c =
            renderPaintColor color
    in
    rect [ onClick <| BoardClick coord, x <| String.fromInt xVal, y <| String.fromInt yVal, rx "3", width "10", height "10", fill c ] []


renderPiece : Color -> Piece -> Html Msg
renderPiece perspective piece =
    let
        ( xVal, yVal ) =
            coordToLoc perspective piece.coordinate

        link =
            "/public/" ++ colorName piece.color ++ "-" ++ pieceName piece.pieceType ++ ".svg"
    in
    image
        [ onClickNoProp <| BoardClick piece.coordinate
        , x <| String.fromInt xVal
        , y <| String.fromInt yVal
        , width "10"
        , height "10"
        , xlinkHref link
        ]
        []


renderDots : Color -> Model -> Html Msg
renderDots perspective boardState =
    case boardState.pieceMoving of
        Nothing ->
            g [] []

        Just sourcePiece ->
            let
                moves =
                    movesFrom boardState.state sourcePiece.coordinate

                dots =
                    moves |> List.map (\m -> renderDot perspective m.dest)
            in
            g [] dots


renderDot : Color -> Coordinate -> Html Msg
renderDot perspective coord =
    let
        ( xVal, yVal ) =
            coordToLoc perspective coord
    in
    circle
        [ onClick <| BoardClick coord
        , cx <| String.fromInt <| xVal + 5
        , cy <| String.fromInt <| yVal + 5
        , r "1.5"
        , fill "rgba(0,0,0,0.6)"
        ]
        []


renderPieces : Color -> List Piece -> Html Msg
renderPieces perspective pieces =
    g [] <| List.map (renderPiece perspective) pieces


renderBg : Html Msg
renderBg =
    rect [ x "-3", y "-3", width "86", height "86", fill cream ] []


renderTurnIndicator : Color -> Html Msg
renderTurnIndicator onTurn =
    if onTurn == White then
        circle [ cx "-1.25", cy "-1.25", r ".75", fill white, stroke "black", strokeWidth "0.25", strokeOpacity "0.25" ] []

    else
        circle [ cx "-1.25", cy "-1.25", r ".75", fill "rgba(0,0,0,0.7)", stroke "rgba(0,0,0,.25)", strokeWidth "0.25" ] []


renderCoords : Color -> Html Msg
renderCoords perspective =
    let
        rowNames =
            [ "a", "b", "c", "d", "e", "f", "g", "h" ]

        correctedRowNames =
            if perspective == White then
                rowNames

            else
                List.reverse rowNames

        drawLetter i letter =
            text_ [ x <| String.fromInt <| i * 10 + 5, y "82.3", fontSize "2.5", textAnchor "middle" ] [ Svg.text letter ]

        collNames =
            List.range 1 8

        correctedColNames =
            if perspective == Black then
                collNames

            else
                List.reverse collNames

        drawNumber i num =
            text_ [ y <| String.fromInt <| i * 10 + 6, x "81.4", fontSize "2.5", textAnchor "middle" ] [ text <| String.fromInt num ]

        rowG =
            g [] <| List.indexedMap drawLetter correctedRowNames

        colG =
            g [] <| List.indexedMap drawNumber correctedColNames
    in
    g [ fill "rgba(0,0,0,.5)", Svg.Attributes.style "user-select: none" ] [ rowG, colG ]


encodeMove : String -> Encode.Value
encodeMove m =
    Encode.object
        [ ( "move", Encode.string m )
        ]


renderGrid : Color -> Html Msg
renderGrid perspective =
    let
        row j =
            g [] <| List.map (\i -> renderSquare perspective { x = i, y = j }) <| List.range 0 7
    in
    g [] <| List.map row <| List.range 0 7


renderSquare : Color -> Coordinate -> Html Msg
renderSquare perspective coord =
    let
        ( xCoord, yCoord ) =
            coordToLoc perspective coord

        isEven =
            modBy 2 (coord.x + coord.y) == 0

        color =
            if not isEven then
                white

            else
                blue
    in
    rect [ onClick <| BoardClick coord, x <| String.fromInt xCoord, y <| String.fromInt yCoord, height "10", width "10", fill color ] []


init : Session -> String -> ( Model, Cmd Msg )
init session id =
    { gameId = id
    , state = newGame
    , perspective = White
    , paintings = []
    , pieceMoving = Maybe.Nothing
    , session = session
    }
        |> withCmds
            [ joinGame <| "ws://localhost:3000/v1/games/" ++ id ++ "/follow"
            , Http.get
                { url = "/v1/games/" ++ id
                , expect = Http.expectJson GotGame gameDecoder
                }
            ]


gameDecoder : Decoder String
gameDecoder =
    field "fen" Decode.string


alwaysOn : Msg -> ( Msg, Bool )
alwaysOn msg =
    ( msg, True )


onClickNoProp : Msg -> Attribute Msg
onClickNoProp msg =
    stopPropagationOn "click" (Decode.map alwaysOn (Decode.succeed msg))


onRightClick : Msg -> Attribute Msg
onRightClick msg =
    preventDefaultOn "contextmenu" (Decode.map alwaysOn (Decode.succeed msg))


cream =
    "rgb(215, 215, 215)"


white =
    "rgb(217, 230, 240)"


coolWhite =
    "rgb(0, 230, 240)"


red =
    "#e39774"


lightBlue =
    "#5c9ead"


blue =
    "rgb(45, 89, 145)"

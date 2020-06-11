module Chess exposing (Color(..), Coordinate, Piece, PieceType(..), State, applyMove, fen2State, isLegal, isOn, moves, movesFrom, newGame)

import Array
import Maybe exposing (Maybe(..))


type alias State =
    { pieces : List Piece
    , onMove : Color
    , enPassant : Maybe Coordinate
    , castlingPrivledges : CastlePrivledges
    }


type alias CastlePrivledges =
    { whiteKing : Bool
    , whiteQueen : Bool
    , blackKing : Bool
    , blackQueen : Bool
    }


type alias Coordinate =
    { x : Int, y : Int }


type PieceType
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type Color
    = White
    | Black


type alias Piece =
    { color : Color
    , pieceType : PieceType
    , coordinate : Coordinate
    }


type alias Move =
    { src : Coordinate
    , dest : Coordinate
    , promotion : Maybe PieceType
    }


moves : State -> List Move
moves s =
    s.pieces
        |> List.map (\p -> p.coordinate)
        |> List.map (movesFrom s)
        |> List.concat


movesFrom : State -> Coordinate -> List Move
movesFrom state c =
    let
        piece =
            state.pieces |> List.filter (isOn c) |> List.head

        info =
            piece |> Maybe.map (\p -> ( p.color == state.onMove, p.pieceType, p ))

        candidateMoves =
            case info of
                Nothing ->
                    []

                Just ( False, _, _ ) ->
                    []

                Just ( True, Knight, knight ) ->
                    knightMoves state knight

                Just ( True, Pawn, pawn ) ->
                    pawnMoves state pawn

                Just ( True, Bishop, bishop ) ->
                    bishopMoves state bishop

                Just ( True, Rook, rook ) ->
                    rookMoves state rook

                Just ( True, Queen, queen ) ->
                    queenMoves state queen

                Just ( True, King, king ) ->
                    kingMoves state king

        finalMoves =
            candidateMoves |> List.filter (\m -> not <| isNonsenseMove state m)
    in
    finalMoves


bishopMoves : State -> Piece -> List Move
bishopMoves state bishop =
    let
        dirs =
            [ { x = 1, y = 1 }, { x = 1, y = -1 }, { x = -1, y = 1 }, { x = -1, y = -1 } ]
    in
    slidingPiece state bishop.color bishop.coordinate dirs


rookMoves : State -> Piece -> List Move
rookMoves state rook =
    let
        dirs =
            [ { x = 1, y = 0 }, { x = -1, y = 0 }, { x = 0, y = 1 }, { x = 0, y = -1 } ]
    in
    slidingPiece state rook.color rook.coordinate dirs


queenMoves : State -> Piece -> List Move
queenMoves state piece =
    List.append (rookMoves state piece) (bishopMoves state piece)


knightMoves : State -> Piece -> List Move
knightMoves state knight =
    let
        ox =
            knight.coordinate.x

        oy =
            knight.coordinate.y
    in
    [ { src = { x = ox, y = oy }, dest = { x = ox + 1, y = oy + 2 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + 1, y = oy + -2 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + -1, y = oy + 2 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + -1, y = oy + -2 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + 2, y = oy + -1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + 2, y = oy + 1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + -2, y = oy - 1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + -2, y = oy + 1 }, promotion = Nothing }
    ]


kingMoves : State -> Piece -> List Move
kingMoves state king =
    let
        ox =
            king.coordinate.x

        oy =
            king.coordinate.y
    in
    [ { src = { x = ox, y = oy }, dest = { x = ox + 1, y = oy + 1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + 1, y = oy + -1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + -1, y = oy + 1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + -1, y = oy + -1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox, y = oy + -1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox, y = oy + 1 }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + 1, y = oy }, promotion = Nothing }
    , { src = { x = ox, y = oy }, dest = { x = ox + -1, y = oy }, promotion = Nothing }
    ]


pawnMoves : State -> Piece -> List Move
pawnMoves state piece =
    let
        possibleMoves =
            pawnPossibleMoves state piece.color piece.coordinate

        -- TODO: Promotion
    in
    possibleMoves |> List.filterMap (\x -> x)


pawnPossibleMoves : State -> Color -> Coordinate -> List (Maybe Move)
pawnPossibleMoves state color coord =
    let
        ( direction, start, promo ) =
            pawnConf color

        nextSquareCoord =
            { x = coord.x, y = coord.y + direction }

        nextSquareMove =
            if state.pieces |> List.any (isOn nextSquareCoord) then
                Nothing

            else
                Just { src = coord, dest = nextSquareCoord, promotion = Nothing }

        secondSquareCoord =
            { nextSquareCoord | y = nextSquareCoord.y + direction }

        secondMoveSquare =
            if nextSquareMove == Nothing || List.any (isOn secondSquareCoord) state.pieces || coord.y /= start then
                Nothing

            else
                Just { src = coord, dest = secondSquareCoord, promotion = Nothing }

        cap1 =
            { nextSquareCoord | x = coord.x + 1 }

        cap1Move =
            if List.any (isOnWithColor cap1 (otherColor color)) state.pieces || Just cap1 == state.enPassant then
                Just { src = coord, dest = cap1, promotion = Nothing }

            else
                Nothing

        cap2 =
            { nextSquareCoord | x = coord.x - 1 }

        cap2Move =
            if List.any (isOnWithColor cap2 (otherColor color)) state.pieces || Just cap2 == state.enPassant then
                Just { src = coord, dest = cap2, promotion = Nothing }

            else
                Nothing
    in
    [ nextSquareMove, secondMoveSquare, cap1Move, cap2Move ]


slidingPiece : State -> Color -> Coordinate -> List Coordinate -> List Move
slidingPiece state color startCoord directions =
    let
        expand start dir =
            let
                next =
                    { x = start.x + dir.x, y = start.y + dir.y }

                move =
                    { src = startCoord, dest = next, promotion = Nothing }

                hasPiece =
                    List.any (isOn next) state.pieces

                isNonsense =
                    isNonsenseMove state move
            in
            if hasPiece || isNonsense then
                [ move ]

            else
                move :: expand next dir
    in
    List.concatMap (expand startCoord) directions


isLegal : State -> Move -> Bool
isLegal state move =
    List.member move <| movesFrom state <| move.src


applyMove : State -> Move -> State
applyMove state move =
    let
        newList =
            state.pieces
                |> List.filter (isNotOn move.dest)
                |> List.filter (isNotOn move.src)

        oldPiece =
            state.pieces
                |> List.filter (isOn move.src)
                |> List.head

        newPiece =
            Maybe.map (updateCoordinate move.dest) oldPiece

        pieces =
            Maybe.map (\p -> p :: newList) newPiece
    in
    case pieces of
        Nothing ->
            state

        Just p ->
            { state | pieces = p, onMove = otherColor state.onMove }


isNonsenseMove : State -> Move -> Bool
isNonsenseMove state move =
    let
        outOfRange x =
            x < 0 || x > 7

        outsideBoard =
            outOfRange move.src.x || outOfRange move.src.y || outOfRange move.dest.x || outOfRange move.dest.y

        captureFriendly =
            state.pieces |> List.filter (isOn move.dest) |> List.any (\p -> p.color == state.onMove)

        noop =
            move.src == move.dest
    in
    outsideBoard || captureFriendly || noop


isCapture : State -> Move -> Bool
isCapture state move =
    state.pieces |> List.filter (isOn move.dest) |> List.any (\p -> p.color /= state.onMove)


otherColor : Color -> Color
otherColor c =
    if c == White then
        Black

    else
        White


isOn : Coordinate -> Piece -> Bool
isOn coord piece =
    piece.coordinate == coord


isOnWithColor : Coordinate -> Color -> Piece -> Bool
isOnWithColor coord color piece =
    piece.coordinate == coord && piece.color == color


isNotOn : Coordinate -> Piece -> Bool
isNotOn coord piece =
    piece.coordinate /= coord


updateCoordinate : Coordinate -> Piece -> Piece
updateCoordinate c p =
    { p | coordinate = c }


fen2State : String -> State
fen2State fen =
    let
        parts =
            String.split " " fen

        arr =
            Array.fromList parts

        castleDefaults =
            { whiteKing = True, whiteQueen = True, blackKing = True, blackQueen = True }
    in
    { onMove = Maybe.withDefault White <| Maybe.map fen2Color <| Array.get 1 arr
    , pieces = Maybe.withDefault [] <| Maybe.map fen2Pieces <| Array.get 0 arr
    , enPassant = fen2EnPassant <| Array.get 3 arr
    , castlingPrivledges = Maybe.withDefault castleDefaults <| Maybe.map fen2CastlingPrivledges <| Array.get 2 arr
    }


fen2Color : String -> Color
fen2Color fen =
    if fen == "w" then
        White

    else
        Black


fen2EnPassant : Maybe String -> Maybe Coordinate
fen2EnPassant fen =
    case fen of
        Just "-" ->
            Nothing

        Just sq ->
            square2Coord sq

        Nothing ->
            Nothing


fen2CastlingPrivledges : String -> CastlePrivledges
fen2CastlingPrivledges chars =
    { whiteKing = String.contains "K" chars
    , whiteQueen = String.contains "Q" chars
    , blackKing = String.contains "k" chars
    , blackQueen = String.contains "q" chars
    }


fen2Pieces : String -> List Piece
fen2Pieces fen =
    let
        rows : List String
        rows =
            String.split "/" fen |> List.reverse

        rowToPieces : Int -> String -> List Piece
        rowToPieces rowNum chars =
            let
                ( _, res ) =
                    List.foldl (parseChar rowNum) ( 0, [] ) <| String.split "" chars
            in
            res

        parseChar : Int -> String -> ( Int, List Piece ) -> ( Int, List Piece )
        parseChar rowNum char ( colNum, ps ) =
            case ( String.toInt char, char ) of
                ( Just digit, _ ) ->
                    ( colNum + digit, ps )

                ( _, "r" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Rook, color = Black } :: ps )

                ( _, "R" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Rook, color = White } :: ps )

                ( _, "n" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Knight, color = Black } :: ps )

                ( _, "N" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Knight, color = White } :: ps )

                ( _, "b" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Bishop, color = Black } :: ps )

                ( _, "B" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Bishop, color = White } :: ps )

                ( _, "q" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Queen, color = Black } :: ps )

                ( _, "Q" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Queen, color = White } :: ps )

                ( _, "k" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = King, color = Black } :: ps )

                ( _, "K" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = King, color = White } :: ps )

                ( _, "p" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Pawn, color = Black } :: ps )

                ( _, "P" ) ->
                    ( colNum + 1, { coordinate = { x = colNum, y = rowNum }, pieceType = Pawn, color = White } :: ps )

                _ ->
                    ( colNum + 1, ps )
    in
    List.concat <| List.indexedMap rowToPieces rows


square2Coord : String -> Maybe Coordinate
square2Coord sq =
    let
        arr =
            String.split "" sq

        colSq =
            Maybe.andThen charToCol <| List.head arr

        rowSq =
            Maybe.map (\r -> r - 1) <| Maybe.andThen String.toInt <| Array.get 1 <| Array.fromList arr
    in
    Maybe.map2 (\a b -> { x = a, y = b }) colSq rowSq


newGame : State
newGame =
    fen2State "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


charToCol : String -> Maybe Int
charToCol str =
    case str of
        "a" ->
            Just 0

        "b" ->
            Just 1

        "c" ->
            Just 2

        "d" ->
            Just 3

        "e" ->
            Just 4

        "f" ->
            Just 5

        "g" ->
            Just 6

        "h" ->
            Just 7

        _ ->
            Nothing


pawnConf : Color -> ( Int, Int, Int )
pawnConf color =
    let
        direction =
            if color == White then
                1

            else
                -1

        start =
            if color == White then
                1

            else
                6

        promo =
            if color == White then
                6

            else
                1
    in
    ( direction, start, promo )

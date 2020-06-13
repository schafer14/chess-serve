module Chess exposing (Color(..), Coordinate, Move, Piece, PieceType(..), State, applyMove, fen2State, isLegal, isOn, move2Str, moves, movesFrom, newGame)

import Array
import Maybe exposing (Maybe(..), withDefault)


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
    { x : Int
    , y : Int
    }


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
    state.pieces
        |> List.filter (isOn c)
        |> List.filter (\p -> p.color == state.onMove)
        |> List.head
        |> Maybe.map (candidateMoves state)
        |> Maybe.withDefault []
        |> List.filter (\m -> not <| isNonsenseMove state state.onMove m)
        |> List.filter (safeFromCheck state)


candidateMoves : State -> Piece -> List Move
candidateMoves state piece =
    let
        possibleMoves =
            case piece.pieceType of
                Knight ->
                    knightMoves state piece

                Pawn ->
                    pawnMoves state piece

                Bishop ->
                    bishopMoves state piece

                Rook ->
                    rookMoves state piece

                Queen ->
                    queenMoves state piece

                King ->
                    List.concat [ kingMoves state piece, castlingMoves state piece ]
    in
    List.filter (\m -> not <| isNonsenseMove state piece.color m) possibleMoves


attackSurface : State -> Color -> List Coordinate
attackSurface state color =
    state.pieces
        |> List.filter (\p -> p.color == color)
        |> List.map (attackSurfacePiece state)
        |> List.concat


attackSurfacePiece : State -> Piece -> List Coordinate
attackSurfacePiece state piece =
    let
        possibleMoves =
            case piece.pieceType of
                Knight ->
                    knightMoves state piece

                Pawn ->
                    pawnMoves state piece

                Bishop ->
                    bishopMoves state piece

                Rook ->
                    rookMoves state piece

                Queen ->
                    queenMoves state piece

                King ->
                    kingMoves state piece
    in
    possibleMoves
        |> List.filter (\m -> not <| isNonsenseMove state piece.color m)
        |> List.map (\m -> m.dest)


safeFromCheck : State -> Move -> Bool
safeFromCheck state move =
    let
        stateAfterMove =
            applyMove state move

        opponentsMoves =
            attackSurface stateAfterMove stateAfterMove.onMove

        endsInCheck =
            stateAfterMove.pieces
                |> List.filter (\p -> p.pieceType == King)
                |> List.filter (\p -> p.color == state.onMove)
                |> List.map (\p -> p.coordinate)
                |> List.head
                |> Maybe.map (\kingCoord -> List.member kingCoord opponentsMoves)
                |> Maybe.withDefault False
    in
    not endsInCheck


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
knightMoves _ knight =
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
kingMoves _ king =
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


castlingMoves : State -> Piece -> List Move
castlingMoves state _ =
    let
        rank =
            if state.onMove == White then
                0

            else
                7

        opp =
            if state.onMove == White then
                Black

            else
                White

        priv =
            if state.onMove == White then
                [ state.castlingPrivledges.whiteKing, state.castlingPrivledges.whiteQueen ]

            else
                [ state.castlingPrivledges.blackKing, state.castlingPrivledges.blackQueen ]

        dangerSquares =
            attackSurface state opp

        kingSideChecks =
            [ { x = 4, y = rank }, { x = 5, y = rank } ]

        queenSideChecks =
            [ { x = 4, y = rank }, { x = 3, y = rank } ]

        canKingSide =
            0 == List.length (intersect kingSideChecks dangerSquares)

        canQueenSide =
            0 == List.length (intersect queenSideChecks dangerSquares)

        results =
            [ { src = { x = 4, y = rank }, dest = { x = 6, y = rank }, promotion = Nothing }
            , { src = { x = 2, y = rank }, dest = { x = 2, y = rank }, promotion = Nothing }
            ]
    in
    zip [ canKingSide, canQueenSide ] priv
        |> List.map (\( x, y ) -> x && y)
        |> zip results
        |> List.filter (\( _, x ) -> x)
        |> List.map (\( y, _ ) -> y)


intersect : List a -> List a -> List a
intersect xs ys =
    List.filter (\x -> List.member x ys) xs


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
                    isNonsenseMove state state.onMove move
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
            Maybe.map (updateCoordinate move) oldPiece

        pieces =
            Maybe.map (\p -> p :: newList) newPiece
    in
    case pieces of
        Nothing ->
            state

        Just p ->
            { state | pieces = p, onMove = otherColor state.onMove }


isNonsenseMove : State -> Color -> Move -> Bool
isNonsenseMove state color move =
    let
        outOfRange x =
            x < 0 || x > 7

        outsideBoard =
            outOfRange move.src.x || outOfRange move.src.y || outOfRange move.dest.x || outOfRange move.dest.y

        captureFriendly =
            state.pieces |> List.filter (isOn move.dest) |> List.any (\p -> p.color == color)

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


updateCoordinate : Move -> Piece -> Piece
updateCoordinate m p =
    let
        pType =
            Maybe.withDefault p.pieceType m.promotion
    in
    { p | coordinate = m.dest, pieceType = pType }


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


move2Str : Move -> Maybe String
move2Str m =
    let
        src =
            coord2Str m.src

        dest =
            coord2Str m.dest

        movePart =
            Maybe.map2 (++) src dest

        promoPart =
            Maybe.withDefault "" <| Maybe.map pieceToChar m.promotion
    in
    Maybe.map (\x -> x ++ promoPart) movePart


pieceToChar : PieceType -> String
pieceToChar p =
    case p of
        King ->
            "K"

        Queen ->
            "Q"

        Rook ->
            "R"

        Bishop ->
            "B"

        Knight ->
            "K"

        Pawn ->
            "P"


coord2Str : Coordinate -> Maybe String
coord2Str c =
    let
        rowName =
            rowToChar c.y

        colName =
            colToChar c.x
    in
    Maybe.map2 (++) colName rowName


newGame : State
newGame =
    fen2State "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"


colNames : List ( String, Int )
colNames =
    zip (String.split "" "abcdefgh") (List.range 0 25)


rowNames : List ( String, Int )
rowNames =
    zip (String.split "" "12345678") (List.range 0 25)


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xRest, y :: yRest ) ->
            ( x, y ) :: zip xRest yRest


find : List ( a, b ) -> a -> Maybe b
find hayStack needle =
    case hayStack of
        [] ->
            Nothing

        ( s, i ) :: rest ->
            if s == needle then
                Just i

            else
                find rest needle


rowToChar : Int -> Maybe String
rowToChar i =
    find (List.map flipTuple rowNames) i


colToChar : Int -> Maybe String
colToChar i =
    find (List.map flipTuple colNames) i


flipTuple : ( a, b ) -> ( b, a )
flipTuple ( x, y ) =
    ( y, x )


charToRow : String -> Maybe Int
charToRow str =
    find rowNames str


charToCol : String -> Maybe Int
charToCol str =
    find colNames str


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

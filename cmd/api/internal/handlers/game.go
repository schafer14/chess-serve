package handlers

import (
	"fmt"
	"log"
	"net/http"
	"strings"
	"time"

	"github.com/go-chi/chi"
	"github.com/google/uuid"
	"github.com/gorilla/sessions"
	"github.com/gorilla/websocket"
	"github.com/nats-io/nats.go"
	"github.com/pkg/errors"
	"github.com/schafer14/chess-serve/internal/chess"
	"github.com/volatiletech/authboss"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
)

type GameHandler struct {
	coll *mongo.Collection
	nc   *nats.Conn
	ab   *authboss.Authboss
}

var store = sessions.NewCookieStore([]byte("aasdf;oi4jra"))

func getPlayer(w http.ResponseWriter, r *http.Request, ab *authboss.Authboss) chess.Player {
	var name, id string
	name = "Guest"

	t, err := ab.CurrentUser(r)
	if err != nil {
		session, err := store.Get(r, "chess-anon")
		if err != nil {
			fmt.Println(err)
		}
		if session.Values["id"] != nil {
			id = session.Values["id"].(string)
		} else {
			id = uuid.New().String()
			session.Values["id"] = id
			err = session.Save(r, w)
			if err != nil {
				fmt.Println(err)
			}
		}
	} else {
		u := t.(authboss.ArbitraryUser)
		id = u.GetPID()
		if u.GetArbitrary()["name"] != "" {
			name = u.GetArbitrary()["name"]
		}
	}

	return chess.Player{id, name}
}

// Create starts a game. The game will originally be in a initalizing phase
// until enough (2) participants have joined.
func (g GameHandler) Create(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	now := time.Now()

	p := getPlayer(w, r, g.ab)

	game := chess.NewGame(primitive.NewObjectID(), now, p)

	err := game.Save(ctx, g.coll)
	if err != nil {
		RespondError(ctx, w, errors.Wrap(err, "creating game"))
		return
	}

	Respond(ctx, w, game, http.StatusOK)
	return
}

// Find gets data about a game.
func (g GameHandler) Find(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()

	gameId := chi.URLParam(r, "gameId")

	p := getPlayer(w, r, g.ab)

	game, err := chess.FindById(ctx, g.coll, gameId, p)
	if err != nil {
		RespondError(ctx, w, errors.Wrap(err, "creating game"))
		return
	}

	Respond(ctx, w, game, http.StatusOK)
	return
}

// Fen returns a fen string.
func (g GameHandler) Fen(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()

	gameId := chi.URLParam(r, "gameId")

	p := getPlayer(w, r, g.ab)

	game, err := chess.FindById(ctx, g.coll, gameId, p)
	if err != nil {
		RespondError(ctx, w, errors.Wrap(err, "creating game"))
		return
	}

	Respond(ctx, w, game.Fen(), http.StatusOK)
	return
}

// Join a game
func (g GameHandler) Join(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()

	gameId := chi.URLParam(r, "gameId")

	p := getPlayer(w, r, g.ab)

	game, err := chess.FindById(ctx, g.coll, gameId, p)
	if err != nil {
		RespondError(ctx, w, errors.Wrap(err, "creating game"))
		return
	}

	game.Join(p)
	if err != nil {
		RespondError(ctx, w, errors.Wrap(err, "joining game"))
		return
	}
	err = game.Save(ctx, g.coll)
	if err != nil {
		RespondError(ctx, w, errors.Wrap(err, "saving game"))
		return
	}

	sub := fmt.Sprintf("game.%v.join", gameId)
	msg := fmt.Sprintf("{\"color\": \"black\", \"name\": \"%s\"}", p.Name)
	g.nc.Publish(sub, []byte(msg))

	Respond(ctx, w, game, http.StatusOK)
	return
}

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

type WsMessage struct {
	Type    string      `json:"t"`
	Message interface{} `json:"m"`
}

// Follow uses websockets to get updates of the game in real time.
func (g GameHandler) Follow(w http.ResponseWriter, r *http.Request) {
	gameId := chi.URLParam(r, "gameId")

	upgrader.CheckOrigin = func(r *http.Request) bool { return true }
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
		return
	}

	g.nc.Subscribe(fmt.Sprintf("game.%v.*", gameId), func(m *nats.Msg) {
		parts := strings.Split(m.Subject, ".")
		sub := parts[len(parts)-1]
		msg := WsMessage{sub, string(m.Data)}
		conn.WriteJSON(msg)
	})
}

type Move struct {
	Move string `json:"move"`
}

// Move applies a move to the game.
func (g GameHandler) Move(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()

	gameId := chi.URLParam(r, "gameId")

	var m Move
	if err := Decode(r, &m); err != nil {
		RespondError(ctx, w, err)
		return
	}

	p := getPlayer(w, r, g.ab)

	game, err := chess.FindById(ctx, g.coll, gameId, p)
	if err != nil {
		RespondError(ctx, w, errors.Wrap(err, "creating game"))
		return
	}

	err = game.Move(m.Move, p.Id)
	if err != nil {
		http.Error(w, http.StatusText(422), 422)
	}

	game.Save(ctx, g.coll)
	if err != nil {
		RespondError(ctx, w, errors.Wrap(err, "saving game"))
		return
	}

	g.nc.Publish(fmt.Sprintf("game.%v.fen", gameId), []byte(game.Fen()))

	Respond(ctx, w, nil, http.StatusNoContent)
	return
}

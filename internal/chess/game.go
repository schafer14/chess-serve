package chess

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/schafer14/MtM/board"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
)

type Game interface {
	Save(context.Context, *mongo.Collection) error
	Join(Player)
	Move(string, string) error
	Fen() string
}

type game struct {
	Id      primitive.ObjectID `json:"id" bson:"_id"`
	Date    time.Time          `json:"date"`
	White   string             `json:"white"`
	WhiteId string             `json:"whiteId"`
	Black   string             `json:"black"`
	BlackId string             `json:"blackId"`
	Moves   []string           `json:"moves"`
	Status  status             `json:"status"`
}

type status int

const (
	StatusInitiating = iota
	StatusInProgress
	StatusDone
)

type Player struct {
	Id   string `json:"id"`
	Name string `json:"name"`
}

func NewGame(id primitive.ObjectID, date time.Time, p Player) Game {
	var g = game{}
	g.Id = id
	g.White = p.Name
	g.WhiteId = p.Id
	g.Date = date
	g.Status = StatusInitiating
	g.Moves = []string{}

	return &g
}

func (g *game) Join(p Player) {
	g.Black = p.Name
	g.BlackId = p.Id
	g.Status = StatusInProgress
	return
}

func (g *game) Save(ctx context.Context, coll *mongo.Collection) error {

	result, err := coll.ReplaceOne(ctx, bson.D{primitive.E{Key: "_id", Value: g.Id}}, g)
	if err != nil {
		return errors.Wrap(err, "saving game")
	}
	if result.MatchedCount == 0 {
		_, err := coll.InsertOne(ctx, g)
		if err != nil {
			return errors.Wrap(err, "saving game")
		}
	}

	return nil
}

func FindById(ctx context.Context, coll *mongo.Collection, id string) (Game, error) {
	var g game
	oid, err := primitive.ObjectIDFromHex(id)
	if err != nil {
		return nil, errors.Wrap(err, "getting object id")
	}
	filter := bson.D{primitive.E{Key: "_id", Value: oid}}

	err = coll.FindOne(context.Background(), filter).Decode(&g)

	if err != nil {
		return nil, errors.Wrap(err, "retrieving game")
	}

	return &g, nil

}

func (g *game) Move(move string, playerId string) error {
	b := board.New()
	b.ApplyMoves(g.Moves)
	if (b.Turn == 0 && playerId != g.WhiteId) || (b.Turn == 1 && playerId != g.BlackId) {
		return fmt.Errorf("Invalid player")
	}

	m, err := b.MoveFromSrcDestNotation(move)
	if err != nil {
		return fmt.Errorf("Invalid move format")
	}

	if !b.IsLegal(m) {
		return fmt.Errorf("Illegal move")
	}

	g.Moves = append(g.Moves, move)

	return nil
}

func (g *game) Fen() string {
	b := board.New()
	b.ApplyMoves(g.Moves)

	return b.String()
}

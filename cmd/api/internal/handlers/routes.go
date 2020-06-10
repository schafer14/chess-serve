package handlers

import (
	"net/http"
	"time"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/cors"
	"github.com/gorilla/context"
	"github.com/nats-io/nats.go"
	"github.com/volatiletech/authboss"
	"github.com/volatiletech/authboss/confirm"
	"github.com/volatiletech/authboss/expire"
	"github.com/volatiletech/authboss/lock"
	"github.com/volatiletech/authboss/remember"
	"go.mongodb.org/mongo-driver/mongo"
)

type Collections struct {
	Observations string
	People       string
}

func API(build string, db *mongo.Database, ab *authboss.Authboss, nc *nats.Conn, cfg Collections, corsMid *cors.Cors, version string) chi.Router {
	r := chi.NewRouter()

	// Middleware
	r.Use(context.ClearHandler)
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.AllowContentType("application/json"))
	r.Use(middleware.Throttle(50))
	r.Use(corsMid.Handler)
	r.Use(middleware.Timeout(time.Second))
	r.Use(middleware.Compress(5))
	r.Use(middleware.Recoverer)
	r.Use(ab.LoadClientStateMiddleware)
	r.Use(remember.Middleware(ab))

	authHandler := AuthHandler{ab}
	checkHandler := Check{build, db, version}
	gameHandler := GameHandler{db.Collection("games"), nc, ab}

	// ======================================
	// Protected routes
	// ======================================
	r.Group(func(r chi.Router) {
		r.Use(authboss.Middleware2(ab, authboss.RequireNone, authboss.RespondUnauthorized))
		r.Use(lock.Middleware(ab))
		r.Use(confirm.Middleware(ab))
		r.Use(expire.Middleware(ab))

		// Information about currently logged in user
		r.MethodFunc("GET", "/v1/me", authHandler.CurrentlyLoggedIn)
	})

	// ======================================
	// Auth routes
	// ======================================
	r.Group(func(r chi.Router) {
		r.Use(authboss.ModuleListMiddleware(ab))
		r.Mount("/v1/auth", http.StripPrefix("/v1/auth", ab.Config.Core.Router))
	})

	// ======================================
	// Unprotected Routes
	// ======================================

	// Game handler
	r.Route("/v1/games", func(r chi.Router) {
		r.Get("/{gameId}", gameHandler.Find)
		r.Get("/{gameId}/follow", gameHandler.Follow)
		r.Get("/{gameId}/fen", gameHandler.Fen)
		r.Put("/{gameId}/join", gameHandler.Join)
		r.Put("/{gameId}/move", gameHandler.Move)
		r.Post("/", gameHandler.Create)
	})

	// Health Check
	r.Get("/health", checkHandler.Health)
	r.Get("/v1/health", checkHandler.Health)
	r.Get("/version", checkHandler.Version)
	r.Get("/v1/version", checkHandler.Version)

	return r
}

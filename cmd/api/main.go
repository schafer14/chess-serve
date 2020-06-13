package main

import (
	"context"
	"encoding/base64"
	"expvar"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"github.com/nats-io/nats.go"
	"github.com/schafer14/chess-serve/cmd/api/internal/handlers"
	"github.com/schafer14/chess-serve/internal/auth"
	"github.com/schafer14/chess-serve/internal/platform/database"

	"github.com/ardanlabs/conf"
	"github.com/go-chi/cors"
	"github.com/gorilla/sessions"
	"github.com/pkg/errors"
	"github.com/volatiletech/authboss"
	abclientstate "github.com/volatiletech/authboss-clientstate"
	abrenderer "github.com/volatiletech/authboss-renderer"
	"github.com/volatiletech/authboss/defaults"

	_ "github.com/volatiletech/authboss/auth"
	_ "github.com/volatiletech/authboss/confirm"
	_ "github.com/volatiletech/authboss/expire"
	_ "github.com/volatiletech/authboss/lock"
	_ "github.com/volatiletech/authboss/logout"
	_ "github.com/volatiletech/authboss/recover"
	_ "github.com/volatiletech/authboss/register"
	_ "github.com/volatiletech/authboss/remember"
)

var build = "develop"
var version = "v0.0.1"

func main() {
	if err := run(); err != nil {
		fmt.Println("error :", err)
		os.Exit(1)
	}
}

func run() error {
	ctx := context.Background()

	// =============================================== //
	// Read Configuration
	// =============================================== //
	var cfg struct {
		APIHost string `conf:"default:0.0.0.0:3000"`
		Cors    struct {
			AllowedHosts []string
		}
		Database struct {
			Uri         string `conf:"default:mongodb://localhost:27017"`
			Name        string `conf:"default:chess"`
			Collections struct {
				Users    string `conf:"default:users"`
				Sessions string `conf:"default:sessions"`
				People   string `conf:"default:people"`
				Groups   string `conf:"default:groups"`
			}
		}
		Auth struct {
			CookieStoreKey    string `conf:"default:NpEPi8pEjKVjLGJ6kYCS+VTCzi6BUuDzU0wrwXyf5uDPArtlofn2AG6aTMiPmN3C909rsEWMNqJqhIVPGP3Exg==,noprint"`
			SessionStoreKey   string `conf:"default:AbfYwmmt8UCwUuhd9qvfNA9UCuN1cVcKJN1ofbiky6xCyyBj20whe40rJa3Su0WOWLWcPpO1taqJdsEI/65+JA==,noprint"`
			SessionCookieName string `conf:"default:chess,noprint"`
			RootURL           string `conf:"default:localhost:3000"`
		}
		Nats struct {
			Server string `json:"default:nats://localhost:4222"`
		}
	}

	if err := conf.Parse(os.Args[1:], "CHESS", &cfg); err != nil {
		fmt.Println(err)
		if err == conf.ErrHelpWanted {
			usage, err := conf.Usage("CHESS", &cfg)
			if err != nil {
				return errors.Wrap(err, "generating config usage")
			}
			fmt.Println(usage)
			return nil
		}
		return errors.Wrap(err, "parsing config")
	}

	// =============================================== //
	// Report Build Parameters
	// =============================================== //
	expvar.NewString("build").Set(build)
	log.Printf("main : Started : Application initializing : version %q", build)
	defer log.Println("main : Completed")

	out, err := conf.String(&cfg)
	if err != nil {
		return errors.Wrap(err, "generating config for output")
	}
	log.Printf("main : Config :\n%v\n", out)

	// =============================================== //
	// Configure NATS
	// =============================================== //
	nc, _ := nats.Connect(cfg.Nats.Server)

	// =============================================== //
	// Configure database
	// =============================================== //
	log.Println("main : Started : Initializing mongo database support")

	db, err := database.Open(ctx, cfg.Database.Uri, cfg.Database.Name)
	if err != nil {
		return errors.Wrap(err, "connecting to db")
	}

	// =============================================== //
	// Configure Authentication
	// =============================================== //
	ab := authboss.New()

	cookieStoreKey, _ := base64.StdEncoding.DecodeString(cfg.Auth.CookieStoreKey)
	sessionStoreKey, _ := base64.StdEncoding.DecodeString(cfg.Auth.SessionStoreKey)

	sessionStorer := abclientstate.NewSessionStorer(cfg.Auth.SessionCookieName, sessionStoreKey, nil)
	cstore := sessionStorer.Store.(*sessions.CookieStore)
	cstore.Options.HttpOnly = true
	cstore.Options.Secure = false
	cstore.MaxAge(int((30 * 24 * time.Hour) / time.Second))

	ab.Config.Storage.Server = auth.NewStorer(db, auth.CollectionConfiguration{cfg.Database.Collections.Users, cfg.Database.Collections.Sessions})
	ab.Config.Storage.SessionState = sessionStorer
	ab.Config.Storage.CookieState = abclientstate.NewCookieStorer(cookieStoreKey, nil)
	ab.Config.Modules.RecoverLoginAfterRecovery = false

	ab.Config.Core.ViewRenderer = defaults.JSONRenderer{}
	defaults.SetCore(&ab.Config, true, false)

	ab.Config.Core.Mailer = defaults.NewLogMailer(log.Writer())
	ab.Config.Modules.RegisterPreserveFields = []string{"email", "name"}
	ab.Config.Modules.MailRouteMethod = "POST"
	ab.Config.Core.MailRenderer = abrenderer.NewEmail("/v1/auth", "ab_views")
	ab.Config.Paths.Mount = "/v1/auth"
	ab.Config.Paths.RootURL = cfg.Auth.RootURL
	ab.Config.Modules.LogoutMethod = "GET"

	ab.Config.Core.Redirector = &redirector{}

	if err := ab.Init(); err != nil {
		return errors.Wrap(err, "configuring authboss")
	}

	// =============================================== //
	// Configure CORS
	// =============================================== //
	cors := cors.New(cors.Options{
		AllowedOrigins:   cfg.Cors.AllowedHosts,
		AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowedHeaders:   []string{},
		AllowCredentials: true,
		MaxAge:           300, // Maximum value not ignored by any of major browsers
	})

	// =============================================== //
	// Starting API
	// =============================================== //
	log.Println("main : Started : Initializing API support")

	collections := handlers.Collections{
		People: cfg.Database.Collections.People,
	}

	router := handlers.API(build, db, ab, nc, collections, cors, version)

	// =============================================== //
	// Add File Server
	// =============================================== //

	filesDir := http.Dir(filepath.Join("./static"))
	FileServer(router, "/public", filesDir)
	ServeFile(router, "favicon.ico", "./static/favicon.ico")
	ServeFile(router, "*", "./static/index.html")

	http.ListenAndServe(cfg.APIHost, router)

	return nil
}

// FIND A WAY TO GET RID OF THIS
type redirector struct{}

func (re *redirector) Redirect(w http.ResponseWriter, r *http.Request, ro authboss.RedirectOptions) error {

	w.Header().Add("Content-Type", "application/json")
	if ro.Code == 307 {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("{\"status\": \"ok\"}"))
	} else {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte(fmt.Sprintf("{\"status\": \"%s\"}", ro.Failure)))
	}

	return nil
}

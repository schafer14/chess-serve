package handlers

import (
	"net/http"

	"github.com/volatiletech/authboss"
)

type AuthHandler struct {
	ab *authboss.Authboss
}

// CurrentlyLoggedIn handles an http request for the currently logged in user.
func (a *AuthHandler) CurrentlyLoggedIn(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()

	p := getPlayer(w, r, a.ab)

	Respond(ctx, w, p, http.StatusOK)
}

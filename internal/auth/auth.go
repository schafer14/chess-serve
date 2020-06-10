package auth

import (
	"context"
	"time"

	"github.com/pkg/errors"
	"github.com/volatiletech/authboss"
	"github.com/volatiletech/authboss/otp/twofactor/totp2fa"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
)

type CollectionConfiguration struct {
	Users    string
	Sessions string
}

// User struct for authboss
type User struct {

	// Non-authboss related field
	Name string `json:"name"`

	// Auth
	Email    string `json:"email"`
	Password string `json:"-"`

	// Confirm
	ConfirmSelector string `json:"-"`
	ConfirmVerifier string `json:"-"`
	Confirmed       bool   `json:"confirmed"`

	// Lock
	AttemptCount int       `json:"-"`
	LastAttempt  time.Time `json:"-"`
	Locked       time.Time `json:"-"`

	// Recover
	RecoverSelector    string    `json:"-"`
	RecoverVerifier    string    `json:"-"`
	RecoverTokenExpiry time.Time `json:"-"`

	// OAuth2
	OAuth2UID          string    `json:"-"`
	OAuth2Provider     string    `json:"-"`
	OAuth2AccessToken  string    `json:"-"`
	OAuth2RefreshToken string    `json:"-"`
	OAuth2Expiry       time.Time `json:"-"`

	// 2fa
	TOTPSecretKey      string `json:"-"`
	SMSPhoneNumber     string `json:"-"`
	SMSSeedPhoneNumber string `json:"-"`
	RecoveryCodes      string `json:"-"`
}

// This pattern is useful in real code to ensure that
// we've got the right interfaces implemented.
var (
	assertUser   = &User{}
	assertStorer = &Storer{}

	_ authboss.User            = assertUser
	_ authboss.AuthableUser    = assertUser
	_ authboss.ConfirmableUser = assertUser
	_ authboss.LockableUser    = assertUser
	_ authboss.RecoverableUser = assertUser
	_ authboss.ArbitraryUser   = assertUser

	_ totp2fa.User = assertUser

	_ authboss.CreatingServerStorer   = assertStorer
	_ authboss.ConfirmingServerStorer = assertStorer
	// _ authboss.RecoveringServerStorer  = assertStorer
	// _ authboss.RememberingServerStorer = assertStorer
)

// PutPID into user
func (u *User) PutPID(pid string) { u.Email = pid }

// PutPassword into user
func (u *User) PutPassword(password string) { u.Password = password }

// PutEmail into user
func (u *User) PutEmail(email string) { u.Email = email }

// PutConfirmed into user
func (u *User) PutConfirmed(confirmed bool) { u.Confirmed = confirmed }

// PutConfirmSelector into user
func (u *User) PutConfirmSelector(confirmSelector string) {
	u.ConfirmSelector = confirmSelector
}

// PutConfirmVerifier into user
func (u *User) PutConfirmVerifier(confirmVerifier string) {
	u.ConfirmVerifier = confirmVerifier
}

// PutLocked into user
func (u *User) PutLocked(locked time.Time) { u.Locked = locked }

// PutAttemptCount into user
func (u *User) PutAttemptCount(attempts int) { u.AttemptCount = attempts }

// PutLastAttempt into user
func (u *User) PutLastAttempt(last time.Time) { u.LastAttempt = last }

// PutRecoverSelector into user
func (u *User) PutRecoverSelector(token string) { u.RecoverSelector = token }

// PutRecoverVerifier into user
func (u *User) PutRecoverVerifier(token string) { u.RecoverVerifier = token }

// PutRecoverExpiry into user
func (u *User) PutRecoverExpiry(expiry time.Time) { u.RecoverTokenExpiry = expiry }

// PutTOTPSecretKey into user
func (u *User) PutTOTPSecretKey(key string) { u.TOTPSecretKey = key }

// PutSMSPhoneNumber into user
func (u *User) PutSMSPhoneNumber(key string) { u.SMSPhoneNumber = key }

// PutRecoveryCodes into user
func (u *User) PutRecoveryCodes(key string) { u.RecoveryCodes = key }

// PutOAuth2UID into user
func (u *User) PutOAuth2UID(uid string) { u.OAuth2UID = uid }

// PutOAuth2Provider into user
func (u *User) PutOAuth2Provider(provider string) { u.OAuth2Provider = provider }

// PutOAuth2AccessToken into user
func (u *User) PutOAuth2AccessToken(token string) { u.OAuth2AccessToken = token }

// PutOAuth2RefreshToken into user
func (u *User) PutOAuth2RefreshToken(refreshToken string) { u.OAuth2RefreshToken = refreshToken }

// PutOAuth2Expiry into user
func (u *User) PutOAuth2Expiry(expiry time.Time) { u.OAuth2Expiry = expiry }

// PutArbitrary into user
func (u *User) PutArbitrary(values map[string]string) {
	if n, ok := values["name"]; ok {
		u.Name = n
	}
}

// GetPID from user
func (u User) GetPID() string { return u.Email }

// GetPassword from user
func (u User) GetPassword() string { return u.Password }

// GetEmail from user
func (u User) GetEmail() string { return u.Email }

// GetConfirmed from user
func (u User) GetConfirmed() bool { return u.Confirmed }

// GetConfirmSelector from user
func (u User) GetConfirmSelector() string { return u.ConfirmSelector }

// GetConfirmVerifier from user
func (u User) GetConfirmVerifier() string { return u.ConfirmVerifier }

// GetLocked from user
func (u User) GetLocked() time.Time { return u.Locked }

// GetAttemptCount from user
func (u User) GetAttemptCount() int { return u.AttemptCount }

// GetLastAttempt from user
func (u User) GetLastAttempt() time.Time { return u.LastAttempt }

// GetRecoverSelector from user
func (u User) GetRecoverSelector() string { return u.RecoverSelector }

// GetRecoverVerifier from user
func (u User) GetRecoverVerifier() string { return u.RecoverVerifier }

// GetRecoverExpiry from user
func (u User) GetRecoverExpiry() time.Time { return u.RecoverTokenExpiry }

// GetTOTPSecretKey from user
func (u User) GetTOTPSecretKey() string { return u.TOTPSecretKey }

// GetSMSPhoneNumber from user
func (u User) GetSMSPhoneNumber() string { return u.SMSPhoneNumber }

// GetSMSPhoneNumberSeed from user
func (u User) GetSMSPhoneNumberSeed() string { return u.SMSSeedPhoneNumber }

// GetRecoveryCodes from user
func (u User) GetRecoveryCodes() string { return u.RecoveryCodes }

// IsOAuth2User returns true if the user was created with oauth2
func (u User) IsOAuth2User() bool { return len(u.OAuth2UID) != 0 }

// GetOAuth2UID from user
func (u User) GetOAuth2UID() (uid string) { return u.OAuth2UID }

// GetOAuth2Provider from user
func (u User) GetOAuth2Provider() (provider string) { return u.OAuth2Provider }

// GetOAuth2AccessToken from user
func (u User) GetOAuth2AccessToken() (token string) { return u.OAuth2AccessToken }

// GetOAuth2RefreshToken from user
func (u User) GetOAuth2RefreshToken() (refreshToken string) { return u.OAuth2RefreshToken }

// GetOAuth2Expiry from user
func (u User) GetOAuth2Expiry() (expiry time.Time) { return u.OAuth2Expiry }

// GetArbitrary from user
func (u User) GetArbitrary() map[string]string {
	return map[string]string{
		"name": u.Name,
	}
}

// Storer stores users in memory
type Storer struct {
	UsersC    *mongo.Collection
	SessionsC *mongo.Collection
}

// NewStorer constructor
func NewStorer(db *mongo.Database, collectionNames CollectionConfiguration) *Storer {
	return &Storer{
		UsersC:    db.Collection(collectionNames.Users),
		SessionsC: db.Collection(collectionNames.Sessions),
	}
}

// Save the user
func (m Storer) Save(ctx context.Context, user authboss.User) error {
	u := user.(*User)

	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()

	_, err := m.UsersC.ReplaceOne(ctx, bson.D{primitive.E{Key: "email", Value: u.Email}}, u)

	if err != nil {
		return errors.Wrap(err, "storing user")
	}

	return nil
}

// Load the user
func (m Storer) Load(ctx context.Context, key string) (user authboss.User, err error) {
	var u User
	err = m.UsersC.FindOne(ctx, bson.D{primitive.E{Key: "email", Value: key}}).Decode(&u)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			return nil, authboss.ErrUserNotFound
		}
		return nil, errors.Wrap(err, "fetching user")
	}

	return &u, nil
}

// New user creation
func (m Storer) New(_ context.Context) authboss.User {
	return &User{}
}

// Create the user
func (m Storer) Create(ctx context.Context, user authboss.User) error {
	u := user.(*User)

	// err := m.UsersC.FindOne(ctx, bson.D{{"email", user.GetPID()}}).Decode(&u)
	err := m.UsersC.FindOne(ctx, bson.D{primitive.E{Key: "email", Value: user.GetPID()}}).Decode(&u)
	if err == nil {
		return authboss.ErrUserFound
	}

	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()

	_, err = m.UsersC.InsertOne(ctx, u)

	if err != nil {
		return errors.Wrap(err, "storing user")
	}

	return nil
}

// LoadByConfirmSelector looks a user up by confirmation token
func (m Storer) LoadByConfirmSelector(ctx context.Context, selector string) (user authboss.ConfirmableUser, err error) {
	var u User
	// err = m.UsersC.FindOne(ctx, bson.D{{"confirmselector", selector}}).Decode(&u)
	err = m.UsersC.FindOne(ctx, bson.D{primitive.E{Key: "confirmselector", Value: selector}}).Decode(&u)
	if err != nil {
		if err == mongo.ErrNoDocuments {
			return nil, authboss.ErrUserNotFound
		}
		return nil, errors.Wrap(err, "fetching user")
	}

	return &u, nil

}

// // LoadByRecoverSelector looks a user up by confirmation selector
// func (m Storer) LoadByRecoverSelector(_ context.Context, selector string) (user authboss.RecoverableUser, err error) {
// 	for _, v := range m.Users {
// 		if v.RecoverSelector == selector {
// 			return &v, nil
// 		}
// 	}

// 	return nil, authboss.ErrUserNotFound
// }

// // AddRememberToken to a user
// func (m Storer) AddRememberToken(_ context.Context, pid, token string) error {
// 	m.Tokens[pid] = append(m.Tokens[pid], token)
// 	spew.Dump(m.Tokens)
// 	return nil
// }

// // DelRememberTokens removes all tokens for the given pid
// func (m Storer) DelRememberTokens(_ context.Context, pid string) error {
// 	delete(m.Tokens, pid)
// 	spew.Dump(m.Tokens)
// 	return nil
// }

// // UseRememberToken finds the pid-token pair and deletes it.
// // If the token could not be found return ErrTokenNotFound
// func (m Storer) UseRememberToken(_ context.Context, pid, token string) error {
// 	tokens, ok := m.Tokens[pid]
// 	if !ok {
// 		return authboss.ErrTokenNotFound
// 	}

// 	for i, tok := range tokens {
// 		if tok == token {
// 			tokens[len(tokens)-1] = tokens[i]
// 			m.Tokens[pid] = tokens[:len(tokens)-1]
// 			return nil
// 		}
// 	}

// 	return authboss.ErrTokenNotFound
// }

// // NewFromOAuth2 creates an oauth2 user (but not in the database, just a blank one to be saved later)
// func (m Storer) NewFromOAuth2(_ context.Context, provider string, details map[string]string) (authboss.OAuth2User, error) {
// 	switch provider {
// 	case "google":
// 		email := details[aboauth.OAuth2Email]

// 		var user *User
// 		if u, ok := m.Users[email]; ok {
// 			user = &u
// 		} else {
// 			user = &User{}
// 		}

// 		// Google OAuth2 doesn't allow us to fetch real name without more complicated API calls
// 		// in order to do this properly in your own app, look at replacing the authboss oauth2.GoogleUserDetails
// 		// method with something more thorough.
// 		user.Name = "Unknown"
// 		user.Email = details[aboauth.OAuth2Email]
// 		user.OAuth2UID = details[aboauth.OAuth2UID]
// 		user.Confirmed = true

// 		return user, nil
// 	}

// 	return nil, errors.Errorf("unknown provider %s", provider)
// }

// // SaveOAuth2 user
// func (m Storer) SaveOAuth2(_ context.Context, user authboss.OAuth2User) error {
// 	u := user.(*User)
// 	m.Users[u.Email] = *u

// 	return nil
// }

/*
func (s Storer) PutOAuth(uid, provider string, attr authboss.Attributes) error {
	return s.Create(uid+provider, attr)
}

func (s Storer) GetOAuth(uid, provider string) (result interface{}, err error) {
	user, ok := s.Users[uid+provider]
	if !ok {
		return nil, authboss.ErrUserNotFound
	}

	return &user, nil
}

func (s Storer) AddToken(key, token string) error {
	s.Tokens[key] = append(s.Tokens[key], token)
	fmt.Println("AddToken")
	spew.Dump(s.Tokens)
	return nil
}

func (s Storer) DelTokens(key string) error {
	delete(s.Tokens, key)
	fmt.Println("DelTokens")
	spew.Dump(s.Tokens)
	return nil
}

func (s Storer) UseToken(givenKey, token string) error {
	toks, ok := s.Tokens[givenKey]
	if !ok {
		return authboss.ErrTokenNotFound
	}

	for i, tok := range toks {
		if tok == token {
			toks[i], toks[len(toks)-1] = toks[len(toks)-1], toks[i]
			s.Tokens[givenKey] = toks[:len(toks)-1]
			return nil
		}
	}

	return authboss.ErrTokenNotFound
}

func (s Storer) ConfirmUser(tok string) (result interface{}, err error) {
	fmt.Println("==============", tok)

	for _, u := range s.Users {
		if u.ConfirmToken == tok {
			return &u, nil
		}
	}

	return nil, authboss.ErrUserNotFound
}

func (s Storer) RecoverUser(rec string) (result interface{}, err error) {
	for _, u := range s.Users {
		if u.RecoverToken == rec {
			return &u, nil
		}
	}

	return nil, authboss.ErrUserNotFound
}
*/

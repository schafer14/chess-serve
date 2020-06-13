module Session exposing (Session, isLoggedIn, login, logout, navKey, new)

import Browser.Navigation as Nav



-- TYPES


type Session
    = LoggedIn Nav.Key
    | Guest Nav.Key


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key ->
            key

        Guest key ->
            key


login : Session -> Session
login session =
    case session of
        Guest nav ->
            LoggedIn nav

        _ ->
            session


logout : Session -> Session
logout session =
    case session of
        LoggedIn nav ->
            Guest nav

        _ ->
            session


isLoggedIn : Session -> Bool
isLoggedIn s =
    case s of
        LoggedIn _ ->
            True

        _ ->
            False



-- This will have to change at some point to check if the user
-- is already logged in.


new : Nav.Key -> Session
new k =
    Guest k

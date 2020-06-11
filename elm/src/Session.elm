module Session exposing (Session, navKey, new)

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



-- This will have to change at some point to check if the user
-- is already logged in.


new : Nav.Key -> Session
new k =
    Guest k

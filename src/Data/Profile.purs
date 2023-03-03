-- | Profiles are an example of an entity (a persistent data type with a unique identity) and are
-- | widely used in the Conduit application.
-- |
-- | We have two versions of the `Profile` type, as with the `Article` type: one with a few core
-- | fields about the user and another that also contains their email address. It's tedious and
-- | error-prone to write out multiple variations of a type which all share the same core fields
-- | so we'll use extensible to solve that problem.
-- |
-- | The backend returns a `following` flag as part of a profile, but this can lead to invalid
-- | states. What happens if you're viewing your own profile? There's no concept of following
-- | yourself. We'll create a custom type to rule out invalid relationships like this.
-- |
-- | This module also demonstrates how to create lenses for record types. Optics, which include
-- | lenses, let you work with values within nested structures. We'll use optics to drill down from
-- | large types like a component `State` to a particular field in a profile stored in that state.
module CleanAs.Data.Profile where

import CleanAs.Data.Email (Email)
import CleanAs.Data.Email as Email
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)

-- | Now, let's describe the fields of our main `Profile` type, which should also be shared with
-- | the extended `ProfileWithEmail` type.
-- |
-- | A user profile contains a mandatory username, a biography which is allowed to be empty, an
-- | optional avatar, and a relation to the current user, if there is one. We've already designed
-- | types that nicely capture the semantics of each field, so all we need to do here is assemble
-- | them into a row that can be used to implement `Profile` and `ProfileWithEmail`.
type ProfileRep row =
  ( email :: Email
  | row
  )

type Profile = { | ProfileRep () }

type ProfileWithPassword = { | ProfileRep (password :: Maybe String) }

profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { email: Email.codec
    }

profileWithPasswordCodec :: JsonCodec ProfileWithPassword
profileWithPasswordCodec =
  CAR.object "Profile"
    { email: Email.codec
    , password: CAC.maybe CA.string
    }


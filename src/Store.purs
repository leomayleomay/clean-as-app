-- | A global state containing information useful to most components in the
-- | application. If you've ever used Redux, this should look familiar.
-- | Components can read, write, and subscribe to this central state, which is
-- | called a "store" by convention.
-- |
-- | More in-depth documentation on creating a store can be found in the
-- | docs for the `halogen-store` library:
-- | https://github.com/thomashoneyman/purescript-halogen-store
module CleanAs.Store where

import CleanAs.Api.Request (BaseURL)
import CleanAs.Data.Profile (Profile)
import Data.Maybe (Maybe(..))

-- | We can now construct our central state which will be available to all
-- | components (if they opt-in).
-- |
-- | First, we'll use a `LogLevel` flag to indicate whether we'd like to log
-- | everything (`Dev`) or only critical messages (`Prod`). Next, we'll maintain
-- | a configurable base URL. We'll also hold on to the currently-logged-in user.
type Store =
  { baseUrl :: BaseURL
  , currentUser :: Maybe Profile
  }

-- | Ordinarily we'd write an initialStore function, but in our case we construct
-- | all three values in our initial store during app initialization. For that
-- | reason, you can see the store get constructed in the `Main` module.

-- | Next, we'll define a data type that represents state updates to our store.
-- | The log level and base URL should remain constant, but we'll need to be
-- | able to set the current user.
data Action
  = Login Profile
  | Logout

-- | Finally, we'll map this action to a state update in a function called a
-- | 'reducer'. If you're curious to learn more, see the `halogen-store`
-- | documentation!
reduce :: Store -> Action -> Store
reduce store = case _ of
  Login profile ->
    store { currentUser = Just profile }

  Logout ->
    store { currentUser = Nothing }

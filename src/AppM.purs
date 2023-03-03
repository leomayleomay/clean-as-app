-- | A custom application monad that provides concrete implementations for capabilities like
-- | logging, navigation, and resource management. This is our production monad -- it interprets
-- | our capabilities as they are meant to run on our production site.
-- |
-- | However, since capabilities like logging are implemented as type classes, we can also provide
-- | one or more test monads that provide different interpretations.
-- |
-- | For example, this monad will actually hit the server with API requests when we manage a
-- | resource, but our test monad might just return mock JSON or error responses.
-- |
-- | See the various `Conduit.Capability.*` modules for deeper explanations of each capability, and
-- | the accompanying guide for a thorough introduction to this style of application architecture.
-- |
-- | https://thomashoneyman.com/guides/real-world-halogen
module CleanAs.AppM where

import Prelude

import CleanAs.Api.Request as Request
import CleanAs.Api.Utils (authenticate)
import CleanAs.Capability.Navigate (class Navigate, navigate)
import CleanAs.Capability.Resource.User (class ManageUser)
import CleanAs.Data.Route as Route
import CleanAs.Store (Action(..), Store)
import CleanAs.Store as Store
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)

-- | In the capability modules (`Conduit.Capability.*`), we wrote some abstract, high-level
-- | interfaces for business logic that tends to be highly effectful, like resource management and
-- | logging. We wrote interfaces (just the types, no actual implementation) so that we could write
-- | the same code once and swap in different implementations as we see fit.
-- |
-- | This carries two main benefits. First, it helps abstract away the implementation so we can
-- | focus on logic: businesses care about reading, writing, and deleting resources, not whether
-- | that's done on the file system, over RPC, a REST API, or something else. Second, it lets us
-- | write code once and swap in different implementations under the hood. That means we can run
-- | the same section of code in production using one implementation and as part of a test suite
-- | using another. For example, our production code might use a REST API, but our test code might
-- | just provide mock JSON responses.
-- |

-- | This module implements a monad that can run all the abstract capabilities we've defined. This
-- | is our production monad. We'll implement the monad first, and then we'll provide concrete
-- | instances for each of our abstract capabilities.
-- |
-- | The `Aff` monad allows us to run asynchronous effects. When you're in `Aff`, you can write
-- | code that makes API requests, writes files, and so on. In addition, using the `liftEffect`
-- | function, you can use any function that relies on the `Effect` monad in `Aff`. For example,
-- | you can log messages to the console within `Aff` using (liftEffect <<< Console.log). It's
-- | powerful stuff.
-- |
-- | The `StoreT` monad transformer adds the ability to have a central state in our Halogen
-- | application on top of the abilities of `Aff`. See the `halogen-store` library for more
-- | details!
-- |
-- | `AppM` combines the `Aff` and `Store` monads under a new type, which we can now use to write
-- | instances for our capabilities. We're able to combine these monads because `StoreT` is a
-- | monad transformer. Monad transformers are too large a topic to delve into here; for now, it's
-- | enough to know that they let you combine the abilities of two or more monads.
newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

-- | We can get a monad out of our `AppM` type essentially for free by deferring to the underlying
-- | `ReaderT` instances. PureScript allows any newtype to re-use the type class instances of the
-- | type it wraps with the `derive newtype instance` syntax. It's as if the newtype didn't exist
-- | and the function was being applied to the type underneath directly.
-- |
-- | To be a monad, a type must implement the `Functor`, `Apply`, `Applicative`, `Bind`, and
-- | `Monad` type classes.
-- |
-- | In addition, because we used `Aff` as the base of our custom monad, we can also derive
-- | `MonadEffect` and `MonadAff`, two type classes that let us use any functions that run
-- | in `Effect` or in `Aff`. Having access to these two type classes lets us perform pretty much
-- | any effect we see fit, from API requests to local storage access.
-- |
-- | Finally, since we're using the `halogen-store` library, we can also derive a `MonadStore`
-- | constraint that lets us use our central state anywhere in the application.
-- |
-- | With the compiler by your side, you don't need to know how to implement a monad from scratch.
-- | You can derive everything you need! We can now focus just on the instances that matter to us:
-- | our app environment and our capabilities.
derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

-- | Our app uses hash-based routing, so to navigate from place to place, we'll just set the hash.
-- | Note how our navigation capability uses our routing data type rather than let you set any
-- | arbitrary hash. Logging out is a little more involved, because we need to clean up things like
-- | the auth token. Navigating home will take care of emptying the reference to the current user.
instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    liftEffect $ Request.removeToken
    updateStore Logout
    navigate Route.Home

-- | Our first resource class describes what operations we have available to manage users. Logging
-- | in and registration require manipulating a token, but we've designed the `Token` type so its
-- | contents can't be read by any function outside the `Api.Request` module. For that reason,
-- | the `login` and `register` implementations are directly imported. The others use our nicer
-- | `mkRequest` and `mkAuthRequest` helpers.
instance manageUserAppM :: ManageUser AppM where
  login =
    authenticate Request.login

  register =
    authenticate Request.register

  forgotPassword fields = do
    { baseUrl } <- getStore
    Request.forgotPassword baseUrl fields >>= case _ of
      Left _ -> pure unit
      Right _ -> pure unit

  resetPassword token fields = do
    { baseUrl } <- getStore
    Request.resetPassword baseUrl token fields >>= case _ of
      Left _ -> pure unit
      Right _ -> pure unit
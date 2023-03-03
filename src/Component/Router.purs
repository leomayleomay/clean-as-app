-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module CleanAs.Component.Router where

import Prelude

import CleanAs.Capability.Navigate (class Navigate, navigate)
import CleanAs.Capability.Resource.User (class ManageUser)
import CleanAs.Component.Utils (OpaqueSlot)
import CleanAs.Data.Profile (Profile)
import CleanAs.Data.Route (Route(..), routeCodec)
import CleanAs.Page.Home as Home
import CleanAs.Page.Login as Login
import CleanAs.Page.Register as Register
import CleanAs.Page.ForgotPassword as ForgotPassword
import CleanAs.Page.ResetPassword as ResetPassword
import CleanAs.Store as Store
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))

data Query a = Navigate Route a

type State =
  { route :: Maybe Route
  , currentUser :: Maybe Profile
  }

data Action
  = Initialize
  | Receive (Connected (Maybe Profile) Unit)

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , register :: OpaqueSlot Unit
  , forgotPassword :: OpaqueSlot Unit
  , resetPassword :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageUser m
  => H.Component Query Unit Void m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState: \{ context: currentUser } -> { route: Nothing, currentUser }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

    Receive { context: currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        -- don't change routes if there is a logged-in user trying to access
        -- a route only meant to be accessible to a not-logged-in session
        case (isJust currentUser && dest `elem` [ Login, Register ]) of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home ->
        HH.slot_ (Proxy :: _ "home") unit Home.component unit
      Login ->
        HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: true }
      Register ->
        HH.slot_ (Proxy :: _ "register") unit Register.component unit
      ForgotPassword ->
        HH.slot_ (Proxy :: _ "forgotPassword") unit ForgotPassword.component unit
      ResetPassword { token } ->
        HH.slot_ (Proxy :: _ "resetPassword") unit ResetPassword.component { token }
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]

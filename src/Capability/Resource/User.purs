-- | A capability representing the ability to manage users in our system. That includes logging in,
-- | registering, following / unfollowing, fetching an article's author, and more.
-- |
-- | This capability lets us ignore the mechanics of managing a resource and focus on our
-- | business logic. For now our app implements this capability with a REST API, but we could
-- | easily swap in a database, RPC, local filesystem, or something else without having to touch
-- | any application code besides the application monad, `Conduit.AppM`. In addition, we can test
-- | our business logic by mocking responses in our test monad instead of hitting the server.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:
-- | https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/
module CleanAs.Capability.Resource.User where

import Prelude

import CleanAs.Api.Request (LoginFields, RegisterFields, ForgotPasswordFields, ResetPasswordFields)
import CleanAs.Data.Profile (Profile)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

-- | This capability represents the ability to manage users in our system. We support logging users
-- | in, and registering them, as well as reading information about various users and who follows
-- | who.
-- |
-- | We'll handle all the mechanics of making the request, decoding responses, handling errors, and
-- | so on in the implementation.
class Monad m <= ManageUser m where
  login :: LoginFields -> m (Maybe Profile)
  register :: RegisterFields -> m (Maybe Profile)
  forgotPassword :: ForgotPasswordFields -> m Unit
  resetPassword :: String -> ResetPasswordFields -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  login = lift <<< login
  register = lift <<< register
  forgotPassword = lift <<< forgotPassword
  resetPassword token = lift <<< resetPassword token
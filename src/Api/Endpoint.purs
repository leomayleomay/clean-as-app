module CleanAs.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', prefix, root, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

-- | First, let's define a few types necessary for our larger `Endpoint` type.

-- | Some endpoints are paginated and accept a limit (maximum count) and offset (number of items
-- | to skip over). Since some endpoints accept pagination in addition to other parameters, we'll
-- | create a row that can be shared by multiple types.
type PaginationRep =
  ( limit :: Maybe Int
  , offset :: Maybe Int
  )

-- | This record type is useful for endpoints that only need pagination information.
type Pagination = { | PaginationRep }

-- | This data type captures each endpoint our API supports. In a larger application this would be
-- | tedious to maintain, and it's more common to generate endpoints from a Swagger or Open API
-- | spec. For the time being, though, we'll take the same approach as we did for our routes and
-- | create an encompassing sum type to represent all endpoints. With this type, requests to
-- | invalid endpoints (endpoints not captured in this type) will fail to compile.
data Endpoint
  = Login
  | Register
  | CurrentUser
  | ForgotPassword
  | ResetPassword { reset_password_token :: String }

derive instance genericEndpoint :: Generic Endpoint _

-- Our codec will cause a compile-time error if we fail to handle any of our
-- route cases.

-- | We need to be able to write our `Endpoint` type to a valid path in order to make requests. We
-- | can use `routing-duplex` the same way we did with our `Route` type to provide both a printer
-- | (write to `String`) and a parser (parse from `String`) that stays in sync with our `Endpoint`
-- | type automatically.
-- |
-- | For a full treatment of how this function produces both a parser and printer guaranteed to
-- | produce valid paths, see the `routing-duplex` tutorial:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "Login": "login" / noArgs
  , "Register": "register" / noArgs
  , "CurrentUser": "current_user" / noArgs
  , "ForgotPassword": "forgot_password" / noArgs
  , "ResetPassword": "reset_password" ? { reset_password_token: string }
  }


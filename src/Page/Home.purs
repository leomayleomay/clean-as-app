module CleanAs.Page.Home where

import Prelude

import CleanAs.Component.HTML.Utils (css, safeHref)
import CleanAs.Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

component
  :: forall q o m
   . MonadAff m
  => H.Component q Unit o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
    }
  where

  render :: forall slots. Unit -> H.ComponentHTML Void slots m
  render _ = HH.li
    [ css "nav-item" ]
    [ HH.a
        [ css $ "nav-link active"
        , safeHref Login
        ]
        [ HH.text
            "Login"
        ]
    ]

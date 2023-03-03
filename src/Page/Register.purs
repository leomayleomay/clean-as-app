-- | The registration form allows new users to sign up to the Conduit service and authenticate
-- | their session.
module CleanAs.Page.Register where

import Prelude

import CleanAs.Capability.Navigate (class Navigate, navigate)
import CleanAs.Capability.Resource.User (class ManageUser, register)
import CleanAs.Component.HTML.Utils (css, safeHref)
import CleanAs.Data.Email (Email)
import CleanAs.Data.Route (Route(..))
import CleanAs.Form.Field as Field
import CleanAs.Form.Validation (FormError)
import CleanAs.Form.Validation as V
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( email :: f String FormError Email
  , password :: f String FormError String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

component
  :: forall query output m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => H.Component query Unit output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: \context -> context
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      onSubmit = register >=> traverse_ (\_ -> navigate Home)
      validation =
        { email: V.required >=> V.emailFormat
        , password: V.required >=> V.minLength 8 >=> V.maxLength 20
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: FormContext -> H.ComponentHTML Action () m
  render { formActions, fields, actions } =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign Up" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Login ]
              [ HH.text "Already have an account?" ]
          ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ HH.fieldset_
              [ Field.textInput
                  { state: fields.email, action: actions.email }
                  [ HP.placeholder "Email"
                  , HP.type_ HP.InputEmail
                  ]
              , Field.textInput
                  { state: fields.password, action: actions.password }
                  [ HP.placeholder "Password "
                  , HP.type_ HP.InputPassword
                  ]
              , Field.submitButton "Sign up"
              ]
          ]
      ]
    where
    container html =
      HH.div_
        [ HH.div
            [ css "auth-page" ]
            [ HH.div
                [ css "container page" ]
                [ HH.div
                    [ css "row" ]
                    [ HH.div
                        [ css "col-md-6 offset-md-3 col-xs12" ]
                        html
                    ]
                ]
            ]
        ]

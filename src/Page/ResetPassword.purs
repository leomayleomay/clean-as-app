module CleanAs.Page.ResetPassword where

import Prelude

import CleanAs.Capability.Navigate (class Navigate, navigate)
import CleanAs.Capability.Resource.User (class ManageUser, resetPassword)
import CleanAs.Component.HTML.Utils (css)
import CleanAs.Data.Route (Route(..))
import CleanAs.Form.Field as Field
import CleanAs.Form.Validation (FormError)
import CleanAs.Form.Validation as V
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

type Input = { token :: String }

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( password :: f String FormError String
  , password_confirmation :: f String FormError String
  )

-- The form context includes our input, so we don't need to implement another
-- `Input` type.
type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

type State =
  { form :: FormContext
  }

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component query Input output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: \context -> { form: context }
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
    Receive context -> H.modify_ _ { form = context }
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      onSubmit outputs = do
        { token } <- H.gets _.form.input
        resetPassword token outputs
        void $ navigate Login

      validation =
        { password: V.required >=> V.minLength 2 >=> V.maxLength 20
        , password_confirmation: V.required >=> V.minLength 2 >=> V.maxLength 20
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: State -> H.ComponentHTML Action () m
  render { form: { formActions, fields, actions } } =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Reset Password" ]
      , HH.form
          [ HE.onSubmit formActions.handleSubmit ]
          [ HH.fieldset_
              [ Field.textInput
                  { state: fields.password, action: actions.password }
                  [ HP.placeholder "Password"
                  , HP.type_ HP.InputPassword
                  ]
              , Field.textInput
                  { state: fields.password_confirmation, action: actions.password_confirmation }
                  [ HP.placeholder "Password Confirmation"
                  , HP.type_ HP.InputPassword
                  ]
              , Field.submitButton "Submit"
              ]
          ]
      ]
    where
    container html =
      HH.div
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

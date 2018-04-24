module Component.DiagTree.Editor.SlideEditor.Action
     ( diagTreeEditorSlideEditorAction
     ) where

import Prelude hiding (div)

import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe (..))

import React (ReactClass, createElement)
import React.DOM (div) as R
import React.DOM.Props (className)
import React.Spaces ((!.), renderIn, text, element)
import React.Spaces.DOM (div, label)

import Utils ((<.>), createClassStatelessWithName, unfoldrBoundedEnum)
import Component.Generic.DropDownSelect (OnSelectedEff, dropDownSelect)
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Types (DiagTreeSlideAction)


type Props eff =
  { appContext :: AppContext
  , isDisabled :: Boolean
  , action     :: Maybe DiagTreeSlideAction
  , onSelected :: Maybe DiagTreeSlideAction -> Eff (OnSelectedEff eff) Unit
  }

diagTreeEditorSlideEditorActionRender :: forall eff . ReactClass (Props eff)
diagTreeEditorSlideEditorActionRender = createClassStatelessWithName name $
  \ { appContext, isDisabled, action, onSelected } -> renderer $ do

  label !. "control-label" $ text "Рекомендация"

  div $ element $
    dropDownSelectEl
      { appContext
      , isDisabled
      , variants
      , selected: action
      , variantView: show
      , onSelected: Just onSelected
      , placeholder: Just "Что делать?"
      , notSelectedTitle: Just "(не выбрано)"
      } []

  where
    name = "DiagTreeEditorSlideEditorAction"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className $ "form-group" <.> name]
    renderer = renderIn wrapper
    variants = (unfoldrBoundedEnum :: Array DiagTreeSlideAction)
    dropDownSelectEl = createElement dropDownSelect


diagTreeEditorSlideEditorAction :: forall eff . ReactClass (Props eff)
diagTreeEditorSlideEditorAction = diagTreeEditorSlideEditorActionRender

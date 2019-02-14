module Component.DiagTree.Editor.SlideEditor.Action
     ( diagTreeEditorSlideEditorAction
     ) where

import Prelude hiding (div)

import Data.Monoid (mempty)
import Data.Maybe (Maybe (..))

import React (ReactClass, EventHandler, createElement)
import React.DOM (text, div, div', label)
import React.DOM.Props (className)

import Utils ((<.>), createClassStatelessWithName, unfoldrBoundedEnum)
import Component.Generic.DropDownSelect (dropDownSelect)
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Types (DiagTreeSlideAction)


type Props =
  { appContext :: AppContext
  , isDisabled :: Boolean
  , action     :: Maybe DiagTreeSlideAction
  , onSelected :: EventHandler (Maybe DiagTreeSlideAction)
  }

diagTreeEditorSlideEditorActionRender :: ReactClass Props
diagTreeEditorSlideEditorActionRender = createClassStatelessWithName name $
  \ { appContext, isDisabled, action, onSelected } -> wrapper

  [ label [className "control-label"] [text "Рекомендация"]

  , div' $ pure $
      flip dropDownSelectEl mempty
        { appContext
        , isDisabled
        , variants
        , selected: action
        , variantView: show
        , onSelected: Just onSelected
        , placeholder: Just "Что делать?"
        , notSelectedTitle: Just "(не выбрано)"
        }
  ]

  where
    name = "DiagTreeEditorSlideEditorAction"
    classSfx s = name <> "--" <> s
    wrapper = div [className $ "form-group" <.> name]
    variants = (unfoldrBoundedEnum :: Array DiagTreeSlideAction)
    dropDownSelectEl = createElement dropDownSelect


diagTreeEditorSlideEditorAction :: ReactClass Props
diagTreeEditorSlideEditorAction = diagTreeEditorSlideEditorActionRender

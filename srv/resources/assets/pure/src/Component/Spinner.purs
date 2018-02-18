module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import React (ReactClass)
import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.Spaces.DOM (div)
import React.Spaces ((!.), renderIn, text, empty)

import Utils (createClassStatelessWithSpec)
import App.Store (AppContext)


spinnerRender
  :: ReactClass { withLabel  :: Boolean
                , appContext :: AppContext
                }

spinnerRender = createClassStatelessWithSpec specMiddleware $ \props ->
  if props.withLabel

     then renderIn wrapper $ do
            div !. classSfx "with-label" $ do
              div $ text "Загрузка…"
              div !. classSfx "icon" $ empty

     else renderIn wrapper $
            div !. classSfx "icon" $ empty

  where
    name = "circle-spinner"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    specMiddleware = _
      { displayName = "Spinner"
      , shouldComponentUpdate = \_ _ _ -> pure false
      }


spinner :: ReactClass { withLabel :: Boolean, appContext :: AppContext }
spinner = spinnerRender

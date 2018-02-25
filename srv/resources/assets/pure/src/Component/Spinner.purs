module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import React (ReactClass)
import React.DOM (div) as R
import React.DOM.Props (className)
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
    name = "CircleSpinner"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className name]

    specMiddleware = _
      { displayName = name
      , shouldComponentUpdate = \_ _ _ -> pure false -- TODO FIXME
      }


spinner :: ReactClass { withLabel :: Boolean, appContext :: AppContext }
spinner = spinnerRender

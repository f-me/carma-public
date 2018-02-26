module Component.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import Data.Either (Either (..))

import React (ReactClass, getProps)
import React.DOM (div) as R
import React.DOM.Props (className)
import React.Spaces.DOM (div)
import React.Spaces ((!.), renderIn, text, empty)

import Utils (createClassStatelessWithSpec)
import App.Store (AppContext)


type Props =
  { appContext :: AppContext

  , withLabel  :: Either Boolean String
  -- ^ `Left`'s flag indicates is label shown or not
  --   `Right` for specific custom label to show
  }


spinnerRender :: ReactClass Props
spinnerRender = createClassStatelessWithSpec specMiddleware $ \props ->
  if props.withLabel /= Left false

     then renderIn wrapper $
            div !. classSfx "with-label" $ do
              div $ text $ case props.withLabel of
                                Left  _ -> "Загрузка…"
                                Right x -> x

              div !. classSfx "icon" $ empty

     else renderIn wrapper $
            div !. classSfx "icon" $ empty

  where
    name = "CircleSpinner"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className name]

    specMiddleware = _
      { displayName = name

      , shouldComponentUpdate = \this nextProps _ -> do
          prevProps <- getProps this
          pure $ prevProps.withLabel /= nextProps.withLabel
      }


spinner :: ReactClass Props
spinner = spinnerRender

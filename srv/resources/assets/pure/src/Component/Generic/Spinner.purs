module Component.Generic.Spinner
     ( spinner
     ) where

import Prelude hiding (div)

import Data.Either (Either (..))

import React (ReactClass, component, getProps)
import React.DOM (div, div', text)
import React.DOM.Props (className)

import App.Store (AppContext)


type Props =
   { appContext :: AppContext

   , withLabel  :: Either Boolean String
   -- ^ `Left`'s flag indicates is label shown or not
   --   `Right` for specific custom label to show
   }


spinnerRender :: ReactClass Props
spinnerRender = go where
  go = component name spec

  renderFn props
    = wrapper $ pure
    $ if props.withLabel /= Left false

         then div
                [ className $ classSfx "with-label" ]
                [ div' $ pure $ text $ case props.withLabel of
                                            Left  _ -> "Загрузка…"
                                            Right x -> x

                , div [className $ classSfx "icon"] mempty
                ]

         else div [className $ classSfx "icon"] mempty

  spec this = getProps this <#> \props ->
    { render: renderFn <$> getProps this

    , shouldComponentUpdate: \nextProps _ -> do
        prevProps <- getProps this
        pure $ prevProps.withLabel /= nextProps.withLabel
    }

  name = "CircleSpinner"
  classSfx s = name <> "--" <> s
  wrapper = div [className name]


spinner :: ReactClass Props
spinner = spinnerRender

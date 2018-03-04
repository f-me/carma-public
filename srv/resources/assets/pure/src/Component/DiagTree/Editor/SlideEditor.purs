module Component.DiagTree.Editor.SlideEditor
     ( diagTreeEditorSlideEditor
     ) where

import Prelude hiding (div)

import Data.Record.Builder (merge)

import React.DOM (div) as R
import React.DOM.Props (className)
import React.Spaces (renderIn, text)

import React
     ( ReactClass
     , getProps, readState, createClass, spec'
     )

import Utils (storeConnect)
import App.Store (AppContext)


diagTreeEditorSlideEditorRender
  :: ReactClass
       { appContext          :: AppContext
       }

diagTreeEditorSlideEditorRender = createClass $ spec $
  \ { appContext } { foo } -> do

  text "TODO stuff"

  where
    name = "DiagTreeEditorSlideEditor"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className name]

    getInitialState this = do
      { appContext } <- getProps this

      pure { foo: true
           }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name }

      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderFn props state # renderIn wrapper


diagTreeEditorSlideEditor :: ReactClass { appContext :: AppContext }
diagTreeEditorSlideEditor = storeConnect f diagTreeEditorSlideEditorRender
  where
    f appState = let branch = appState.diagTree.editor in merge
      {
      }

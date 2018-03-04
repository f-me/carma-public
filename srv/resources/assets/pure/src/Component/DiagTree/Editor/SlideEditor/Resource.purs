module Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
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


diagTreeEditorSlideEditorResourceRender
  :: ReactClass
       { appContext :: AppContext
       }

diagTreeEditorSlideEditorResourceRender = createClass $ spec $
  \ { appContext } { } -> do

  text "TODO stuff"

  where
    name = "DiagTreeEditorSlideEditorResource"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className name]

    getInitialState this = do
      { appContext } <- getProps this
      pure { }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name }

      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderFn props state # renderIn wrapper


diagTreeEditorSlideEditorResource :: ReactClass { appContext :: AppContext }
diagTreeEditorSlideEditorResource =
  storeConnect f diagTreeEditorSlideEditorResourceRender
  where
    f appState = let branch = appState.diagTree.editor in merge
      {
      }

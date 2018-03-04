module Component.DiagTree.Editor.SlideEditor.Answer
     ( diagTreeEditorSlideEditorAnswer
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


diagTreeEditorSlideEditorAnswerRender
  :: ReactClass
       { appContext :: AppContext
       }

diagTreeEditorSlideEditorAnswerRender = createClass $ spec $
  \ { appContext } { } -> do

  text "TODO stuff"

  where
    name = "DiagTreeEditorSlideEditorAnswer"
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


diagTreeEditorSlideEditorAnswer :: ReactClass { appContext :: AppContext }
diagTreeEditorSlideEditorAnswer =
  storeConnect f diagTreeEditorSlideEditorAnswerRender
  where
    f appState = let branch = appState.diagTree.editor in merge
      {
      }

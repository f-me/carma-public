module Component.DiagTree.Editor.SlideEditor.Answer
     ( diagTreeEditorSlideEditorAnswer
     ) where

import Prelude hiding (div)

import React.DOM (div) as R
import React.DOM.Props (className)
import React.Spaces (renderIn, text)

import React
     ( ReactClass
     , getProps, readState, createClass, spec'
     )

import App.Store (AppContext)


type Props =
  { appContext :: AppContext
  }


diagTreeEditorSlideEditorAnswerRender :: ReactClass Props
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


diagTreeEditorSlideEditorAnswer :: ReactClass Props
diagTreeEditorSlideEditorAnswer = diagTreeEditorSlideEditorAnswerRender

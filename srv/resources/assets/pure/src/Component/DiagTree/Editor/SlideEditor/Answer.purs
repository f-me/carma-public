module Component.DiagTree.Editor.SlideEditor.Answer
     ( diagTreeEditorSlideEditorAnswer
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..))

import React.DOM (li) as R
import React.DOM.Props (className, src, role, title)
import React.Spaces ((!), (!.), renderIn, text, empty)
import React.Spaces.DOM (div, img, span, button, i, p, h4)

import React
     ( ReactClass
     , getProps, readState, createClass, spec'
     )

import Utils ((<.>))
import Utils.DiagTree.Editor (getDiagTreeSlideResourcePath)
import App.Store (AppContext)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideAnswer
     , DiagTreeSlideAttachment (..)
     )


type Props =
  { appContext :: AppContext
  , key        :: String
  , answer     :: DiagTreeSlideAnswer
  }


diagTreeEditorSlideEditorAnswerRender :: ReactClass Props
diagTreeEditorSlideEditorAnswerRender = createClass $ spec $
  \ { appContext, answer } { } -> do

  div !. "list-group-item" <.> classSfx "answer" $ do
    h4 !. "list-group-item-heading" $
      text answer.header

    p !. "list-group-item-text" $ do
      case answer.attachment of
           Nothing -> empty
           Just x  ->
             let f (Legacy y) = y
                 f (Modern y) = getDiagTreeSlideResourcePath y

              in img !. classSfx "image"
                     ! role "presentation"
                     ! src (f x)

      span $ text answer.text

  div !. "btn-toolbar" <.> classSfx "buttons" ! role "toolbar" $ do

    button !. "btn btn-success" ! title "Редактировать" $
      i !. "glyphicon glyphicon-pencil" $ empty

    button !. "btn btn-danger" ! title "Удалить" $
      i !. "glyphicon glyphicon-trash" $ empty

  where
    name = "DiagTreeEditorSlideEditorAnswer"
    classSfx s = name <> "--" <> s
    wrapper = R.li [className $ "list-group-item" <.> name]

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

module Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     ) where

import Prelude hiding (div)

import React.DOM (li) as R
import React.DOM.Props (className, role, src, alt, title)
import React.Spaces ((!), (!.), renderIn, text, empty)
import React.Spaces.DOM (div, img, span, button, i)

import React
     ( ReactClass
     , getProps, readState, createClass, spec'
     )

import Utils ((<.>))
import App.Store (AppContext)
import App.Store.DiagTree.Editor.Types (DiagTreeSlideResource)


type Props =
  { appContext :: AppContext
  , resource   :: DiagTreeSlideResource
  , key        :: String
  }


diagTreeEditorSlideEditorResourceRender :: ReactClass Props
diagTreeEditorSlideEditorResourceRender = createClass $ spec $
  \ { appContext, resource } { } -> do

  div !. "row" $ do

    div !. "col-md-7" $ do

      img ! role "presentation"
          ! alt resource.text
          ! src resource.file

      span $ text resource.text

    div !. "col-md-2" $
      div !. "btn-toolbar" <.> classSfx "buttons" ! role "toolbar" $ do

        button !. "btn btn-success" ! title "Редактировать" $
          i !. "glyphicon glyphicon-pencil" $ empty

        button !. "btn btn-danger" ! title "Удалить" $
          i !. "glyphicon glyphicon-trash" $ empty

  where
    name = "DiagTreeEditorSlideEditorResource"
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
          pure $ renderIn wrapper $ renderFn props state


diagTreeEditorSlideEditorResource :: ReactClass Props
diagTreeEditorSlideEditorResource = diagTreeEditorSlideEditorResourceRender

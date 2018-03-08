module Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     ) where

import Prelude hiding (div)

import React.DOM (li) as R
import React.DOM.Props (className, role, src, title)
import React.Spaces ((!), (!.), renderIn, text, empty)
import React.Spaces.DOM (div, img, span, button, i)

import React
     ( ReactClass
     , getProps, readState, createClass, spec'
     )

import Utils ((<.>))
import Utils.DiagTree.Editor (getDiagTreeSlideResourcePath)
import App.Store (AppContext)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideResource
     , DiagTreeSlideResourceAttachment (..)
     )


type Props =
  { appContext :: AppContext
  , key        :: String
  , resource   :: DiagTreeSlideResource
  }


diagTreeEditorSlideEditorResourceRender :: ReactClass Props
diagTreeEditorSlideEditorResourceRender = createClass $ spec $
  \ { appContext, resource } { } -> do

  case resource.attachment of
       Legacy _ -> div $ do
         span !. "label label-warning" $ text "Внимание"
         text " Картинка хранится в базе неэффективным образом,\
              \ рекомендуется загрузить её заново."

       _ -> empty

  let imagePath =
        case resource.attachment of
             Legacy x -> x
             Modern x -> getDiagTreeSlideResourcePath x.filename

  img !. classSfx "image"
      ! role "presentation"
      ! src imagePath

  span $ text resource.text

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

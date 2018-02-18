module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..))
import Data.Array (fromFoldable)
import Data.Record.Builder (merge)

import Control.Monad.Aff (launchAff_)

import React (ReactClass, getProps)
{-- import React.DOM.Props (onClick) --}
import React.DOM (IsDynamic (IsDynamic), mkDOM, div')
import React.Spaces.DOM (div, p, span, b, button, i)
import React.Spaces ((!.), (^), renderIn, elements, text, empty)

import Utils ((<.>), createClassStatelessWithSpec, storeConnect)
import Component.Spinner (spinner)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import App.Store.DiagTree.Editor.Types (DiagTreeSlides, DiagTreeSlideId)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (LoadSlidesRequest)
     )


diagTreeEditorRender
  :: ReactClass { isSlidesLoading           :: Boolean
                , isSlidesLoaded            :: Boolean
                , isSlidesLoadingFailed     :: Boolean
                , isParsingSlidesDataFailed :: Boolean
                , appContext                :: AppContext
                , slides                    :: DiagTreeSlides
                , selectedSlide             :: Maybe DiagTreeSlideId
                }

diagTreeEditorRender = f $ \props -> do
  div !. "col-md-4" <.> classSfx "tree-panel" $ do
    div !. "btn-toolbar" $ do
      button !. "btn btn-success" $ do
        i !. "glyphicon glyphicon-plus" $ empty
        text " Новое дерево"

  div !. "col-md-8" <.> classSfx "slide-editor-panel" $ do
    elements $ map (renderSlide props.selectedSlide) $ fromFoldable props.slides

  where
    name = "diag-tree-editor"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    f mainRender =
      spec $ \props -> renderIn wrapper $ do
        div !. "container" $
          div !. "row" $
            branching mainRender props

    renderSlide selectedSlide slide = renderIn div' $ do
      text $ "id: " <> show slide.id
      text $ " isRoot: " <> show slide.isRoot

      case selectedSlide of
           Just x | x == slide.id -> b $ text " SELECTED"
           _ -> empty

    branching mainRender props
      | props.isSlidesLoadingFailed = div $ do
          p $ do
            span !. "label label-danger" $ text "Ошибка"
            text if props.isParsingSlidesDataFailed
                    then " Произошла ошибка при обработке\
                         \ полученных от сервера данных"
                    else " Произошла ошибка при загрузке данных"

      | props.isSlidesLoading = do
          div !. "text-center" $
            spinner ^ { withLabel: true
                      , appContext: props.appContext
                      }

      | props.isSlidesLoaded = mainRender props

      | otherwise = div $ do
          p $ do
            span !. "label label-warning" $ text "Ожидание"
            text " Данные ещё не загружены…"

    spec = createClassStatelessWithSpec $ _
      { displayName = "DiagTreeEditor"

      , componentDidMount = \this -> do
          props <- getProps this

          if props.isSlidesLoaded || props.isSlidesLoading
             then pure unit
             else launchAff_
                $ dispatch props.appContext
                $ DiagTree $ Editor $ LoadSlidesRequest
      }


diagTreeEditor :: ReactClass { appContext :: AppContext }
diagTreeEditor = storeConnect f diagTreeEditorRender
  where
    f appState = merge $ let branch = appState.diagTree.editor in
      { isSlidesLoading           : branch.isSlidesLoading
      , isSlidesLoaded            : branch.isSlidesLoaded
      , isSlidesLoadingFailed     : branch.isSlidesLoadingFailed
      , isParsingSlidesDataFailed : branch.isParsingSlidesDataFailed
      , slides                    : branch.slides
      , selectedSlide             : branch.selectedSlide
      }

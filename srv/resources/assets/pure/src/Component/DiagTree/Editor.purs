module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..))
import Data.Array (fromFoldable)
import Data.Record.Builder (merge)

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Console (log)

import React (ReactClass, getProps, readState, createClass, spec')
import React.DOM.Props (onClick)
import React.DOM (IsDynamic (IsDynamic), mkDOM, div')
import React.Spaces.DOM (div, p, span, b, button, i)
import React.Spaces ((!), (!.), (^), renderIn, elements, text, empty)

import Utils ((<.>), storeConnect)
import Component.Spinner (spinner)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlides
     , DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     )

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (LoadSlidesRequest, SelectSlide)
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

diagTreeEditorRender = createClass $ spec $ \props state -> do

  div !. "col-md-4" <.> classSfx "tree-panel" $ do
    div !. "btn-toolbar" $ do
      button !. "btn btn-success" ! onClick state.newSlide $ do
        i !. "glyphicon glyphicon-plus" $ empty
        text " Новое дерево"

  div !. "col-md-8" <.> classSfx "slide-editor-panel" $ do
    elements
      $ map (renderSlide props.selectedSlide state.selectSlide)
      $ fromFoldable props.slides

  where
    name = "diag-tree-editor"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    renderFn mainRender props state =
      renderIn wrapper $ do
        div !. "container" $
          div !. "row" $
            branching mainRender props state

    renderSlide selectedSlide select (DiagTreeSlide slide) = renderIn div' $ do
      text $ "id: " <> show slide.id
      text $ " isRoot: " <> show slide.isRoot

      case selectedSlide of
           Just x | x == slide.id ->
             b $ text " SELECTED"
           _ -> do
             text " "
             button ! onClick (select slide.id) $ text "select"

    branching mainRender props state
      | props.isSlidesLoadingFailed = div $ do
          p $ do
            span !. "label label-danger" $ text "Ошибка"
            text if props.isParsingSlidesDataFailed
                    then " Произошла ошибка при обработке\
                         \ полученных от сервера данных"
                    else " Произошла ошибка при загрузке данных"

      | props.isSlidesLoading =
          div !. "text-center" $
            spinner ^ { withLabel  : true
                      , appContext : props.appContext
                      }

      | props.isSlidesLoaded = mainRender props state

      | otherwise = div $ do
          p $ do
            span !. "label label-warning" $ text "Ожидание"
            text " Данные ещё не загружены…"

    selectSlide appContext slideId _ =
      launchAff_ $ dispatch appContext $
        DiagTree $ Editor $ SelectSlide slideId

    newSlide appContext _ =
      log "TODO new slide"

    getInitialState this = do
      { appContext } <- getProps this

      -- Handlers with prebound `AppContext`
      pure { selectSlide : selectSlide appContext
           , newSlide    : newSlide appContext
           }

    spec mainRender =
      let
        renderWrap = renderFn mainRender
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderWrap props state
      in
        spec' getInitialState renderHandler # _
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

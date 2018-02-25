module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude hiding (div)

import Data.Record.Builder (merge)

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Console (log)

import React (ReactClass, getProps, readState, createClass, spec')
import React.DOM.Props (className, onClick)
import React.DOM (div) as R
import React.Spaces.DOM (div, p, span, button, i, ul, li, h5)
import React.Spaces ((!), (!.), (^), renderIn, text, empty)

import Utils ((<.>), storeConnect)
import Component.Spinner (spinner)
import Component.DiagTree.Editor.Tree (diagTreeEditorTree)
import Component.DiagTree.Editor.TreeSearch (diagTreeEditorTreeSearch)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (LoadSlidesRequest)
     )


diagTreeEditorRender
  :: ReactClass { isSlidesLoading           :: Boolean
                , isSlidesLoaded            :: Boolean
                , isSlidesLoadingFailed     :: Boolean
                , isParsingSlidesDataFailed :: Boolean
                , appContext                :: AppContext
                }

diagTreeEditorRender = createClass $ spec $ \ { appContext } { newSlide } -> do

  div !. "col-md-4" <.> classSfx "tree-panel" $ do

    div !. "btn-toolbar" $ do
      button !. "btn btn-success" ! onClick newSlide $ do
        i !. "glyphicon glyphicon-plus" $ empty
        text " Новое дерево"

    diagTreeEditorTreeSearch ^ { appContext }
    diagTreeEditorTree       ^ { appContext }

    -- A hint for a user
    div !. classSfx "tree-hints" $ do
      h5 $ text "Обозначения:"
      ul $ do
        li $ text "📂 — Раскрытая ветвь"
        li $ text "🏁 — Конец ветви (нет вложенных шагов)"

  div !. "col-md-8" <.> classSfx "slide-editor-panel" $ do
    text "TODO"

  where
    name = "DiagTreeEditor"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className name]

    renderFn mainRender props state =
      renderIn wrapper $ do
        div !. "container" $
          div !. "row" $
            branching mainRender props state

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

    newSlideHandler appContext _ =
      log "TODO new slide"

    getInitialState this = do
      { appContext } <- getProps this

      -- Handlers with prebound `AppContext`
      pure { newSlide : newSlideHandler appContext
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
          { displayName = name

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
      }

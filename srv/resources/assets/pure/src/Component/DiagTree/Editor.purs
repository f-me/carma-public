module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude hiding (div)

import Data.Record.Builder (merge)
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))

import Control.Monad.Aff (launchAff_)

import React
     ( ReactClass
     , getProps, readState, createClass, spec'
     , preventDefault
     )

import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM

import React.DOM.Props (className, onClick, disabled)
import React.DOM (div) as R
import React.Spaces.DOM (div, p, span, button, i, ul, li, h5)
import React.Spaces ((!), (!.), (^), renderIn, text, empty)

import Utils ((<.>), storeConnect)
import Utils.DiagTree.Editor (getSlideByBranch)
import Component.Spinner (spinner)
import Component.DiagTree.Editor.Tree (diagTreeEditorTree)
import Component.DiagTree.Editor.TreeSearch (diagTreeEditorTreeSearch)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import App.Store.DiagTree.Editor.Types (DiagTreeSlide (DiagTreeSlide))

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (LoadSlidesRequest, NewSlideRequest)
     )


diagTreeEditorRender
  :: ReactClass { appContext                :: AppContext
                , isSlidesLoading           :: Boolean
                , isSlidesLoaded            :: Boolean
                , isSlidesLoadingFailed     :: Boolean
                , isParsingSlidesDataFailed :: Boolean
                , isSlideDeletingFailed     :: Boolean
                , slideDeletingFailureSfx   :: Maybe String
                , isNewSlideFailed          :: Boolean
                , isProcessing              :: Boolean
                }

diagTreeEditorRender = createClass $ spec $
  \ { appContext
    , isSlideDeletingFailed
    , slideDeletingFailureSfx
    , isNewSlideFailed
    , isProcessing
    }
    { newSlide, processingSpinnerProps } -> do

  div !. "col-md-4" <.> classSfx "tree-panel" $ do

    div !. "btn-toolbar" $ do
      button !. "btn btn-success"
             ! disabled isProcessing
             ! onClick newSlide $ do

        i !. "glyphicon glyphicon-plus" $ empty
        text " Новое дерево"

    diagTreeEditorTreeSearch ^ { appContext, isDisabled: isProcessing }

    if isProcessing
       then div !. classSfx "processing" $ spinner ^ processingSpinnerProps
       else diagTreeEditorTree ^ { appContext }

    -- A hint for a user
    div !. classSfx "tree-hints" $ do
      h5 $ text "Обозначения:"
      ul $ do
        li $ text "📂 — Раскрытая ветвь"
        li $ text "🏁 — Конец ветви (нет вложенных шагов)"

  div !. "col-md-8" <.> classSfx "slide-editor-panel" $ do

    if not isSlideDeletingFailed
       then pure unit
       else p $ do span !. "label label-danger" $ text "Ошибка"

                   let sfx = slideDeletingFailureSfx
                       msg = " Произошла ошибка при попытке удалить ветвь"

                   case sfx <#> text of
                        Nothing -> text $ msg <> "."
                        Just x  -> text msg *> i x *> text "."

                   -- TODO remove after implementation
                   i $ text " Удаление ветви временно недоступно…"

    if not isNewSlideFailed
       then pure unit
       else p $ do span !. "label label-danger" $ text "Ошибка"
                   text " Произошла ошибка при попытке создать новое дерево."
                   -- TODO remove after implementation
                   i $ text " Создание нового дерева временно недоступно…"

    if isProcessing
       then div !. classSfx "processing" $ spinner ^ processingSpinnerProps
       else i $ text "Временно недоступно…"

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
            spinner ^ { withLabel  : Left true
                      , appContext : props.appContext
                      }

      | props.isSlidesLoaded = mainRender props state

      | otherwise = div $ do
          p $ do
            span !. "label label-warning" $ text "Ожидание"
            text " Данные ещё не загружены…"

    newSlideHandler appContext this event = do
      preventDefault event
      { isProcessing } <- getProps this

      if isProcessing
         then pure unit
         else do
           wnd    <- DOM.window
           create <- DOM.confirm "Подтвердите создание нового дерева" wnd

           if not create
              then pure unit
              else launchAff_
                 $ dispatch appContext $ DiagTree $ Editor NewSlideRequest

    getInitialState this = do
      { appContext } <- getProps this

      -- Handlers with prebound `AppContext`
      pure { newSlide: newSlideHandler appContext this

           , processingSpinnerProps:
               { withLabel: Right "Обработка…", appContext }
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
                    $ DiagTree $ Editor LoadSlidesRequest
          }


diagTreeEditor :: ReactClass { appContext :: AppContext }
diagTreeEditor = storeConnect f diagTreeEditorRender
  where
    f appState = merge $ let branch = appState.diagTree.editor in
      { isSlidesLoading           : branch.isSlidesLoading
      , isSlidesLoaded            : branch.isSlidesLoaded
      , isSlidesLoadingFailed     : branch.isSlidesLoadingFailed
      , isParsingSlidesDataFailed : branch.isParsingSlidesDataFailed
      , isNewSlideFailed          : branch.newSlide.isFailed
      , isProcessing              : branch.slideDeleting.isProcessing
                                      || branch.newSlide.isProcessing

      , isSlideDeletingFailed     : branch.slideDeleting.isFailed
      , slideDeletingFailureSfx   : getSlideDeletingFailureSfx branch
      }

    getSlideDeletingFailureSfx branch = do
      deletingBranch <- branch.slideDeleting.branch
      getSlideByBranch branch.slides deletingBranch <#> slideSfx

    slideSfx (DiagTreeSlide x) =
      " #" <> show x.id <> " (\"" <> x.header <> "\")"

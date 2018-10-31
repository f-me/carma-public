module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude hiding (div)

import Data.Record.Builder (merge)
import Data.Either (Either (..))
import Data.Maybe (Maybe (..), fromJust, fromMaybe)

import Control.Monad.Aff (launchAff_)

import React
     ( ReactClass
     , getProps, readState, createClass, createElement, spec'
     , preventDefault
     )

import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM

import React.DOM.Props (className, onClick, disabled, title)
import React.DOM (div) as R
import React.Spaces.DOM (div, p, span, button, i, ul, li, h5)
import React.Spaces ((!), (!.), renderIn, element, text, empty)

import Utils ((<.>), storeConnect)
import Utils.CopyPasteBuffer
     ( CopyPasteBufferState (..)
     , CopyPasteBuffer
     , getCopyPasteState
     )
import Utils.DiagTree.Editor (getSlideByBranch)
import Component.Generic.Spinner (spinner)
import Component.DiagTree.Editor.Tree (diagTreeEditorTree)
import Component.DiagTree.Editor.TreeSearch (diagTreeEditorTreeSearch)
import Component.DiagTree.Editor.SlideEditor (diagTreeEditorSlideEditor)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlides
     )
import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction ( LoadSlidesRequest
                            , NewSlideRequest
                            , PasteSlideRequest)
     )
import Partial.Unsafe (unsafePartial)


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
                , isPasteFailed             :: Boolean
                , copyPasteBuffer           :: CopyPasteBuffer
                , slides                    :: DiagTreeSlides
                }

diagTreeEditorRender = createClass $ spec $
  \ { appContext
    , isSlideDeletingFailed
    , slideDeletingFailureSfx
    , isNewSlideFailed
    , isProcessing
    , isPasteFailed
    , copyPasteBuffer
    }
    { newSlide, pasteSlide, processingSpinnerProps } -> do

  div !. "col-md-4" <.> classSfx "tree-panel" $ do

    div !. "btn-toolbar" $ do
      button !. "btn btn-success"
             ! disabled isProcessing
             ! onClick newSlide $ do

        i !. "glyphicon glyphicon-plus" $ empty
        text " Новое дерево"

      button !. "btn" <.> classSfx "paste"
             ! onClick pasteSlide
             ! disabled (getCopyPasteState copyPasteBuffer == EmptyBuffer)
             ! title "Вставить ветвь" $

        i !. "glyphicon" <.> "glyphicon-paste" $ empty

    element $ treeSearchEl { appContext, isDisabled: isProcessing } []

    if isProcessing
       then div !. classSfx "processing" $ element $
              spinnerEl processingSpinnerProps []

       else element $ treeEl { appContext } []

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

    if not isNewSlideFailed
       then pure unit
       else p $ do span !. "label label-danger" $ text "Ошибка"
                   text " Произошла ошибка при попытке создать новое дерево."

    if isProcessing
       then div !. classSfx "processing" $ element $
              spinnerEl processingSpinnerProps []

       else element $ slideEditorEl { appContext } []

  where
    name = "DiagTreeEditor"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className name]

    spinnerEl     = createElement spinner
    treeSearchEl  = createElement diagTreeEditorTreeSearch
    treeEl        = createElement diagTreeEditorTree
    slideEditorEl = createElement diagTreeEditorSlideEditor

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
          div !. "text-center" $ element $
            spinnerEl
              { withLabel  : Left true
              , appContext : props.appContext
              } []

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

    pasteSlideHandler appContext this event = do
      preventDefault event
      { isProcessing } <- getProps this
      getSlide <- getProps this <#> _.slides <#> getSlideByBranch
      copyPasteBuffer <- getProps this <#> _.copyPasteBuffer

      if isProcessing
         then pure unit
         else do
           paste <-
             let source = fromMaybe "" $
                   getSlide (unsafePartial $
                             fromJust copyPasteBuffer.branch) <#>
                   \(DiagTreeSlide x) -> " #" <> show x.id <> " (\"" <>
                                         x.header <> "\")"
                 operation = if copyPasteBuffer.cutting
                                then "переместить"
                                else "скопировать"
                 msg = "Вы уверены, что хотите" <.> operation <.> source
                       <.> "в корень?"
             in DOM.window >>= DOM.confirm msg

           if paste
              then launchAff_ $ dispatch appContext $ DiagTree $
                     Editor $ PasteSlideRequest []
              else pure unit

    getInitialState this = do
      { appContext } <- getProps this

      -- Handlers with prebound `AppContext`
      pure { newSlide: newSlideHandler appContext this
           , pasteSlide: pasteSlideHandler appContext this
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
      , isPasteFailed             : branch.copyPasteBuffer.isFailed
      , isProcessing              : branch.slideDeleting.isProcessing
                                      || branch.newSlide.isProcessing
                                      || branch.copyPasteBuffer.isProcessing

      , isSlideDeletingFailed     : branch.slideDeleting.isFailed
      , slideDeletingFailureSfx   : getSlideDeletingFailureSfx branch
      , copyPasteBuffer           : branch.copyPasteBuffer
      , slides                    : branch.slides
      }

    getSlideDeletingFailureSfx branch = do
      deletingBranch <- branch.slideDeleting.branch
      getSlideByBranch branch.slides deletingBranch <#> slideSfx

    slideSfx (DiagTreeSlide x) =
      " #" <> show x.id <> " (\"" <> x.header <> "\")"

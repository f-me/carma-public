module Component.DiagTree.Editor.SlideEditor
     ( diagTreeEditorSlideEditor
     ) where

import Prelude hiding (div)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)

import Data.Record.Builder (merge)
import Data.Maybe (Maybe (..))

import React.DOM (form) as R
import React.Spaces ((!), (!.), renderIn, text)
import React.Spaces.DOM (div, input, button, textarea)

import React.DOM.Props
     ( className, _type, placeholder, value, disabled
     , onChange, onClick
     )

import React
     ( ReactClass
     , createClass, spec'
     , getProps, readState, transformState
     , preventDefault
     )

import Utils ((<.>), storeConnect, toMaybeT, eventInputValue)

import Utils.DiagTree.Editor
     ( getSlideByBranch
     , eqDiagTreeSlideResources
     , eqDiagTreeSlideActions
     , eqIshDiagTreeSlideAnswers
     )

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Types (DiagTreeSlide (DiagTreeSlide))

import Component.DiagTree.Editor.SlideEditor.Answer
     ( diagTreeEditorSlideEditorAnswer
     )

import Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     )


diagTreeEditorSlideEditorRender
  :: ReactClass
       { appContext   :: AppContext
       , slide        :: Maybe DiagTreeSlide
       , isProcessing :: Boolean
       }

diagTreeEditorSlideEditorRender = createClass $ spec $
  \ { appContext, isProcessing }
    { slide: (DiagTreeSlide slide)
    , isChanged
    , onChangeHeader
    , onChangeBody
    , onCancel
    , onSave
    } -> do

  div !. "form-group" $
    input !. "form-control" <.> classSfx "header"
          ! _type "text"
          ! placeholder "Заголовок"
          ! value slide.header
          ! onChange onChangeHeader

  div !. "form-group" $
    -- TODO write bindings to RichTextEditor (react-rte) and use it here
    textarea !. "form-control"
             ! placeholder "Описание"
             ! value slide.body
             ! onChange onChangeBody

  div !. "btn-toolbar" $ do

    let isBlocked = isProcessing || not isChanged

    button !. "btn btn-default"
           ! _type "button"
           ! disabled isBlocked
           ! onClick onCancel
           $ text "Отменить изменения"

    button !. "btn btn-success"
           ! _type "button"
           ! disabled isBlocked
           ! onClick onSave
           $ text "Сохранить"

    -- TODO save message here

  where
    name = "DiagTreeEditorSlideEditor"
    classSfx s = name <> "--" <> s
    wrapper = R.form [className name]
    renderer = renderIn wrapper

    changeHeaderHandler this event = do
      let x = eventInputValue event

      transformState this $
        \s -> s { slide = s.slide <#> headerUpdater x, isChanged = true }

      where headerUpdater x (DiagTreeSlide s) = DiagTreeSlide $ s { header = x }

    changeBodyHandler this event = do
      let x = eventInputValue event

      transformState this $
        \s -> s { slide = s.slide <#> bodyUpdater x, isChanged = true }

      where bodyUpdater x (DiagTreeSlide s) = DiagTreeSlide $ s { body = x }

    cancelHandler this event = do
      { slide } <- getProps this
      resetChanges this slide
      preventDefault event

    saveHandler this event = do
      -- TODO
      preventDefault event

    resetChanges this slide =
      transformState this _ { slide = slide, isChanged = false }

    fetchSlide this state props = do
      (DiagTreeSlide prevSlide) <- toMaybeT state.slide
      (DiagTreeSlide nextSlide) <- toMaybeT props.slide

      if nextSlide.header == prevSlide.header &&
         nextSlide.body   == prevSlide.body &&

         eqDiagTreeSlideResources nextSlide.resources
                                  prevSlide.resources &&

         eqDiagTreeSlideActions nextSlide.actions
                                prevSlide.actions &&

         eqIshDiagTreeSlideAnswers nextSlide.answers
                                   prevSlide.answers

         then toMaybeT $ pure unit
         else liftEff $ resetChanges this props.slide

    getInitialState this = do
      { slide } <- getProps this

      pure { slide
           , isChanged      : false
           , onChangeHeader : changeHeaderHandler this
           , onChangeBody   : changeBodyHandler   this
           , onCancel       : cancelHandler       this
           , onSave         : saveHandler         this
           }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillReceiveProps = \this nextProps -> do
            state <- readState this
            void $ runMaybeT $ fetchSlide this state nextProps
        }

      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this

          pure $ renderer $
            case state.slide of
                 Nothing -> text "…"
                 Just x  -> renderFn props state { slide = x }


diagTreeEditorSlideEditor :: ReactClass { appContext :: AppContext }
diagTreeEditorSlideEditor = storeConnect f diagTreeEditorSlideEditorRender
  where
    f appState = let branch = appState.diagTree.editor in merge
      { slide:
          branch.selectedSlideBranch >>= getSlideByBranch branch.slides

      , isProcessing: false
      }

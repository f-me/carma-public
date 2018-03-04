module Component.DiagTree.Editor.SlideEditor
     ( diagTreeEditorSlideEditor
     ) where

import Prelude hiding (div)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)

import Data.Record.Builder (merge)
import Data.Maybe (Maybe (..))

import React.DOM (form) as R
import React.DOM.Props (className, _type, placeholder, value, onChange)
import React.Spaces ((!), (!.), renderIn, text)
import React.Spaces.DOM (div, input)

import React
     ( ReactClass
     , createClass, spec'
     , getProps, readState, transformState
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
       { appContext :: AppContext
       , slide      :: Maybe DiagTreeSlide
       }

diagTreeEditorSlideEditorRender = createClass $ spec $
  \ { appContext } { slide: (DiagTreeSlide slide), onChangeHeader } -> do

  div !. "form-group" $
    input !. "form-control" <.> classSfx "header"
          ! _type "text"
          ! placeholder "Заголовок"
          ! value slide.header
          ! onChange onChangeHeader

  where
    name = "DiagTreeEditorSlideEditor"
    classSfx s = name <> "--" <> s
    wrapper = R.form [className name]
    renderer = renderIn wrapper

    changeHeaderHandler this event = do
      let x = eventInputValue event
      transformState this $ \s -> s { slide = s.slide <#> headerUpdater x }
      where headerUpdater x (DiagTreeSlide s) = DiagTreeSlide $ s { header = x }

    getInitialState this = do
      { slide } <- getProps this

      pure { slide
           , onChangeHeader: changeHeaderHandler this
           }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillReceiveProps = \this nextProps -> do
            state <- readState this

            void $ runMaybeT $ do
              (DiagTreeSlide prevSlide) <- toMaybeT state.slide
              (DiagTreeSlide nextSlide) <- toMaybeT nextProps.slide

              if nextSlide.header == prevSlide.header &&
                 nextSlide.body   == prevSlide.body &&

                 eqDiagTreeSlideResources nextSlide.resources
                                          prevSlide.resources &&

                 eqDiagTreeSlideActions nextSlide.actions
                                        prevSlide.actions &&

                 eqIshDiagTreeSlideAnswers nextSlide.answers
                                           prevSlide.answers

                 then toMaybeT $ pure unit
                 else liftEff $ transformState this
                              $ _ { slide = nextProps.slide }
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
      }

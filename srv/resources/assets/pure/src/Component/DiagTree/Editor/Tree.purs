module Component.DiagTree.Editor.Tree
     ( diagTreeEditorTree
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), isJust)
import Data.Record.Builder (merge)
import Data.Array (fromFoldable, elemIndex, snoc, last)

import Control.Monad.Aff (launchAff_)

import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.DOM (div) as R
import React.DOM.Props (className, onClick)
import React.Spaces ((!), (!.), renderIn, text, elements, empty)
import React.Spaces.DOM (div)

import React
     ( ReactClass
     , getProps, readState, createClass, spec'
     , preventDefault, stopPropagation
     )

import Utils ((<.>), storeConnect)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (SelectSlide)
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlides
     , DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     )


diagTreeEditorTreeRender
  :: ReactClass { appContext          :: AppContext
                , slides              :: DiagTreeSlides
                , selectedSlideBranch :: Maybe (Array DiagTreeSlideId)
                }

diagTreeEditorTreeRender = createClass $ spec $
  \ { slides, selectedSlideBranch } { selectSlide } -> do

    elements
      $ map (renderItem selectedSlideBranch Nothing selectSlide [])
      $ fromFoldable slides

  where
    name = "diag-tree-editor-tree"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    renderItem selectedSlideBranch
               question
               select
               parents
               (DiagTreeSlide slide) =

      renderIn w $ do
        div !. classSfx "header" ! onClick (select slideBranch) $ do

          case question of
               Just x  -> div !. classSfx "answer" $ text x
               Nothing -> empty

          div !. classSfx "question" $ text slide.header

        div !. classSfx "children" $ do
          elements
            $ map childRender
            $ fromFoldable slide.answers

      where w = R.div [className wClass]
            slideBranch = parents `snoc` slide.id

            childRender { header, nextSlide } =
              renderItem selectedSlideBranch
                         (Just header)
                         select
                         slideBranch
                         nextSlide

            wClass = classSfx "item" #
              case selectedSlideBranch of
                   Just x | last x == Just slide.id ->
                              (_ <.> classSfx "item--selected")
                          | isJust $ slide.id `elemIndex` x ->
                              (_ <.> classSfx "item--parent-selected")
                   _ -> id

    selectSlideHandler appContext slideBranch event = do
      preventDefault  event
      stopPropagation event

      launchAff_ $ dispatch appContext $
        DiagTree $ Editor $ SelectSlide slideBranch

    getInitialState this = do
      { appContext } <- getProps this

      pure { selectSlide : selectSlideHandler appContext
           }

    spec renderFn =
      let
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderFn props state # renderIn wrapper
      in
        spec' getInitialState renderHandler # _
          { displayName = "DiagTreeEditorTree"
          }


diagTreeEditorTree :: ReactClass { appContext :: AppContext }
diagTreeEditorTree = storeConnect f diagTreeEditorTreeRender
  where
    f appState =
      let { slides, selectedSlideBranch } = appState.diagTree.editor
       in merge { slides, selectedSlideBranch }

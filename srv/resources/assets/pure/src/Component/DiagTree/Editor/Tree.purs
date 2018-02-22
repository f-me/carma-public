module Component.DiagTree.Editor.Tree
     ( diagTreeEditorTree
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), isJust, isNothing)
import Data.Record.Builder (merge)
import Data.Array (fromFoldable, elemIndex, snoc, last, delete)

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Console (log)
import Control.Lazy (fix)

import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.DOM (div) as R
import React.DOM.Props (className, onClick, title)
import React.Spaces ((!), (!.), renderIn, text, elements, empty)
import React.Spaces.DOM (div, button, i)

import React
     ( ReactClass
     , getProps, readState, transformState, createClass, spec'
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
  \ { slides, selectedSlideBranch }
    { selectSlide, deleteSlide, unfoldedSlides } -> do

    let renderItemFn =
          renderItem selectedSlideBranch unfoldedSlides
                     selectSlide deleteSlide
                     Nothing []

    elements $ map renderItemFn $ fromFoldable slides

  where
    name = "diag-tree-editor-tree"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    renderItem selectedSlideBranch unfoldedSlides select deleteSlide = fix $
      \again question parents (DiagTreeSlide slide) ->
        let
          slideBranch = parents `snoc` slide.id

          childRender { header, nextSlide } =
            again (Just header) slideBranch nextSlide

          wClass = classSfx "item" #
            case selectedSlideBranch of
                 Just x | last x == Just slide.id ->
                            (_ <.> classSfx "item--selected")
                        | isJust $ slide.id `elemIndex` x ->
                            (_ <.> classSfx "item--parent-selected")
                 _ -> id
        in
          renderIn (R.div [className wClass]) $ do
            div !. classSfx "header" ! onClick (select slideBranch) $ do

              button !. "btn" <.> "btn-danger" <.> classSfx "delete"
                     ! onClick (deleteSlide slideBranch)
                     ! title "Удалить ветвь" $

                i !. "glyphicon" <.> "glyphicon-trash" $ empty

              case question of
                   Just x  -> div !. classSfx "answer" $ text x
                   Nothing -> empty

              div !. classSfx "question" $ text slide.header

            if isNothing $ slide.id `elemIndex` unfoldedSlides
               then empty
               else div !. classSfx "children" $
                      elements
                        $ map childRender
                        $ fromFoldable slide.answers

    selectSlideHandler appContext toggleSlideFold slideBranch event = do
      preventDefault  event
      stopPropagation event

      case last slideBranch of
           Nothing -> pure unit
           Just x  -> do
             toggleSlideFold x
             launchAff_ $ dispatch appContext $
               DiagTree $ Editor $ SelectSlide slideBranch

    deleteSlideHandler slideBranch event = do
      preventDefault  event
      stopPropagation event

      log $ "TODO delete slide: " <> show slideBranch

    toggleSlideFoldHandler this slideId =
      transformState this $ \s@{ unfoldedSlides } ->
        if isJust $ slideId `elemIndex` unfoldedSlides
           then s { unfoldedSlides = slideId `delete` unfoldedSlides }
           else s { unfoldedSlides = unfoldedSlides `snoc` slideId }

    getInitialState this = do
      { appContext } <- getProps this
      let toggleSlideFold = toggleSlideFoldHandler this

      pure { selectSlide     : selectSlideHandler appContext toggleSlideFold
           , unfoldedSlides  : ([] :: Array DiagTreeSlideId)
           , deleteSlide     : deleteSlideHandler
           , toggleSlideFold
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

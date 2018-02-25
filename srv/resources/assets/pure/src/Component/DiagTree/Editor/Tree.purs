module Component.DiagTree.Editor.Tree
     ( diagTreeEditorTree
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), isJust)
import Data.Record.Builder (merge, build)
import Data.Array ((!!), last, head, take, init, length)
import Data.String.NonEmpty (toString)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Foldable (foldr)
import Data.Tuple (Tuple (Tuple))

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Console (log)

import React.DOM (div) as R
import React.DOM.Props (className, onClick, onChange, _type, checked)
import React.Spaces ((!), (!.), renderIn, text, elements, empty)
import React.Spaces.DOM (div, button, i, label, input)
import React.Spaces.DOM (div) as SDyn

import React
     ( ReactClass
     , getProps, readState, transformState, createClass, spec'
     , preventDefault
     , createElement
     )

import Utils ((<.>), storeConnect, eventIsChecked)
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

import Component.DiagTree.Editor.Tree.Item (diagTreeEditorTreeItem)


-- If selected slides has more parents they will be hidden.
-- Two button also will be shown:
--   * Go one level up (select parent slide)
--   * Select root slide of current branch
-- This actually means N selected items of a selected branch,
-- You could actually see (N+1) levels
-- when selected slide have children and it is unfolded.
maxTreeDepth :: Int
maxTreeDepth = 3


diagTreeEditorTreeRender
  :: ReactClass
       { appContext          :: AppContext
       , slides              :: DiagTreeSlides
       , selectedSlideBranch :: Maybe (Array DiagTreeSlideId)

       , search
           :: Maybe { query    :: String
                    , parents  :: Set DiagTreeSlideId

                    , patterns :: Map DiagTreeSlideId
                                      { answer   :: Maybe Int
                                      , question :: Maybe Int
                                      }
                    }
       }

diagTreeEditorTreeRender = createClass $ spec $
  \ { appContext, slides, selectedSlideBranch, search }
    { selectSlide, deleteSlide, unfoldedSlides
    , shiftedSlidesMenu, dontShiftLevels, changeDontShiftLevels
    } -> do

    let shifted = do
          if dontShiftLevels then Nothing else pure unit
          branch     <- selectedSlideBranch
          firstId    <- head branch
          firstSlide <- firstId `Map.lookup` slides
          shiftSlideBranch branch 0 Nothing firstSlide

    div !. "checkbox" <.> classSfx "dont-shift-levels" $ do
      label $ do
        input ! _type "checkbox"
              ! checked dontShiftLevels
              ! onChange changeDontShiftLevels

        text " Не сокращать вложенность"

    case shifted <#> _.parents >>> length of
         Nothing -> empty
         Just n  -> shiftedSlidesMenu n

    SDyn.div !. classSfx "list" $ do

      let itemProps =
            { appContext
            , selectedSlide: selectedSlideBranch, unfoldedSlides, search
            , select: selectSlide, delete: deleteSlide
            }

          Tuple slidesList itemPropsBuilder =
            case shifted of
                 Nothing ->
                   Tuple slides $ \slide ->
                     merge { answerHeader: Nothing, parents: [], slide }

                 Just x@{ slide: slide@(DiagTreeSlide { id: slideId }) } ->
                   Tuple (Map.singleton slideId slide) $ \slide ->
                     merge { answerHeader: x.answer, parents: x.parents, slide }

          itemRender x = createElement diagTreeEditorTreeItem p []
            where p = build (itemPropsBuilder x) itemProps

      elements $ map itemRender slidesList

  where
    name = "DiagTreeEditorTree"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className name]

    shiftedSlidesMenuFn selectRoot selectOneLevelUp levelsHidden = do

      div !. classSfx "folded-parents-menu" $
        div !. "btn-group" $ do

          div !. "btn-group" $
            button !. "btn btn-info btn-sm" ! onClick selectOneLevelUp $
              text "На уровень выше"

          div !. "btn-group" $
            button !. "btn btn-warning btn-sm" ! onClick selectRoot $
              text "К корню ветви"

      div !. classSfx "more-elems" $
        i $ text $ "… (скрыто верхних уровней: " <> show levelsHidden <> ") …"

    selectSlideHandler appContext this =
      \toggleSlideFold unfoldSlideBranch slideBranch -> do
      isSearching <- getProps this <#> _.search <#> isJust

      case last slideBranch of
           Nothing -> pure unit
           Just x  -> do
             if isSearching
                then unfoldSlideBranch slideBranch
                else toggleSlideFold x

             launchAff_ $ dispatch appContext $
               DiagTree $ Editor $ SelectSlide slideBranch

    deleteSlideHandler slideBranch =
      log $ "TODO delete slide: " <> show slideBranch

    toggleSlideFoldHandler this slideId =
      transformState this $ \s@{ unfoldedSlides } ->
        let f = if slideId `Set.member` unfoldedSlides
                   then Set.delete
                   else Set.insert
         in s { unfoldedSlides = slideId `f` unfoldedSlides }

    -- When search mode is on slide could be selected, if search is reset after
    -- we keep whole branch unfolded (to prevent it from disapearing from user).
    unfoldSlideBranchHandler this slideBranch =
      transformState this $ \s@{ unfoldedSlides } ->
        s { unfoldedSlides = _ } $
          foldr Set.insert unfoldedSlides slideBranch

    selectOneLevelUpHandler appContext this event = do
      preventDefault event
      { selectedSlideBranch } <- getProps this

      case selectedSlideBranch >>= init of
           Nothing -> pure unit
           Just [] -> pure unit
           Just x  ->
             launchAff_ $ dispatch appContext $
               DiagTree $ Editor $ SelectSlide x

    selectRootHandler appContext this event = do
      preventDefault event
      { selectedSlideBranch } <- getProps this

      case selectedSlideBranch >>= head of
           Nothing -> pure unit
           Just x  ->
             launchAff_ $ dispatch appContext $
               DiagTree $ Editor $ SelectSlide [x]

    -- Reduce visible levels of slide path
    -- (some parents will be hidden).
    shiftSlideBranch branch n answer slide@(DiagTreeSlide { answers })
      | (length branch - n) <= maxTreeDepth =
          if n == 0
             then Nothing
             else Just { parents: take n branch, answer, slide }
      | otherwise = do
          let nextN = n + 1
          nextSlideId <- branch !! nextN
          { header, nextSlide } <- nextSlideId `Map.lookup` answers
          shiftSlideBranch branch nextN (Just header) nextSlide

    changeDontShiftLevelsHandler this event = do
      let isChecked = eventIsChecked event
      transformState this _ { dontShiftLevels = isChecked }

    getInitialState this = do
      { appContext } <- getProps this

      let toggleSlideFold   = toggleSlideFoldHandler   this
          unfoldSlideBranch = unfoldSlideBranchHandler this

          selectOneLevelUp  = selectOneLevelUpHandler appContext this
          selectRoot        = selectRootHandler       appContext this

          changeDontShiftLevels = changeDontShiftLevelsHandler this

          selectSlide =
            selectSlideHandler appContext this
                               toggleSlideFold unfoldSlideBranch

      pure { selectSlide
           , unfoldedSlides    : (Set.empty :: Set DiagTreeSlideId)
           , deleteSlide       : deleteSlideHandler
           , shiftedSlidesMenu : shiftedSlidesMenuFn selectRoot selectOneLevelUp
           , dontShiftLevels   : false
           , changeDontShiftLevels
           }

    spec renderFn =
      let
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderFn props state # renderIn wrapper
      in
        spec' getInitialState renderHandler # _ { displayName = name }


diagTreeEditorTree :: ReactClass { appContext :: AppContext }
diagTreeEditorTree = storeConnect f diagTreeEditorTreeRender
  where
    f appState = let branch = appState.diagTree.editor in merge
      { slides              : branch.slides
      , selectedSlideBranch : branch.selectedSlideBranch

      , search:
          branch.foundSlides
            >>= \ { matchedParents: parents, matchedPatterns: patterns } -> do
                  query <- branch.treeSearch.searchQuery <#> toString
                  pure { query, parents, patterns }
      }

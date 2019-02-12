module Component.DiagTree.Editor.Tree
     ( diagTreeEditorTree
     ) where

import Prelude hiding (div)

import Data.Monoid (mempty)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe (..), isJust, fromMaybe, maybe)
import Data.Record.Builder (merge, build)
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty (toString)
import Data.Tuple (Tuple (Tuple))

import Data.Array
     ( (!!), index, last, head, take, init, length, snoc, fromFoldable
     )

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.MonadZero (guard)

import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM

import React
     ( ReactClass
     , getProps
     , readState
     , transformState
     , createClass
     , spec'
     , preventDefault
     , createElement
     , handle
     )

import React.DOM (text, div, button, i', label', input)
import React.DOM.Dynamic (div) as RDyn
import React.DOM.Props (className, onClick, onChange, _type, checked)

import Utils (eventIsChecked, storeConnect, toMaybeT, (<.>))
import Utils.DiagTree.Editor (getSlideByBranch)

import Utils.CopyPasteBuffer
     ( CopyPasteBuffer
     , CopyPasteBufferState
     , getCopyPasteState
     )

import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (..))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction (..))

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlides
     , DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     , fromIndexedAnswers
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

       , copyPasteState      :: CopyPasteBufferState
       , copyPasteBuffer     :: CopyPasteBuffer
       }

diagTreeEditorTreeRender = createClass $ spec $
  \ -- props
    { appContext, slides, selectedSlideBranch, search
    , copyPasteState, copyPasteBuffer
    }
    -- state
    { selectSlide, deleteSlide, copySlide, cutSlide, pasteSlide, unfoldedSlides
    , shiftedSlidesMenu, dontShiftLevels, changeDontShiftLevels
    } ->

  [ div [className $ "checkbox" <.> classSfx "dont-shift-levels"] $ pure $
      label'
        [ flip input mempty
            [ _type "checkbox"
            , checked dontShiftLevels
            , onChange changeDontShiftLevels
            ]

        , text " Не сокращать вложенность"
        ]
  ]
  <>
  let
    shifted = do
      guard $ not dontShiftLevels
      branch     <- selectedSlideBranch
      firstId    <- head branch
      firstSlide <- firstId `Map.lookup` slides
      shiftSlideBranch branch 0 Nothing firstSlide

    shiftedSlidesMenuControlButtons =
      case shifted <#> _.parents >>> length of
           Nothing -> mempty
           Just n  -> shiftedSlidesMenu n

    itemProps =
      { appContext
      , selectedSlide: selectedSlideBranch, unfoldedSlides, search
      , select: selectSlide, delete: deleteSlide
      , copy: copySlide
      , cut: cutSlide
      , paste: pasteSlide
      , copyPasteState
      , copyPasteBuffer
      }

    Tuple slidesList itemPropsBuilder =
      case shifted of
           Nothing ->
             Tuple slides $ \slide@(DiagTreeSlide x) ->
               merge { answerHeader: Nothing
                     , parents: []
                     , key: show x.id
                     , slide
                     }

           Just x@{ slide: slide@(DiagTreeSlide { id: slideId }) } ->
             Tuple (Map.singleton slideId slide) $ \slide ->
               merge { answerHeader: x.answer
                     , parents: x.parents
                     , key: show slideId
                     , slide
                     }

    itemRender x = itemEl p mempty where
      p = itemPropsBuilder x `build` itemProps

    itemsListEl
      = RDyn.div [className $ classSfx "list"]
      $ fromFoldable
      $ map itemRender slidesList
  in
    shiftedSlidesMenuControlButtons `snoc` itemsListEl

  where
    name = "DiagTreeEditorTree"
    classSfx s = name <> "--" <> s
    wrapper = div [className name]
    itemEl = createElement diagTreeEditorTreeItem

    shiftedSlidesMenuFn selectRoot selectOneLevelUp levelsHidden =
      [ div [className $ classSfx "folded-parents-menu"] $ pure $
          div
            [ className $ "btn-group" ]
            [ div [className "btn-group"] $ pure $
                button
                  [ className "btn btn-info btn-sm"
                  , onClick selectOneLevelUp
                  ]
                  [ text "На уровень выше" ]

            , div [className "btn-group"] $ pure $
                button
                  [ className "btn btn-warning btn-sm"
                  , onClick selectRoot
                  ]
                  [ text "К корню ветви" ]
            ]

      , div [className $ classSfx "more-elems"] $ pure $
          i' $ pure $ text $
            "… (скрыто верхних уровней: " <> show levelsHidden <> ") …"
      ]

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

    deleteSlideHandler appContext this slideBranch = do
      getSlide <- getProps this <#> _.slides <#> getSlideByBranch

      create <-
        let sfx = fromMaybe "" $ getSlide slideBranch <#> slideSfx
            msg = "Вы уверены, что хотите удалить ветвь" <> sfx <> "?"
         in DOM.window >>= DOM.confirm msg

      if not create
         then pure unit
         else launchAff_
            $ dispatch appContext
            $ DiagTree $ Editor $ DeleteSlideRequest slideBranch

    copySlideHandler appContext this slideBranch = do
      getSlide <- getProps this <#> _.slides <#> getSlideByBranch
      launchAff_ $ dispatch appContext $
        DiagTree $ Editor $ CopySlideRequest slideBranch

    cutSlideHandler appContext this slideBranch = do
      getSlide <- getProps this <#> _.slides <#> getSlideByBranch
      launchAff_ $ dispatch appContext $
        DiagTree $ Editor $ CutSlideRequest slideBranch

    pasteSlideHandler appContext this slideBranch = do
      getSlide <- getProps this <#> _.slides <#> getSlideByBranch
      copyPasteBuffer <- getProps this <#> _.copyPasteBuffer

      paste <-
        let source = copyPasteBuffer.branch >>= getSlide <#> slideSfx
            destination = getSlide slideBranch <#> slideSfx

            operation =
              if copyPasteBuffer.cutting then "переместить" else "скопировать"

            msg
              =  "Вы уверены, что хотите "
              <> operation
              <> fromMaybe "" source
              <> fromMaybe "" ((" в" <> _) <$> destination)
              <> "?"

         in DOM.window >>= DOM.confirm msg

      if not paste
         then pure unit
         else launchAff_
            $ dispatch appContext
            $ DiagTree $ Editor $ PasteSlideRequest slideBranch

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
    shiftSlideBranch branch n answer
      slide@(DiagTreeSlide x)

      | (length branch - n) <= maxTreeDepth =
          if n == 0
             then Nothing
             else Just { parents: take n branch, answer, slide }

      | otherwise = do
          let nextN = n + 1
          slideId <- branch !! nextN
          let Tuple answers answersIndexes = fromIndexedAnswers x.answers

          { header, nextSlide } <-
            slideId `Map.lookup` answersIndexes >>= index answers

          shiftSlideBranch branch nextN (Just header) nextSlide

    changeDontShiftLevelsHandler this event = do
      let isChecked = eventIsChecked event
      transformState this _ { dontShiftLevels = isChecked }

    getInitialState this = do
      { appContext, selectedSlideBranch } <- getProps this

      let toggleSlideFold   = toggleSlideFoldHandler   this
          unfoldSlideBranch = unfoldSlideBranchHandler this

          selectOneLevelUp  = selectOneLevelUpHandler appContext this
          selectRoot        = selectRootHandler       appContext this

          changeDontShiftLevels = changeDontShiftLevelsHandler this

          selectSlide = handle $
            selectSlideHandler appContext this
                               toggleSlideFold unfoldSlideBranch

          -- Not unfolding at initialization step if a root slide is selected
          unfoldedSlides = maybe Set.empty f selectedSlideBranch
            where f x = if length x /= 1 then Set.fromFoldable x else Set.empty

      pure { selectSlide
           , unfoldedSlides
           , deleteSlide       : handle $ deleteSlideHandler appContext this
           , copySlide         : handle $ copySlideHandler appContext this
           , cutSlide          : handle $ cutSlideHandler appContext this
           , pasteSlide        : handle $ pasteSlideHandler appContext this
           , shiftedSlidesMenu : shiftedSlidesMenuFn selectRoot selectOneLevelUp
           , dontShiftLevels   : false
           , changeDontShiftLevels
           }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillReceiveProps =
            \this { selectedSlideBranch: nextSelected, slides } -> do
              { selectedSlideBranch: prevSelected } <- getProps this

              -- Unfolding new selected branch
              void $ runMaybeT $ do
                x <- toMaybeT $ do
                  selectedBranch <- nextSelected
                  guard $ nextSelected /= prevSelected
                  void $ getSlideByBranch slides selectedBranch
                  pure $ Set.fromFoldable selectedBranch

                liftEff $ transformState this $
                  \s -> s { unfoldedSlides = s.unfoldedSlides `Set.union` x }
        }

      where
        renderHandler this =
          wrapper <$> (renderFn <$> getProps this <*> readState this)


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

      , copyPasteState: getCopyPasteState branch.copyPasteBuffer
      , copyPasteBuffer: branch.copyPasteBuffer
      }


slideSfx :: DiagTreeSlide -> String
slideSfx (DiagTreeSlide x) = " #" <> show x.id <> " (\"" <> x.header <> "\")"

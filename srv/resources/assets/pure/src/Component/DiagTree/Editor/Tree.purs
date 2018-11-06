module Component.DiagTree.Editor.Tree
     ( diagTreeEditorTree
     ) where

import Prelude hiding (div)

import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction(..))
import App.Store.DiagTree.Actions (DiagTreeAction(Editor))
import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction(..))
import App.Store.DiagTree.Editor.Types
       ( DiagTreeSlides
       , DiagTreeSlideId
       , DiagTreeSlide (DiagTreeSlide)
       , fromIndexedAnswers
       )
import Component.DiagTree.Editor.Tree.Item (diagTreeEditorTreeItem)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.MonadZero (guard)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM
import Data.Array ((!!), index, last, head, take, init, length)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, fromMaybe, maybe, fromJust)
import Data.Record.Builder (merge, build)
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty (toString)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)
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
import React.DOM (div) as R
import React.DOM.Props (className, onClick, onChange, _type, checked)
import React.Spaces ((!), (!.), renderIn, text, elements, empty)
import React.Spaces.DOM (div, button, i, label, input)
import React.Spaces.DOM.Dynamic (div) as SDyn
import Utils (eventIsChecked, storeConnect, toMaybeT, (<.>))
import Utils.CopyPasteBuffer
       ( CopyPasteBuffer
       , CopyPasteBufferState
       , getCopyPasteState
       )
import Utils.DiagTree.Editor (getSlideByBranch)

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
  \ { appContext, slides, selectedSlideBranch, search
    , copyPasteState, copyPasteBuffer }
    { selectSlide, deleteSlide, copySlide, cutSlide, pasteSlide, unfoldedSlides
    , shiftedSlidesMenu, dontShiftLevels, changeDontShiftLevels
    } -> do

    let shifted = do
          guard $ not dontShiftLevels
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

          itemRender x = itemEl p []
            where p = build (itemPropsBuilder x) itemProps

      elements $ map itemRender slidesList

  where
    name = "DiagTreeEditorTree"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className name]
    itemEl = createElement diagTreeEditorTreeItem

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

    deleteSlideHandler appContext this slideBranch = do
      getSlide <- getProps this <#> _.slides <#> getSlideByBranch

      create <-
        let sfx = fromMaybe "" $
              getSlide slideBranch <#> \(DiagTreeSlide x) ->
                " #" <> show x.id <> " (\"" <> x.header <> "\")"

            msg = "Вы уверены, что хотите удалить ветвь" <> sfx <> "?"

         in DOM.window >>= DOM.confirm msg

      if not create
         then pure unit
         else launchAff_ $ dispatch appContext $
                DiagTree $ Editor $ DeleteSlideRequest slideBranch

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
        let sfx = fromMaybe "" $
              getSlide slideBranch <#> \(DiagTreeSlide x) ->
                " #" <> show x.id <> " (\"" <> x.header <> "\")"

            source = fromMaybe "" $
              getSlide (unsafePartial $ fromJust copyPasteBuffer.branch) <#>
              \(DiagTreeSlide x) ->
                " #" <> show x.id <.> "(\"" <> x.header <> "\")"

            operation = if copyPasteBuffer.cutting
                           then "переместить"
                           else "скопировать"
            msg = "Вы уверены, что хотите" <.> operation <.> source
                  <.> "в" <.> sfx <> "?"

        in DOM.window >>= DOM.confirm msg

      if not paste
         then pure unit
         else launchAff_ $ dispatch appContext $
                DiagTree $ Editor $ PasteSlideRequest slideBranch

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
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderFn props state # renderIn wrapper


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

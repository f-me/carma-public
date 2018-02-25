module Component.DiagTree.Editor.Tree
     ( diagTreeEditorTree
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), isJust, fromMaybe, maybe)
import Data.Record.Builder (merge)
import Data.Array ((!!), elemIndex, snoc, last, null, head, take, init)
import Data.Array (length) as A
import Data.String (length, splitAt)
import Data.String.NonEmpty (toString)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Foldable (foldl, foldr)
import Data.Tuple (Tuple (Tuple))

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Console (log)
import Control.Lazy (fix)

import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.DOM (div) as R
import React.DOM.Props (className, key, onClick, title)
import React.Spaces ((!), (!.), renderIn, text, elements, empty)
import React.Spaces.DOM (div, button, i, span)
import React.Spaces.DOM (div) as SDyn

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


-- If selected slides has more parents they will be hidden.
-- Two button also will be shown:
--   * Go one level up (select parent slide)
--   * Select root slide of current branch
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
  \ { slides, selectedSlideBranch, search }
    { selectSlide, deleteSlide, unfoldedSlides
    , selectRoot, selectOneLevelUp
    } -> do

    let shifted = do
          branch     <- selectedSlideBranch
          firstId    <- head branch
          firstSlide <- firstId `Map.lookup` slides
          shiftSlideBranch branch 0 Nothing firstSlide

    case shifted <#> _.parents >>> A.length of
         Nothing -> empty
         Just n -> do
           div !. classSfx "folded-parents-menu" $
             div !. "btn-group" $ do

               div !. "btn-group" $
                 button !. "btn btn-info btn-sm" ! onClick selectOneLevelUp $
                   text "На уровень выше"

               div !. "btn-group" $
                 button !. "btn btn-warning btn-sm" ! onClick selectRoot $
                   text "К корню ветви"

           div !. classSfx "more-elems" $
             i $ text $ "… (скрыто верхних уровней: " <> show n <> ") …"

    SDyn.div !. classSfx "list" $ do

      let f = renderItem selectedSlideBranch unfoldedSlides search
                         selectSlide deleteSlide

          Tuple renderItemFn slidesList =
            case shifted of
                 Nothing -> Tuple (f Nothing []) slides
                 Just x@{ slide: (DiagTreeSlide slide) }  ->
                   Tuple (f x.answer x.parents) $
                     Map.singleton slide.id x.slide

      elements $ map renderItemFn slidesList

  where
    name = "diag-tree-editor-tree"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    addUnfoldedClass       = (_ <.> classSfx "item--unfolded")
    addLeafClass           = (_ <.> classSfx "item--leaf")
    addSelectedClass       = (_ <.> classSfx "item--selected")
    addParentSelectedClass = (_ <.> classSfx "item--parent-selected")

    childrenRenderer =
      maybe empty $ elements >>> (SDyn.div !. classSfx "children")

    renderItem selectedSlide unfoldedSlides search select deleteSlide = fix $
      -- `fix` to prevent passing a lot of arguments every recursive iteration.
      \again answerHeader parents (DiagTreeSlide slide) ->
        let
          -- Full path of slides ids to current one
          slideBranch = parents `snoc` slide.id

          -- Constructing class name:
          --   * "selected" for currently selected one
          --   * "parent-selected" for all parents of currently selected
          --   * "unfolded" for items with shown children
          --   * "lead" for items that have no children (end of a branch)
          wClass = classSfx "item"
            # (if isJust children then addUnfoldedClass else id)
            # (if Map.isEmpty slide.answers then addLeafClass else id)
            # case selectedSlide of
                   Just x | last x == Just slide.id -> addSelectedClass
                          | isJust $ slide.id `elemIndex` x ->
                              addParentSelectedClass
                   _ -> id

          children =
            if isJust search || slide.id `Set.member` unfoldedSlides
               then let x = foldl childReducer [] slide.answers
                     in if null x then Nothing else Just x
               else Nothing

          -- Fold-reducer of array of elements to render children ("answers")
          childReducer = case search of
            Nothing ->
              \acc { header, nextSlide } ->
                acc `snoc` again (Just header) slideBranch nextSlide

            Just { parents: searchParents } ->
              \acc { header, nextSlide } ->
                -- Filtering only branches that have matched
                if slide.id `Set.member` searchParents
                   then acc `snoc` again (Just header) slideBranch nextSlide
                   else acc
        in
          renderIn (R.div [className wClass, key $ show slide.id]) $ do
            div !. classSfx "header" ! onClick (select slideBranch) $ do

              button !. "btn" <.> "btn-danger" <.> classSfx "delete"
                     ! onClick (deleteSlide slideBranch)
                     ! title "Удалить ветвь" $

                i !. "glyphicon" <.> "glyphicon-trash" $ empty

              let searchPatterns = do
                    { query, patterns }  <- search
                    { answer, question } <- Map.lookup slide.id patterns
                    pure { len: length query, answer, question }

                  searchAnswer = do
                    x <- searchPatterns
                    y <- x.answer
                    pure $ Tuple y x.len

                  searchQuestion = do
                    x <- searchPatterns
                    y <- x.question
                    pure $ Tuple y x.len

              case answerHeader of
                   Nothing -> empty
                   Just x  ->
                     div !. classSfx "answer" $
                       case searchAnswer of
                            Nothing -> text x
                            Just s  -> hlSearch x s

              div !. classSfx "question" $
                case searchQuestion of
                     Nothing -> text slide.header
                     Just s  -> hlSearch slide.header s

            childrenRenderer children

    -- Highlighting matched search patterns
    hlSearch x (Tuple start len) = fromMaybe (text x) $ do
      { before: pfx, after } <- splitAt start x

      { before: hl, after: sfx } <-
        -- Splitting at the end gives you `Nothing`,
        -- that's why we checking it here.
        if length after > len
           then splitAt len after
           else pure $ { before: after, after: "" }

      pure $ do
        if pfx /= "" then text pfx else empty
        span !. classSfx "search-match" $ text hl
        if sfx /= "" then text sfx else empty

    selectSlideHandler appContext this =
      \toggleSlideFold unfoldSlideBranch slideBranch event -> do

      isSearching <- getProps this <#> _.search <#> isJust

      preventDefault  event
      stopPropagation event

      case last slideBranch of
           Nothing -> pure unit
           Just x  -> do
             if isSearching
                then unfoldSlideBranch slideBranch
                else toggleSlideFold x

             launchAff_ $ dispatch appContext $
               DiagTree $ Editor $ SelectSlide slideBranch

    deleteSlideHandler slideBranch event = do
      preventDefault  event
      stopPropagation event

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
    shiftSlideBranch
      :: Array DiagTreeSlideId -> Int -> Maybe String -> DiagTreeSlide
      -> Maybe { parents :: Array DiagTreeSlideId
               , answer  :: Maybe String
               , slide   :: DiagTreeSlide
               }

    shiftSlideBranch branch n answer slide@(DiagTreeSlide { answers })
      | (A.length branch - n) <= maxTreeDepth =
          if n == 0
             then Nothing
             else Just { parents: take n branch, answer, slide }
      | otherwise = do
          let nextN = n + 1
          nextSlideId <- branch !! nextN
          { header, nextSlide } <- nextSlideId `Map.lookup` answers
          shiftSlideBranch branch nextN (Just header) nextSlide

    getInitialState this = do
      { appContext } <- getProps this

      let toggleSlideFold   = toggleSlideFoldHandler   this
          unfoldSlideBranch = unfoldSlideBranchHandler this

          selectSlide =
            selectSlideHandler appContext this
                               toggleSlideFold unfoldSlideBranch

      pure { selectSlide
           , unfoldedSlides   : (Set.empty :: Set DiagTreeSlideId)
           , deleteSlide      : deleteSlideHandler
           , selectOneLevelUp : selectOneLevelUpHandler appContext this
           , selectRoot       : selectRootHandler appContext this
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
    f appState = let branch = appState.diagTree.editor in merge
      { slides              : branch.slides
      , selectedSlideBranch : branch.selectedSlideBranch

      , search:
          branch.foundSlides
            >>= \ { matchedParents: parents, matchedPatterns: patterns } -> do
                  query <- branch.treeSearch.searchQuery <#> toString
                  pure { query, parents, patterns }
      }

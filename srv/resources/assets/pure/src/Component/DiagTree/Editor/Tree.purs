module Component.DiagTree.Editor.Tree
     ( diagTreeEditorTree
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), isJust, isNothing, fromMaybe)
import Data.Record.Builder (merge)
import Data.Array (fromFoldable, elemIndex, snoc, last, delete)
import Data.String (length, splitAt)
import Data.String.NonEmpty (toString)
import Data.Map (Map, lookup)
import Data.Foldable (foldl)
import Data.Tuple (Tuple (Tuple))

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Console (log)
import Control.Lazy (fix)

import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.DOM (div) as R
import React.DOM.Props (className, onClick, title)
import React.Spaces ((!), (!.), renderIn, text, elements, empty)
import React.Spaces.DOM (div, button, i, span)

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
  :: ReactClass
       { appContext          :: AppContext
       , slides              :: DiagTreeSlides
       , selectedSlideBranch :: Maybe (Array DiagTreeSlideId)

       , search
           :: Maybe { query    :: String
                    , slides   :: Array DiagTreeSlideId

                    , patterns :: Map DiagTreeSlideId
                                      { answer   :: Maybe Int
                                      , question :: Maybe Int
                                      }
                    }
       }

diagTreeEditorTreeRender = createClass $ spec $
  \ { slides, selectedSlideBranch, search }
    { selectSlide, deleteSlide, unfoldedSlides } -> do

    let renderItemFn =
          renderItem selectedSlideBranch unfoldedSlides search
                     selectSlide deleteSlide
                     Nothing []

    elements $ map renderItemFn $ fromFoldable slides

  where
    name = "diag-tree-editor-tree"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    renderItem selectedSlide unfoldedSlides search select deleteSlide = fix $
      -- `fix` to prevent passing a lot of arguments every recursive iteration.
      \again answerHeader parents (DiagTreeSlide slide) ->
        let
          -- Full path of slides ids to current one
          slideBranch = parents `snoc` slide.id

          -- Constructing class name:
          --   * "selected" for currently selected one
          --   * "parent-selected" for all parents of currently selected
          wClass = classSfx "item" #
            case selectedSlide of
                 Just x | last x == Just slide.id ->
                            (_ <.> classSfx "item--selected")
                        | isJust $ slide.id `elemIndex` x ->
                            (_ <.> classSfx "item--parent-selected")
                 _ -> id

          -- Fold-reducer of array of elements to render children ("answers")
          childReducer = case search of
            Nothing ->
              \acc { header, nextSlide } ->
                acc `snoc` again (Just header) slideBranch nextSlide

            Just { slides } ->
              \acc { header, nextSlide } ->
                -- Filtering only branches that have matched
                if isJust $ slide.id `elemIndex` slides
                   then acc `snoc` again (Just header) slideBranch nextSlide
                   else acc
        in
          renderIn (R.div [className wClass]) $ do
            div !. classSfx "header" ! onClick (select slideBranch) $ do

              button !. "btn" <.> "btn-danger" <.> classSfx "delete"
                     ! onClick (deleteSlide slideBranch)
                     ! title "Удалить ветвь" $

                i !. "glyphicon" <.> "glyphicon-trash" $ empty

              let searchPatterns = do
                    { query, patterns }  <- search
                    { answer, question } <- lookup slide.id patterns
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
                            Just s -> hlSearch x s

              div !. classSfx "question" $
                case searchQuestion of
                     Nothing -> text slide.header
                     Just s -> hlSearch slide.header s

            if isNothing $ slide.id `elemIndex` unfoldedSlides
               then empty
               else div !. classSfx "children" $
                      elements $ foldl childReducer [] slide.answers

    -- Highlighting matched search patterns
    hlSearch x (Tuple start len) = fromMaybe (text x) $ do
      { before: pfx, after } <- splitAt start x

      { before: hl, after: sfx } <-
        if length after > len
           then splitAt len after
           else pure $ { before: after, after: "" }

      pure $ do
        if pfx /= "" then text pfx else empty
        span !. classSfx "search-match" $ text hl
        if sfx /= "" then text sfx else empty

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
    f appState = let branch = appState.diagTree.editor in merge
      { slides              : branch.slides
      , selectedSlideBranch : branch.selectedSlideBranch

      , search:
          branch.foundSlides
            >>= \ { matchedSlides: slides, matchedPatterns: patterns } -> do
                  query <- branch.treeSearch.searchQuery <#> toString
                  pure { query, slides, patterns }
      }

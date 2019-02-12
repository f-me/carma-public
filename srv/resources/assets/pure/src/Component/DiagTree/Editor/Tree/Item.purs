module Component.DiagTree.Editor.Tree.Item
     ( diagTreeEditorTreeItem
     ) where

import Prelude hiding (div)

import Data.Monoid (mempty)
import Data.Array ((:), elemIndex, snoc, last, null, unsnoc)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe (..), isJust, fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (length, splitAt)
import Data.Tuple (Tuple (Tuple), fst)

import Control.Lazy (fix)

import React (ReactClass, EventHandler, preventDefault, stopPropagation)
import React.DOM (text, div, button, i, span)
import React.DOM.Dynamic (div) as RDyn
import React.DOM.Props (className, key, onClick, title, disabled)

import Utils ( callEventHandler
             , createClassStatelessWithName
             , (<.>)
             )

import Utils.CopyPasteBuffer (CopyPasteBufferState (..), CopyPasteBuffer)

import App.Store (AppContext)
import App.Store.DiagTree.Editor.Types ( DiagTreeSlideId
                                       , DiagTreeSlide(DiagTreeSlide)
                                       , fromIndexedAnswers
                                       )

type Props =
  { appContext     :: AppContext
  , key            :: String

  , selectedSlide  :: Maybe (Array DiagTreeSlideId)
  , unfoldedSlides :: Set DiagTreeSlideId

  , search
      :: Maybe { query    :: String
               , parents  :: Set DiagTreeSlideId
               , patterns :: Map DiagTreeSlideId
                                 { answer   :: Maybe Int
                                 , question :: Maybe Int
                                 }
               }

  , select          :: EventHandler (Array DiagTreeSlideId)
  , delete          :: EventHandler (Array DiagTreeSlideId)
  , copy            :: EventHandler (Array DiagTreeSlideId)
  , cut             :: EventHandler (Array DiagTreeSlideId)
  , paste           :: EventHandler (Array DiagTreeSlideId)
  , copyPasteState  :: CopyPasteBufferState
  , copyPasteBuffer :: CopyPasteBuffer

  , answerHeader    :: Maybe String
  , parents         :: Array DiagTreeSlideId
  , slide           :: DiagTreeSlide
  }


diagTreeEditorTreeItemRender :: ReactClass Props
diagTreeEditorTreeItemRender = f $
  \props@{ selectedSlide, unfoldedSlides
         , search, select, delete
         , copy, cut, paste, copyPasteState, copyPasteBuffer
         } ->
  (\r -> r props.answerHeader props.parents props.slide) $ -- first level call
  fix $ \again answerHeader parents (DiagTreeSlide slide) ->

  let
    -- Full path of slides ids to current one
    slideBranch = parents `snoc` slide.id

    answers = fst $ fromIndexedAnswers slide.answers

    -- Constructing class name:
    --   * "selected" for currently selected one
    --   * "parent-selected" for all parents of currently selected
    --   * "unfolded" for items with shown children
    --   * "lead" for items that have no children (end of a branch)
    wClass = name
      # (if isJust children then addUnfoldedClass else id)
      # (if null answers then addLeafClass else id)
      # case selectedSlide of
             Just x | last x == Just slide.id -> addSelectedClass
                    | isJust $ slide.id `elemIndex` x ->
                        addParentSelectedClass
             _ -> id

    children =
      if isJust search || slide.id `Set.member` unfoldedSlides
         then let x = foldl childReducer [] answers
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

    onHeaderClick event = do
      preventDefault event
      callEventHandler select slideBranch

    onDeleteClick event = do
      preventDefault event
      stopPropagation event
      callEventHandler delete slideBranch

    onCopyClick event = do
      preventDefault event
      stopPropagation event
      callEventHandler copy slideBranch

    onCutClick event = do
      preventDefault event
      stopPropagation event
      callEventHandler cut slideBranch

    onPasteClick event = do
      preventDefault event
      stopPropagation event
      callEventHandler paste slideBranch

    headerClasses =
      case copyPasteState of
        Cutout i | i == slide.id -> "header-cutout"
        Copied i | i == slide.id -> "header-copied"
        _ -> ""

    isPasteDisabled =
      copyPasteState == EmptyBuffer ||
      case copyPasteBuffer.branch of
        Just a  ->
          case unsnoc a of
            Just { init: _, last: id } -> slide.id == id
            Nothing -> true
        Nothing -> true
  in
    div
      [ className wClass
      , key $ show slide.id
      ]
      $
      [ div
          [ className $ classSfx "header" <.> classSfx headerClasses
          , onClick onHeaderClick
          ]
          $
          [ button
              [ className $ "btn" <.> "btn-danger" <.> classSfx "delete"
              , onClick onDeleteClick
              , title "Удалить ветвь"
              ]
              [ i [className $ "glyphicon" <.> "glyphicon-trash"] mempty ]

          , button
              [ className $ "btn" <.> classSfx "copy"
              , onClick onCopyClick
              , title "Скопировать ветвь"
              ]
              [ i [className $ "glyphicon" <.> "glyphicon-copy"] mempty ]

          , button
              [ className $ "btn" <.> classSfx "cut"
              , onClick onCutClick
              , title "Переместить ветвь"
              ]
              [ i [className $ "glyphicon" <.> "glyphicon-scissors"] mempty ]

          , button
              [ className $ "btn" <.> classSfx "paste"
              , onClick onPasteClick
              , disabled isPasteDisabled
              , title "Вставить ветвь"
              ]
              [ i [className $ "glyphicon" <.> "glyphicon-paste"] mempty ]
          ]
          <>
          let
            searchPatterns = do
              { query,  patterns } <- search
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

            answerEls =
              case answerHeader of
                   Nothing -> mempty
                   Just x  -> pure $
                     div [className $ classSfx "answer"] $
                       case searchAnswer of
                            Nothing -> pure $ text x
                            Just s  -> hlSearch x s

            questionEls =
              div [className $ classSfx "question"] $
                case searchQuestion of
                     Nothing -> pure $ text slide.header
                     Just s  -> hlSearch slide.header s
          in
            answerEls `snoc` questionEls
      ]
      <>
      childrenRenderer children

  where
    name = "DiagTreeEditorTreeItem"
    classSfx s = name <> "--" <> s

    addUnfoldedClass       = (_ <.> classSfx "unfolded")
    addLeafClass           = (_ <.> classSfx "leaf")
    addSelectedClass       = (_ <.> classSfx "selected")
    addParentSelectedClass = (_ <.> classSfx "parent-selected")

    childrenRenderer =
      maybe mempty $ pure <<< RDyn.div [className $ classSfx "children"]

    f renderFn =
      createClassStatelessWithName name renderFn

    -- Highlighting matched search patterns
    hlSearch x (Tuple start len) = fromMaybe [text x] $ do
      { before: pfx, after } <- splitAt start x

      { before: hl, after: sfx } <-
        -- Splitting at the end gives you `Nothing`,
        -- that's why we checking it here.
        if length after > len
           then splitAt len after
           else pure { before: after, after: "" }

      pure
        $ (if pfx /= "" then [text pfx] else mempty)
        <> span [className $ classSfx "search-match"] [text hl]
        : (if sfx /= "" then [text sfx] else mempty)


diagTreeEditorTreeItem :: ReactClass Props
diagTreeEditorTreeItem = diagTreeEditorTreeItemRender

module App.Store.DiagTree.Editor.Reducers
     ( DiagTreeEditorState
     , diagTreeEditorInitialState
     , diagTreeEditorReducer
     , FoundSlides
     ) where

import Prelude

import Control.Alt ((<|>))

import Data.Maybe (Maybe (..), fromMaybe, isNothing)
import Data.Array (snoc)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple (Tuple))
import Data.String (Pattern (Pattern), toLower, indexOf)
import Data.String.NonEmpty (toString)
import Data.Foldable (foldl, foldr)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlides
     , DiagTreeSlideId
     , DiagTreeSlide (DiagTreeSlide)
     )

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (..)
     , LoadSlidesFailureReason (..)
     )

import App.Store.DiagTree.Editor.TreeSearch.Reducers
     ( DiagTreeEditorTreeSearchState
     , diagTreeEditorTreeSearchInitialState
     , diagTreeEditorTreeSearchReducer
     )


type FoundSlides =
  { matchedParents
      :: Set DiagTreeSlideId
      -- ^ Found slides in these parents (including all parents of a branch).
      --   Matches slide must NOT be included here
      --   (except if it's parent of another one that matches).

  , matchedPatterns
      :: Map DiagTreeSlideId { answer :: Maybe Int, question :: Maybe Int }
      -- ^ `Int` value is start position of matched pattern,
      --   for example search pattern is "bar baz" and if `answer` of a slide
      --   has value "foo bar baz bzz" then it would be `Just 4`.
      --   (search word length could be determinted from 'treeSearch' branch).
  }

type DiagTreeEditorState =
  { slides                    :: DiagTreeSlides
  , foundSlides               :: Maybe FoundSlides

  -- Selected slide with all parents in ascending order
  , selectedSlideBranch       :: Maybe (Array DiagTreeSlideId)

  , isSlidesLoaded            :: Boolean
  , isSlidesLoading           :: Boolean
  , isSlidesLoadingFailed     :: Boolean
  , isParsingSlidesDataFailed :: Boolean

  , treeSearch                :: DiagTreeEditorTreeSearchState

  , slideDeleting
      :: { isProcessing :: Boolean
         , isFailed     :: Boolean
         , branch       :: Maybe (Array DiagTreeSlideId)
         }

  , newSlide
      :: { isProcessing :: Boolean
         , isFailed     :: Boolean
         }
  }

diagTreeEditorInitialState :: DiagTreeEditorState
diagTreeEditorInitialState =
  { slides                    : Map.empty
  , selectedSlideBranch       : Nothing
  , foundSlides               : Nothing

  , isSlidesLoaded            : false
  , isSlidesLoading           : false
  , isSlidesLoadingFailed     : false
  , isParsingSlidesDataFailed : false

  , treeSearch                : diagTreeEditorTreeSearchInitialState

  , slideDeleting:
      { isProcessing : false
      , isFailed     : false
      , branch       : Nothing
      }

  , newSlide:
      { isProcessing : false
      , isFailed     : false
      }
  }


diagTreeEditorReducer
  :: DiagTreeEditorState -> DiagTreeEditorAction -> Maybe DiagTreeEditorState


diagTreeEditorReducer state LoadSlidesRequest =
  Just state { slides                    = (Map.empty :: DiagTreeSlides)
             , selectedSlideBranch       = Nothing

             , isSlidesLoaded            = false
             , isSlidesLoading           = true
             , isSlidesLoadingFailed     = false
             , isParsingSlidesDataFailed = false
             }

diagTreeEditorReducer state (LoadSlidesSuccess { slides, rootSlide }) =
  Just state { slides              = slides
             , selectedSlideBranch = Just [rootSlide]
             , isSlidesLoaded      = true
             , isSlidesLoading     = false
             }

diagTreeEditorReducer state (LoadSlidesFailure LoadingSlidesFailed) =
  Just state { isSlidesLoading       = false
             , isSlidesLoadingFailed = true
             }

diagTreeEditorReducer state (LoadSlidesFailure ParsingSlidesDataFailed) =
  Just state { isSlidesLoading           = false
             , isSlidesLoadingFailed     = true
             , isParsingSlidesDataFailed = true
             }


diagTreeEditorReducer _ (SelectSlide []) = Nothing


diagTreeEditorReducer _ (DeleteSlideRequest []) = Nothing

diagTreeEditorReducer state (DeleteSlideRequest slidePath) = do
  when state.slideDeleting.isProcessing Nothing
  Just state { slideDeleting
                 { isProcessing = true
                 , isFailed     = false
                 , branch       = Just slidePath
                 }
             }

diagTreeEditorReducer state (DeleteSlideSuccess slidePath) = do
  isSameBranch <- state.slideDeleting.branch <#> (_ == slidePath)
  unless isSameBranch Nothing
  Just state { slideDeleting
                 { isProcessing = false
                 , isFailed     = false
                 , branch       = Nothing
                 }
             }

diagTreeEditorReducer state (DeleteSlideFailure slidePath) = do
  isSameBranch <- state.slideDeleting.branch <#> (_ == slidePath)
  unless isSameBranch Nothing
  Just state { slideDeleting
                 { isProcessing = false
                 , isFailed     = true
                 }
             }


diagTreeEditorReducer state NewSlideRequest = do
  when state.newSlide.isProcessing Nothing
  Just state { newSlide { isProcessing = true, isFailed = false } }

diagTreeEditorReducer state NewSlideSuccess =
  Just state { newSlide { isProcessing = false, isFailed = false } }

diagTreeEditorReducer state NewSlideFailure =
  Just state { newSlide { isProcessing = false, isFailed = true } }


diagTreeEditorReducer state (SelectSlide branch) =
  Just state { selectedSlideBranch = Just branch }


diagTreeEditorReducer state (TreeSearch subAction) =
  diagTreeEditorTreeSearchReducer state.treeSearch subAction
    <#> state { treeSearch = _ }
    <#> \s@{ slides, treeSearch: { searchQuery: q } } ->
          let f = toString >>> toLower >>> Pattern >>> search slides
           in s { foundSlides = q <#> f }

  where
    search slides query =
      let initial = { matchedParents: Set.empty, matchedPatterns: Map.empty }
       in foldl (searchReduce query) initial slides

    searchReduce query acc (DiagTreeSlide x) =
      foldl rootChildrenReduce rootSearch rootChildren
      where
        -- Matching only `question` for root silde
        rootSearch =
          fromMaybe acc $
            query `indexOf` toLower x.header <#>
              \pos -> acc { matchedPatterns = m pos acc.matchedPatterns }
          where
            m pos = Map.insert x.id { answer: Nothing, question: Just pos }

        -- Matching all children deep to end of the branches
        rootChildren = foldl (childReduce $ Set.singleton x.id) [] x.answers

        -- Merging found matches to root accumulator
        rootChildrenReduce rootAcc (Tuple parents { id, answer, question }) =
          rootAcc { matchedParents = matchedParents, matchedPatterns = matches }
          where
            matchedParents = foldr Set.insert rootAcc.matchedParents parents
            matches = Map.insert id { answer, question } rootAcc.matchedPatterns

        -- Traverser for an "answer" (single child with all his children)
        childReduce parents matches item@{ nextSlide: (DiagTreeSlide slide) } =
          foldl ownChildReduce newMatches slide.answers
          where
            answer         = query `indexOf` toLower item.header
            question       = query `indexOf` toLower slide.header
            ownChildReduce = childReduce $ slide.id `Set.insert` parents

            newMatches =
              if isNothing $ answer <|> question
                 then matches
                 else matches `snoc`
                        Tuple parents { id: slide.id, answer, question }

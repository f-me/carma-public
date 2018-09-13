module App.Store.DiagTree.Editor.Reducers
     ( DiagTreeEditorState
     , diagTreeEditorInitialState
     , diagTreeEditorReducer
     , FoundSlides
     ) where

import Prelude

import App.Store.DiagTree.Editor.Actions
  ( DiagTreeEditorAction (..)
  , LoadSlidesFailureReason (..)
  )
import App.Store.DiagTree.Editor.TreeSearch.Reducers
  ( DiagTreeEditorTreeSearchState
  , diagTreeEditorTreeSearchInitialState
  , diagTreeEditorTreeSearchReducer
  )
import App.Store.DiagTree.Editor.Types
  ( DiagTreeSlides
  , DiagTreeSlideId
  , DiagTreeSlide (DiagTreeSlide)
  , fromIndexedAnswers
  )
import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array (snoc, init, length)
import Data.Foldable (foldl, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(Pattern), toLower, indexOf)
import Data.String.NonEmpty (toString)
import Data.Tuple (Tuple(Tuple), fst)
import Utils.DiagTree.Editor (getSlideByBranch)


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

  , copyPasteBuffer
      :: { isProcessing :: Boolean
         , isFailed     :: Boolean
         , branch       :: Maybe (Array DiagTreeSlideId)
         , cutting      :: Boolean
         }

  , newSlide
      :: { isProcessing :: Boolean
         , isFailed     :: Boolean
         }

  , slideSaving
      :: { isProcessing :: Boolean
         , isFailed     :: Boolean
         , branch       :: Maybe (Array DiagTreeSlideId)
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
  , copyPasteBuffer:
      { isProcessing : false
      , isFailed     : false
      , branch       : Nothing
      , cutting      : false
      }

  , newSlide:
      { isProcessing : false
      , isFailed     : false
      }

  , slideSaving:
      { isProcessing : false
      , isFailed     : false
      , branch       : Nothing
      }
  }


diagTreeEditorReducer
  :: DiagTreeEditorState -> DiagTreeEditorAction -> Maybe DiagTreeEditorState


diagTreeEditorReducer state LoadSlidesRequest = do
  guard $ not $ isAnyProcessing state

  Just state { slides                    = (Map.empty :: DiagTreeSlides)

             , isSlidesLoaded            = false
             , isSlidesLoading           = true
             , isSlidesLoadingFailed     = false
             , isParsingSlidesDataFailed = false
             }

diagTreeEditorReducer state (LoadSlidesSuccess { slides, rootSlide }) =
  Just state { slides              = slides
             , isSlidesLoaded      = true
             , isSlidesLoading     = false
             , selectedSlideBranch = previouslySelected <|> Just [rootSlide]
             }
  where
    -- Trying to restore selected slide branch as deep as possible by slicing
    -- last slide from branch one by one until reach first existing branch.
    previouslySelected = f =<< state.selectedSlideBranch
      where
        f selected =
          (selected <$ getSlideByBranch slides selected) <|>
            (init selected >>= (\x -> x <$ guard (length x > 0)) >>= f)

diagTreeEditorReducer state (LoadSlidesFailure LoadingSlidesFailed) =
  Just state { isSlidesLoading       = false
             , isSlidesLoadingFailed = true
             , selectedSlideBranch   = Nothing
             }

diagTreeEditorReducer state (LoadSlidesFailure ParsingSlidesDataFailed) =
  Just state { isSlidesLoading           = false
             , isSlidesLoadingFailed     = true
             , isParsingSlidesDataFailed = true
             , selectedSlideBranch       = Nothing
             }


diagTreeEditorReducer _ (SelectSlide []) = Nothing


diagTreeEditorReducer _ (DeleteSlideRequest []) = Nothing

diagTreeEditorReducer state (DeleteSlideRequest slidePath) = do
  guard $ not $ isAnyProcessing state

  Just state { slideDeleting
                 { isProcessing = true
                 , isFailed     = false
                 , branch       = Just slidePath
                 }
             }

diagTreeEditorReducer state (DeleteSlideSuccess slidePath) = do
  guard =<< map (_ == slidePath) state.slideDeleting.branch

  Just state { slideDeleting
                 { isProcessing = false
                 , isFailed     = false
                 , branch       = Nothing
                 }
             }

diagTreeEditorReducer state (DeleteSlideFailure slidePath) = do
  guard =<< map (_ == slidePath) state.slideDeleting.branch

  Just state { slideDeleting
                 { isProcessing = false
                 , isFailed     = true
                 }
             }

diagTreeEditorReducer _ (CopySlideRequest []) = Nothing

diagTreeEditorReducer state (CopySlideRequest slidePath) = do
  guard $ not $ isAnyProcessing state

  Just state { copyPasteBuffer
                 { isProcessing = false
                 , isFailed     = false
                 , branch       = Just slidePath
                 , cutting      = false
                 }
             }

diagTreeEditorReducer state (CopySlideSuccess slidePath) = do
  guard =<< map (_ == slidePath) state.copyPasteBuffer.branch

  Just state { copyPasteBuffer
                 { isProcessing = false
                 , isFailed     = false
                 , branch       = Just slidePath
                 , cutting      = false
                 }
             }

diagTreeEditorReducer state (CopySlideFailure slidePath) = do
  guard =<< map (_ == slidePath) state.copyPasteBuffer.branch

  Just state { copyPasteBuffer
                 { isProcessing = false
                 , isFailed     = true
                 , branch       = Nothing
                 , cutting      = false
                 }
             }

diagTreeEditorReducer _ (CutSlideRequest []) = Nothing

diagTreeEditorReducer state (CutSlideRequest slidePath) = do
  guard $ not $ isAnyProcessing state

  Just state { copyPasteBuffer
                 { isProcessing = false
                 , isFailed     = false
                 , branch       = Just slidePath
                 , cutting      = true
                 }
             }

diagTreeEditorReducer state (CutSlideSuccess slidePath) = do
  guard =<< map (_ == slidePath) state.copyPasteBuffer.branch

  Just state { copyPasteBuffer
                 { isProcessing = false
                 , isFailed     = false
                 , branch       = Just slidePath
                 , cutting      = false
                 }
             }

diagTreeEditorReducer state (CutSlideFailure slidePath) = do
  guard =<< map (_ == slidePath) state.copyPasteBuffer.branch

  Just state { copyPasteBuffer
                 { isProcessing = false
                 , isFailed     = true
                 , branch       = Nothing
                 , cutting      = true
                 }
             }

diagTreeEditorReducer _ (PasteSlideRequest []) = Nothing

diagTreeEditorReducer state (PasteSlideRequest slidePath) = do
  guard $ not $ isAnyProcessing state

  Just state { copyPasteBuffer
                 { isProcessing = true
                 , isFailed     = false
                 , branch       = Just slidePath
                 , cutting      = false
                 }
             }

diagTreeEditorReducer state (PasteSlideSuccess slidePath) = do
  guard =<< map (_ == slidePath) state.copyPasteBuffer.branch

  Just state { copyPasteBuffer
                 { isProcessing = false
                 , isFailed     = false
                 , branch       = Nothing
                 , cutting      = false
                 }
             }

diagTreeEditorReducer state (PasteSlideFailure slidePath) = do
  guard =<< map (_ == slidePath) state.copyPasteBuffer.branch

  Just state { copyPasteBuffer
                 { isProcessing = false
                 , isFailed     = true
                 }
             }

diagTreeEditorReducer state NewSlideRequest = do
  guard $ not $ isAnyProcessing state
  Just state { newSlide { isProcessing = true, isFailed = false } }

diagTreeEditorReducer state (NewSlideSuccess slide@(DiagTreeSlide x)) =
  Just state { newSlide { isProcessing = false, isFailed = false }
             , slides = Map.insert x.id slide state.slides
             }

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

        getAnswers = _.answers >>> fromIndexedAnswers >>> fst

        -- Matching all children deep to end of the branches
        rootChildren =
          foldl (childReduce $ Set.singleton x.id) [] $ getAnswers x

        -- Merging found matches to root accumulator
        rootChildrenReduce rootAcc (Tuple parents { id, answer, question }) =
          rootAcc { matchedParents = matchedParents, matchedPatterns = matches }
          where
            matchedParents = foldr Set.insert rootAcc.matchedParents parents
            matches = Map.insert id { answer, question } rootAcc.matchedPatterns

        -- Traverser for an "answer" (single child with all his children)
        childReduce parents matches item@{ nextSlide: (DiagTreeSlide slide) } =
          foldl ownChildReduce newMatches $ getAnswers slide
          where
            answer         = query `indexOf` toLower item.header
            question       = query `indexOf` toLower slide.header
            ownChildReduce = childReduce $ slide.id `Set.insert` parents

            newMatches =
              if isNothing $ answer <|> question
                 then matches
                 else matches `snoc`
                        Tuple parents { id: slide.id, answer, question }


diagTreeEditorReducer state (SaveSlideRequest slidePath _) = do
  guard $ not $ isAnyProcessing state

  Just state { slideSaving
                 { isProcessing = true
                 , isFailed     = false
                 , branch       = Just slidePath
                 }
             }

diagTreeEditorReducer state (SaveSlideSuccess slidePath) = do
  guard =<< map (_ == slidePath) state.slideSaving.branch

  Just state { slideSaving
                 { isProcessing = false
                 , isFailed     = false
                 , branch       = Nothing
                 }
             }

diagTreeEditorReducer state (SaveSlideFailure slidePath) = do
  guard =<< map (_ == slidePath) state.slideSaving.branch
  Just state { slideSaving { isProcessing = false, isFailed = true } }


isAnyProcessing :: DiagTreeEditorState -> Boolean
isAnyProcessing x
   = x.isSlidesLoading
  || x.newSlide.isProcessing
  || x.slideDeleting.isProcessing
  || x.slideSaving.isProcessing

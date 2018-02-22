module App.Store.DiagTree.Editor.Reducers
     ( DiagTreeEditorState
     , diagTreeEditorInitialState
     , diagTreeEditorReducer
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Map (empty)

import App.Store.DiagTree.Editor.Types (DiagTreeSlides, DiagTreeSlideId)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (..)
     , LoadSlidesFailureReason (..)
     )

import App.Store.DiagTree.Editor.TreeSearch.Reducers
     ( DiagTreeEditorTreeSearchState
     , diagTreeEditorTreeSearchInitialState
     , diagTreeEditorTreeSearchReducer
     )


type DiagTreeEditorState =
  { slides                    :: DiagTreeSlides

  -- Selected slide with all parents in ascending order
  , selectedSlideBranch       :: Maybe (Array DiagTreeSlideId)

  , isSlidesLoaded            :: Boolean
  , isSlidesLoading           :: Boolean
  , isSlidesLoadingFailed     :: Boolean
  , isParsingSlidesDataFailed :: Boolean

  , treeSearch                :: DiagTreeEditorTreeSearchState
  }

diagTreeEditorInitialState :: DiagTreeEditorState
diagTreeEditorInitialState =
  { slides                    : empty
  , selectedSlideBranch       : Nothing

  , isSlidesLoaded            : false
  , isSlidesLoading           : false
  , isSlidesLoadingFailed     : false
  , isParsingSlidesDataFailed : false

  , treeSearch                : diagTreeEditorTreeSearchInitialState
  }


diagTreeEditorReducer
  :: DiagTreeEditorState -> DiagTreeEditorAction -> Maybe DiagTreeEditorState

diagTreeEditorReducer state LoadSlidesRequest =
  Just state { slides                    = (empty :: DiagTreeSlides)
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

diagTreeEditorReducer state (SelectSlide branch) =
  Just state { selectedSlideBranch = Just branch }

diagTreeEditorReducer state (TreeSearch x) =
  diagTreeEditorTreeSearchReducer state.treeSearch x
    <#> state { treeSearch = _ }

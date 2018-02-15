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


type DiagTreeEditorState =
  { slides        :: DiagTreeSlides
  , selectedSlide :: Maybe DiagTreeSlideId

  , isSlidesLoaded            :: Boolean
  , isSlidesLoading           :: Boolean
  , isSlidesLoadingFailed     :: Boolean
  , isParsingSlidesDataFailed :: Boolean
  }

diagTreeEditorInitialState :: DiagTreeEditorState
diagTreeEditorInitialState =
  { slides        : empty
  , selectedSlide : Nothing

  , isSlidesLoaded            : false
  , isSlidesLoading           : false
  , isSlidesLoadingFailed     : false
  , isParsingSlidesDataFailed : false
  }


diagTreeEditorReducer
  :: DiagTreeEditorState -> DiagTreeEditorAction -> Maybe DiagTreeEditorState

diagTreeEditorReducer state LoadSlidesRequest =
  Just $ state { slides        = (empty :: DiagTreeSlides)
               , selectedSlide = Nothing

               , isSlidesLoaded            = false
               , isSlidesLoading           = true
               , isSlidesLoadingFailed     = false
               , isParsingSlidesDataFailed = false
               }

diagTreeEditorReducer state (LoadSlidesSuccess x) =
  Just $ state { slides          = x.slides
               , selectedSlide   = Just x.rootSlide
               , isSlidesLoaded  = true
               , isSlidesLoading = false
               }

diagTreeEditorReducer state (LoadSlidesFailure LoadingSlidesFailed) =
  Just $ state { isSlidesLoading       = false
               , isSlidesLoadingFailed = true
               }

diagTreeEditorReducer state (LoadSlidesFailure ParsingSlidesDataFailed) =
  Just $ state { isSlidesLoading           = false
               , isSlidesLoadingFailed     = true
               , isParsingSlidesDataFailed = true
               }

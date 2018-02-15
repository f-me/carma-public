module App.Store.DiagTree.Editor
     ( DiagTreeEditorState
     , DiagTreeEditorAction (..)
     , LoadSlidesFailureReason (..)

     , DiagTreeSlideId
     , DiagTreeSlides
     , DiagTreeSlide

     , diagTreeEditorInitialState
     , diagTreeEditorReducer
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Map (Map, empty)


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


data DiagTreeEditorAction
  = LoadSlidesRequest

  | LoadSlidesSuccess
      { slides    :: DiagTreeSlides
      , rootSlide :: DiagTreeSlideId
      }

  | LoadSlidesFailure LoadSlidesFailureReason


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


type DiagTreeSlideId = Int
type DiagTreeSlides  = Map DiagTreeSlideId DiagTreeSlide

type DiagTreeSlide =
  { id     :: DiagTreeSlideId
  , isRoot :: Boolean
  }

data LoadSlidesFailureReason
  = LoadingSlidesFailed
  | ParsingSlidesDataFailed

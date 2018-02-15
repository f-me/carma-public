module App.Store.DiagTree.Editor
     ( DiagTreeEditorState
     , DiagTreeEditorAction (..)
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
  }

diagTreeEditorInitialState :: DiagTreeEditorState
diagTreeEditorInitialState =
  { slides        : empty
  , selectedSlide : Nothing
  }


data DiagTreeEditorAction
  = LoadSlidesRequest

  | LoadSlidesSuccess
      { slides    :: DiagTreeSlides
      , rootSlide :: DiagTreeSlideId
      }

  | LoadSlidesFailure


diagTreeEditorReducer
  :: DiagTreeEditorState -> DiagTreeEditorAction -> Maybe DiagTreeEditorState

diagTreeEditorReducer state (LoadSlidesSuccess x) =
  Just $ state { slides        = x.slides
               , selectedSlide = Just x.rootSlide
               }

diagTreeEditorReducer _ _ = Nothing


type DiagTreeSlideId = Int
type DiagTreeSlides  = Map DiagTreeSlideId DiagTreeSlide

type DiagTreeSlide =
  { id     :: DiagTreeSlideId
  , isRoot :: Boolean
  }

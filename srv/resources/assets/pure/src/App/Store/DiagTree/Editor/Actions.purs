module App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (..)
     , LoadSlidesFailureReason (..)
     ) where

import App.Store.DiagTree.Editor.Types (DiagTreeSlideId, DiagTreeSlides)


data DiagTreeEditorAction
  = LoadSlidesRequest

  | LoadSlidesSuccess
      { slides    :: DiagTreeSlides
      , rootSlide :: DiagTreeSlideId
      }

  | LoadSlidesFailure LoadSlidesFailureReason


data LoadSlidesFailureReason
  = LoadingSlidesFailed
  | ParsingSlidesDataFailed

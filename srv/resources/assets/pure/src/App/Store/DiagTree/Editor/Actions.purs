module App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (..)
     , LoadSlidesFailureReason (..)
     ) where

import App.Store.DiagTree.Editor.Types (DiagTreeSlideId, DiagTreeSlides)

import App.Store.DiagTree.Editor.TreeSearch.Actions
     ( DiagTreeEditorTreeSearchAction
     )


data DiagTreeEditorAction
  = LoadSlidesRequest

  | LoadSlidesSuccess
      { slides    :: DiagTreeSlides
      , rootSlide :: DiagTreeSlideId
      }

  | LoadSlidesFailure LoadSlidesFailureReason

  | SelectSlide (Array DiagTreeSlideId)

  | TreeSearch DiagTreeEditorTreeSearchAction


data LoadSlidesFailureReason
  = LoadingSlidesFailed
  | ParsingSlidesDataFailed

module App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (..)
     , LoadSlidesFailureReason (..)
     , SlideIdentity
     ) where

import Data.Maybe (Maybe)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlides
     , DiagTreeSlide
     , DiagTreeSlideAttachment
     )

import App.Store.DiagTree.Editor.TreeSearch.Actions
     ( DiagTreeEditorTreeSearchAction
     )


type SlideIdentity = Array DiagTreeSlideId

data DiagTreeEditorAction
  = LoadSlidesRequest

  | LoadSlidesSuccess
      { slides    :: DiagTreeSlides
      , rootSlide :: DiagTreeSlideId
      }

  | LoadSlidesFailure LoadSlidesFailureReason

  | SelectSlide (Array DiagTreeSlideId)

  | TreeSearch DiagTreeEditorTreeSearchAction

  | DeleteSlideRequest SlideIdentity
  | DeleteSlideSuccess SlideIdentity
  | DeleteSlideFailure SlideIdentity

  | NewSlideRequest
  | NewSlideSuccess DiagTreeSlide
  | NewSlideFailure

  | SaveSlideRequest SlideIdentity
      { slide :: DiagTreeSlide

      , newAnswers
          :: Array { header     :: String
                   , text       :: String
                   , attachment :: Maybe DiagTreeSlideAttachment
                   }
      }

  | SaveSlideSuccess SlideIdentity
  | SaveSlideFailure SlideIdentity


data LoadSlidesFailureReason
  = LoadingSlidesFailed
  | ParsingSlidesDataFailed

module App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlides
     , DiagTreeSlide
     ) where

import Data.Map (Map)


type DiagTreeSlideId = Int
type DiagTreeSlides  = Map DiagTreeSlideId DiagTreeSlide

type DiagTreeSlide =
  { id     :: DiagTreeSlideId
  , isRoot :: Boolean
  }

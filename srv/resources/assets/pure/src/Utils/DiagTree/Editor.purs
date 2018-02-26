module Utils.DiagTree.Editor
     ( getSlideByBranch
     ) where

import Prelude

import Data.Array (uncons)
import Data.Map (lookup)
import Data.Foldable (foldM)
import Data.Maybe (Maybe)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlides
     , DiagTreeSlideId
     )


getSlideByBranch
  :: DiagTreeSlides
  -> Array DiagTreeSlideId
  -> Maybe DiagTreeSlide

getSlideByBranch slides branch = do
  { head: x, tail: xs } <- uncons branch
  first@(DiagTreeSlide firstSlide) <- x `lookup` slides

  let f (DiagTreeSlide { answers }) slideId =
        slideId `lookup` answers <#> _.nextSlide

  foldM f first xs

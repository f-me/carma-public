module Utils.DiagTree.Editor
     ( getSlideByBranch
     , eqDiagTreeSlideResource
     , eqDiagTreeSlideResources
     , getDiagTreeSlideResourcePath
     , eqIshDiagTreeSlideAnswer
     , eqIshDiagTreeSlideAnswers
     , diagTreeSlideActionToBackend
     , diagTreeSlideActionFromBackend
     ) where

import Prelude

import Data.Tuple (Tuple (Tuple))
import Data.Array (uncons, length, zip, fromFoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldM)
import Data.Maybe (Maybe (..), fromMaybe)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlides
     , DiagTreeSlideId
     , DiagTreeSlideResource
     , DiagTreeSlideAction (..)
     , DiagTreeSlideAnswer
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAction
     ( BackendAction
     )


getSlideByBranch
  :: DiagTreeSlides
  -> Array DiagTreeSlideId
  -> Maybe DiagTreeSlide

getSlideByBranch slides branch = do
  { head: x, tail: xs } <- uncons branch
  first@(DiagTreeSlide firstSlide) <- x `Map.lookup` slides

  let f (DiagTreeSlide { answers }) slideId =
        slideId `Map.lookup` answers <#> _.nextSlide

  foldM f first xs


eqDiagTreeSlideResource
  :: DiagTreeSlideResource -> DiagTreeSlideResource -> Boolean
eqDiagTreeSlideResource a b =
  a.text == b.text && a.attachment == b.attachment

eqDiagTreeSlideResources
  :: Array DiagTreeSlideResource -> Array DiagTreeSlideResource -> Boolean
eqDiagTreeSlideResources a b =
  length a == length b &&

  let reducer _ (Tuple xa xb) =
        if eqDiagTreeSlideResource xa xb then Just true else Nothing

   in zip a b # foldM reducer true # fromMaybe false


getDiagTreeSlideResourcePath :: String -> String
getDiagTreeSlideResourcePath = ("/s/fileupload/attachment/" <> _)


-- Keep in mind that in this `Ish` version we do not checking recursively all of
-- the children slides, only checking id of own children.
eqIshDiagTreeSlideAnswer
  :: DiagTreeSlideAnswer -> DiagTreeSlideAnswer -> Boolean

eqIshDiagTreeSlideAnswer a b =
  a.header == b.header &&
  a.text == b.text &&
  a.file == b.file &&
  slideIdLens a.nextSlide == slideIdLens b.nextSlide

  where
    slideIdLens (DiagTreeSlide x) = x.id

eqIshDiagTreeSlideAnswers
  :: Map DiagTreeSlideId DiagTreeSlideAnswer
  -> Map DiagTreeSlideId DiagTreeSlideAnswer
  -> Boolean

eqIshDiagTreeSlideAnswers a b =
  Map.size a == Map.size b &&

  let reducer _ (Tuple xa xb) =
        if eqIshDiagTreeSlideAnswer xa xb then Just true else Nothing

   in zip (fromFoldable a) (fromFoldable b)
    # foldM reducer true
    # fromMaybe false


diagTreeSlideActionToBackend :: DiagTreeSlideAction -> BackendAction
diagTreeSlideActionToBackend Towage       = aTowage
diagTreeSlideActionToBackend BikeTowage   = aBikeTowage
diagTreeSlideActionToBackend Tech         = aTech
diagTreeSlideActionToBackend Consultation = aConsultation

diagTreeSlideActionFromBackend :: BackendAction -> Maybe DiagTreeSlideAction
diagTreeSlideActionFromBackend { label, service }
  | label   == aTowage.label &&
    service == aTowage.service = Just Towage

  | label   == aBikeTowage.label &&
    service == aBikeTowage.service = Just BikeTowage

  | label   == aTech.label &&
    service == aTech.service = Just Tech

  | label   == aConsultation.label &&
    service == aConsultation.service = Just Consultation

  | otherwise = Nothing

aTowage :: BackendAction
aTowage = { label: show Towage, service: "Towage" }

aBikeTowage :: BackendAction
aBikeTowage = { label: show BikeTowage, service: "BikeTowage" }

aTech :: BackendAction
aTech = { label: show Tech, service: "Tech" }

aConsultation :: BackendAction
aConsultation = { label: show Consultation, service: "Consultation" }

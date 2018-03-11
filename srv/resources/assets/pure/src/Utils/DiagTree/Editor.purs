module Utils.DiagTree.Editor
     ( getSlideByBranch
     , eqDiagTreeSlideResource
     , eqDiagTreeSlideResources
     , getDiagTreeSlideResourcePath
     , eqIshDiagTreeSlideAnswer
     , eqIshDiagTreeSlideAnswers
     , diagTreeSlideActionToBackend
     , diagTreeSlideActionFromBackend
     , uploadFile
     , dropzoneDefaultProps
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.Exception (message, stack)
import Control.Monad.Error.Class (catchError)

import Data.Functor (voidRight)
import Data.Tuple (Tuple (Tuple))
import Data.Array (uncons, length, zip, fromFoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldM)
import Data.Nullable (toNullable)
import Data.Maybe (Maybe (..), maybe, fromMaybe)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.MediaType.Common (applicationJSON)
import Data.Argonaut.Core as A

import DOM.File.Types (File)
import DOM.File.File as File
import DOM.XHR.FormData as FormData

import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)
import Network.HTTP.RequestHeader (RequestHeader (..))

import Utils.Affjax (postRequest)
import Bindings.ReactDropzone as ReactDropzone

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlides
     , DiagTreeSlideId
     , DiagTreeSlideResource
     , DiagTreeSlideAction (..)
     , DiagTreeSlideAnswer
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendResource
     ( BackendResourceAttachment
     , fromAttachment
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


getDiagTreeSlideResourcePath
  :: forall props. { id :: Int, filename :: String | props } -> String
getDiagTreeSlideResourcePath x =
  "/s/fileupload/attachment/" <> show x.id <> "/" <> x.filename


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


-- Uploading a file attaching it to a slide
uploadFile
  :: forall eff
   . DiagTreeSlideId
  -> File
  -> Aff ( ajax    :: AJAX
         , console :: CONSOLE
         | eff
         ) (Maybe BackendResourceAttachment)

uploadFile slideId file = flip catchError handleError $ do
  let url      = "/upload/DiagSlide/" <> show slideId <> "/files"
      fdFile   = FormData.FormDataFile (File.name file) file
      formData = FormData.toFormData [Tuple "file" fdFile]

  (res :: AffjaxResponse Foreign) <- affjax $
    postRequest url formData # _ { headers = [Accept applicationJSON] }

  let json       = unsafeFromForeign res.response :: A.Json
      attachment = fromAttachment json

  case attachment of
       Nothing -> logParseError
       _       -> pure attachment

  where
    handleError err = voidRight Nothing $ liftEff $ error $
      "Uploading file (" <> File.name file <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

    logParseError = voidRight Nothing $ liftEff $ error $
      "Parsing upload response of file (" <> File.name file <> ") failed!"


dropzoneDefaultProps :: ReactDropzone.Props () () () () () ()
dropzoneDefaultProps = ReactDropzone.dropzoneDefaultProps
  { accept   = toNullable $ Just "image/jpeg,image/png,image/svg+xml"
  , multiple = false

  , className         = setClassName id
  , activeClassName   = setClassName (_ <> "--active")
  , acceptClassName   = setClassName (_ <> "--accept")
  , rejectClassName   = setClassName (_ <> "--reject")
  , disabledClassName = setClassName (_ <> "--disabled")
  }

  where setClassName f = toNullable $ Just $ f ReactDropzone.dropzoneName

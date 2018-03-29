module Utils.DiagTree.Editor
     ( getSlideByBranch
     , eqDiagTreeSlideResource
     , eqDiagTreeSlideResources
     , getDiagTreeSlideAttachmentPath
     , eqIshDiagTreeSlideAnswer
     , eqIshDiagTreeSlideAnswers
     , diagTreeSlideActionToBackend
     , diagTreeSlideActionFromBackend
     , uploadFile
     , dropzoneDefaultProps
     , rejectedFilesAlert
     ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.Exception (message, stack)
import Control.Monad.Error.Class (catchError)

import Data.Functor (voidRight)
import Data.Tuple (Tuple (Tuple))
import Data.Array (index, uncons, length, zip)
import Data.Map as Map
import Data.Foldable (foldM, foldl)
import Data.Nullable (toNullable)
import Data.Maybe (Maybe (..), maybe, fromMaybe)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.MediaType.Common (applicationJSON)
import Data.Argonaut.Core as A

import DOM.HTML (window) as DOM
import DOM.HTML.Window (alert) as DOM
import DOM (DOM)
import DOM.HTML.Types (ALERT)
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
     , fromIndexedAnswers
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , BackendAttachmentMediaType (..)
     , fromBackendAttachment
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

  let f (DiagTreeSlide slide) slideId = do
        let Tuple answers indexes = fromIndexedAnswers slide.answers
        slideId `Map.lookup` indexes >>= index answers <#> _.nextSlide

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


getDiagTreeSlideAttachmentPath
  :: forall props. { id :: Int, filename :: String | props } -> String
getDiagTreeSlideAttachmentPath x =
  "/s/fileupload/attachment/" <> show x.id <> "/" <> x.filename


-- Keep in mind that in this `Ish` version we do not checking recursively all of
-- the children slides, only checking id of own children.
eqIshDiagTreeSlideAnswer
  :: DiagTreeSlideAnswer -> DiagTreeSlideAnswer -> Boolean

eqIshDiagTreeSlideAnswer a b =
  a.header                == b.header &&
  a.text                  == b.text &&
  a.attachment            == b.attachment &&
  slideIdLens a.nextSlide == slideIdLens b.nextSlide

  where
    slideIdLens (DiagTreeSlide x) = x.id

eqIshDiagTreeSlideAnswers
  :: Array DiagTreeSlideAnswer
  -> Array DiagTreeSlideAnswer
  -> Boolean

eqIshDiagTreeSlideAnswers a b =
  length a == length b &&

  let reducer _ (Tuple xa xb) =
        if eqIshDiagTreeSlideAnswer xa xb then Just true else Nothing

   in zip a b # foldM reducer true # fromMaybe false


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
         ) (Maybe BackendAttachment)

uploadFile slideId file = flip catchError handleError $ do
  let url      = "/upload/DiagSlide/" <> show slideId <> "/files"
      fdFile   = FormData.FormDataFile (File.name file) file
      formData = FormData.toFormData [Tuple "file" fdFile]

  (res :: AffjaxResponse Foreign) <- affjax $
    postRequest url formData # _ { headers = [Accept applicationJSON] }

  let json       = unsafeFromForeign res.response :: A.Json
      attachment = fromBackendAttachment json

  case attachment of
       Nothing -> logParseError
       _       -> pure attachment

  where
    handleError err = voidRight Nothing $ liftEff $ error $
      "Uploading file (" <> File.name file <> ") failed: " <> message err
      # \x -> maybe x (\y -> x <> "\nStack trace:\n" <> y) (stack err)

    logParseError = voidRight Nothing $ liftEff $ error $
      "Parsing upload response of file (" <> File.name file <> ") failed!"


dropzoneDefaultProps
  :: BackendAttachmentMediaType -> ReactDropzone.Props () () () () () ()

dropzoneDefaultProps mediaType = ReactDropzone.dropzoneDefaultProps
  { accept = toNullable $ Just
      case mediaType of
           ImageMediaType -> "image/jpeg, image/png, image/svg+xml"
           AudioMediaType -> "audio/mpeg, audio/ogg, audio/wav"

           VideoMediaType ->
             "video/mp4, application/mp4,\
             \ video/ogg, application/ogg,\
             \ video/webm"

  , multiple          = false
  , className         = setClassName id
  , activeClassName   = setClassName (_ <> "--active")
  , acceptClassName   = setClassName (_ <> "--accept")
  , rejectClassName   = setClassName (_ <> "--reject")
  , disabledClassName = setClassName (_ <> "--disabled")
  }

  where setClassName f = toNullable $ Just $ f ReactDropzone.dropzoneName


rejectedFilesAlert
  :: forall eff. Array File -> Eff (dom :: DOM, alert :: ALERT | eff) Unit

rejectedFilesAlert files = DOM.window >>= DOM.alert message
  where
    fileReducer acc file = acc <> "\n  • \"" <> File.name file <> "\""

    message =
      "Допустимые расширения загружаемых файлов:"
        <> "\n  • Для картинок: .jpg, .jpeg, .png, .svg"
        <> "\n  • Для видеофайлов: .mp4, .ogv, .webm"
        <> "\n  • Для аудиофайлов: .mp3, .ogg, .wav"
        <> "\n\nСледующие файлы не могут быть загружены:"
        <> foldl fileReducer "" files

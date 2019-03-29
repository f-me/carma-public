module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment.Types
     ( BackendAttachmentMediaType (..)
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.String.Read (class Read)
import Data.Enum (class Enum, class BoundedEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)

import Data.Generic.Rep.Enum
     ( genericPred
     , genericSucc
     , genericCardinality
     , genericToEnum
     , genericFromEnum
     )

import Utils.ShowCase (class ShowCase)
import Utils.Sex (class GetSex, Sex (..))


data BackendAttachmentMediaType
   = ImageMediaType
   | VideoMediaType
   | AudioMediaType

derive instance eqBackendAttachmentMediaType :: Eq BackendAttachmentMediaType
derive instance ordBackendAttachmentMediaType :: Ord BackendAttachmentMediaType

derive instance genericBackendAttachmentMediaType ::
  Generic BackendAttachmentMediaType _

instance genericBoundedBackendAttachmentMediaType
  :: Bounded BackendAttachmentMediaType
  where
  bottom = genericBottom
  top    = genericTop

instance genericEnumBackendAttachmentMediaType
  :: Enum BackendAttachmentMediaType
  where
  pred = genericPred
  succ = genericSucc

instance genericBoundedEnumBackendAttachmentMediaType ::
  BoundedEnum BackendAttachmentMediaType
  where
  cardinality = genericCardinality
  toEnum      = genericToEnum
  fromEnum    = genericFromEnum

instance showBackendAttachmentMediaType :: Show BackendAttachmentMediaType where
  show ImageMediaType = "image"
  show VideoMediaType = "video"
  show AudioMediaType = "audio"

instance readBackendAttachmentMediaType :: Read BackendAttachmentMediaType where
  read "image" = Just ImageMediaType
  read "video" = Just VideoMediaType
  read "audio" = Just AudioMediaType
  read _       = Nothing

instance showCaseBackendAttachmentMediaType
  :: ShowCase BackendAttachmentMediaType
  where

  showNominative  ImageMediaType = "картинка"
  showNominative  VideoMediaType = "видеофайл"
  showNominative  AudioMediaType = "аудиофайл"

  showGenitive    ImageMediaType = "картинки"
  showGenitive    VideoMediaType = "видеофайла"
  showGenitive    AudioMediaType = "аудиофайла"

  showDative      ImageMediaType = "картинке"
  showDative      VideoMediaType = "видеофайлу"
  showDative      AudioMediaType = "аудиофайлу"

  showAccusative  ImageMediaType = "картинку"
  showAccusative  VideoMediaType = "видеофайл"
  showAccusative  AudioMediaType = "аудиофайл"

  showAblative    ImageMediaType = "картинкой"
  showAblative    VideoMediaType = "видеофайлом"
  showAblative    AudioMediaType = "аудиофайлом"

  showPrepositive ImageMediaType = "картинке"
  showPrepositive VideoMediaType = "видеофайле"
  showPrepositive AudioMediaType = "аудиофайле"

instance getSexBackendAttachmentMediaType :: GetSex BackendAttachmentMediaType
  where
  getSex ImageMediaType = FemaleSex
  getSex VideoMediaType = MaleSex
  getSex AudioMediaType = MaleSex

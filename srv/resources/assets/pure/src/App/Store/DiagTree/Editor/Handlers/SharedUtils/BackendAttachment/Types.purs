module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment.Types
     ( BackendAttachmentMediaType (..)
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.String.Read (class Read)
import Data.Generic (class Generic)
import Data.Enum (class Enum, class BoundedEnum)
import Data.Generic.Rep as GRep
import Data.Generic.Rep.Bounded as GRepBounded
import Data.Generic.Rep.Enum as GRepEnum

import Utils.ShowCase (class ShowCase)
import Utils.Sex (class GetSex, Sex (..))


data BackendAttachmentMediaType
  = ImageMediaType
  | VideoMediaType
  | AudioMediaType

derive instance genericBackendAttachmentMediaType
  :: Generic BackendAttachmentMediaType

derive instance eqBackendAttachmentMediaType
  :: Eq      BackendAttachmentMediaType

derive instance ordBackendAttachmentMediaType
  :: Ord     BackendAttachmentMediaType

derive instance genericRepBackendAttachmentMediaType ::
  GRep.Generic BackendAttachmentMediaType _

instance genericRepBoundedBackendAttachmentMediaType
  :: Bounded BackendAttachmentMediaType
  where
  bottom = GRepBounded.genericBottom
  top    = GRepBounded.genericTop

instance genericRepEnumBackendAttachmentMediaType
  :: Enum BackendAttachmentMediaType
  where
  pred = GRepEnum.genericPred
  succ = GRepEnum.genericSucc

instance genericRepBoundedEnumBackendAttachmentMediaType ::
  BoundedEnum BackendAttachmentMediaType
  where
  cardinality = GRepEnum.genericCardinality
  toEnum      = GRepEnum.genericToEnum
  fromEnum    = GRepEnum.genericFromEnum

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

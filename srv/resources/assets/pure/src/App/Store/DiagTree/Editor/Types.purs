module App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlides
     , DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideResource
     , DiagTreeSlideAction (..)
     , BackendAction
     , BackendActionFields
     , diagTreeSlideActionToBackend
     , diagTreeSlideActionFromBackend
     , DiagTreeSlideAnswer
     ) where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe (..))
import Data.DateTime (DateTime)
import Data.Generic (class Generic, gShow)
import Data.Enum (class Enum, class BoundedEnum)

import Data.Generic.Rep as GRep
import Data.Generic.Rep.Bounded as GRepBounded
import Data.Generic.Rep.Enum as GRepEnum


type DiagTreeSlideId = Int
type DiagTreeSlides  = Map DiagTreeSlideId DiagTreeSlide


type DiagTreeSlideResource =
  { text :: String
  , file :: String
  }


type BackendActionFields = (label :: String, service :: String)
type BackendAction = Record BackendActionFields

data DiagTreeSlideAction
  = Towage
  | BikeTowage
  | Tech
  | Consultation

derive instance genericDiagTreeSlideAction :: Generic DiagTreeSlideAction
derive instance eqDiagTreeSlideAction      :: Eq      DiagTreeSlideAction
derive instance ordDiagTreeSlideAction     :: Ord     DiagTreeSlideAction

instance showDiagTreeSlideAction :: Show DiagTreeSlideAction where
  show = gShow

derive instance genericRepDiagTreeSlideAction ::
  GRep.Generic DiagTreeSlideAction _

instance genericRepBoundedDiagTreeSlideAction :: Bounded DiagTreeSlideAction
  where
  bottom = GRepBounded.genericBottom
  top = GRepBounded.genericTop

instance genericRepEnumDiagTreeSlideAction :: Enum DiagTreeSlideAction where
  pred = GRepEnum.genericPred
  succ = GRepEnum.genericSucc

instance genericRepBoundedEnumDiagTreeSlideAction ::
  BoundedEnum DiagTreeSlideAction
  where
  cardinality = GRepEnum.genericCardinality
  toEnum      = GRepEnum.genericToEnum
  fromEnum    = GRepEnum.genericFromEnum

aTowage :: BackendAction
aTowage = { label: "Создать Эвакуацию", service: "Towage" }

aBikeTowage :: BackendAction
aBikeTowage = { label: "Создать Мотоэвакуация", service: "BikeTowage" }

aTech :: BackendAction
aTech = { label: "Создать Техпомощь", service: "Tech" }

aConsultation :: BackendAction
aConsultation = { label: "Создать Консультацию", service: "Consultation" }

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


type DiagTreeSlideAnswer =
  { nextSlide :: DiagTreeSlide

  , header    :: String
  -- ^ Also known as "answer" of a slide
  --   (a user's answer that leads to this slide - `nextSlide`).

  , text      :: String
  , file      :: Maybe String
  }


-- `newtype` is required here to allow recursive types
newtype DiagTreeSlide
  = DiagTreeSlide
  { id        :: DiagTreeSlideId
  , isRoot    :: Boolean
  , ctime     :: DateTime
  , header    :: String -- ^ Also known as "question" of a slide
  , body      :: String
  , resources :: Array DiagTreeSlideResource
  , action    :: Maybe DiagTreeSlideAction

  , answers   :: Map DiagTreeSlideId DiagTreeSlideAnswer
  -- ^ You could read "answers" here as "children slides"
  }

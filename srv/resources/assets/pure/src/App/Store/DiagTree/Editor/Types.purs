module App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlides
     , DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideResource
     , DiagTreeSlideAttachment (..)
     , DiagTreeSlideAction (..)
     , DiagTreeSlideAnswer
     , IndexedAnswers
     , toIndexedAnswers
     , fromIndexedAnswers
     ) where

import Prelude

import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)
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

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachmentMediaType
     )


type DiagTreeSlideId = Int
type DiagTreeSlides  = Map DiagTreeSlideId DiagTreeSlide


data DiagTreeSlideAttachment
  = Legacy String
  -- ^ This is legacy field with inlined uploaded file as a string of base64
  --   (this is only for old uploads).
  --   TODO Get rid of this field, write some migration, and then type
  --   `DiagTreeSlideAttachment` must be removed too, only record from
  --   `Modern` must be type of `attachment` field of `DiagTreeSlideResource`.

  | Modern { id        :: Int
           , hash      :: String
           , filename  :: String
           , mediaType :: BackendAttachmentMediaType
           }

instance eqDiagTreeSlideAttachment :: Eq DiagTreeSlideAttachment
  where

  eq (Modern a) (Modern b) =
    a.id        == b.id       &&
    a.hash      == b.hash     &&
    a.filename  == b.filename &&
    a.mediaType == b.mediaType

  eq (Legacy a) (Legacy b) = a == b
  eq _ _ = false

type DiagTreeSlideResource =
  { text       :: String
  , attachment :: DiagTreeSlideAttachment
  }


data DiagTreeSlideAction
  = Towage
  | BikeTowage
  | Tech
  | Consultation

derive instance genericDiagTreeSlideAction :: Generic DiagTreeSlideAction _
derive instance eqDiagTreeSlideAction      :: Eq      DiagTreeSlideAction
derive instance ordDiagTreeSlideAction     :: Ord     DiagTreeSlideAction

instance showDiagTreeSlideAction :: Show DiagTreeSlideAction where
  show Towage       = "Создать Эвакуацию"
  show BikeTowage   = "Создать Мотоэвакуация"
  show Tech         = "Создать Техпомощь"
  show Consultation = "Создать Консультацию"

instance genericBoundedDiagTreeSlideAction :: Bounded DiagTreeSlideAction where
  bottom = genericBottom
  top    = genericTop

instance genericEnumDiagTreeSlideAction :: Enum DiagTreeSlideAction where
  pred = genericPred
  succ = genericSucc

instance genericBoundedEnumDiagTreeSlideAction ::
  BoundedEnum DiagTreeSlideAction
  where
  cardinality = genericCardinality
  toEnum      = genericToEnum
  fromEnum    = genericFromEnum


type DiagTreeSlideAnswer =
  { nextSlide  :: DiagTreeSlide

  , header     :: String
    -- ^ Also known as "answer" of a slide
    --   (a user's answer that leads to this slide - `nextSlide`).

  , text       :: String
  , attachment :: Maybe DiagTreeSlideAttachment
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

  , answers   :: IndexedAnswers -- ^ You could read "answers" here
                                --   kinda as "children slides".
                                --   It is a list of answer data which
                                --   references to some child slide.
  }


-- Constructor isn't supposed to be exported, the main reason why this type
-- exists is to prevent broken `Map` of indexes, so, everytime we get new list
-- of answers new `Map` of indexes must be created. By this type we're avoiding
-- errors caused by human factor.
data IndexedAnswers =
  IndexedAnswers (Array DiagTreeSlideAnswer) (Map DiagTreeSlideId Int)

toIndexedAnswers :: Array DiagTreeSlideAnswer -> IndexedAnswers
toIndexedAnswers answers = IndexedAnswers answers indexes
  where
    indexes = snd $ foldl reducer (Tuple 0 Map.empty) answers
    reducer (Tuple n acc) { nextSlide: DiagTreeSlide x } =
      Tuple (n + 1) $ acc # x.id `Map.insert` n

fromIndexedAnswers
  :: IndexedAnswers
  -> Tuple (Array DiagTreeSlideAnswer) (Map DiagTreeSlideId Int)

fromIndexedAnswers (IndexedAnswers x y) = Tuple x y

module App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlides
     , DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideResource
     , DiagTreeSlideAnswer
     , DiagTreeSlideAction
     ) where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)


type DiagTreeSlideId = Int
type DiagTreeSlides  = Map DiagTreeSlideId DiagTreeSlide

type DiagTreeSlideResource =
  { file :: String
  , text :: String
  }

type DiagTreeSlideAnswer =
  { nextSlide :: DiagTreeSlide
  , header    :: String
  , text      :: String
  , file      :: Maybe String -- Nullable
  }

type DiagTreeSlideAction =
  { label :: String
  , svc   :: String -- Service model name
  }

-- `newtype` is required here to allow recursive types
newtype DiagTreeSlide
  = DiagTreeSlide
  { id        :: DiagTreeSlideId
  , isRoot    :: Boolean
  , ctime     :: DateTime
  , header    :: String
  , body      :: String
  , resources :: Array DiagTreeSlideResource
  , answers   :: Array DiagTreeSlideAnswer
  , actions   :: Array DiagTreeSlideAction
  }

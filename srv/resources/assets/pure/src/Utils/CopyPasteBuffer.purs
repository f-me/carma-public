module Utils.CopyPasteBuffer
       ( CopyPasteBuffer
       , CopyPasteBufferState (..)
       , getCopyPasteState
       ) where

import Prelude

import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)
import Data.Array (last)
import Data.Maybe (Maybe (..), fromJust)
import Partial.Unsafe (unsafePartial)


type CopyPasteBuffer =
  { isProcessing :: Boolean
  , isFailed     :: Boolean
  , branch       :: Maybe (Array DiagTreeSlideId)
  , cutting      :: Boolean
  }


data CopyPasteBufferState
  = Copied Int
  | Cutout Int
  | EmptyBuffer

derive instance eqCopyPasteBufferState :: Eq CopyPasteBufferState


getCopyPasteState :: CopyPasteBuffer -> CopyPasteBufferState
getCopyPasteState buffer =
  case buffer.branch, buffer.cutting of
    Just ids, cutting ->
      if cutting
         then Cutout $ unsafePartial $ fromJust $ last ids
         else Copied $ unsafePartial $ fromJust $ last ids
    Nothing, _ -> EmptyBuffer

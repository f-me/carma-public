module App.Store.DiagTree.Editor.Handlers.SharedUtils
     ( BackendAnswer
     , fromBackendAnswer
     , toBackendAnswer
     ) where

import Prelude

import Data.Int (fromNumber, toNumber)
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe (..), maybe)
import Data.StrMap as StrMap
import Data.Argonaut.Core as A

import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)


type BackendAnswer =
  { nextSlide :: DiagTreeSlideId
  , header    :: String
  , text      :: String
  , file      :: Maybe String -- could be not set or `null`
  }


toBackendAnswer :: BackendAnswer -> A.Json
toBackendAnswer x = A.fromObject $ StrMap.fromFoldable
  [ Tuple "nextSlide" $ A.fromNumber $ toNumber x.nextSlide
  , Tuple "header"    $ A.fromString x.header
  , Tuple "text"      $ A.fromString x.text
  , Tuple "file"      $ maybe A.jsonNull A.fromString x.file
  ]


fromBackendAnswer :: A.Json -> Maybe BackendAnswer
fromBackendAnswer json = do
  obj <- A.toObject json

  nextSlide <- StrMap.lookup "nextSlide" obj >>= A.toNumber >>= fromNumber
  header    <- StrMap.lookup "header"    obj >>= A.toString
  text      <- StrMap.lookup "text"      obj >>= A.toString

  file <-
    case StrMap.lookup "file" obj of
         Nothing  -> pure Nothing  -- Field is not set (that's okay)
         Just raw -> if A.isNull raw -- Field is set to `null`
                        then pure Nothing
                        else A.toString raw <#> Just

  pure { nextSlide, header, text, file }

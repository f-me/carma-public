module Utils.Affjax
     ( defaultHeaders
     , getRequest
     , putRequest
     ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Either (Either (Left))
import Data.HTTP.Method (Method (GET, PUT))
import Data.MediaType.Common (applicationJSON)

import Network.HTTP.Affjax as Affjax
import Network.HTTP.RequestHeader (RequestHeader (..))


defaultHeaders :: Array RequestHeader
defaultHeaders =
  [ Accept      applicationJSON
  , ContentType applicationJSON
  ]


getRequest
  :: Affjax.URL -> Affjax.AffjaxRequest Unit
getRequest url = Affjax.defaultRequest
  { url     = url
  , method  = Left GET
  , headers = defaultHeaders
  }


putRequest
  :: Affjax.URL -> Affjax.AffjaxRequest Unit
putRequest url = Affjax.defaultRequest
  { url     = url
  , method  = Left PUT
  , headers = defaultHeaders
  }

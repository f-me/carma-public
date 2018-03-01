module Utils.Affjax
     ( defaultHeaders
     , getRequest
     , putRequest
     ) where

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
  :: forall content. Affjax.URL -> Maybe content -> Affjax.AffjaxRequest content
getRequest url content = Affjax.defaultRequest
  { url     = url
  , method  = Left GET
  , headers = defaultHeaders
  , content = content
  }


putRequest
  :: forall content. Affjax.URL -> Maybe content -> Affjax.AffjaxRequest content
putRequest url content = Affjax.defaultRequest
  { url     = url
  , method  = Left PUT
  , headers = defaultHeaders
  , content = content
  }

module Utils.Affjax
     ( defaultHeaders
     , getRequest
     , putRequest
     , postRequest
     ) where

import Prelude

import Data.Maybe (Maybe (Just))
import Data.Either (Either (Left))
import Data.HTTP.Method (Method (GET, PUT, POST))
import Data.MediaType.Common (applicationJSON)

import Network.HTTP.Affjax as Affjax
import Network.HTTP.RequestHeader (RequestHeader (..))


defaultHeaders :: Array RequestHeader
defaultHeaders =
  [ ContentType applicationJSON
  , Accept      applicationJSON
  ]


getRequest
  :: Affjax.URL -> Affjax.AffjaxRequest Unit
getRequest url = Affjax.defaultRequest
  { url     = url
  , method  = Left GET
  , headers = defaultHeaders
  }


putRequest
  :: forall content. Affjax.URL -> content -> Affjax.AffjaxRequest content
putRequest url content = Affjax.defaultRequest
  { url     = url
  , method  = Left PUT
  , headers = defaultHeaders
  , content = Just content
  }


postRequest
  :: forall content. Affjax.URL -> content -> Affjax.AffjaxRequest content
postRequest url content = Affjax.defaultRequest
  { url     = url
  , method  = Left POST
  , headers = defaultHeaders
  , content = Just content
  }

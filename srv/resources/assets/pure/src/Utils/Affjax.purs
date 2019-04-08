module Utils.Affjax
     ( defaultHeaders
     , getRequest
     , putRequest
     , postRequest
     ) where

import Data.Maybe (Maybe (Just))
import Data.Either (Either (Left))
import Data.HTTP.Method (Method (GET, PUT, POST))
import Data.MediaType.Common (applicationJSON)

import Affjax (URL, Request, defaultRequest)
import Affjax.RequestHeader (RequestHeader (..))
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.RequestBody (RequestBody)


defaultHeaders :: Array RequestHeader
defaultHeaders =
  [ ContentType applicationJSON
  , Accept      applicationJSON
  ]


getRequest :: forall a. URL -> ResponseFormat a -> Request a
getRequest url fmt = defaultRequest
  { url            = url
  , method         = Left GET
  , headers        = defaultHeaders
  , responseFormat = fmt
  }


putRequest :: forall a. URL -> RequestBody -> ResponseFormat a -> Request a
putRequest url content fmt = defaultRequest
  { url            = url
  , method         = Left PUT
  , headers        = defaultHeaders
  , content        = Just content
  , responseFormat = fmt
  }


postRequest :: forall a. URL -> RequestBody -> ResponseFormat a -> Request a
postRequest url content fmt = defaultRequest
  { url            = url
  , method         = Left POST
  , headers        = defaultHeaders
  , content        = Just content
  , responseFormat = fmt
  }

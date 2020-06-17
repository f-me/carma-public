module Carma.HTTP.Util
    ( mkRequestWithBody
    )

where

import Network.HTTP
import Network.URI (parseURI)


-- | Construct an HTTP request, possibly with body.
--
-- Derived from 'postRequestWithBody' from HTTP package.
mkRequestWithBody :: String
                  -- ^ URL string.
                  -> RequestMethod
                  -> Maybe (String, String)
                  -- ^ Content-type header value and request body.
                  -> [Header]
                  -> Request_String
mkRequestWithBody urlString method payload headers =
  case parseURI urlString of
    Nothing -> error ("mkRequestWithBody: Not a valid URL - " ++ urlString)
    Just u  -> case payload of
                 Just tb -> setRequestBody rq tb
                 Nothing -> rq
                 where
                   rq = Request u method headers ""

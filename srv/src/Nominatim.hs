{-# LANGUAGE OverloadedStrings #-}

module Nominatim (geodecode) where

import Network.HTTP
import Network.URI
import qualified Data.ByteString.Char8 as B


nominatimURI :: URI
nominatimURI = URI
  {uriScheme    = "http:"
  ,uriAuthority
    = Just $ URIAuth "" "nominatim.openstreetmap.org" ""
  ,uriPath      = "/search"
  ,uriQuery     = "?format=json&countrycodes=ru&limit=1"
  ,uriFragment  = ""
  }


geodecode :: B.ByteString -> IO B.ByteString
geodecode addr
  = simpleHTTP (Request uri GET [] "")
  >>= return . either (const "[]") rspBody
  where
    addrStr = urlEncode $ B.unpack addr
    uri = nominatimURI
      {uriQuery = uriQuery nominatimURI ++ "&q=" ++ addrStr}

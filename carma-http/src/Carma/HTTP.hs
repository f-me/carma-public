{-# LANGUAGE OverloadedStrings #-}

{-|

  Interface to HTTP API for CRUD operations on model instances as
  provided by CaRMa server running on localhost.

-}

module Carma.HTTP
    ( FieldName
    , FieldValue
    , InstanceData
    , modelRequest
    , instanceExists
    )

where

import Data.Aeson
import Data.Functor
import Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Network.HTTP

import Carma.HTTP.Util


type FieldValue = BS.ByteString

type FieldName = BS.ByteString

-- | An instance of a model is a set of key-value pairs.
type InstanceData = M.Map FieldName FieldValue


-- | CaRMa JSON response containing "id" field. The rest of fields are
-- ignored.
--
-- TODO: carma-mobile-server contains this code too.
newtype IdResponse = IdResponse Int deriving Show

instance FromJSON IdResponse where
    parseJSON (Object v) = IdResponse . read <$> v .: "id"
    parseJSON _          = error "Malformed CaRMa response"


-- | Model API endpoint.
modelURI :: Int
           -- ^ CaRMa port.
         -> String
         -- ^ Model name.
         -> String
modelURI cp model = concat ["http://localhost:", show cp, "/_/", model, "/"]


-- | Model read/update/delete API endpoint.
modelPidURI :: Int -> String -> Int -> String
modelPidURI cp model pid = (modelURI cp model) ++ (show pid)


-- | Send request to c/r/u/d an instance of model, possibly using
-- model data, and return its id.
modelRequest :: Int
             -- ^ CaRMa port.
             -> String
             -- ^ Model name.
             -> Maybe Int
             -- ^ Model id.
             -> RequestMethod
             -> Maybe InstanceData
             -- ^ Request payload.
             -> IO Int
modelRequest cp model pid rm row = do
  let uri =
          case pid of
            Just n  -> modelPidURI cp model n
            Nothing -> modelURI cp model
  rs <- simpleHTTP $
        case row of
          Just payload ->
              mkRequestWithBody uri rm $
              Just ("application/json", BSL.unpack $ encode payload)
          Nothing -> mkRequestWithBody uri rm Nothing
  rsBody <- getResponseBody rs
  let Just (IdResponse carmaPid) =
          case pid of
            Just n -> Just (IdResponse n)
            Nothing ->
                case decode' (BSL.pack rsBody) of
                  Nothing -> error "CaRMa response contains no id field"
                  justIdr -> justIdr
  return carmaPid


-- | Check if instance exists in the CaRMa database.
instanceExists :: Int
               -- ^ CaRMa port.
               -> String
               -- ^ Model name.
               -> Int
               -- ^ Instance id.
               -> IO Bool
instanceExists cp modelName pid = do
  rs <- simpleHTTP $ getRequest $ modelPidURI cp modelName pid
  code <- getResponseCode rs
  return $
   case code of
     (2, 0, 0) -> True
     (4, 0, 4) -> False
     _ -> error "Unexpected CaRMa response"

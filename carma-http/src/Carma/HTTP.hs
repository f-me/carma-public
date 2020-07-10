{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|

HTTP API using DbLayer legacy types.

-}

module Carma.HTTP
    (
      FieldName
    , FieldValue
    , InstanceData

    -- * CaRMa API monad
    , CarmaOptions(..)
    , defaultCarmaOptions
    , CarmaIO
    , runCarma

    -- * CRUD operations
    , instanceRequest
    , createInstance
    , readInstance
    , updateInstance
    , deleteInstance
    , instanceExists

    -- * Parsing reference lists
    , read1Reference
    , readReferences

    -- * Auxiliary methods
    , methodURI
    , readDictionary
    , manyFieldDivisor
    -- ** Dict-objects JSON helpers
    , getKeyedJsonValue
    , getAllKeyedJsonValues
    )

where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import           Data.Aeson                 hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B8L
import           Data.Dict
import           Data.Functor
import           Data.HashMap.Strict        as M hiding (filter, mapMaybe)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Read             as T

import           Network.HTTP hiding (getHeaders)

import           Carma.HTTP.Base
import           Carma.HTTP.Util


type FieldValue = Text

type FieldName = Text

-- | An instance of a model is a set of key-value pairs.
type InstanceData = M.HashMap FieldName FieldValue

-- | Send request to c/r/u/d an instance of model, possibly using new
-- instance data. Return id and instance data from server response.
instanceRequest :: String
                -- ^ Model name.
                -> Maybe Int
                -- ^ Model id.
                -> RequestMethod
                -> Maybe InstanceData
                -- ^ Request payload.
                -> CarmaIO (Int, Maybe InstanceData)
instanceRequest model rid rm row = do
  cp <- getPort
  headers <- getHeaders
  let uri =
          case rid of
            Just n  -> modelPidURI cp model n
            Nothing -> modelURI cp model
  s <- asks snd
  rs <- liftIO $ sendHTTP s $
        case row of
          Just payload ->
              mkRequestWithBody uri rm
                (Just ("application/json; charset=utf-8", B8L.unpack $ encode payload)) headers
          Nothing -> mkRequestWithBody uri rm Nothing headers
  inst <- liftIO $ (decode' . B8L.pack) <$> getResponseBody rs
  return $ case rid of
    -- We already know id
    Just n -> (n, inst)
    -- If no id provided, then we expect server to give us one
    Nothing ->
        -- Try to parse instance data from server response
        case inst of
          Just d -> case M.lookup "id" d of
                      Just carmaId ->
                          case T.decimal carmaId of
                            Right (n, _) -> (n, Just d)
                            Left _     ->
                                error "Could not read id from CaRMa response"
                      Nothing -> error "CaRMa response contains no id field"
          -- Fail if no id provided and could not read response
          Nothing -> error "Could not read CaRMa response"


-- | Wrapper for 'instanceRequest' which requires non-Nothing server
-- response.
requireValidResponse :: Monad m =>
                        (Int, Maybe InstanceData)
                     -> m (Int, InstanceData)
requireValidResponse (cid, rs) =
    case rs of
      Just d -> return (cid, d)
      Nothing -> error "No valid CaRMa response"


createInstance :: String -> InstanceData -> CarmaIO (Int, InstanceData)
createInstance model row =
    instanceRequest model Nothing POST (Just row)
                        >>= requireValidResponse


readInstance :: String -> Int -> CarmaIO InstanceData
readInstance model rid =
    snd <$> (instanceRequest model (Just rid) GET Nothing
                                 >>= requireValidResponse)


updateInstance :: String -> Int -> InstanceData -> CarmaIO InstanceData
updateInstance model rid row =
    snd <$> (instanceRequest model (Just rid) PUT (Just row)
                        >>= requireValidResponse)


deleteInstance :: String -> Int -> CarmaIO ()
deleteInstance model rid =
    void $ instanceRequest model (Just rid) DELETE Nothing


-- | Check if instance exists in the CaRMa database.
instanceExists :: String
               -- ^ Model name.
               -> Int
               -- ^ Instance id.
               -> CarmaIO Bool
instanceExists modelName rid = do
  cp <- getPort
  rs <- sendRequest $ getRequest $ modelPidURI cp modelName rid
  code <- liftIO $ getResponseCode rs
  return $
   case code of
     (2, 0, 0) -> True
     (4, 0, 4) -> False
     _ -> error "Unexpected CaRMa response"


-- | Separates individual values in dictionary-many fields and
-- reference list fields. @Text.split manyFieldDivisor@ may be applied
-- to a 'FieldValue' to obtain the corresponding list.
manyFieldDivisor :: Char
manyFieldDivisor = ','


-- | Read reference of format @foo:32@ into model name and id.
read1Reference :: FieldValue -> Maybe (String, Int)
read1Reference val =
    case T.split (== ':') val of
      [ref, sid] ->
          case T.decimal sid of
            Right (n, _) -> Just (T.unpack ref, n)
            Left _ -> Nothing
      _ -> Nothing


-- | Read input of format @foo:32,bar:48@ into list of model names and
-- ids. Invalid references are ignored.
readReferences :: FieldValue -> [(String, Int)]
readReferences refs =
    mapMaybe read1Reference (T.split (== manyFieldDivisor) refs)


-- | Load a dictionary with given name from CaRMa.
readDictionary :: String
               -- ^ Dictionary name.
               -> CarmaIO (Maybe Dict)
readDictionary name = do
  uri <- methodURI "cfg/dictionaries"
  rs <- sendRequest $ getRequest uri
  rsb <- liftIO $ getResponseBody rs
  -- Read server response into @HashMap String Value@, since
  -- carma-dict does not support multi-level dictionaries yet
  let dicts = decode' $ B8L.pack rsb :: Maybe (M.HashMap String Value)
  return $ case M.lookup name <$> dicts of
    Just (Just v) -> decode' $ encode v
    _             -> Nothing


type JsonDictObjects = [M.HashMap FieldName FieldValue]


-- | Extract value of the first object from "dict-objects"-field JSON
-- contents with matching "key". If no such entries found, return
-- Nothing.
getKeyedJsonValue :: FieldValue
                  -- ^ Contents of JSON field using dict-objects
                  -- schema.
                  -> FieldValue
                  -- ^ Key.
                  -> Maybe FieldValue
getKeyedJsonValue field key =
  case getAllKeyedJsonValues field key of
    (e:_) -> Just e
    []    -> Nothing


-- | Extract values of all objects from "dict-objects"-field JSON
-- contents with matching keys.
getAllKeyedJsonValues :: FieldValue
                      -> FieldValue
                      -> [FieldValue]
getAllKeyedJsonValues field key =
  mapMaybe (M.lookup "value") $
  filter (\o -> fromMaybe "" (M.lookup "key" o) == key)
  (fromMaybe [] $ decode' $ B8L.fromStrict $ T.encodeUtf8 field :: JsonDictObjects)

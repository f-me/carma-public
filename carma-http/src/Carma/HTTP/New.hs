{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

HTTP API using carma-models types.

-}

module Carma.HTTP.New
    (
      -- * CRUD
      createInstance
    , readInstance
    , updateInstance
    , deleteInstance
    , instanceExists

      -- * Dictionary handling
    , readDictionary

      -- * JSON dict-objects
    , setKeyedJsonValue
    )

where

import           Control.Lens               hiding (createInstance)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import           Data.Aeson                 hiding (Result)
import           Data.Maybe
import           Data.HashMap.Strict        as M
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Functor
import qualified Data.Text                  as T
import qualified Data.Vector                as V

import           Network.HTTP

import           Data.Model
import           Data.Model.Patch           as Patch
import           Data.Dict.New

import           Carma.HTTP.Base
import           Carma.HTTP.Util


-- | Send request to c/r/u/d an instance of model, possibly using new
-- instance data. Return id and instance data from server response.
instanceRequest :: forall m. Model m =>
                   Maybe (IdentI m)
                -- ^ Model id.
                -> RequestMethod
                -> Maybe (Patch m)
                -- ^ Request payload.
                -> CarmaIO (IdentI m, Maybe (Patch m))
instanceRequest rid rm row = do
  cp <- getPort
  let model = T.unpack $ modelName (modelInfo :: ModelInfo m)
      idAcc = undefined :: (m -> PK Int m desc)
      uri =
        case rid of
          Just (Ident n)  -> modelPidURI cp model n
          Nothing -> modelURI cp model
  s <- asks snd
  rs <- liftIO $ sendHTTP s $
        case row of
          Just payload ->
              mkRequestWithBody uri rm $
              Just ("application/json", BSL.unpack $ encode payload)
          Nothing -> mkRequestWithBody uri rm Nothing
  inst <- liftIO $ (decode' . BSL.pack) <$> getResponseBody rs
  return $ case rid of
    -- We already know id
    Just n -> (n, inst)
    -- If no id provided, then we expect server to give us one
    Nothing ->
        -- Try to parse instance data from server response
        case inst of
          Just d -> case Patch.get d idAcc of
                      Just carmaId -> (carmaId, inst)
                      Nothing -> error "CaRMa response contains no id field"
          -- Fail if no id provided and could not read response
          Nothing -> error "Could not read CaRMa response"


-- | Wrapper for 'instanceRequest' which requires non-Nothing server
-- response.
requireValidResponse :: (Model m, Monad monad) =>
                        (IdentI m, Maybe (Patch m))
                     -> monad (IdentI m, Patch m)
requireValidResponse (cid, rs) =
    case rs of
      Just d -> return (cid, d)
      Nothing -> error "No valid CaRMa response"


createInstance :: Model m => Patch m -> CarmaIO (IdentI m, Patch m)
createInstance row =
  instanceRequest Nothing POST (Just row) >>= requireValidResponse


readInstance :: Model m => IdentI m -> CarmaIO (Patch m)
readInstance rid =
  snd <$> (instanceRequest (Just rid) GET Nothing >>=
           requireValidResponse)


updateInstance :: Model m => IdentI m -> Patch m -> CarmaIO (Patch m)
updateInstance rid row =
  snd <$> (instanceRequest (Just rid) PUT (Just row) >>=
           requireValidResponse)


deleteInstance :: Model m => IdentI m -> CarmaIO ()
deleteInstance rid =
  void $ instanceRequest (Just rid) DELETE Nothing


-- | Check if instance exists in the CaRMa database.
instanceExists :: forall m. Model m =>
                  IdentI m
               -- ^ Instance id.
               -> CarmaIO Bool
instanceExists (Ident rid) = do
  let model = T.unpack $ modelName (modelInfo :: ModelInfo m)
  cp <- getPort
  rs <- sendRequest $ getRequest $ modelPidURI cp model rid
  checkCode =<< liftIO (getResponseCode rs)


-- | Load a new-style dictionary with given name from CaRMa.
readDictionary :: String
               -- ^ Dictionary name.
               -> CarmaIO (Maybe PreDict)
readDictionary name = do
  uri <- methodURI $ "_/" ++ name
  rs <- sendRequest $ getRequest uri
  rsb <- liftIO $ getResponseBody rs
  return $ decode' $ BSL.pack rsb


-- | Set value of the first object from "dict-objects"-field JSON
-- contents with matching "key" (create it if no object matches key),
-- return new JSON string.
setKeyedJsonValue :: Value
                  -- ^ Contents of JSON field using dict-objects
                  -- schema.
                  -> T.Text
                  -- ^ Key.
                  -> Value
                  -- ^ Value.
                  -> Value
setKeyedJsonValue parsed key value =
  let
    newEntry :: Value
    newEntry = toJSON $
               M.fromList
               [ ("key" :: T.Text, String key)
               , ("value", value)
               ]
    keyPred (Object o) = fromMaybe "" (M.lookup "key" o) == String key
    keyPred _          = False
  in
    toJSON $
    case parsed of
      Array objs ->
        if V.null objs
        then (V.singleton newEntry)
        else
          case V.findIndex keyPred objs of
            Just iMatch -> objs & element iMatch .~ newEntry
            Nothing -> V.snoc objs newEntry
      _ -> V.singleton newEntry

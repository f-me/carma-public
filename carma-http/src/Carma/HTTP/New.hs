{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

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
     , searchByFilter

       -- * Dictionary handling
     , readDictionary

       -- * JSON dict-objects
     , setKeyedJsonValue

     , CarmaHTTPReqException (..)
     )

where

import           Data.Monoid
import           Data.Aeson                 hiding (Result)
import           Data.Maybe
import           Data.HashMap.Strict        as M
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.String
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Dynamic               as D
import qualified Data.Binary.Builder        as B
import           Data.Typeable

import           Control.Lens
import           Control.Monad (guard, foldM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Applicative ((<|>))
import           Control.Exception

import           Network.HTTP hiding (getHeaders)
import qualified Network.HTTP.Types         as H

import           Data.Model
import           Data.Model.Patch           as Patch
import           Data.Dict.New

import           Carma.HTTP.Base
import           Carma.HTTP.Util
import           Carma.Model (modelMap)


data CarmaHTTPReqException
   = CouldNotReadCaRMaResponse
   | NoIdFieldInCaRMaResponse
   | NoValidCaRMaResponse
   | UnexpectedDynamicType TypeRep
     deriving Eq

instance Show CarmaHTTPReqException where
  show = \case
    CouldNotReadCaRMaResponse -> "Could not read CaRMa response"
    NoIdFieldInCaRMaResponse  -> "CaRMa response contains no id field"
    NoValidCaRMaResponse      -> "No valid CaRMa response"
    UnexpectedDynamicType x   -> "Unexpected dynamic type: " <> show x

instance Exception CarmaHTTPReqException


-- | Send request to c/r/u/d an instance of model, possibly using new
--   instance data. Return id and instance data from server response.
--
-- @container@ - is a polymorphic list type.
instanceRequest
  :: forall m container itemParsed itemResult
   .
   ( Model m
   , Applicative container
   , Foldable container
   , Monoid (container itemResult)
   , Monoid (container itemParsed)
   , FromJSON (container itemParsed)
   , itemParsed ~ Patch m
   , itemResult ~ (IdentI m, Maybe (Patch m))
   )
  => Maybe (IdentI m) -- ^ Model id
  -> RequestMethod
  -> Maybe (Patch m)  -- ^ Request payload
  -> CarmaIO (container itemResult)

instanceRequest rid rm row = do
  cp <- getPort

  let model = T.unpack $ modelName (modelInfo :: ModelInfo m)
      idAcc = undefined :: (m -> PK Int m desc)

  let uri = go where
        go = uriWithoutQs <> f (guard (rm == GET) >> row)

        f Nothing = mempty
        f (Just (Patch.Patch p))
          = BSL.unpack
          $ B.toLazyByteString
          $ H.renderQueryText True
          $ M.toList
          $ fmap patchValueMapFn p

        -- | WARNING! You probably want to extend this list of possible types,
        --            this isn't a complete list of possible dynamic types.
        --
        -- This transforms any data type for a query string value.
        typeSet dyn
           =  fmap (fromString . show) (D.fromDynamic dyn :: Maybe Int)
          <|> fmap (fromString . show) (D.fromDynamic dyn :: Maybe Word)
          <|> fmap fromString (D.fromDynamic dyn :: Maybe String)
          <|> (D.fromDynamic dyn :: Maybe T.Text)
          <|> foldl1 (<|>) (modelMap $ resolveModel dyn)

        -- | Resolving identity of any model.
        resolveModel
          :: forall model. Model model => D.Dynamic -> model -> Maybe T.Text

        resolveModel dyn _ =
          fromString . show . identVal <$>
          (D.fromDynamic dyn :: Maybe (IdentI model))

        patchValueMapFn dyn =
          case typeSet dyn of
               Just x  -> Just (x :: T.Text)
               Nothing -> throw $ UnexpectedDynamicType $ D.dynTypeRep dyn

        uriWithoutQs =
          case rid of
               Just (Ident n) -> modelPidURI cp model n
               Nothing        -> modelURI cp model

  s <- asks snd

  headers <- getHeaders
  rs <-
    liftIO $ sendHTTP s $
      case guard (rm /= GET) >> row of
           Just payload ->
             mkRequestWithBody uri rm 
               (Just ("application/json; charset=utf-8", BSL.unpack $ encode payload))
               headers

           Nothing -> mkRequestWithBody uri rm Nothing headers

  inst <- liftIO $ BSL.pack <$> getResponseBody rs
  let isMultiple = rm == GET && isJust row

  case rid of
       -- We already know id
       Just n ->
         -- It supposed to never happen when model id is provided and at the
         -- same time we're searching for items by some filter in payload.
         if not isMultiple
            then pure $ pure (n, decode' inst)
            else error "instanceRequest: unexpected case: model id is provided\
                       \ when it is in multiple mode"

       -- If no id provided, then we expect server to give us one
       Nothing ->
         -- Try to parse instance data from server response
         if isMultiple
            then case decode' inst of
                      -- Fail when it's not a list
                      Nothing -> throw CouldNotReadCaRMaResponse

                      Just (xs :: container itemParsed) ->
                        let
                          reducer acc x =
                            case Patch.get x idAcc of
                                 Just id' -> pure $ acc <> pure (id', Just x)
                                 Nothing  -> throw NoIdFieldInCaRMaResponse
                        in
                          foldM reducer (mempty :: container itemResult) xs

            else case decode' inst of
                      -- Fail if no id provided and could not read response
                      Nothing -> throw CouldNotReadCaRMaResponse

                      Just (x :: itemParsed) ->
                        case Patch.get x idAcc of
                             Just id' -> pure $ pure (id', Just x)
                             Nothing  -> throw NoIdFieldInCaRMaResponse



-- | Wrapper for 'instanceRequest' which requires non-Nothing server response.
requireValidResponse
  :: (Model m, Monad monad)
  => (IdentI m, Maybe (Patch m))
  -> monad (IdentI m, Patch m)

requireValidResponse (cid, rs) =
  case rs of
       Just d  -> pure (cid, d)
       Nothing -> throw NoValidCaRMaResponse


createInstance :: Model m => Patch m -> CarmaIO (IdentI m, Patch m)
createInstance row =
  instanceRequest Nothing POST (Just row) >>= \case
    [x] -> requireValidResponse x
    [ ] -> error "createInstance: unexpectedly received empty result"
    _   -> error "createInstance: unexpectedly received multiple result"


readInstance :: Model m => IdentI m -> CarmaIO (Patch m)
readInstance rid =
  instanceRequest (Just rid) GET Nothing >>= \case
    [x] -> snd <$> requireValidResponse x
    [ ] -> error "readInstance: unexpectedly received empty result"
    _   -> error "readInstance: unexpectedly received multiple result"


updateInstance :: Model m => IdentI m -> Patch m -> CarmaIO (Patch m)
updateInstance rid row =
  instanceRequest (Just rid) PUT (Just row) >>= \case
    [x] -> snd <$> requireValidResponse x
    [ ] -> error "updateInstance: unexpectedly received empty result"
    _   -> error "updateInstance: unexpectedly received multiple result"


deleteInstance :: Model m => IdentI m -> CarmaIO ()
deleteInstance rid =
  instanceRequest (Just rid) DELETE Nothing >>= \case
    [_] -> pure ()
    [ ] -> error "deleteInstance: unexpectedly received empty result"
    _   -> error "deleteInstance: unexpectedly received multiple result"


-- | Check if instance exists in the CaRMa database.
instanceExists
  :: forall m. Model m
  => IdentI m -- ^ Instance id
  -> CarmaIO Bool

instanceExists (Ident rid) = do
  let model = T.unpack $ modelName (modelInfo :: ModelInfo m)
  cp <- getPort
  rs <- sendRequest $ getRequest $ modelPidURI cp model rid
  checkCode =<< liftIO (getResponseCode rs)


searchByFilter
  ::
   ( Model m
   , Traversable list
   , Applicative list
   , Monoid (list itemPre)
   , Monoid (list itemPost)
   , Monoid (list itemParsed)
   , FromJSON (list itemParsed)
   , itemParsed ~ Patch m
   , itemPre ~ (IdentI m, Maybe (Patch m))
   , itemPost ~ (IdentI m, Patch m)
   )
  => Patch m
  -> CarmaIO (list itemPost)

searchByFilter filter' =
  instanceRequest Nothing GET (Just filter') >>=
    sequence . fmap requireValidResponse


-- | Load a new-style dictionary with given name from CaRMa.
readDictionary
  :: String -- ^ Dictionary name.
  -> CarmaIO (Maybe PreDict)

readDictionary name = do
  uri <- methodURI $ "_/" <> name
  rs <- sendRequest $ getRequest uri
  rsb <- liftIO $ getResponseBody rs
  pure $ decode' $ BSL.pack rsb


-- | Set value of the first object from "dict-objects"-field JSON
--   contents with matching "key" (create it if no object matches key),
--   return new JSON string.
setKeyedJsonValue
  :: Value  -- ^ Contents of JSON field using dict-objects schema
  -> T.Text -- ^ Key
  -> Value  -- ^ Value
  -> Value

setKeyedJsonValue parsed key value = toJSON go where
  newEntry :: Value
  newEntry
    = toJSON $ M.fromList
    [ ("key" :: T.Text, String key)
    , ("value", value)
    ]

  keyPred (Object o) = fromMaybe "" (M.lookup "key" o) == String key
  keyPred _          = False

  go = case parsed of
    Array objs ->
      if V.null objs
         then V.singleton newEntry
         else case V.findIndex keyPred objs of
                   Just iMatch -> objs & element iMatch .~ newEntry
                   Nothing     -> V.snoc objs newEntry

    _ -> V.singleton newEntry

module Snaplet.DbLayer.Triggers.Dsl where

import Control.Applicative
import qualified Control.Monad.State as ST

import qualified Data.Map as Map
import Data.Maybe

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types


tryAll :: Alternative f => (a -> f b) -> [a] -> f b
tryAll f = foldl (<|>) empty . map f

get :: MonadTrigger m b => ObjectId -> FieldName -> m b FieldValue
get objId field = do
  TriggerContext{..} <- ST.get
  let objs = mapMaybe (Map.lookup objId) [current, updates, dbCache]
  case tryAll (Map.lookup field) objs of
    Just val -> return val
    Nothing
      | Map.member objId dbCache -> return ""
      | otherwise -> do
        obj <- readObject objId
        ST.modify $ \st -> st{dbCache = Map.insert objId obj dbCache}
        return $ fromMaybe "" $ Map.lookup field obj

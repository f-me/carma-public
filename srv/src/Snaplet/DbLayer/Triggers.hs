{-# LANGUAGE ScopedTypeVariables #-}
module Snaplet.DbLayer.Triggers
  (triggerUpdate
  ,triggerCreate
  , applyDefaults
  ) where

import Data.Functor ((<$>))
import Control.Monad (foldM)
import Control.Monad.Trans.State (execStateT)

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B

import qualified Snaplet.DbLayer.RedisCRUD as Redis
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Defaults
import Snaplet.DbLayer.Triggers.Actions
import Snaplet.DbLayer.Triggers.Users
import Snaplet.Auth.Class
import Snaplet.Messenger.Class

triggerCreate :: ModelName -> Object -> DbHandler b Object
triggerCreate model obj = 
    case model of
      "usermeta" -> createUsermetaTrigger obj
      _ -> return obj


triggerUpdate :: (HasAuth b, HasMsg b)
              => ModelName -> ObjectId -> Object -> DbHandler b ([DbHandler b ()], ObjectMap)
triggerUpdate model objId commit = do
  let fullId = B.concat [model, ":", objId]
  let stripUnchanged orig = Map.filterWithKey (\k v -> Map.lookup k orig /= Just v)
  commit' <- (`stripUnchanged` commit) <$> Redis.read' redis fullId
  commit'' <- case model of
                "usermeta" -> updateUsermetaTrigger objId commit'
                _          -> return commit'
  -- Seems that we don't need recursive triggers actually.
  -- There is only one place where they are used intentionally: filling car
  -- dimensions when car model is determined.
  loop actions (1 :: Int) emptyContext $ Map.singleton fullId commit''
  where
    loop _ 0 cxt changes = return (futures cxt, unionMaps changes $ updates cxt)
    loop cfg n cxt changes
      | Map.null changes = return (futures cxt, updates cxt)
      | otherwise = do
        let tgs = matchingTriggers cfg changes
        let cxt' = cxt
              {updates = unionMaps changes $ updates cxt
              ,current = Map.empty
              }
        cxt'' <- foldM (flip execStateT) cxt' $ map runTriggerMonad tgs
        loop cfg (n-1) cxt'' $ current cxt''


unionMaps :: ObjectMap -> ObjectMap -> ObjectMap
unionMaps = Map.unionWith Map.union

matchingTriggers :: TriggerMap b -> ObjectMap -> [TriggerMonad b ()]
matchingTriggers cfg updates
  = concatMap triggerModels $ Map.toList updates
  where
    triggerModels (objId, obj)
      = concat $ Map.elems
      $ Map.intersectionWith applyTriggers modelTriggers obj
      where
        model = fst $ B.break (==':') objId
        modelTriggers = Map.findWithDefault Map.empty model cfg
        applyTriggers tgs val = map (\t -> t objId val) tgs

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
import qualified Data.Text as T

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


triggerUpdate
  :: (HasAuth b, HasMsg b)
  => ModelName -> ObjectId -> Object
  -> DbHandler b ([DbHandler b ()], ObjectMap)
triggerUpdate model objId commit = do
  let fullId = T.concat [model, ":", objId]
  let stripUnchanged orig = Map.filterWithKey (\k v -> (Map.lookup k orig /= Just v) && (not $ ((k == "program") && (v == "") && (model == "case"))))
  commit' <- (`stripUnchanged` commit) <$> Redis.read' redis fullId
  commit'' <- case model of
                "usermeta" -> updateUsermetaTrigger objId commit'
                _          -> return commit'
  -- Seems that we don't need recursive triggers actually.
  -- There is only one place where they are used intentionally: filling car
  -- dimensions when car model is determined.
  do
    let changes = Map.singleton fullId commit''
    let tgs = matchingTriggers actions changes
    let cxt = emptyContext{updates = changes}
    cxt' <- foldM (flip execStateT) cxt $ map runTriggerMonad tgs
    return (futures cxt', unionMaps (current cxt') $ updates cxt')


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
        model:_ = T.splitOn ":" objId
        modelTriggers = Map.findWithDefault Map.empty model cfg
        applyTriggers tgs val = map (\t -> t objId val) tgs


module Snaplet.DbLayer.Triggers
  (triggerUpdate
  ,triggerCreate
  , applyDefaults
  ) where

import Data.Functor ((<$>))
import Control.Monad (foldM)
import Control.Monad.State (gets)
import Control.Monad.Trans.State (execStateT)

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B

import qualified Snaplet.DbLayer.RedisCRUD as Redis
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Defaults
import Snaplet.DbLayer.Triggers.Actions
import Snaplet.DbLayer.Triggers.Dsl
import Snaplet.DbLayer.Triggers.Users


triggerCreate :: ModelName -> Object -> DbHandler b Object
triggerCreate model obj = 
    case model of
      "usermeta" -> createUsermetaTrigger obj
      _ -> return obj


triggerUpdate :: ModelName -> ObjectId -> Object -> DbHandler b ObjectMap
triggerUpdate model objId commit = do
  let fullId = B.concat [model, ":", objId]
  recs <- gets (recommendations . triggers)
  let stripUnchanged orig = Map.filterWithKey (\k v -> Map.lookup k orig /= Just v)
  commit' <- (`stripUnchanged` commit) <$> Redis.read' redis fullId
  commit'' <- case model of
                "usermeta" -> updateUsermetaTrigger objId commit'
                _          -> return commit'
  let cfg = unionTriggers (compileRecs recs) actions
  -- Seems that we don't need recursive triggers actually.
  -- There is only one place where they are used intentionally: filling car
  -- dimensions when car model is determined.
  loop cfg 1 emptyContext $ Map.singleton fullId commit''
  where
    loop _ 0 cxt changes = return $ unionMaps changes $ updates cxt
    loop cfg n cxt changes
      | Map.null changes = return $ updates cxt
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

unionTriggers = Map.unionWith (Map.unionWith (++))

compileRecs :: MonadTrigger m b =>
  Map.Map k (Map.Map s (Map.Map B.ByteString (Map.Map FieldName FieldValue))) ->
  Map.Map k (Map.Map s [ObjectId -> B.ByteString -> m b ()])
compileRecs = Map.map (Map.map mkT)
  where
    mkT m = [\objId val -> case Map.lookup val m of
      Nothing -> return ()
      Just upds -> mapM_ (uncurry $ set objId) $ Map.toList upds]

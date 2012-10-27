
module Snaplet.DbLayer.Triggers
  (triggerUpdate
  ,triggerCreate
  ) where

import Data.Functor ((<$>))
import Control.Monad (foldM)
import Control.Monad.State (gets)
import Control.Monad.Trans
import Control.Monad.Trans.State (execStateT)

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Snap.Snaplet (Handler(..))
import qualified Snaplet.DbLayer.RedisCRUD as Redis
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Defaults
import Snaplet.DbLayer.Triggers.Actions
import Snaplet.DbLayer.Triggers.Dsl



triggerCreate :: ModelName -> Object -> DbHandler b Object
triggerCreate = applyDefaults

triggerUpdate :: ObjectId -> Object -> DbHandler b ObjectMap
triggerUpdate objId commit = do
  recs <- gets (recommendations . triggers)
  let stripUnchanged orig = Map.filterWithKey (\k v -> Map.lookup k orig /= Just v)
  commit' <- (`stripUnchanged` commit) <$> Redis.read' redis objId
  let cfg = unionTriggers (compileRecs recs) actions
  loop cfg 5 emptyContext $ Map.singleton objId commit'
  where
    loop cfg 0 cxt changes = return $ unionMaps changes $ updates cxt
    loop cfg n cxt changes
      | Map.null changes = return $ updates cxt
      | otherwise = do
        let tgs = matchingTriggers cfg changes
        let cxt' = cxt
              {updates = unionMaps changes $ updates cxt
              ,current = Map.empty
              }
        cxt'' <- foldM (flip execStateT) cxt' tgs
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

compileRecs = Map.map (Map.map mkT)
  where
    mkT m = [\objId val -> case Map.lookup val m of
      Nothing -> return ()
      Just upds -> mapM_ (uncurry $ set objId) $ Map.toList upds]

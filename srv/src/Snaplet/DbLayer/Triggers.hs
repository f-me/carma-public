{-# LANGUAGE RankNTypes #-}

module Snaplet.DbLayer.Triggers
  (triggerUpdate
  ,triggerCreate
  ) where

import Control.Monad (foldM)
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Snap.Snaplet (Handler(..))
import Snaplet.DbLayer.Types



data TriggerContext = TriggerContext
  {dbCache :: ObjectMap
  ,updates :: ObjectMap
  ,current :: ObjectMap
  }

emptyContext = TriggerContext Map.empty Map.empty Map.empty

type TriggerMonad b = StateT TriggerContext (Handler b (DbLayer b)) ()
type Trigger b = ObjectId -> FieldValue -> TriggerMonad b
type TriggerMap b = Map ModelName (Map FieldName [Trigger b])

triggerCreate :: ObjectId -> Object -> DbHandler b ObjectMap
triggerCreate = runTriggers Map.empty

triggerUpdate :: ObjectId -> Object -> DbHandler b ObjectMap
triggerUpdate = runTriggers Map.empty

runTriggers
  :: TriggerMap b -> ObjectId -> Object
  -> DbHandler b ObjectMap
runTriggers cfg objId commit
  = loop 5 emptyContext $ Map.singleton objId commit
  where
    loop 0 cxt changes = return $ unionMaps changes $ updates cxt
    loop n cxt changes
      | Map.null changes = return $ updates cxt
      | otherwise = do
        let tgs = matchingTriggers cfg changes
        let cxt' = cxt
              {updates = unionMaps changes $ updates cxt
              ,current = Map.empty
              }
        cxt'' <- foldM (flip execStateT) cxt' tgs
        loop (n-1) cxt'' $ current cxt''


unionMaps :: ObjectMap -> ObjectMap -> ObjectMap
unionMaps = Map.unionWith Map.union

matchingTriggers :: TriggerMap b -> ObjectMap -> [TriggerMonad b]
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

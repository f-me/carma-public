
module Snaplet.DbLayer.Triggers where

import Control.Monad (foldM)
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Snap.Snaplet (Handler(..))


type ObjectId = ByteString
type Object = Map FieldName ByteString
type ObjectMap = Map ObjectId Object
type ModelName = ByteString
type FieldName = ByteString
type FieldValue = ByteString

data TriggerContext = TriggerContext
  {dbCache :: ObjectMap
  ,updates :: ObjectMap
  ,current :: ObjectMap
  }

emptyContext = TriggerContext Map.empty Map.empty Map.empty

-- type TriggerMonad a = StateT TriggerContext (Handler () a) ()

type Trigger = ObjectId -> FieldValue -> StateT TriggerContext IO ()
type TriggerMap = Map ModelName (Map FieldName [Trigger])


runTriggers
  :: MonadIO m
  => TriggerMap -> ObjectId -> Object
  -> m ObjectMap
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
        cxt'' <- liftIO $ foldM (flip execStateT) cxt' tgs
        loop (n-1) cxt'' $ current cxt''


unionMaps :: ObjectMap -> ObjectMap -> ObjectMap
unionMaps = Map.unionWith Map.union

-- matchingTriggers :: TriggerMap -> ObjectMap -> [TriggerMonad]
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

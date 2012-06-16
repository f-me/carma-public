
module Snaplet.DbLayer.Triggers.Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State

import Snap.Snaplet
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

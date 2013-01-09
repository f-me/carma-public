
module Snaplet.DbLayer.Triggers.Types where

import Control.Monad.Trans.State
import Data.Map (Map)
import qualified Data.Map as Map

import Snap.Snaplet
import Snaplet.DbLayer.Types


data TriggerContext = TriggerContext
  {dbCache :: ObjectMap
  ,updates :: ObjectMap
  ,current :: ObjectMap
  }

emptyContext :: TriggerContext
emptyContext = TriggerContext Map.empty Map.empty Map.empty

type TriggerMonad b r = StateT TriggerContext (Handler b (DbLayer b)) r
type Trigger b = ObjectId -> FieldValue -> TriggerMonad b ()
type TriggerMap b = Map ModelName (Map FieldName [Trigger b])

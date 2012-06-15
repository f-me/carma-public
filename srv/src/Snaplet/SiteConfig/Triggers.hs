
module Snaplet.SiteConfig.Triggers
  (TriggersConfig(..)
  ,triggersConfig
  ) where

import qualified Data.Map as Map
import Snaplet.DbLayer.Triggers


data TriggersConfig = TriggersConfig
  {createTriggers :: TriggerMap
  ,updateTriggers :: TriggerMap
  }

triggersConfig = TriggersConfig
  {createTriggers = Map.fromList []
  ,updateTriggers = Map.fromList []
  }

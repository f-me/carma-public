
module Snaplet.SiteConfig.Triggers
  (TriggersConfig(..)
  ,triggersConfig
  ) where

import qualified Data.Map as Map
import DbTriggers


data TriggersConfig = TriggersConfig
  {createTriggers :: TriggerMap
  ,updateTriggers :: TriggerMap
  }

triggersConfig = TriggersConfig
  {createTriggers = Map.fromList []
  ,updateTriggers = Map.fromList []
  }

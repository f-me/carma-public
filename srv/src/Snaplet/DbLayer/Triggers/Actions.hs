module Snaplet.DbLayer.Triggers.Actions (actions)

where

import Control.Monad
import qualified Data.Map as Map

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.MailToDealer


actions :: MonadTrigger m b => Map.Map ModelName (Map.Map FieldName [ObjectId -> FieldValue -> m b ()])
-- actions :: TriggerMap a
actions
    = Map.fromList
      [("case", Map.fromList
          [("psaExportNeeded",
            [\caseRef val -> when (val == "1") $ tryRepTowageMail caseRef])
          ])
        ]

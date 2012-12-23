
module RuntimeFlag
  (RuntimeFlag(..)
  ,RuntimeFlags
  ) where

import Data.Set (Set)

data RuntimeFlag = ReducedActionsMode
  deriving (Eq, Ord, Show, Read)

type RuntimeFlags = Set RuntimeFlag

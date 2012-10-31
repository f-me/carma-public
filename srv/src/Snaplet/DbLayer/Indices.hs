
module Snaplet.DbLayer.Indices where

import Control.Concurrent.STM.TVar
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)

type IndexName = ByteString
type IndexData = Map ByteString (Set ByteString)
type Indices = Map IndexName (TVar IndexData)


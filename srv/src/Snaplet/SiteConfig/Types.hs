
module Snaplet.SiteConfig.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)

type ModelName  = ByteString
type FieldName  = ByteString
type FieldValue = ByteString
type Commit     = Map FieldName FieldValue

{-
newtype ModelName  = ModelName  ByteString
newtype FieldName  = FieldName  ByteString
newtype FieldValue = FieldValue ByteString
-}

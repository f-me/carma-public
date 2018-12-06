-- | An some sort of alternative to "Show" type-class.
--
-- While "Show" instance could be applied for many different data-types
-- "StringyEnum" is supposed to be instanced for simple enum types which
-- have some string representation
-- (usually related to some external service API).
--
-- Instances of "StringyEnum" helps to write instances for Swagger "ToSchema"
-- and/or "FromJSON", etc.
module Carma.Utils.StringyEnum
     ( StringyEnum (..)
     ) where

import           Data.Text (Text)


class StringyEnum a where
  toStringy :: a -> Text

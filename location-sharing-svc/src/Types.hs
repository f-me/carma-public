module Types where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Pool (Pool)
import qualified Database.PostgreSQL.Simple as PG

data AppContext = AppContext
  { pgUri     :: ByteString
  , pgPool    :: Pool PG.Connection
  , httpPort  :: Int
  , urlPrefix :: Text
  , indexTpl  :: Text
  , err404Tpl :: Text
  }

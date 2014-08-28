module Snaplet.DbLayer.Triggers.Defaults
  (applyDefaults
  ) where

import Prelude hiding (id)

import Control.Monad.IO.Class
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Snap
import Snaplet.Auth.Class

import qualified Database.Redis       as Redis
import qualified Snap.Snaplet.RedisDB as Redis

import Snaplet.DbLayer.Types


-- | Populate a commit with default field values.
applyDefaults :: HasAuth b
              => ModelName
              -> Object
              -> Handler b (DbLayer b) Object
applyDefaults model obj = do
  ct <- liftIO $ round . utcTimeToPOSIXSeconds
              <$> getCurrentTime
  obj' <- case model of
    "cost_serviceTarifOption" -> return $ Map.insert "count" "1" obj
    "taxi" -> do
      let parentId = T.encodeUtf8 $ obj Map.! "parentId"
      Right caseAddr <- Redis.runRedisDB redis
                $ Redis.hget parentId "caseAddress_address"
      return $ case T.decodeUtf8 <$> caseAddr of
        Just addr -> Map.insert "taxiFrom_address" addr obj
        Nothing   -> obj
    _ -> return obj

  return $ Map.union (Map.insert "ctime" (T.pack $ show ct) obj')
         $ Map.findWithDefault Map.empty model defaults


defaults :: Map ModelName Object
defaults = Map.fromList
  [("case", Map.fromList
    [("services", "")
    ])
  ]

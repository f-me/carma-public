{-# LANGUAGE QuasiQuotes #-}
module Snaplet.SiteConfig.SpecialPermissions (stripContract) where

import qualified Data.Map as M
import           Data.ByteString (ByteString)
import           Data.Pool

import           Control.Applicative

import           Database.PostgreSQL.Simple (Query, query)
import           Database.PostgreSQL.Simple.SqlQQ

import           Snaplet.SiteConfig.Models
import           Snaplet.SiteConfig.Config

import           Snap

q :: Query
q = [sql|
SELECT contractfield, (case showform when true then 't' else 'f' end)
FROM programpermissionstbl
WHERE parentid is not null AND split_part(parentid, ':', 2) = ?
|]

stripContract :: Model -> ByteString -> Handler b (SiteConfig b) Model
stripContract model pid = do
  pg    <- gets pg_search
  perms <- liftIO $ withResource pg $ getPerms pid
  return model{fields = filterFields perms (fields model)}
    where
      getPerms progid conn = M.fromList <$>
        (query conn q [progid] :: IO [(ByteString, ByteString)])
      filterFields perms flds = filter (isCanShow perms) flds
      isCanShow perms f =
        case M.lookup (name f) perms >>= return . ("t" ==) of
          Nothing -> False
          Just _  -> True
module Snaplet.SiteConfig.SpecialPermissions (stripContract) where

import           Data.Maybe
import qualified Data.Map as M
import           Data.ByteString (ByteString)
import           Data.Pool

import           Control.Applicative

import qualified Data.ByteString.Char8 as B8
import           Data.String (fromString)

import           Database.PostgreSQL.Simple (Query, query)
-- import           Database.PostgreSQL.Simple.SqlQQ

import           Snaplet.SiteConfig.Models
import           Snaplet.SiteConfig.Config

import           Snap

q :: ByteString -> Query
q field = fromString
          $ "SELECT contractfield, "
          ++ "(case " ++ (show field) ++ " when true then 't' else 'f' end) "
          ++ "FROM programpermissionstbl "
          ++ "WHERE contractfield is not null "
          ++ "AND parentid is not null "
          ++ "AND split_part(parentid, ':', 2) = ?"

stripContract :: Model -> ByteString -> ByteString
                 -> Handler b (SiteConfig b) Model
stripContract model pid field = do
  pg    <- gets pg_search
  perms <- liftIO $ withResource pg $ getPerms pid
  return model{fields = filterFields perms (fields model)}
    where
      getPerms progid conn = M.fromList <$>
        (query conn (q field) [progid] :: IO [(ByteString, ByteString)])
      filterFields perms flds = filter (isCanShow perms) flds
      isCanShow perms f  = fromMaybe False $ check perms (name f)
      check perms "dixi" = return True
      check perms name   = M.lookup name perms >>= return . ("t" ==)
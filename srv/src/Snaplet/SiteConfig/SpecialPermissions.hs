module Snaplet.SiteConfig.SpecialPermissions (stripContract) where

import           Data.Maybe
import qualified Data.Map as M
import           Data.ByteString (ByteString)
import           Data.Pool

import           Control.Applicative

import           Data.String (fromString)

import           Database.PostgreSQL.Simple (Query, query)
-- import           Database.PostgreSQL.Simple.SqlQQ

import           Snaplet.SiteConfig.Models
import           Snaplet.SiteConfig.Config

import           Snap

q :: ByteString -> Query
q field = fromString
          $ "SELECT contractField, "
          ++ "(case " ++ (show field) ++ " when true then 't' else 'f' end) "
          ++ "FROM \"SubProgramContractPermission\" "
          ++ "WHERE contractfield is not null "
          ++ "AND parent = ?"

stripContract :: Model
              -> ByteString
              -- ^ SubProgram id
              -> ByteString
              -- ^ Contract field name
              -> Handler b (SiteConfig b) Model
stripContract model sid field = do
  pg    <- gets pg_search
  perms <- liftIO $ withResource pg $ getPerms sid
  return model{fields = filterFields perms (fields model)}
    where
      getPerms progid conn = M.fromList <$>
        (query conn (q field) [progid] :: IO [(ByteString, ByteString)])
      filterFields perms flds = filter (isCanShow perms) flds
      isCanShow perms f  = fromMaybe False $ check perms (name f)
      check _ "dixi"       = return True
      check _ "committer"  = return True
      check _ "isActive"   = return True
      check _ "ctime"      = return True
      check perms name     = M.lookup name perms >>= return . ("t" ==)

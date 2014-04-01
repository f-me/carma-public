module Snaplet.SiteConfig.SpecialPermissions
    ( stripContract
    , FilterType(..)
    )

where

import           Data.Maybe
import qualified Data.Map as M
import           Data.ByteString (ByteString)
import           Data.Pool

import           Control.Applicative

import           Data.String (fromString)

import           Database.PostgreSQL.Simple (Query, query)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField

import           Snaplet.SiteConfig.Models
import           Snaplet.SiteConfig.Config

import           Snap
import           Util

q :: Query
q = [sql|
     SELECT contractField,
     (case ? when true then 't' else 'f' end)
     FROM "SubProgramContractPermission"
     WHERE contractfield IS NOT NULL
     AND parent = ?
     |]

data FilterType = Form | Table

instance ToField FilterType where
  toField Form = toField $ PT "showform"
  toField Table = toField $ PT "showtable"

stripContract :: Model
              -> ByteString
              -- ^ SubProgram id.
              -> FilterType
              -> Handler b (SiteConfig b) Model
stripContract model sid flt = do
  pg    <- gets pg_search
  perms <- liftIO $ withResource pg $ getPerms sid
  return model{fields = filterFields perms (fields model)}
    where
      getPerms progid conn = M.fromList <$>
        (query conn q (flt, progid) :: IO [(ByteString, ByteString)])
      filterFields perms flds = filter (isCanShow perms) flds
      isCanShow perms f  = fromMaybe False $ check flt perms (name f)
      check Form "dixi"      = return True
      check Form "committer" = return True
      check Form "isActive"  = return True
      check Form "ctime"     = return True
      check _    perms name  = M.lookup name perms >>= return . ("t" ==)

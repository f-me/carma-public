{-|

Contract model processing for portal screen (per-subprogram field
permissions and hacks).

-}

module Snaplet.SiteConfig.SpecialPermissions
    ( stripContract
    , FilterType(..)
    )

where

import           Data.Aeson as Aeson
import           Data.Maybe
import qualified Data.Map as M
import           Data.Text (Text)

import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField

import           Data.Model.Utils.PostgreSQL.InterpolationHelpers
import           Carma.Model.Role as Role

import           Snap
import           Snap.Snaplet.PostgresqlSimple

import           Snaplet.Auth.PGUsers (currentUserRoles)
import           Snaplet.SiteConfig.Models
import           Snaplet.SiteConfig.Config
import           Carma.Utils.Snap (withLens)

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

stripContract :: (HasPostgresAuth b (SiteConfig b))
              => Model
              -> Text
              -- ^ SubProgram id.
              -> FilterType
              -> Handler b (SiteConfig b) Model
stripContract model sid flt = do
  perms <- getPerms sid
  Just userRoles <- currentUserRoles
  let procField = if Role.partner `elem` userRoles
                  then reqField
                  else id
  return model{fields = map procField $ filterFields perms (fields model)}
    where
      reqField f =
          if name f /= "comment"
          then f{meta = Just $ M.insert "required" (Aeson.Bool True) $
                 fromMaybe M.empty $ meta f}
          else f
      getPerms progid = M.fromList <$>
        ((withLens db $ query q (flt, progid)) ::
             Handler b (SiteConfig b) [(Text, Text)])
      filterFields perms flds = filter (isCanShow perms) flds
      isCanShow perms f = fromMaybe False $ check flt perms (name f)
      check Form _ "dixi"        = return True
      check Form _ "committer"   = return True
      check Form _ "isActive"    = return True
      check Form _ "ctime"       = return True
      check Table _ "id"         = return True
      check _ _ "subprogram"     = return True
      check _ perms name         = M.lookup name perms >>= return . ("t" ==)

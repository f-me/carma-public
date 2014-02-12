module Utils.Roles where

import qualified Data.ByteString.Char8 as B

import Data.Model
import Carma.Model.Role as M

import Snap.Snaplet.Auth as A
import Snaplet.Auth.PGUsers
import Snap.Snaplet.PostgresqlSimple


bs2roleid :: A.Role -> IdentI M.Role
bs2roleid (A.Role s) =
  case B.readInt s of
    Just (n, "") -> Ident n
    _            -> error $ "can't parse role id, expecting int, but got: " ++
                    show s

userRolesIds :: HasPostgres m => AuthUser -> m [IdentI M.Role]
userRolesIds user = return . map bs2roleid =<< userRolesPG user

{-|
Postgres-based roles & user meta storage for Snap authentication
system.

Roles are stored in @usermetatbl@ table as created from the @usermeta@
model.
-}

module Snaplet.Auth.PGUsers
    ( currentUserMeta
    , currentUserMetaId
    , currentUserRoles
    , Usermeta
    ) where

import           Snap
import           Snaplet.Auth.Class
import           Snap.Snaplet.Auth hiding (Role)
import           Snap.Snaplet.PostgresqlSimple as PG

import qualified Data.Vector as V

import           Data.Model
import           Data.Model.Patch (Patch)
import qualified Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch
import           Carma.Model.Role (Role)
import           Carma.Model.Usermeta (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta


-- FIXME: should return Object not a Patch
currentUserMeta
  :: (HasPostgresAuth b v)
  => Handler b v (Maybe (Patch Usermeta))
currentUserMeta = withAuth currentUser >>= \case
  Nothing  -> do
    req <- getRequest
    -- Consider current user to be admin when accessing from localhost
    -- (HTTP API)
    case rqRemoteAddr req == rqLocalAddr req of
      True ->
        (withAuthPg $ PG.liftPG $ Patch.read Usermeta.admin) >>=
          \case
            Left e -> error $ show e
            Right r -> return $ Just r
      False -> return Nothing
  Just usr -> case userId usr of
    Nothing  -> error $ "BUG! currentUser without id: " ++ show usr
    Just (UserId uid) -> do
      res <- withAuthPg $ PG.liftPG $
             Patch.readManyWithFilter 1 0 [(fieldName Usermeta.uid, uid)]
      case res of
        [obj] -> return $ Just obj
        _     -> error $ "BUG! select Usermeta.uid " ++ show uid


currentUserMetaId
  :: (HasPostgresAuth b v)
  => Handler b v (Maybe (IdentI Usermeta))
currentUserMetaId = (>>= flip Patch.get Usermeta.ident) <$> currentUserMeta


currentUserRoles
  :: (HasPostgresAuth b v)
  => Handler b v (Maybe [IdentI Role])
currentUserRoles = do
  meta <- currentUserMeta
  return $ meta >>= flip Patch.get Usermeta.roles >>= return . V.toList

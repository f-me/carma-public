{-# LANGUAGE DoAndIfThenElse #-}

{-|

Combinators and helpers for user permission checking and serving user
data/states.

-}

module AppHandlers.Users
    ( chkAuth
    , chkAuthLocal
    , chkAuthAdmin
    , chkAuthPartner
    , serveUserCake
    , serveUserStates
    , userIsInState
    , userIsReady
    )

where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text           as T
import           Data.String (fromString)
import           Data.Time.Calendar (Day)
import qualified Data.ByteString.Char8 as BS

import           Text.Printf

import           Database.PostgreSQL.Simple (query)

import Snap

import Data.Model
import Data.Model.Patch     as Patch
import qualified Data.Model.Patch.Sql as Patch

import Carma.Model.Role      as Role
import Carma.Model.Usermeta  as Usermeta
import Carma.Model.UserState as UserState

import Application
import AppHandlers.Util
import Snaplet.Auth.PGUsers
import Snaplet.Search.Types (mkSel)

import Utils.LegacyModel (readIdent)


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated users.
chkAuth :: AppHandler () -> AppHandler ()
chkAuth = chkAuthRoles alwaysPass


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated or non-local users.
chkAuthLocal :: AppHandler () -> AppHandler ()
chkAuthLocal = chkAuthRoles (hasNoneOfRoles [Role.partner])


chkAuthAdmin :: AppHandler () -> AppHandler ()
chkAuthAdmin = chkAuthRoles (hasAnyOfRoles [Role.lovAdmin])


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated or non-partner users.
--
-- Auth checker for partner screens
chkAuthPartner :: AppHandler () -> AppHandler ()
chkAuthPartner f =
  chkAuthRoles (hasAnyOfRoles [ Role.partner
                              , Role.head
                              , Role.supervisor]) f


------------------------------------------------------------------------------
-- | A predicate for a list of user roles.
type RoleChecker = [IdentI Role] -> Bool


------------------------------------------------------------------------------
-- | Produce a predicate which matches any list of roles
alwaysPass :: RoleChecker
alwaysPass = const True


hasAnyOfRoles :: [IdentI Role] -> RoleChecker
hasAnyOfRoles authRoles = any (`elem` authRoles)


hasNoneOfRoles :: [IdentI Role] -> RoleChecker
hasNoneOfRoles authRoles = all (not . (`elem` authRoles))


------------------------------------------------------------------------------
-- | Pass only requests from localhost users or non-localhost users
-- with a specific set of roles.
chkAuthRoles :: RoleChecker
             -- ^ Check succeeds if non-localhost user roles satisfy
             -- this predicate.
             -> AppHandler () -> AppHandler ()
chkAuthRoles roleCheck handler = do
  ipHeaderFilter
  req <- getRequest
  case rqRemoteAddr req /= rqLocalAddr req of
    False -> handler -- No checks for requests from localhost
    True  -> currentUserRoles >>= \case
      Nothing -> handleError 401
      Just roles ->
        if roleCheck roles
        then handler
        else handleError 401


-- | True if a user is in any of given states.
userIsInState :: IdentI Usermeta -> [UserStateVal] -> AppHandler Bool
userIsInState uid uStates =
  withPG pg_search $ \conn -> Patch.read uid conn >>=
  \case
    Left e -> error $
              "Could not fetch usermeta for user " ++ show uid ++
              ", error " ++ show e
    Right p -> do
      p' <- liftIO $ fillCurrentState p uid conn
      case p' `Patch.get` Usermeta.currentState of
        Just v  -> return $ v `elem` uStates
        Nothing -> error $ "Could not obtain a state for user " ++ show uid


-- | True if a user is in @Ready@ state.
userIsReady :: IdentI Usermeta -> AppHandler Bool
userIsReady uid = uid `userIsInState` [Ready]


-- | Serve user account data back to client.
serveUserCake :: AppHandler ()
serveUserCake = currentUserMeta >>= maybe (handleError 401) writeJSON


-- | Serve states for a user within a time interval.
serveUserStates :: AppHandler ()
serveUserStates = do
  usrId <- readUsermeta <$> getParamT "userId"
  from  <- readDay <$> getParam "from"
  to    <- readDay <$> getParam "to"
  states <- withPG pg_search $ \c ->
    query c (fromString $ printf
      -- Get more then asked, we need this during drawing of timeline
      ("SELECT %s FROM \"UserState\" WHERE userId = ? " ++
       " AND ctime BETWEEN timestamp ? - interval '1 month' " ++
       "           AND     timestamp ? + interval '1 month' " ++
       " ORDER BY id ASC"
      )
      (T.unpack $ mkSel (undefined :: Patch UserState))) $
      (identVal usrId, from, to)
  writeJSON (states :: [Patch UserState])
  where
    readDay :: Maybe BS.ByteString -> Day
    readDay = read . BS.unpack . fromJust
    readUsermeta :: Maybe Text -> IdentI Usermeta
    readUsermeta = readIdent . fromJust

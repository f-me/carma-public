{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes #-}

{-|

Combinators and helpers for user permission checking and serving user
data/states.

-}

module AppHandlers.Users
    ( chkAuth
    , chkAuthLocal
    , chkAuthAdmin
    , chkAuthPartner

    , chkUserActiveness

    , chkAuthRoles
    , hasAnyOfRoles
    , hasNoneOfRoles

    , serveUserCake
    , serveUserStates
    , userIsInState
    , userIsReady
    , usersInStates
    )

where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text           as T
import           Data.String (fromString)
import           Data.Time.Calendar (Day)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

import           Control.Monad
import           Control.Monad.IO.Class

import           Text.Printf

import           Database.PostgreSQL.Simple (query)
import           Database.PostgreSQL.Simple.SqlQQ.Alt

import           Snap
import           Snap.Snaplet.PostgresqlSimple hiding (query)

import Data.Model
import Data.Model.Patch as Patch
import qualified Data.Model.Patch.Sql as Patch
import Data.Model.Utils.LegacyModel (readIdent)
import Data.Model.Utils.PostgreSQL.InterpolationHelpers

import Carma.Model.Role      as Role
import Carma.Model.Usermeta  as Usermeta
import Carma.Model.UserState as UserState

import Application
import AppHandlers.Util
import Snaplet.Auth.PGUsers
import Snaplet.Search.Types (mkSel)


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated users.
chkAuth :: AppHandler () -> AppHandler ()
chkAuth m = do
  chkUserActiveness
  chkAuthRoles alwaysPass m


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated or non-local users.
chkAuthLocal :: AppHandler () -> AppHandler ()
chkAuthLocal m = do
  chkUserActiveness
  chkAuthRoles (hasNoneOfRoles [Role.partner]) m


chkAuthAdmin :: AppHandler () -> AppHandler ()
chkAuthAdmin m = do
  chkUserActiveness
  chkAuthRoles (hasAnyOfRoles [Role.lovAdmin]) m


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated or non-partner users.
--
-- Auth checker for partner screens
chkAuthPartner :: AppHandler () -> AppHandler ()
chkAuthPartner m = do
  chkUserActiveness
  chkAuthRoles isPartnerAccess m
  where
    isPartnerAccess = hasAnyOfRoles [Role.partner, Role.head, Role.supervisor]


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
hasNoneOfRoles authRoles = all $ not . (`elem` authRoles)


------------------------------------------------------------------------------
-- | Pass only requests from localhost users or non-localhost users
-- with a specific set of roles.
chkAuthRoles :: RoleChecker
             -- ^ Check succeeds if non-localhost user roles satisfy
             -- this predicate.
             -> AppHandler ()
             -> AppHandler ()
chkAuthRoles roleCheck handler = do
  ipHeaderFilter
  req <- getRequest
  if rqClientAddr req == rqServerAddr req
     then handler -- No checks for requests from @localhost@
     else currentUserRoles >>= \case
            Nothing    -> handleError 401
            Just roles -> if roleCheck roles then handler else handleError 401


-- | Deny requests from deactivated users.
chkUserActiveness :: AppHandler ()
chkUserActiveness = go where
  forbid = handleError 403
  isUserActive = flip Patch.get Usermeta.isActive
  go = currentUserMeta >>= \usermeta ->
    unless (fromMaybe False $ usermeta >>= isUserActive) forbid


-- | True if a user is in any of given states.
-- Nothing if we could not determine user's state due to his long inactivity
-- period.
userIsInState :: [UserStateVal] -> IdentI Usermeta -> AppHandler (Maybe Bool)
userIsInState uStates uid =
  liftPG' $ \conn -> Patch.read uid conn >>=
  \case
    Left e -> error $
              "Could not fetch usermeta for user " ++ show uid ++
              ", error " ++ show e
    Right p -> do
      p' <- liftIO $ fillCurrentState p uid conn
      return
        $ (`elem` uStates)
        <$> Patch.get p' Usermeta.currentState


-- | True if a user is in @Ready@ state.
userIsReady :: IdentI Usermeta -> AppHandler Bool
userIsReady uid = fromMaybe False <$> userIsInState [Ready] uid


-- | Serve users with any of given roles in any of given states.
--
-- Response is a list of triples: @[["realName", "login", <id>],...]@
usersInStates :: [IdentI Role.Role] -> [UserStateVal] -> AppHandler ()
usersInStates roles uStates = do
  -- TODO use @msql@ instead of @sql@
  rows <- liftPG' $ \c -> uncurry (query c) [sql|
   SELECT
   u.$(plainFieldName Usermeta.realName)$,
   u.$(plainFieldName Usermeta.login)$,
   u.$(plainFieldName Usermeta.ident)$
   FROM $(plainTableName Usermeta.ident)$ u
   LEFT JOIN (SELECT DISTINCT ON ($(plainFieldName UserState.userId)$)
                $(plainFieldName UserState.state)$,
                $(plainFieldName UserState.userId)$
              FROM $(plainTableName UserState.ident)$
              WHERE range IS NULL
              ORDER BY
              $(plainFieldName UserState.userId)$,
              $(plainFieldName UserState.ident)$ DESC) s
   ON u.$(plainFieldName Usermeta.ident)$ =
        s.$(plainFieldName UserState.userId)$
   WHERE s.$(plainFieldName UserState.state)$ IN $(In uStates)$
   AND u.$(plainFieldName Usermeta.roles)$ && ($(V.fromList roles)$)::int[];
   |]
  writeJSON (rows :: [(Text, Text, Int)])


-- | Serve user account data back to client.
serveUserCake :: AppHandler ()
serveUserCake = currentUserMeta >>= maybe (handleError 401) writeJSON


-- | Serve states for a user within a time interval.
serveUserStates :: AppHandler ()
serveUserStates = do
  usrId <- readUsermeta <$> getParamT "userId"
  from  <- readDay <$> getParam "from"
  to    <- readDay <$> getParam "to"
  states <- liftPG' $ \c ->
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

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
    , claimUserActivity
    , claimUserLogout
    , serveUserCake
    , serveUserStates
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
import Snap.Snaplet.PostgresqlSimple hiding (query)

import Data.Model
import Data.Model.Patch

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


claimUserActivity :: AppHandler ()
claimUserActivity = currentUserMetaId >>= \case
  Nothing        -> return ()
  Just (Ident u) -> void $ execute
    "UPDATE usermetatbl SET lastactivity = NOW() WHERE id = ?" [u]

claimUserLogout :: AppHandler ()
claimUserLogout = currentUserMetaId >>= \case
  Nothing        -> return ()
  Just (Ident u) -> void $ execute
    "UPDATE usermetatbl SET lastlogout = NOW() WHERE id = ?" [u]


------------------------------------------------------------------------------
-- | Serve user account data back to client.
serveUserCake :: AppHandler ()
serveUserCake = currentUserMeta >>= maybe (handleError 401) writeJSON

--    let homePage = case [T.decodeUtf8 r | Snap.Role r <- userRoles usr] of
--          rs | identFv Role.head       `elem` rs -> "/#rkc"
--             | identFv Role.supervisor `elem` rs -> "/#supervisor"
--             | identFv Role.call       `elem` rs -> "/#call"
--             | identFv Role.back       `elem` rs -> "/#back"
--             | identFv Role.parguy     `elem` rs -> "/#partner"
--             | otherwise                   -> ""

-- | Serve user states
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

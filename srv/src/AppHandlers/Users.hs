{-# LANGUAGE DoAndIfThenElse, ScopedTypeVariables #-}

{-|

Combinators and helpers for user permission checking.

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
import qualified Data.Text.Encoding  as T
import qualified Data.HashMap.Strict as HM
import           Data.String (fromString)
import           Data.Time.Calendar (Day)
import qualified Data.ByteString.Char8 as BS

import           Text.Printf

import           Database.PostgreSQL.Simple (query)

import Snap
import Snap.Snaplet.Auth hiding (Role, session)
import qualified Snap.Snaplet.Auth as Snap (Role(..))
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

import Util (identFv)
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
type RoleChecker = [Snap.Role] -> Bool


------------------------------------------------------------------------------
-- | Produce a predicate which matches any list of roles
alwaysPass :: RoleChecker
alwaysPass = const True


hasAnyOfRoles :: [IdentI Role] -> RoleChecker
hasAnyOfRoles authRoles =
    any (`elem` ar)
        where ar = map (Snap.Role . T.encodeUtf8 . identFv) authRoles


hasNoneOfRoles :: [IdentI Role] -> RoleChecker
hasNoneOfRoles authRoles =
    all (not. (`elem` ar))
        where ar = map (Snap.Role . T.encodeUtf8 . identFv) authRoles


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
  if rqRemoteAddr req /= rqLocalAddr req
  then with auth currentUser >>= maybe
       (handleError 401)
       (\u -> do
          uRoles <- with db $ userRolesPG u
          if roleCheck uRoles
          then handler
          else handleError 401)
  -- No checks for requests from localhost
  else handler


claimUserActivity :: AppHandler ()
claimUserActivity = with auth currentUser >>= \case
  Nothing -> return ()
  Just u  -> void $ execute
    "UPDATE usermetatbl SET lastactivity = NOW() WHERE login = ?"
    [userLogin u]

claimUserLogout :: AppHandler ()
claimUserLogout = with auth currentUser >>= \case
  Nothing -> return ()
  Just u  -> void $ execute
    "UPDATE usermetatbl SET lastlogout = NOW() WHERE login = ?"
    [userLogin u]


------------------------------------------------------------------------------
-- | Serve user account data back to client.
serveUserCake :: AppHandler ()
serveUserCake
  = ifTop $ with auth currentUser
  >>= \case
    Nothing -> handleError 401
    Just u'  -> do
      usr <- with db $ replaceMetaRolesFromPG u'
      let homePage = case [T.decodeUtf8 r | Snap.Role r <- userRoles usr] of
            rs | identFv Role.head       `elem` rs -> "/#rkc"
               | identFv Role.supervisor `elem` rs -> "/#supervisor"
               | identFv Role.call       `elem` rs -> "/#call"
               | identFv Role.back       `elem` rs -> "/#back"
               | identFv Role.parguy     `elem` rs -> "/#partner"
               | otherwise                   -> ""
      writeJSON $ usr
        {userMeta = HM.insert "homepage" homePage $ userMeta usr
        }

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

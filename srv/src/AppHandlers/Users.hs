{-# LANGUAGE DoAndIfThenElse #-}

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
    )

where

import Data.Aeson
import qualified Data.HashMap.Strict as HM

import Snap
import Snap.Snaplet.Auth hiding (Role, session)
import qualified Snap.Snaplet.Auth as Snap (Role(..))
import Snap.Snaplet.PostgresqlSimple

import Data.Model
import Carma.Model.Role as Role

import Application
import AppHandlers.Util
import AppHandlers.UserAchievements
import Snaplet.Auth.PGUsers

import Util (roleIdent)


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated users.
chkAuth :: AppHandler () -> AppHandler ()
chkAuth h = chkAuthRoles alwaysPass h


------------------------------------------------------------------------------
-- | Deny requests from unauthenticated or non-local users.
chkAuthLocal :: AppHandler () -> AppHandler ()
chkAuthLocal f = chkAuthRoles (hasNoneOfRoles [Role.partner]) f


chkAuthAdmin :: AppHandler () -> AppHandler ()
chkAuthAdmin f = chkAuthRoles (hasAnyOfRoles [Role.lovAdmin]) f


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


hasAnyOfRoles :: [Ident Role] -> RoleChecker
hasAnyOfRoles authRoles =
    \userRoles -> any (flip elem ar) userRoles
        where ar = map (\i -> Snap.Role $ roleIdent i) authRoles


hasNoneOfRoles :: [Ident Role] -> RoleChecker
hasNoneOfRoles authRoles =
    \userRoles -> not $ any (flip elem ar) userRoles
        where ar = map (\i -> Snap.Role $ roleIdent i) authRoles


------------------------------------------------------------------------------
-- | Pass only requests from localhost users or non-localhost users
-- with a specific set of roles.
chkAuthRoles :: RoleChecker
             -- ^ Check succeeds if non-localhost user roles satisfy
             -- this predicate.
             -> AppHandler () -> AppHandler ()
chkAuthRoles roleCheck handler = do
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
      achievements <- userAchievements usr
      let homePage = case map (\(Snap.Role r) -> r) $ userRoles usr of
            rs | (roleIdent Role.head)       `elem` rs -> "/#rkc"
               | (roleIdent Role.supervisor) `elem` rs -> "/#supervisor"
               | (roleIdent Role.front)      `elem` rs -> "/#call"
               | (roleIdent Role.back)       `elem` rs -> "/#back"
               | (roleIdent Role.bo_control) `elem` rs -> "/#back"
               | (roleIdent Role.parguy)     `elem` rs -> "/#partner"
               | otherwise                   -> ""
      writeJSON $ usr
        {userMeta
          = HM.insert "achievements" (toJSON achievements)
          $ HM.insert "homepage" homePage
          $ userMeta usr
        }

{-|

Bind operations on usermeta instances to updates of corresponding
users in Postgres.

-}

module Snaplet.DbLayer.Triggers.Users
    ( createUsermetaTrigger
    , updateUsermetaTrigger
    )

where

import Data.Maybe
import Data.Map as M ((!), delete, insert, lookup)
import Data.Text.Encoding
import Data.Time.Calendar.Julian
import Data.Time.Clock

import Snap
import Snap.Snaplet.Auth

import Snaplet.DbLayer.RedisCRUD as Redis
import Snaplet.DbLayer.Types


-- | Special handling for new user meta: read @login@ & @password@
-- fields from commit, create new user, then strip @password@ field
-- from commit, store @uid@ with newly created user id in commit and
-- proceed. Must be used with @usermeta@ commits only.
createUsermetaTrigger :: Object -> DbHandler b Object
createUsermetaTrigger obj =
  case (M.lookup "login" obj,
        M.lookup "password" obj) of
    (Just login, Just password) -> do
      let user = lockoutUser (M.lookup "isActive" obj) $
                 defAuthUser{userLogin = decodeUtf8 login}
      user' <- liftIO $ setPassword user password
      uRes <- with auth $ withBackend $ \bk -> liftIO $ save bk user'
      case uRes of
        Left e -> error $ "Could not create new user: " ++ show e
        Right newUser -> do
            let (Just (UserId uid)) = userId newUser
            return $ M.delete "password"
                   $ M.insert "uid" (encodeUtf8 uid) obj
    (_, _) -> 
        error "Login and password not set when creating new user"


-- | If @login@, @password@ or @isActive@ field is present in a
-- @usermeta@ commit, then change login/password/active status of the
-- corresponding user and strip @password@ from commit. Must be used
-- with @usermeta@ commits only.
updateUsermetaTrigger :: ObjectId -> Object -> DbHandler b Object
updateUsermetaTrigger objId obj = do
  case (M.lookup "login" obj,
        M.lookup "password" obj,
        M.lookup "isActive" obj) of
    (Nothing, Nothing, Nothing) -> return obj
    (login', password', active') -> do
      -- Could use DB.read here if it didn't result in a cyclic module
      -- dependency
      fullMeta <- Redis.read redis "usermeta" objId
      let uid = UserId (decodeUtf8 $ fullMeta ! "uid")
      -- Read Auth user
      uRes <- with auth $ withBackend $ \bk -> liftIO $ lookupByUserId bk uid
      case uRes of
        Nothing -> error $ "Could not find user with uid=" ++ show uid
        Just user -> do
             let newLogin = fromMaybe (userLogin user) (decodeUtf8 <$> login')
                 pwAction = maybe return (flip setPassword) password'
             -- Save new user data
             uRes' <- with auth $ withBackend $
                      \bk -> liftIO $ save bk =<<
                             (pwAction . lockoutUser active') user{userLogin=newLogin}
             case uRes' of
               Left e -> error $
                         "Could not save login/password for user: " ++ show e
               Right _ -> return $ M.delete "password" obj


-- | Depending on @isActive@ field value, produce a function which
-- either locks out a user for eternity (when @isActive@ is @0@) or
-- removes the lock (if it is @1@). In case of any other value, 'id'
-- is returned.
lockoutUser :: Maybe FieldValue
            -- ^ Value of @isActive@ field (@1@ or @0@).
            -> (AuthUser -> AuthUser)
lockoutUser active =
    case active of
      Just "1" -> \u -> u{userLockedOutUntil = Nothing}
      Just "0" -> \u -> u{userLockedOutUntil = zabriskiePoint}
                  where
                    zabriskiePoint = Just $ UTCTime (fromJulian 3001 0 0) 0
      _ -> id

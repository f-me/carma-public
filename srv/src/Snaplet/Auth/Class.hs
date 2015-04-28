{-# LANGUAGE MultiParamTypeClasses #-}

module Snaplet.Auth.Class where

import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple


-- | Defined for snaplets which access Postgres-based authentication
-- engine.
--
-- If a snaplet @Foo b@ includes @auth :: SnapletLens b (AuthManager b)@
-- and @db :: SnapletLens b Postgres@ in its state, define the
-- instance as follows:
--
-- > withAuth = withLens auth
-- > withAuthPg = withLens db
--
-- In this case, avoid using @withAuthPg@ as a drop-in replacement for
-- @with db@ (@withAuthPg@ is specifically for authentication-related
-- database access).
class HasPostgresAuth b v where
  withAuth :: Handler b (AuthManager b) a -> Handler b v a
  withAuthPg :: Handler b Postgres a -> Handler b v a

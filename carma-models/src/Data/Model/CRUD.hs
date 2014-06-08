{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.CRUD
  (getModelCRUD
  ,withEphemeralField
  ,CrudError(..)
  ,CRUD(..)
  ) where

import Control.Error
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PG
import Data.Typeable
import GHC.TypeLits

import Data.Model
import Data.Model.Types
import Data.Model.Patch as Patch
import Data.Model.Patch.Sql as Sql


getModelCRUD :: Model m => CRUD m
getModelCRUD = fromMaybe defaultCRUD $ modelCRUD modelInfo


defaultCRUD :: forall m . Model m => CRUD m
defaultCRUD = CRUD
  { crud_create = \obj pg -> do
      p     <- hoistEither $ parseJSON obj
      ident <- tryPg $ Sql.create p pg
      res   <- tryPg $ Sql.read (ident :: IdentI m) pg
      hoistEither $ unparseRes ident res

  , crud_read = \ident pg
    -> tryPg (Sql.read ident pg)
    >>= hoistEither . unparseRes ident

  , crud_update = \ident obj pg -> do
      p <- hoistEither $ parseJSON obj
      tryPg (Sql.update ident p pg) >>= hoistEither . \case
        1 -> Right $ Aeson.object []
        0 -> Left  $ NoSuchObject $ show ident
        _ -> Left  $ InconsistentDbState $ show ident

  , crud_delete = error "not implemented"
  }


withEphemeralField
  :: forall m t n d
  .  (SingI n, Model m, Typeable t)
  => ModelInfo m -> (m -> EF t n d, IdentI m -> PG.Connection -> IO t)
  -> ModelInfo m
withEphemeralField m (f, fn) = m {modelCRUD = Just crud'}
  where
    crud = fromMaybe defaultCRUD $ modelCRUD m :: CRUD m
    crud' = crud { crud_read = crud_read'}
    crud_read' ident pg = do
      obj <- crud_read crud ident pg
      p   <- hoistEither $ parseJSON obj
      v   <- tryPg $ fn ident pg
      return $ Aeson.toJSON $ Patch.put f v p


parseJSON :: Model m => Aeson.Value -> Either CrudError (Patch m)
parseJSON jsn = case Aeson.fromJSON jsn of
  Aeson.Error msg -> Left $ MalformedJSON msg
  Aeson.Success p -> Right p


tryPg :: IO a -> EitherT CrudError IO a
tryPg = fmapLT (PgException . show) . syncIO


unparseRes :: Model m => IdentI m -> [Patch m] -> Either CrudError Aeson.Value
unparseRes ident = \case
  [p'] -> Right $ Aeson.toJSON p'
  []   -> Left  $ NoSuchObject $ show ident
  _    -> Left  $ InconsistentDbState $ show ident

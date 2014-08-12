{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.CRUD
  (getModelCRUD
  ,customizeRead
  ,replaceReadManyWithFilter
  ,CrudError(..)
  ,CRUD(..)
  ) where

import Control.Error
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as PG
import Data.Text (Text)

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
      liftIO (Sql.create p pg) >>= \case
        Left ex -> left $ PgException ex
        Right ident -> do
          res <- tryPg $ Sql.read (ident :: IdentI m) pg
          hoistEither $ unparseRes ident res

  , crud_read = \ident pg
    -> tryPg (Sql.read ident pg)
    >>= hoistEither . unparseRes ident

  , crud_update = \ident obj pg -> do
      p <- hoistEither $ parseJSON obj
      liftIO (Sql.update ident p pg) >>= \case
        Right 1 -> right $ Aeson.object []
        Right 0 -> left  $ NoSuchObject $ show ident
        Right _ -> left  $ InconsistentDbState $ show ident
        Left ex -> left  $ PgException ex

  , crud_delete = error "not implemented"
  , crud_readManyWithFilter = \lim off flt pg -> do
      ps :: [Patch m] <- tryPg $ Sql.readManyWithFilter lim off flt pg
      hoistEither $ return $ Aeson.toJSON ps
  }


customizeRead
  :: forall m .  (Model m)
  => ModelInfo m -> (Patch m -> IdentI m -> PG.Connection -> IO (Patch m))
  -> ModelInfo m
customizeRead m fn = m {modelCRUD = Just crud'}
  where
    crud = fromMaybe defaultCRUD $ modelCRUD m :: CRUD m
    crud' = crud { crud_read = crud_read'}
    crud_read' ident pg = do
      obj <- crud_read crud ident pg
      p   <- hoistEither $ parseJSON obj
      p'  <- tryPg $ fn p ident pg
      return $ Aeson.toJSON p'

replaceReadManyWithFilter
  :: forall m .  (Model m)
  => ModelInfo m
  -> (Limit -> Offset -> [(Text,Text)] -> PG.Connection -> IO [Patch m])
  -> ModelInfo m
replaceReadManyWithFilter m fn = m {modelCRUD = Just crud'}
  where
    crud = fromMaybe defaultCRUD $ modelCRUD m :: CRUD m
    crud' = crud { crud_readManyWithFilter = read'}
    read' lim off flt pg = Aeson.toJSON <$> (tryPg $ fn lim off flt pg)

parseJSON :: Model m => Aeson.Value -> Either CrudError (Patch m)
parseJSON jsn = case Aeson.fromJSON jsn of
  Aeson.Error msg -> Left $ MalformedJSON msg
  Aeson.Success p -> Right p


tryPg :: IO a -> EitherT CrudError IO a
tryPg = fmapLT PgException . syncIO


unparseRes :: Model m => IdentI m -> [Patch m] -> Either CrudError Aeson.Value
unparseRes ident = \case
  [p'] -> Right $ Aeson.toJSON p'
  []   -> Left  $ NoSuchObject $ show ident
  _    -> Left  $ InconsistentDbState $ show ident

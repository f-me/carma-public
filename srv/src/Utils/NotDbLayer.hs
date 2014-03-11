{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

DbLayer <-> carma-models compatibility layer.

This module provides a subset of DbLayer-compliant interface for new
(carma-models) models using Aeson-level representation for Objects
(DbLayer)/Patches (carma-models) as a common denominator.

'read'/'update' reduce the required amount of carma-models dispatch
scaffolding.

-}

module Utils.NotDbLayer
    ( readIdent
    , exists
    , read
    , fieldProj
    , update
    , fieldPatch
    )

where

import Prelude hiding (read)

import Control.Monad.IO.Class
import Data.Functor

import qualified Data.Aeson as Aeson
import Data.ByteString.Char8 (readInt)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as HM
import Data.Pool (withResource)
import qualified Data.Text.Encoding as T
import qualified Snap.Snaplet.PostgresqlSimple as PS

import Carma.Model
import qualified Data.Model.Patch.Sql as Patch

import qualified Snaplet.DbLayer as DB (exists, read, update)
import Snaplet.DbLayer.Types hiding (Object)


readIdent :: ObjectId -> IdentI m
readIdent bs = case readInt bs of
                 Just (n, _) -> Ident n
                 Nothing     -> error "readIdent: no integer"


type ProtoObject = Aeson.Object


-- | 'DbLayer.exists' replacement.
exists :: ModelName -> ObjectId -> DbHandler b Bool
exists model objId =
  let modelExists :: forall m b. Model m =>
                     m
                  -> DbHandler b Bool
      modelExists _ = do
        let ident = readIdent objId :: IdentI m
        s <- PS.getPostgresState
        res <- liftIO $
               withResource (PS.pgPool s) (Patch.read ident)
        return $ not $ null res
  in
    case Carma.Model.dispatch (T.decodeUtf8 model) modelExists of
      Just fn -> fn
      _       -> DB.exists model objId

recode :: (Aeson.FromJSON t, Aeson.ToJSON f) => f -> t
recode o = case Aeson.decode $ Aeson.encode o of
             Just v -> v
             Nothing -> error "NDB: JSON conversion failed"

-- | 'DbLayer.read' replacement.
read :: ModelName -> ObjectId -> DbHandler b ProtoObject
read model objId = do
  let readModel :: forall m b. Model m =>
                   m
                -> DbHandler b (Either String ProtoObject)
      readModel _ = do
        let ident = readIdent objId :: IdentI m
        s <- PS.getPostgresState
        res <- liftIO $
               withResource (PS.pgPool s) (Patch.read ident)
        return $ case res of
          [obj] -> Right $ recode obj
          []    -> Left  $ "NDB.read: could not read model"
          _     -> Left  $ "NDB.read: " ++ show (Aeson.encode res)
  case Carma.Model.dispatch (T.decodeUtf8 model) readModel of
    Just fn -> fn >>= \case
               Right obj -> return $ recode obj
               -- Short-circuit in case of errors or if no object is
               -- read. Old-style DB.read returns empty object in
               -- this case.
               Left s    -> error s
    _ -> recode <$> DB.read model objId


-- | Convenience helper to be used with 'read' to read just one field
-- value.
fieldProj :: FieldName -> ProtoObject -> FieldValue
fieldProj field p =
    maybe "" (\case
              (Aeson.String t) -> T.encodeUtf8 t
              _                -> "") $
    HM.lookup (T.decodeUtf8 field) p


-- | 'DbLayer.update' replacement. All triggers are ignored with new
-- models. Return empty object when used with new models.
update :: ModelName -> ObjectId -> ProtoObject
       -> DbHandler b ProtoObject
update model objId commit =
  let -- Return error message or count of rows that have been updated,
      -- False if no objects matched.
      updateModel :: forall m b. Model m =>
                     m
                  -> DbHandler b (Either String Int64)
      updateModel _ = do
        let ident = readIdent objId :: IdentI m
            commit' = recode commit
        s <- PS.getPostgresState
        res <- liftIO $
               withResource (PS.pgPool s) (Patch.update ident commit')
        return $ Right res
  in
    case Carma.Model.dispatch (T.decodeUtf8 model) updateModel of
      Just fn -> fn >>= \case
                 Right 0  -> error "NDB.update: could not update model"
                 Right _  -> return $ HM.empty
                 Left s   -> error s
      Nothing -> recode <$> DB.update model objId (recode commit)


-- | Convenience helper to be used with 'update' to update just one field
-- value.
fieldPatch :: FieldName -> Aeson.Value -> ProtoObject
fieldPatch f v = HM.singleton (T.decodeUtf8 f) v
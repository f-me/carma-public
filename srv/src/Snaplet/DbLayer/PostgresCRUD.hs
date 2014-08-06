module Snaplet.DbLayer.PostgresCRUD (
    loadRelations,
    insert, update, updateMany, insertUpdate, insertUpdateMany
    ) where


import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO

import qualified Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as T

import qualified Database.PostgreSQL.Simple as P
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Data.Pool as Pool

import qualified Database.PostgreSQL.Sync as S
import Database.PostgreSQL.Sync.JSON ()

import Snaplet.DbLayer.Types
import qualified Carma.ModelTables as MT
import Util


inPsql :: (PS.HasPostgres m, MonadCatchIO m) => (P.Connection -> m ()) -> m ()
inPsql act = do
    s <- PS.getPostgresState
    Pool.withResource (PS.pgPool s) act

loadRelations :: FilePath -> IO S.Relations
loadRelations
  = liftIO
  . fmap (fromMaybe (error "Unable to load relations") . A.decode)
  . LB.readFile

trObj :: Object -> Map ByteString ByteString
trObj = M.map T.encodeUtf8 . M.mapKeys T.encodeUtf8


getModel :: Monad m => ModelName -> [MT.TableDesc] -> m MT.TableDesc
getModel modelName descs = maybe (error $ "No model " ++ show modelName) return $ MT.tableByModel (T.encodeUtf8 modelName) descs

insert :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> ModelName -> Object -> m ()
insert descs modelName dat
  = hushExceptions "PostgresCRUD/insert" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.insert con desc' (MT.addType desc' $ trObj dat)

update :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> ModelName -> ObjectId -> Object -> m ()
update descs modelName modelId dat
  = hushExceptions "PostgresCRUD/update" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.update con desc' (T.encodeUtf8 modelId) $ trObj dat

insertUpdate :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> ModelName -> ObjectId -> Object -> m ()
insertUpdate descs modelName modelId dat
  = hushExceptions "PostgresCRUD/insertUpdate" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.insertUpdate con desc' (T.encodeUtf8 modelId) $ trObj dat

updateMany :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> M.Map (ModelName, ObjectId) Object -> m ()
updateMany descs m
  = logExceptions "PostgresCRUD/updateMany" $ forM_ (M.toList m) $ uncurry update'
  where
    update' (mdlName, mdlId) = update descs mdlName mdlId

insertUpdateMany :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> M.Map (ModelName, ObjectId) Object -> m ()
insertUpdateMany descs m
  = logExceptions "PostgresCRUD/insertUpdateMany" $ forM_ (M.toList m) $ uncurry insertUpdate'
  where
    insertUpdate' (mdlName, mdlId) = insertUpdate descs mdlName mdlId

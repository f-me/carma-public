module Snaplet.DbLayer.PostgresCRUD (
    loadRelations,
    insert, update, updateMany, insertUpdate, insertUpdateMany
    ) where


import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO

import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Database.PostgreSQL.Simple as P
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Data.Pool as Pool

import qualified Database.PostgreSQL.Sync as S
import Database.PostgreSQL.Sync.JSON ()

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

toStr :: ByteString -> String
toStr = T.unpack . T.decodeUtf8

getModel :: Monad m => ByteString -> [MT.TableDesc] -> m MT.TableDesc
getModel modelName descs = maybe (error $ "No model " ++ toStr modelName) return $ MT.tableByModel modelName descs

insert :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> ByteString -> M.Map ByteString ByteString -> m ()
insert descs modelName dat
  = hushExceptions "PostgresCRUD/insert" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.insert con desc' (MT.addType desc' dat)

update :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> ByteString -> ByteString -> M.Map ByteString ByteString -> m ()
update descs modelName modelId dat
  = hushExceptions "PostgresCRUD/update" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.update con desc' modelId dat

insertUpdate :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> ByteString -> ByteString -> M.Map ByteString ByteString -> m ()
insertUpdate descs modelName modelId dat
  = hushExceptions "PostgresCRUD/insertUpdate" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.insertUpdate con desc' modelId dat

updateMany :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> M.Map (ByteString, ByteString) (M.Map ByteString ByteString) -> m ()
updateMany descs m
  = logExceptions "PostgresCRUD/updateMany" $ forM_ (M.toList m) $ uncurry update'
  where
    update' (mdlName, mdlId) = update descs mdlName mdlId

insertUpdateMany :: (PS.HasPostgres m, MonadCatchIO m) => [MT.TableDesc] -> M.Map (ByteString, ByteString) (M.Map ByteString ByteString) -> m ()
insertUpdateMany descs m
  = logExceptions "PostgresCRUD/insertUpdateMany" $ forM_ (M.toList m) $ uncurry insertUpdate'
  where
    insertUpdate' (mdlName, mdlId) = insertUpdate descs mdlName mdlId

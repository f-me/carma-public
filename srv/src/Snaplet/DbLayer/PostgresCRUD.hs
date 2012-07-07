module Snaplet.DbLayer.PostgresCRUD (
	caseModel,
	models,

	createIO,
	create, insert, select, update, updateMany
	) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Exception as E

import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.ToField as P
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Data.Pool as Pool

import qualified Database.PostgreSQL.Syncs as S

withPG :: (PS.HasPostgres m) => (P.Connection -> IO a) -> m a
withPG f = do
	s <- PS.getPostgresState
	liftIO $ Pool.withResource (PS.pgPool s) f

caseModel :: S.Sync
caseModel = S.sync "casetbl" "garbage" [
	S.field_ "id" S.int,
--	S.field_ "car_make" S.string,
--	S.field_ "car_program" S.string,
--	S.field_ "car_vin" S.string,
--	S.field_ "car_buyDate" S.time,
	S.field_ "callDate" S.time,
	S.field_ "callTaker" S.string,
	S.field_ "callerOwner" S.int,
	S.field_ "caller_name" S.string]

models :: S.Syncs
models = S.syncs [
	("case", caseModel)] []

local :: P.ConnectInfo
local = P.ConnectInfo {
	P.connectHost = "localhost",
	P.connectPort = 5432,
	P.connectUser = "postgres",
	P.connectPassword = "2741001",
	P.connectDatabase = "postgres" }

elog :: IO () -> IO ()
elog act = E.catch act onError where
    onError :: E.SomeException -> IO ()
    onError e = putStrLn $ "Failed with: " ++ show e

createIO :: S.Syncs -> IO ()
createIO ss = do
	con <- P.connect local
	elog $ void $ P.execute_ con "create extension hstore"
	S.transaction con $ S.create ss

toStr :: ByteString -> String
toStr = T.unpack . T.decodeUtf8

toCond :: S.Syncs -> ByteString -> ByteString -> S.Condition
toCond ss m c = S.condition ss (toStr m ++ ".id = ?") [P.toField (toStr c)]

create :: (PS.HasPostgres m) => S.Syncs -> m ()
create ss = withPG (S.inPG $ S.create ss)

insert :: (PS.HasPostgres m) => S.Syncs -> ByteString -> S.SyncMap -> m ()
insert ss name m = withPG (S.inPG $ S.insert ss (toStr name) m)

select :: (PS.HasPostgres m) => S.Syncs -> ByteString -> ByteString -> m S.SyncMap
select ss name c = withPG (S.inPG $ S.select ss (toStr name) cond) where
	cond = toCond ss name c

update :: (PS.HasPostgres m) => S.Syncs -> ByteString -> ByteString -> S.SyncMap -> m ()
update ss name c m = withPG (S.inPG $ S.update ss (toStr name) cond m) where
	cond = toCond ss name c

updateMany :: (PS.HasPostgres m) => S.Syncs -> ByteString -> M.Map ByteString S.SyncMap -> m ()
updateMany ss name ms = forM_ (M.toList ms) $ uncurry (update ss name) where
	update' k obj = update ss name k (M.insert (C8.pack "id") k obj)

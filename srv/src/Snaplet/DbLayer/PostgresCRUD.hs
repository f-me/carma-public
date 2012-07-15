module Snaplet.DbLayer.PostgresCRUD (
	modelSyncs, modelModels,

	createIO,
	create, insert, select, exists, update, updateMany, insertUpdate,
	generateReport
	) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Exception as E

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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
import qualified Database.PostgreSQL.Models as SM
import qualified Database.PostgreSQL.Report.Xlsx as R

import Snaplet.DbLayer.Dictionary

withPG :: (PS.HasPostgres m) => (P.Connection -> IO a) -> m a
withPG f = do
	s <- PS.getPostgresState
	liftIO $ Pool.withResource (PS.pgPool s) f

caseModel :: S.Sync
caseModel = S.sync "casetbl" "garbage" [
    S.indexed $ S.field_ "id" S.int,
    S.indexed $ S.field_ "car_make" S.string,
    S.indexed $ S.field_ "car_model" S.string,
    S.indexed $ S.field_ "car_program" S.string,
    S.indexed $ S.field_ "car_vin" S.string,
    S.field_ "car_buyDate" S.time,
    S.field_ "car_plateNum" S.string,
    S.field_ "car_carModel" S.string,
    S.indexed $ S.field_ "diagnosis1" S.string,
    S.indexed $ S.field_ "diagnosis2" S.string,
    S.indexed $ S.field_ "dealerCause" S.string,
    S.field_ "caseAddress_address" S.string,
    S.indexed $ S.field_ "callDate" S.time,
    S.field_ "callTaker" S.string,
    S.field_ "callerOwner" S.int,
    S.field_ "caller_name" S.string,
    S.indexed $ S.field_ "comment" S.string,
    S.field_ "program" S.string,
    S.field_ "services" S.string,
    S.field_ "owner_name" S.string,
    S.field_ "partner_name" S.string]

serviceModel :: S.Sync
serviceModel = S.sync "servicetbl" "garbage" [
    S.field_ "id" S.int,
    S.field_ "parentId" S.int,
    S.field_ "status" S.string,
    S.field_ "type" S.string,
    S.field_ "falseCall" S.string,
    S.field_ "towDealer_name" S.string,
    S.field_ "orderNumber" S.int,
    S.field_ "suburbanMilage" S.int,
    S.field_ "warrantyCase" S.string,
    S.field_ "times_repairEndDate" S.time,
    S.field_ "times_factServiceStart" S.time,
    S.field_ "times_factServiceEnd" S.time,
    S.field_ "carProvidedFor" S.int,
    S.field_ "hotelProvidedFor" S.int,
    S.field_ "payment_limitedCost" S.int,
    S.field_ "payment_partnerCost" S.int,
    S.field_ "payType" S.string,
    S.field_ "clientSatisfied" S.string]

service :: String -> (String, SM.Model)
service tp = (tp, mdl) where
	mdl = SM.model tp serviceModel [
		SM.constant "type" tp,
		SM.adjustField "parentId" (drop 5) ("case:" ++)]

modelSyncs :: S.Syncs
modelSyncs = S.syncs [
	("case", caseModel),
	("service", serviceModel)] [
	"case.id = service.parentId"]

modelModels :: SM.Models
modelModels = SM.models modelSyncs $ [("case", SM.model "case" caseModel [])] ++ map service [
    "deliverCar",
    "deliverParts",
    "hotel",
    "information",
    "rent",
    "sober",
    "taxi",
    "tech",
    "towage",
    "transportation"]

local :: P.ConnectInfo
local = P.ConnectInfo {
	P.connectHost = "localhost",
	P.connectPort = 5432,
	P.connectUser = "postgres",
	P.connectPassword = "pass",
	P.connectDatabase = "postgres" }

elog :: IO () -> IO ()
elog act = E.catch act onError where
    onError :: E.SomeException -> IO ()
    onError e = putStrLn $ "Failed with: " ++ show e

elogv :: a -> IO a -> IO a
elogv v act = E.catch act (onError v) where
	onError :: a -> E.SomeException -> IO a
	onError x e = do
		putStrLn $ "Failed with: " ++ show e
		return x

createIO :: SM.Models -> IO ()
createIO ms = do
	con <- P.connect local
	elog $ void $ P.execute_ con "create extension hstore"
	S.transaction con $ S.create (SM.modelsSyncs ms)

toStr :: ByteString -> String
toStr = T.unpack . T.decodeUtf8

toCond :: SM.Models -> ByteString -> ByteString -> S.Condition
toCond ms m c = S.condition (SM.modelsSyncs ms) (tableName ++ ".id = ?") [P.toField (toStr c)] where
	tableName = fromMaybe (error "Invalid model name") $ fmap (SM.syncTable . SM.modelSync) $ M.lookup (toStr m) (SM.modelsModels ms)

create :: (PS.HasPostgres m) => S.Syncs -> m ()
create ss = withPG (elog . S.inPG (S.create ss))

insert :: (PS.HasPostgres m) => SM.Models -> ByteString -> S.SyncMap -> m ()
insert ms name m = withPG (elog . S.inPG (SM.insert ms (toStr name) m))

select :: (PS.HasPostgres m) => SM.Models -> ByteString -> ByteString -> m S.SyncMap
select ms name c = withPG (S.inPG $ SM.select ms (toStr name) cond) where
	cond = toCond ms name c

exists :: (PS.HasPostgres m) => SM.Models -> ByteString -> ByteString -> m Bool
exists ms name c = withPG (elogv False . S.inPG (SM.exists ms (toStr name) cond)) where
	cond = toCond ms name c

update :: (PS.HasPostgres m) => SM.Models -> ByteString -> ByteString -> S.SyncMap -> m ()
update ms name c m = withPG (elog . S.inPG (SM.update ms (toStr name) cond m)) where
	cond = toCond ms name c

updateMany :: (PS.HasPostgres m) => SM.Models -> ByteString -> M.Map ByteString S.SyncMap -> m ()
updateMany ms name m = forM_ (M.toList m) $ uncurry (update ms name) where
	update' k obj = update ms name k (M.insert (C8.pack "id") k obj)

insertUpdate :: (PS.HasPostgres m) => SM.Models -> ByteString -> ByteString -> S.SyncMap -> m Bool
insertUpdate ms name c m = withPG (elogv False . S.inPG (SM.insertUpdate ms (toStr name) cond m)) where
	cond = toCond ms name c

generateReport :: (PS.HasPostgres m, MonadIO m) => SM.Models -> FilePath -> FilePath -> m ()
generateReport ms tpl file = do
	dicts <- liftIO $ loadDicts "/home/voidex/Documents/Projects/carma/srv/resources/site-config/dictionaries"
	withPG (S.inPG $ R.createReport (SM.modelsSyncs ms) dicts tpl file)

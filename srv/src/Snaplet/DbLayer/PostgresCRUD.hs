{-# OverloadedStrings #-}

module Snaplet.DbLayer.PostgresCRUD (
    modelSyncs, modelModels,

    createIO,
    create, insert, select, exists, update, updateMany, insertUpdate,
    generateReport
    ) where

import Prelude hiding (log, catch)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO
import qualified Control.Exception as E

import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.ByteString (ByteString)
import Data.Char
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
import qualified Database.PostgreSQL.Report.Function as R

import Snaplet.DbLayer.Dictionary
import Snap.Snaplet.SimpleLog

withPG :: (PS.HasPostgres m, MonadLog m) => S.TIO a -> m a
withPG f = do
    s <- PS.getPostgresState
    l <- askLog
    liftIO $ Pool.withResource (PS.pgPool s) (withLog l . S.inPG f)

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

functions :: M.Map String (M.Map String String) -> [R.ReportFunction]
functions ds = [
    R.onString "NAME" (fromMaybe "" . listToMaybe . drop 1 . words),
    R.onString "LASTNAME" (fromMaybe "" . listToMaybe . words),
    R.onString "UPPER" (map toUpper),
    R.onString "LOWER" (map toLower),
    R.function "CONCAT" concatFields,
    R.functionMaybe "LOOKUP" lookupField,
    R.function "IF" ifFun,
    R.uses ["case.callerOwner", "case.caller_name", "case.owner_name"] $ R.constFunction "OWNER" ownerFun,
    R.uses ["case.program"] $ R.constFunction "FDDS" fddsFun,
    R.uses ["case.falseCall"] $ R.constFunction "FALSECALL" falseFun,
    R.uses ["case.falseCall"] $ R.constFunction "BILL" billFun,
    R.uses ["case.diagnosis1", "service.type"] $ R.constFunction "FAULTCODE" faultFun,
    R.uses ["case.car_make"] $ R.constFunction "VEHICLEMAKE" vehicleMakeFun,
    R.uses ["case.car_make", "case.car_model"] $ R.constFunction "VEHICLEMODEL" vehicleModelFun]
    where
        concatFields fs = SM.StringValue $ concat $ mapMaybe fromStringField fs
        fromStringField (SM.StringValue s) = Just s
        fromStringField _ = Nothing

        lookupField [SM.StringValue s, SM.StringValue d] = look <|> Just (SM.StringValue s) where
            look = do
                d' <- M.lookup d ds
                s' <- M.lookup s d'
                return $ SM.StringValue s'
        lookupField _ = Nothing
        
        ifFun [i, t, f]
            | i `elem` [SM.StringValue "1", SM.StringValue "true", SM.StringValue "Y", SM.IntValue 1, SM.BoolValue True] = t
            | otherwise = f
        
        fddsFun fs = do
            (SM.StringValue pr) <- M.lookup "case.program" fs
            case pr of
                "Ford" -> return $ SM.StringValue "3351"
                "GM" -> return $ SM.StringValue "3275"
                _ -> return $ SM.StringValue ""

        ownerFun fs = do
            (SM.IntValue isOwner) <- M.lookup "case.callerOwner" fs
            (if isOwner == 1 then M.lookup "case.caller_name" else M.lookup "case.owner_name") fs
        
        falseFun fs = do
            (SM.StringValue isFalse) <- M.lookup "case.falseCall" fs
            return $ SM.StringValue (if isFalse `elem` ["bill", "nobill"] then "Y" else "N")
        
        billFun fs = do
            (SM.StringValue isFalse) <- M.lookup "case.falseCall" fs
            return $ SM.StringValue (if isFalse == "bill" then "Y" else "N")
            
        faultFun fs = do
            (SM.StringValue d) <- M.lookup "case.diagnosis1" fs
            (SM.StringValue s) <- M.lookup "service.type" fs
            d' <- M.lookup d $ M.fromList [
                ("engine", "355"),
                ("keyLost", "667"),
                ("engcool", "349"),
                ("steer", "260"),
                ("signal", "652"),
                ("electro", "400"), -- ???
                ("fuel", "599"),
                ("brake", "298"),
                ("gears", "745"),
                ("needDiagnostic", "921"),
                ("tread", "246"),
                ("electronics", "514"),
                ("dtp", "144")]
            s' <- M.lookup s $ M.fromList [
                ("tech", "55"),
                ("towage", "6G")]
            return $ SM.StringValue $ d' ++ "09" ++ s'
            
        vehicleMakeFun fs = do
            (SM.StringValue m) <- M.lookup "case.car_make" fs
            m' <- M.lookup m $ M.fromList [
                ("ford", "09"),
                ("chevy", "10"),
                ("opel", "23"),
                ("cad", "11")]
            return $ SM.StringValue m'
            
        vehicleModelFun fs = do
            (SM.StringValue mk) <- M.lookup "case.car_make" fs
            (SM.StringValue md) <- M.lookup "case.car_model" fs
            r <- M.lookup mk $ M.fromList [
                ("ford", fromMaybe "0900" $ M.lookup md $ M.fromList [
                      ("ka", "0908"),
                      ("sportKa", "0912"),
                      ("streetKa", "0911"),
                      ("fiesta", "0913"),
                      ("fusion", "0918"),
                      ("puma", "0916"),
                      ("escort", "0920"),
                      ("cMaxII", "0928"),
                      ("focus", "0934"),
                      ("mondeo", "0944"),
                      ("cougar", "0951"),
                      ("maverick", "0955"),
                      ("explorer", "0960"),
                      ("expedition", "0986"),
                      ("excursion", "0987"),
                      ("galaxy", "0967"),
                      ("edge", "0973"),	
                      ("s-max", "0975"),
                      ("kuga", "0977"),	
                      ("windstar", "0970"),
                      ("transit", "0978"),
                      ("tourneoConnect", "0979"),
                      ("ranger", "0985"),
                      ("spark", "1003"),
                      ("matiz", "1005"),
                      ("j200", "1007"),
                      ("volt", "1011"),
                      ("lanos", "1020"),
                      ("cruze", "1021"),
                      ("evanda", "1022"),
                      ("epica", "1033"),
                      ("kalos", "1023"),
                      ("aveo", "1027"),
                      ("nubira", "1025"),
                      ("leganza", "1030"),
                      ("rezzo", "1035"),
                      ("lacetti", "1040"),
                      ("hhr", "1043"),
                      ("orlando", "1047"),
                      ("captiva", "1045")]),
                ("opel", fromMaybe "2300" $ M.lookup md $ M.fromList [
                      ("agila", "2308"),
                      ("corsa", "2310"),
                      ("meriva", "2317"),
                      ("combo", "2311"),
                      ("astra", "2334"),
                      ("insignia", "2322"),
                      ("zafira", "2333"),
                      ("vectra", "2335"),
                      ("signum", "2337"),
                      ("omega", "2340"),
                      ("tigra", "2350"),
                      ("speedster", "2352"),
                      ("gt", "2358"),
                      ("ampera", "2361"),
                      ("frontera", "2365"),
                      ("monterey", "2370"),
                      ("campo", "2375"),
                      ("iseze", "2385"),
                      ("arena", "2390"),
                      ("vivaro", "2392"),
                      ("movano", "2391"),
                      ("antara", "2396")]),
                ("chevy", fromMaybe "1000" $ M.lookup md $ M.fromList [
                      ("spark", "1003"),
                      ("matiz", "1005"),
                      ("j200",	"1007"),
                      ("volt", "1011"),
                      ("lanos", "1020"),
                      ("cruze", "1021"),
                      ("evanda", "1022"),
                      ("epica", "1033"),
                      ("kalos", "1023"),
                      ("aveo", "1027"),
                      ("nubira", "1025"),
                      ("leganza", "1030"),
                      ("tacuma", "1035"),
                      ("lacetti", "1040"),
                      ("hhr", "1043"),
                      ("orlando", "1047"),
                      ("captiva", "1045")]),
                ("cad", fromMaybe "1115" $ M.lookup md $ M.fromList [
                      ("seville", "1116"),
                      ("bls", "1121"),
                      ("cts", "1117"),
                      ("xlr", "1118"),
                      ("dts", "1122"),
                      ("sts", "1123"),
                      ("srx", "1119"),
                      ("escalade", "1124")])]
            return $ SM.StringValue r
                      
local :: P.ConnectInfo
local = P.ConnectInfo {
	P.connectHost = "localhost",
	P.connectPort = 5432,
	P.connectUser = "carma_db_sync",
	P.connectPassword = "pass",
	P.connectDatabase = "carma" }

escope :: (MonadLog m) => T.Text -> m () -> m ()
escope s act = catch (scope s act) onError where
    onError :: (MonadLog m) => E.SomeException -> m ()
    onError _ = return ()

escopev :: (MonadLog m) => T.Text -> a -> m a -> m a
escopev s v act = catch (scope s act) (onError v) where
    onError :: (MonadLog m) => a -> E.SomeException -> m a
    onError x _ = return x

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

createIO :: SM.Models -> Log -> IO ()
createIO ms l = do
    con <- P.connect local
    let
        onError :: E.SomeException -> IO ()
        onError _ = return ()
    E.catch (void $ P.execute_ con "create extension hstore") onError
    withLog l $ S.transaction con $ S.create (SM.modelsSyncs ms)

toStr :: ByteString -> String
toStr = T.unpack . T.decodeUtf8

toCond :: SM.Models -> ByteString -> ByteString -> S.Condition
toCond ms m c = S.conditionComplex (SM.modelsSyncs ms) (tableName ++ ".id = ?") [P.toField (toStr c)] where
    tableName = fromMaybe (error "Invalid model name") $ fmap (SM.syncTable . SM.modelSync) $ M.lookup (toStr m) (SM.modelsModels ms)

create :: (PS.HasPostgres m, MonadLog m) => S.Syncs -> m ()
create ss = escope "create" $ withPG (S.create ss)

insert :: (PS.HasPostgres m, MonadLog m) => SM.Models -> ByteString -> S.SyncMap -> m ()
insert ms name m = escope "insert" $ withPG (SM.insert ms (toStr name) m)

select :: (PS.HasPostgres m, MonadLog m) => SM.Models -> ByteString -> ByteString -> m S.SyncMap
select ms name c = scoper "select" $ do
    log Trace $ T.concat ["Selecting id ", T.decodeUtf8 c]
    withPG (SM.select ms (toStr name) cond)
    where
        cond = toCond ms name c

exists :: (PS.HasPostgres m, MonadLog m) => SM.Models -> ByteString -> ByteString -> m Bool
exists ms name c = escopev "exists" False $ do
    log Trace $ T.concat ["Checking for existing id ", T.decodeUtf8 c]
    withPG (SM.exists ms (toStr name) cond)
    where
        cond = toCond ms name c

update :: (PS.HasPostgres m, MonadLog m) => SM.Models -> ByteString -> ByteString -> S.SyncMap -> m ()
update ms name c m = escope "update" $ do
    log Trace $ T.concat ["Updating id ", T.decodeUtf8 c]
    withPG (SM.update ms (toStr name) cond m)
    where
        cond = toCond ms name c

updateMany :: (PS.HasPostgres m, MonadLog m) => SM.Models -> ByteString -> M.Map ByteString S.SyncMap -> m ()
updateMany ms name m = scope "updateMany" $ forM_ (M.toList m) $ uncurry (update ms name) where
    update' k obj = update ms name k (M.insert (C8.pack "id") k obj)

insertUpdate :: (PS.HasPostgres m, MonadLog m) => SM.Models -> ByteString -> ByteString -> S.SyncMap -> m Bool
insertUpdate ms name c m = escopev "insertUpdate" False $ do
    log Trace $ T.concat ["Processing id ", T.decodeUtf8 c]
    withPG (SM.insertUpdate ms (toStr name) cond m)
    where
        cond = toCond ms name c

generateReport :: (PS.HasPostgres m, MonadLog m) => SM.Models -> [T.Text] -> FilePath -> FilePath -> m ()
generateReport ms conds tpl file = scope "generateReport" $ do
    log Info "Generating report"
    log Trace "Loading dictionaries"
    dicts <- scope "dictionaries" $ liftIO $ loadDicts "resources/site-config/dictionaries"
    scope "createReport" $ withPG (R.createReport (SM.modelsSyncs ms) (functions dicts) conds tpl file)
    log Info "Report generated"

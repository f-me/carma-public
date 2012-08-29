{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.PostgresCRUD (
    modelModels,

    loadModels,
    createIO,
    create, insert, select, exists, update, updateMany, insertUpdate,
    search,
    generateReport
    ) where

import Prelude hiding (log, catch)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO
import qualified Control.Exception as E

import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.ByteString (ByteString)
import Data.Char
import Data.List (isPrefixOf)
import Data.String
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX

import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.ToField as P
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Data.Pool as Pool

import qualified Database.PostgreSQL.Syncs as S
import qualified Database.PostgreSQL.Models as SM
import Database.PostgreSQL.Sync.JSON
import qualified Database.PostgreSQL.Report.Xlsx as R
import qualified Database.PostgreSQL.Report.Function as R

import Snaplet.DbLayer.Dictionary
import Snap.Snaplet.SimpleLog

import qualified Snaplet.DbLayer.ARC as ARC
import qualified Snaplet.DbLayer.RKC as RKC

withPG :: (PS.HasPostgres m, MonadLog m) => S.TIO a -> m a
withPG f = do
    s <- PS.getPostgresState
    l <- askLog
    liftIO $ Pool.withResource (PS.pgPool s) (withLog l . S.inPG f)

service :: S.Sync -> String -> (String, SM.Model)
service srv tp = (tp, mdl) where
    mdl = SM.model tp srv [
        SM.constant "type" tp,
        SM.adjustField "parentId" (drop 5) ("case:" ++)]

modelModels :: S.Syncs -> Maybe SM.Models
modelModels ss = do
    srv <- M.lookup "service" $ S.syncsSyncs ss
    return $ SM.models ss $ allModels ++ map (service srv) services
    where
        noService = M.delete "service" $ S.syncsSyncs ss
        allModels = map toModel . M.toList $ noService
        toModel (name, snc) = (name, SM.model name snc [])
        services = [
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

functions :: TimeZone -> Dictionary -> [R.ReportFunction]
functions tz dict = [
    R.onString "NAME" (fromMaybe "" . listToMaybe . drop 1 . words),
    R.onString "LASTNAME" (fromMaybe "" . listToMaybe . words),
    R.onString "UPPER" (map toUpper),
    R.onString "LOWER" (map toLower),
    R.onString "PHONE" phoneFmt,
    R.function "CONCAT" concatFields,
    R.functionMaybe "LOOKUP" lookupField,
    R.function "IF" ifFun,
    R.functionMaybe "DATEDIFF" dateDiff,
    R.functionMaybe "YESNO" yesNo,
    R.function "DATE" (formatTimeFun "%d.%m.%Y"),
    R.function "TIME" (formatTimeFun "%d.%m.%Y %H:%M:%S"),
    R.uses ["case.callerOwner", "case.caller_name", "case.owner_name"] $ R.constFunction "OWNER" ownerFun,
    R.uses ["case.program"] $ R.constFunction "FDDS" fddsFun,
    R.uses ["service.falseCall"] $ R.constFunction "FALSECALL" falseFun,
    R.uses ["service.falseCall"] $ R.constFunction "BILL" billFun,
    R.uses ["case.diagnosis1", "service.type"] $ R.constFunction "FAULTCODE" faultFun,
    R.uses ["case.car_make"] $ R.constFunction "VEHICLEMAKE" vehicleMakeFun,
    R.uses ["case.car_make", "case.car_model"] $ R.constFunction "VEHICLEMODEL" vehicleModelFun]
    where
        phoneFmt s
            | ("+7" `isPrefixOf` s) && all isDigit (drop 2 s) && length s == 12 = unwords ["+7", "(" ++ substr 2 3 s ++ ")", substr 5 3 s, substr 8 2 s, substr 10 2 s]
            | otherwise = s
        
        substr f l = take l . drop f
        
        concatFields fs = SM.StringValue $ concat $ mapMaybe fromStringField fs
        fromStringField (SM.StringValue s) = Just s
        fromStringField _ = Nothing

        lookupField [] = Nothing
        lookupField fs = tryLook <|> justLast where
            tryLook = do
                ks <- mapM (fmap T.pack . fromStringField) fs
                fmap (SM.StringValue . T.unpack) $ lookAny ks dict
            justLast = Just $ last fs
        
        ifFun [i, t, f]
            | i `elem` [SM.StringValue "1", SM.StringValue "true", SM.StringValue "Y", SM.IntValue 1, SM.BoolValue True] = t
            | otherwise = f
        ifFun _ = SM.StringValue ""
        
        toPosix :: SM.FieldValue -> Maybe POSIXTime
        toPosix (SM.TimeValue t) = Just t
        toPosix (SM.StringValue s) = case reads s of
            [(t, "")] -> Just $ fromInteger t
            _ -> Nothing
        toPosix _ = Nothing
        
        -- | Dirty: xlsx doesn't have datediff type, but we can specify diff
        -- by days (double)
        dateDiff [from, to] = do
            f <- toPosix from
            t <- toPosix to
            return $ SM.DoubleValue $ fromInteger $ round $ (t - f) / 60.0
        dateDiff _ = Nothing

        yesNo [v]
            | v `elem` [SM.IntValue 0, SM.StringValue "0", SM.DoubleValue 0.0, SM.BoolValue False] = Just $ SM.StringValue "N"
            | v `elem` [SM.IntValue 1, SM.StringValue "1", SM.DoubleValue 1.0, SM.BoolValue True] = Just $ SM.StringValue "Y"
            | otherwise = Nothing
        yesNo _ = Nothing

        formatTimeFun :: String -> [SM.FieldValue] -> SM.FieldValue
        formatTimeFun _ [] = SM.StringValue ""
        formatTimeFun defFmt [v] =
            maybe
                v
                (SM.StringValue . formatTime defaultTimeLocale defFmt)
                (fmap (utcToLocalTime tz . posixSecondsToUTCTime) $ toPosix v)
        formatTimeFun _ [v, SM.StringValue fmt] =
            maybe
                v
                (SM.StringValue . formatTime defaultTimeLocale fmt)
                (fmap (utcToLocalTime tz . posixSecondsToUTCTime) $ toPosix v)
        formatTimeFun _ (v:_) = v
        
        fddsFun fs = do
            pr <- M.lookup "case.program" fs
            lookupField [SM.StringValue "FDDS", pr]

        ownerFun fs = do
            (SM.IntValue isOwner) <- M.lookup "case.callerOwner" fs
            (if isOwner == 1 then M.lookup "case.caller_name" else M.lookup "case.owner_name") fs
        falseFun fs = do
            (SM.StringValue isFalse) <- M.lookup "service.falseCall" fs
            return $ SM.StringValue (if isFalse `elem` ["bill", "nobill"] then "Y" else "N")
        
        billFun fs = do
            (SM.StringValue isFalse) <- M.lookup "service.falseCall" fs
            return $ SM.StringValue (if isFalse == "bill" then "Y" else "N")
            
        faultFun fs = do
            d <- M.lookup "case.diagnosis1" fs
            s <- M.lookup "service.type" fs
            (SM.StringValue d') <- lookupField [SM.StringValue "FaultCode", SM.StringValue "diagnosis1", d]
            (SM.StringValue s') <- lookupField [SM.StringValue "FaultCode", SM.StringValue "service", s]
            return $ SM.StringValue $ d' ++ "09" ++ s'
            
        vehicleMakeFun fs = do
            m <- M.lookup "case.car_make" fs
            lookupField [SM.StringValue "VehicleMake", m]
            
        vehicleModelFun fs = do
            mk <- M.lookup "case.car_make" fs
            md <- M.lookup "case.car_model" fs
            lookupField [SM.StringValue "VehicleModel", mk, md]
                      
local :: P.ConnectInfo
local = P.ConnectInfo {
	P.connectHost = "localhost",
	P.connectPort = 5432,
	P.connectUser = "carma_db_sync",
	P.connectPassword = "pass",
	P.connectDatabase = "carma" }

escope :: (MonadLog m) => T.Text -> m () -> m ()
escope s act = catch (scope_ s act) onError where
    onError :: (MonadLog m) => E.SomeException -> m ()
    onError _ = return ()

escopev :: (MonadLog m) => T.Text -> a -> m a -> m a
escopev s v act = catch (scope_ s act) (onError v) where
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

loadModels :: FilePath -> Log -> IO SM.Models
loadModels f l = withLog l $ do
    log Trace "Loading models"
    ss <- liftIO $ fmap (fromMaybe (error "Unable to load syncs") . A.decode) $ L8.readFile f
    maybe (error "Unable to create models on syncs loaded") return $ modelModels ss

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
    withPG (SM.insertUpdate ms (toStr name) cond m)
    where
        cond = toCond ms name c

-- FIXME: ARC has same function
query_ :: (PS.HasPostgres m, MonadLog m, PS.FromRow r) => PS.Query -> m [r]
query_ s = do
    bs <- PS.formatQuery s ()
    log Trace $ T.concat ["query: ", T.decodeUtf8 bs]
    PS.query_ s

-- FIXME: ARC has same function
query :: (PS.HasPostgres m, MonadLog m, PS.ToRow q, PS.FromRow r) => PS.Query -> q -> m [r]
query s v = do
    bs <- PS.formatQuery s v
    log Trace $ T.concat ["query: ", T.decodeUtf8 bs]
    PS.query s v

-- TODO: Use model field names and convert them by models
search :: (PS.HasPostgres m, MonadLog m) => SM.Models -> ByteString -> [ByteString] -> [ByteString] -> ByteString -> Int -> m [[S.FieldValue]]
search ms mname fs sels q lim = escopev "search" [] search' where
    search'
        | C8.null q = do
            log Warning "Empty query"
            return []
        | otherwise = do
            log Trace $ T.concat ["Search query: ", T.decodeUtf8 q]
            log Trace $ T.concat ["Search limit: ", T.pack $ show lim]
            log Trace $ T.concat ["Search fields: ", T.intercalate ", " (map T.decodeUtf8 fs)]
            log Trace $ T.concat ["Search select fields: ", T.intercalate ", " (map T.decodeUtf8 sels)]
            res <- query (fromString . T.unpack . T.decodeUtf8 $ searchQuery) argsS
            log Debug $ T.concat ["Found ", T.pack (show $ length res), " results"]
            return res

    -- Columns to search in
    rs = mapMaybe (fmap (C8.pack . toText . SM.catField) . SM.modelsField ms (C8.unpack mname) . C8.unpack) fs where
        toText = (++ "::text")

    -- Columns to select
    cols = mapMaybe (fmap (C8.pack . SM.catField) . SM.modelsField ms (C8.unpack mname) . C8.unpack) sels

    qs = C8.words q
    -- (row like ?)
    like :: ByteString -> ByteString
    like row = C8.concat ["(", row, " like ?)"]
    -- ((row1 like ?) or (row2 like ?) or ...)
    likes :: [ByteString] -> ByteString
    likes rows = C8.concat ["(", C8.intercalate " or " (map like rows), ")"]
    -- (like or like...) and (like or like...) and...
    whereClause :: [ByteString] -> [ByteString] -> (ByteString, [ByteString])
    whereClause [] _ = (C8.empty, [])
    whereClause _ [] = (C8.empty, [])
    whereClause rows querys = (whereString, whereArgs) where
        queryCount = length querys
        rowCount = length rows
        whereString = C8.intercalate " and " $ replicate queryCount (likes rows)
        -- [q1, q1, q1, q1, q2, q2, q2, q2, ...]
        --  <- rowCount ->  <- rowCount ->  ...
        whereArgs = concatMap (replicate rowCount) (map procentize querys)
        procentize s = C8.concat ["%", s, "%"]

    tblname = SM.withModel ms (C8.unpack mname) $ SM.syncTable . SM.modelSync
    (whereS, argsS) = whereClause rs qs

    searchQuery = C8.concat ["select ", C8.intercalate ", " cols, " from ", fromString tblname, " where ", whereS, " limit ", C8.pack (show lim)]

generateReport :: (PS.HasPostgres m, MonadLog m) => SM.Models -> [T.Text] -> FilePath -> FilePath -> m ()
generateReport ms conds tpl file = scope "generate" $ do
    log Info "Generating report"
    log Trace "Loading dictionaries"
    tz <- liftIO getCurrentTimeZone
    dicts <- scope "dictionaries" . liftIO . loadDictionaries $ "resources/site-config/dictionaries"
    -- test ARC
    -- scope "test" $ do
    --     log Info "ARC report test"
    --     arcReport dicts 2012 8

    -- test RKC
    scope "test" RKC.test

    withPG (R.createReport (SM.modelsSyncs ms) (functions tz dicts) conds tpl file)
    log Info "Report generated"

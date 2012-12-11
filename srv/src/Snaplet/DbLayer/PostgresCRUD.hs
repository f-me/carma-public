{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.PostgresCRUD (
    loadRelations,
    createIO,
    insert, update, updateMany, insertUpdate, insertUpdateMany,
    search,
    generateReport
    ) where

import Prelude hiding (log)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO
import qualified Control.Exception as E

import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, catMaybes)
import Data.ByteString (ByteString)
import Data.Char
import Data.List (isPrefixOf, find, elemIndex)
import Data.String
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX

import qualified Database.PostgreSQL.Simple as P
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Data.Pool as Pool

import qualified Database.PostgreSQL.Sync as S
import qualified Database.PostgreSQL.Sync.Base as S
import qualified Database.PostgreSQL.Sync.Types as S
import Database.PostgreSQL.Sync.JSON ()
import qualified Database.PostgreSQL.Report as R
import qualified Database.PostgreSQL.Report.Xlsx as R
import System.Locale

import Snaplet.DbLayer.Dictionary
import Snap.Snaplet.SimpleLog

import qualified Carma.ModelTables as MT

withPG :: (PS.HasPostgres m, MonadLog m) => S.TIO a -> m a
withPG f = do
    s <- PS.getPostgresState
    l <- askLog
    liftIO $ Pool.withResource (PS.pgPool s) (withLog l . S.inPG f)

inPsql :: (PS.HasPostgres m, MonadLog m) => (P.Connection -> m ()) -> m ()
inPsql act = do
    s <- PS.getPostgresState
    Pool.withResource (PS.pgPool s) act

functions :: TimeZone -> Dictionary -> [R.ReportFunction]
functions tz dict = [
    R.onString "NAME" (capitalize. fromMaybe "" . listToMaybe . drop 1 . words),
    R.onString "LASTNAME" (capitalize . fromMaybe "" . listToMaybe . words),
    R.onString "UPPER" (map toUpper),
    R.onString "LOWER" (map toLower),
    R.onString "PHONE" phoneFmt,
    R.function "CONCAT" concatFields,
    R.functionMaybe "LOOKUP" lookupField,
    R.function "IF" ifFun,
    R.functionMaybe "DATEDIFF" dateDiff,
    R.functionMaybe "YESNO" yesNo,
    R.function "DATE" (formatTimeFun "%d.%m.%Y"),
    R.function "TIME" (formatTimeFun "%d.%m.%Y %H:%M"),
    R.uses ["servicesview.callerOwner", "servicesview.caller_name", "servicesview.owner_name"] $ R.constFunction "OWNER" ownerFun,
    R.uses ["servicesview.program"] $ R.constFunction "FDDS" fddsFun,
    R.uses ["servicesview.falseCall"] $ R.constFunction "FALSECALL" falseFun,
    R.uses ["servicesview.falseCall"] $ R.constFunction "BILL" billFun,
    R.uses ["servicesview.clientSatisfied"] $ R.constFunction "SATISFIED" satisfiedFun,
    R.uses ["servicesview.diagnosis1", "servicesview.type"] $ R.constFunction "FAULTCODE" faultFun,
    R.uses ["servicesview.car_make"] $ R.constFunction "VEHICLEMAKE" vehicleMakeFun,
    R.uses ["servicesview.car_make", "servicesview.car_model"] $ R.constFunction "VEHICLEMODEL" vehicleModelFun,
    R.uses ["servicesview.id", "servicesview.services", "servicesview.id", "servicesview.type"] $ R.constFunction "SERVICEID" serviceId,
    R.uses ["servicesview.backoperator"] $ R.constFunction "BACKOPERATOR" backOperator]
    where
        capitalize "" = ""
        capitalize (c:cs) = toUpper c : map toLower cs

        phoneFmt s
            | ("+7" `isPrefixOf` s) && all isDigit (drop 2 s) && length s == 12 = unwords ["+7", "(" ++ substr 2 3 s ++ ")", substr 5 3 s, substr 8 2 s, substr 10 2 s]
            | otherwise = s
        
        substr f l = take l . drop f
        
        concatFields fs = S.StringValue $ concat $ mapMaybe fromStringField fs
        fromStringField (S.StringValue s) = Just s
        fromStringField _ = Nothing

        lookupField [] = Nothing
        lookupField fs = tryLook <|> justLast where
            tryLook = do
                ks <- mapM (fmap T.pack . fromStringField) fs
                fmap (S.StringValue . T.unpack) $ lookAny ks dict
            justLast = Just $ last fs
        
        ifFun [i, t, f]
            | i `elem` [S.StringValue "1", S.StringValue "true", S.StringValue "Y", S.IntValue 1, S.BoolValue True] = t
            | otherwise = f
        ifFun _ = S.StringValue ""
        
        toPosix :: S.FieldValue -> Maybe POSIXTime
        toPosix (S.TimeValue t) = Just t
        toPosix (S.StringValue s) = case reads s of
            [(t, "")] -> Just $ fromInteger t
            _ -> Nothing
        toPosix _ = Nothing
        
        -- | Dirty: xlsx doesn't have datediff type, but we can specify diff
        -- by days (double)
        dateDiff [from, to] = do
            f <- toPosix from
            t <- toPosix to
            return $ S.DoubleValue $ fromInteger $ round $ (t - f) / 60.0
        dateDiff _ = Nothing

        yesNo [v]
            | v `elem` [S.IntValue 0, S.StringValue "0", S.DoubleValue 0.0, S.BoolValue False] = Just $ S.StringValue "N"
            | v `elem` [S.IntValue 1, S.StringValue "1", S.DoubleValue 1.0, S.BoolValue True] = Just $ S.StringValue "Y"
            | otherwise = Nothing
        yesNo _ = Nothing

        formatTimeFun :: String -> [S.FieldValue] -> S.FieldValue
        formatTimeFun _ [] = S.StringValue ""
        formatTimeFun defFmt [v] =
            maybe
                v
                (S.StringValue . formatTime defaultTimeLocale defFmt)
                (fmap (utcToLocalTime tz . posixSecondsToUTCTime) $ toPosix v)
        formatTimeFun _ [v, S.StringValue fmt] =
            maybe
                v
                (S.StringValue . formatTime defaultTimeLocale fmt)
                (fmap (utcToLocalTime tz . posixSecondsToUTCTime) $ toPosix v)
        formatTimeFun _ (v:_) = v
        
        fddsFun fs = do
            pr <- M.lookup "servicesview.program" fs
            lookupField [S.StringValue "FDDS", pr]

        ownerFun fs = do
            (S.IntValue isOwner) <- M.lookup "servicesview.callerOwner" fs
            (if isOwner == 1 then M.lookup "servicesview.caller_name" else M.lookup "servicesview.owner_name") fs
        falseFun fs = do
            (S.StringValue isFalse) <- M.lookup "servicesview.falseCall" fs
            return $ S.StringValue (if isFalse `elem` ["bill", "nobill"] then "Y" else "N")
        
        billFun fs = do
            (S.StringValue isFalse) <- M.lookup "servicesview.falseCall" fs
            return $ S.StringValue (if isFalse == "bill" then "Y" else "N")

        satisfiedFun fs = do
            (S.StringValue sat) <- M.lookup "servicesview.clientSatisfied" fs
            return $ S.StringValue $ case sat of
                "satis" -> "Y"
                "notSatis" -> "N"
                _ -> ""
            
        faultFun fs = do
            d <- M.lookup "servicesview.diagnosis1" fs
            s <- M.lookup "servicesview.type" fs
            (S.StringValue d') <- lookupField [S.StringValue "FaultCode", S.StringValue "diagnosis1", d]
            (S.StringValue s') <- lookupField [S.StringValue "FaultCode", S.StringValue "service", s]
            return $ S.StringValue $ d' ++ "09" ++ s'
            
        vehicleMakeFun fs = do
            m <- M.lookup "servicesview.car_make" fs
            lookupField [S.StringValue "VehicleMake", m]
            
        vehicleModelFun fs = do
            mk <- M.lookup "servicesview.car_make" fs
            md <- M.lookup "servicesview.car_model" fs
            lookupField [S.StringValue "VehicleModel", mk, md]

        serviceId fs = do
            (S.IntValue caseId) <- M.lookup "servicesview.id" fs
            (S.StringValue caseSrvs) <- M.lookup "servicesview.services" fs
            (S.IntValue srvId) <- M.lookup "servicesview.id" fs
            (S.StringValue serviceType) <- M.lookup "servicesview.type" fs
            -- form service complex id type:id
            let
                srvIdName = serviceType ++ ":" ++ show srvId
                splitByComma = words . map (\c -> if c == ',' then ' ' else c)
                getIndex v = fmap succ . elemIndex v
                defaultIdx = S.StringValue $ show caseId ++ "/" ++ serviceType ++ ":" ++ show srvId
                formIdx i = S.StringValue $ show caseId ++ "/" ++ show i
            return . maybe defaultIdx formIdx . getIndex srvIdName . splitByComma $ caseSrvs

        backOperator fs = M.lookup "servicesview.backoperator" fs
                      
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

loadRelations :: FilePath -> Log -> IO S.Relations
loadRelations f l = withLog l $ do
    log Trace "Loading relations"
    liftIO $ fmap (fromMaybe (error "Unable to load relations") . A.decode) $ L8.readFile f

createIO :: [MT.TableDesc] -> Log -> IO ()
createIO tbls l = do
    con <- P.connect local
    let
        onError :: E.SomeException -> IO ()
        onError _ = return ()
    E.catch (void $ P.execute_ con "create extension hstore") onError
    mapM_ (withLog l . MT.createExtend con) tbls
    --withLog l $ S.transaction con $ S.create (S.modelsSyncs ms)

toStr :: ByteString -> String
toStr = T.unpack . T.decodeUtf8

getModel :: Monad m => ByteString -> [MT.TableDesc] -> m MT.TableDesc
getModel modelName descs = maybe (error $ "No model " ++ toStr modelName) return $ MT.tableByModel modelName descs

insert :: (PS.HasPostgres m, MonadLog m) => [MT.TableDesc] -> ByteString -> M.Map ByteString ByteString -> m ()
insert descs modelName dat = escope "insert" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.insert con desc' (MT.addType desc' dat)

update :: (PS.HasPostgres m, MonadLog m) => [MT.TableDesc] -> ByteString -> ByteString -> M.Map ByteString ByteString -> m ()
update descs modelName modelId dat = escope "update" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.update con desc' modelId dat

insertUpdate :: (PS.HasPostgres m, MonadLog m) => [MT.TableDesc] -> ByteString -> ByteString -> M.Map ByteString ByteString -> m ()
insertUpdate descs modelName modelId dat = escope "insertUpdate" $ inPsql $ \con -> do
    desc' <- getModel modelName descs
    MT.insertUpdate con desc' modelId dat

updateMany :: (PS.HasPostgres m, MonadLog m) => [MT.TableDesc] -> M.Map (ByteString, ByteString) (M.Map ByteString ByteString) -> m ()
updateMany descs m = scope "updateMany" $ forM_ (M.toList m) $ uncurry update' where
    update' (mdlName, mdlId) = update descs mdlName mdlId

insertUpdateMany :: (PS.HasPostgres m, MonadLog m) => [MT.TableDesc] -> M.Map (ByteString, ByteString) (M.Map ByteString ByteString) -> m ()
insertUpdateMany descs m = scope "insertUpdateMany" $ forM_ (M.toList m) $ uncurry insertUpdate' where
    insertUpdate' (mdlName, mdlId) = insertUpdate descs mdlName mdlId

-- FIXME: ARC has same function
query :: (PS.HasPostgres m, MonadLog m, PS.ToRow q, PS.FromRow r) => PS.Query -> q -> m [r]
query s v = do
    bs <- PS.formatQuery s v
    log Trace $ T.concat ["query: ", T.decodeUtf8 bs]
    PS.query s v

-- TODO: Use model field names and convert them by models
search :: (PS.HasPostgres m, MonadLog m) => [MT.TableDesc] -> ByteString -> [ByteString] -> [ByteString] -> ByteString -> Int -> m [[S.FieldValue]]
search descs mname fs sels q lim = liftIO getCurrentTimeZone >>= search' where
    search' tz = escopev "search" [] search'' where
        search''
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

        tzHours = timeZoneMinutes tz `div` 60

        desc = fromMaybe err $ find ((== mname) . fromString . MT.tableModel) descs where
            err = error $ "Unknown model " ++ T.unpack (T.decodeUtf8 mname)

        columnDescs = MT.tableFlatFields desc

        -- Columns to search in
        rs = map (T.encodeUtf8 . T.pack . wrapColumn . T.unpack . T.decodeUtf8) fs where
            wrapColumn f = fromMaybe err $ do
                tp <- fmap MT.columnType $ find ((== f) . fromString . MT.columnName) columnDescs
                return $ case tp of
                    "timestamp" -> "to_char(" ++ f ++ " + '" ++ show tzHours ++ ":00','DD.MM.YYYY')"
                    _ -> f ++ "::text"
                where
                    err = error $ "Unknown column: " ++ f

        -- Columns to select
        cols = map (\s -> C8.concat [fromString $ MT.tableName desc, ".", s]) sels

        qs = C8.words q
        -- (row like ?)
        like :: ByteString -> ByteString
        like row = C8.concat ["(lower(", row, ") like lower(?))"]
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

        (whereS, argsS) = whereClause rs qs

        searchQuery = C8.concat ["select ", C8.intercalate ", " cols, " from ", fromString (MT.tableName desc), " where ", whereS, " limit ", C8.pack (show lim)]

generateReport :: (PS.HasPostgres m, MonadLog m) => [MT.TableDesc] -> [S.Condition] -> (T.Text -> [T.Text]) -> FilePath -> FilePath -> m ()
generateReport tbls relations superCond tpl fileName = scope "generate" $ do
    log Debug "Generating report "
    log Trace "Loading dictionaries"
    tz <- liftIO getCurrentTimeZone
    dicts <- scope "dictionaries" . liftIO . loadDictionaries $ "resources/site-config/dictionaries"
    -- TODO: Orderby must not be here!
    withPG (R.createReport tbls relations (functions tz dicts) superCond [] [] tpl fileName)
    log Debug "Report generated"

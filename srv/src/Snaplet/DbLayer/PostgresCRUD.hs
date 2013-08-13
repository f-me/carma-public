{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.PostgresCRUD (
    loadRelations,
    createIO,
    insert, update, updateMany, insertUpdate, insertUpdateMany,
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
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.ByteString (ByteString)
import Data.Char
import Data.List (isPrefixOf, elemIndex)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector  as V

import qualified Database.PostgreSQL.Simple as P
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Data.Pool as Pool

import qualified Database.PostgreSQL.Sync as S
import Database.PostgreSQL.Sync.JSON ()
import qualified Database.PostgreSQL.Report as R
import qualified Database.PostgreSQL.Report.Xlsx as R
import System.Locale
import Text.Printf

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
    R.function "COMMENT" composeComment,
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
    R.uses ["servicesview.diagnosis2", "servicesview.diagnosis3", "servicesview.type"] $ R.constFunction "FAULTCODE" faultFun,
    R.uses ["servicesview.car_make"] $ R.constFunction "VEHICLEMAKE" vehicleMakeFun,
    R.uses ["servicesview.car_make", "servicesview.car_model"] $ R.constFunction "VEHICLEMODEL" vehicleModelFun,
    R.uses ["servicesview.caseid", "servicesview.services", "servicesview.id", "servicesview.type"] $ R.constFunction "SERVICEID" serviceId,
    R.uses ["servicesview.backoperator"] $ R.constFunction "BACKOPERATOR" backOperator]
    where
        capitalize "" = ""
        capitalize (c:cs) = toUpper c : map toLower cs

        phoneFmt s@('+':'7': xs@[a,b,c,d,e,f,g,h,i,j])
          | all isDigit xs
            = printf "+7 (%s) %s %s %s" [a,b,c] [d,e,f] [g,h] [i,j]
          | otherwise = s

        substr f l = take l . drop f

        composeComment = S.StringValue . \case
          [S.StringValue s]
            -> case A.decode $ L.encodeUtf8 $ L.pack s of
              Just [A.Array a] -> concatMap mkComment $ V.toList a
              _ -> ""
          _ -> ""
         where
          mkComment = \case
            A.Object o -> case HM.lookup "comment" o of
              Just (A.String s) -> T.unpack s ++ "\n--\n"
              _ -> ""
            _ -> ""

        concatFields fs = S.StringValue $ concat $ mapMaybe fromStringField fs
        fromStringField (S.StringValue s) = Just s
        fromStringField _ = Nothing

        lookupField fs = lookupFieldWithDefault (last fs) fs

        lookupFieldWithDefault _   [] = Nothing
        lookupFieldWithDefault def fs = tryLook <|> Just def where
            tryLook = do
                ks <- mapM (fmap T.pack . fromStringField) fs
                fmap (S.StringValue . T.unpack) $ lookAny ks dict

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
            d2 <- M.lookup "servicesview.diagnosis2" fs
            d3 <- M.lookup "servicesview.diagnosis3" fs
            s  <- M.lookup "servicesview.type" fs
            (S.StringValue d2') <- lookupFieldWithDefault (S.StringValue "150")
                                      [S.StringValue "FaultCode", S.StringValue "diagnosis2", d2]
            (S.StringValue d3') <- lookupFieldWithDefault (S.StringValue "09")
                                      [S.StringValue "FaultCode", S.StringValue "diagnosis3", d3]
            (S.StringValue s')  <- lookupField [S.StringValue "FaultCode", S.StringValue "service", s]
            return $ S.StringValue $ d2' ++ d3' ++ s'
            
        vehicleMakeFun fs = do
            m <- M.lookup "servicesview.car_make" fs
            lookupField [S.StringValue "VehicleMake", m]
            
        vehicleModelFun fs = do
            mk <- M.lookup "servicesview.car_make" fs
            md <- M.lookup "servicesview.car_model" fs
            lookupField [S.StringValue "VehicleModel", mk, md]

        serviceId fs = do
            (S.IntValue caseId) <- M.lookup "servicesview.caseid" fs
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

                srvsWords = splitByComma caseSrvs
                srvIndex
                    | length srvsWords == 1 = S.StringValue $ show caseId
                    | otherwise = maybe defaultIdx formIdx $ getIndex srvIdName srvsWords

            return srvIndex

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

loadRelations :: FilePath -> Log -> IO S.Relations
loadRelations f l = withLog l $ do
    log Trace "Loading relations"
    liftIO $ fmap (fromMaybe (error "Unable to load relations") . A.decode) $ LB.readFile f

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

generateReport :: (PS.HasPostgres m, MonadLog m) => [MT.TableDesc] -> [S.Condition] -> (T.Text -> [T.Text]) -> FilePath -> FilePath -> m ()
generateReport tbls relations superCond tpl fileName = scope "generate" $ do
    log Debug "Generating report "
    log Trace "Loading dictionaries"
    tz <- liftIO getCurrentTimeZone
    dicts <- scope "dictionaries" . loadDictionaries $ "resources/site-config/dictionaries"
    -- TODO: Orderby must not be here!
    withPG (R.createReport tbls relations (functions tz dicts) superCond [] [] tpl fileName)
    log Debug "Report generated"

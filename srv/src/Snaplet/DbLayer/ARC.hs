{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.ARC (
    arcReport
    ) where

import Prelude hiding (log, catch)

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Snap.Snaplet.PostgresqlSimple as PS
import System.Locale

import Snap.Snaplet.SimpleLog

query_ :: (PS.HasPostgres m, MonadLog m, PS.FromRow r) => PS.Query -> m [r]
query_ s = do
    bs <- PS.formatQuery s ()
    log Trace $ T.concat ["query: ", T.decodeUtf8 bs]
    PS.query_ s

query :: (PS.HasPostgres m, MonadLog m, PS.ToRow q, PS.FromRow r) => PS.Query -> q -> m [r]
query s v = do
    bs <- PS.formatQuery s v
    log Trace $ T.concat ["query: ", T.decodeUtf8 bs]
    PS.query s v

-- create count query for tables and 
count :: (PS.HasPostgres m, MonadLog m) => [T.Text] -> [T.Text] -> m Integer
count ts cs = do
    rs <- query_ $ fromString $ T.unpack $ T.concat [
        "select count(*) from ",
        T.intercalate ", " ts,
        " where ",
        T.intercalate " and " (map (T.cons '(' . (`T.snoc` ')')) cs)]
    case rs of
        [] -> error "Count query returns no rows"
        ((PS.Only r):_) -> do
            log Debug $ T.concat ["Count result: ", T.pack (show r)]
            return r

-- | Create ARC report for year and month
arcReport :: (PS.HasPostgres m, MonadLog m) => Integer -> Int -> m ()
arcReport year month = scope "arc" $ do
    log Info $ T.concat ["Creating arc report for ", fromString (formatTime defaultTimeLocale "%m.%Y" (fromGregorian year month 1))]
    tz <- liftIO getCurrentTimeZone
    let
        daysCount = gregorianMonthLength year month
        startOfDay n = localTimeToUTC tz localTm where
            localTm = LocalTime (fromGregorian year month n) midnight
        withinDay st field = T.concat [field, " - '", fromString st, "' <= '1 day'"]
        starts = map (formatTime defaultTimeLocale "%F %T" . startOfDay) [1..daysCount]
        getData st = do
            count ["calltbl"] [withinDay st "calltbl.callDate"]
            count ["casetbl", "servicetbl"] [
                "casetbl.id = servicetbl.parentId",
                "servicetbl.type = 'tech'",
                withinDay st "servicetbl.createTime"]
            count ["casetbl", "servicetbl"] [
                "casetbl.id = servicetbl.parentId",
                "servicetbl.type = 'towage'",
                "position('tech' in casetbl.services) != 0",
                withinDay st "servicetbl.createTime"]
            count ["casetbl", "servicetbl"] [
                "casetbl.id = servicetbl.parentId",
                "servicetbl.type = 'towage'",
                withinDay st "servicetbl.createTime"]
            count ["casetbl", "servicetbl"] [
                "casetbl.id = servicetbl.parentId",
                "servicetbl.type = 'towage'",
                "casetbl.diagnosis1 = 'dtp'",
                withinDay st "servicetbl.createTime"]
            count ["casetbl", "servicetbl"] [
                "casetbl.id = servicetbl.parentId",
                "servicetbl.type not in ('tech', 'towage')",
                withinDay st "servicetbl.createTime"]
    forM_ (zip [1..daysCount] starts) $ \(i, st) -> scope (T.pack (show i)) $ do
        getData st
    

-- ARC
-- select count(*) from calltbl where (date_trunc('day', calltbl.callDate) = TIMESTAMP '2012-08-06');
-- select count (*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type = 'tech') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (position('tech' in casetbl.services) != 0) and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (casetbl.diagnosis1 = 'dtp') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type not in ('tech', 'towage')) and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- 

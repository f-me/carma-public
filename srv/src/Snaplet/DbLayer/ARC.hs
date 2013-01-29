{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.ARC (
    query_, query,
    PreQuery(..), preQuery, preQuery_,
    strQuery,
    runQuery, intQuery,

    arcReport,
    saveArcReport
    ) where

import Prelude hiding (log)

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.List
import Data.Function (on)
import qualified Data.Map as M
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Database.PostgreSQL.Simple.ToField as PS
import Snaplet.DbLayer.Dictionary
import System.Locale
import Text.Format

import qualified Codec.Xlsx as Xlsx
import qualified Codec.Xlsx.Writer as Xlsx

import Snap.Snaplet.SimpleLog hiding ((%=))

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

-- pre-query, holds fields, table names and conditions in separate list to edit
data PreQuery = PreQuery {
    preFields :: [T.Text],
    preTables :: [T.Text],
    preConditions :: [T.Text],
    preGroups :: [T.Text],
    preOrders :: [T.Text],
    preArgs :: [PS.Action] }

preQuery :: (PS.ToRow q) => [T.Text] -> [T.Text] -> [T.Text] -> [T.Text] -> [T.Text] -> q -> PreQuery
preQuery fs ts cs gs os as = PreQuery fs ts cs gs os (PS.toRow as) 

preQuery_ :: [T.Text] -> [T.Text] -> [T.Text] -> [T.Text] -> [T.Text] -> PreQuery
preQuery_ fs ts cs gs os = PreQuery fs ts cs gs os []

instance Monoid PreQuery where
    mempty = PreQuery [] [] [] [] [] []
    (PreQuery lf lt lc lg lo la) `mappend` (PreQuery rf rt rc rg ro ra) = PreQuery
        (nub $ lf ++ rf)
        (nub $ lt ++ rt)
        (nub $ lc ++ rc)
        (nub $ lg ++ rg)
        (nub $ lo ++ ro)
        (la ++ ra)

strQuery :: PreQuery -> (String, [PS.Action])
strQuery (PreQuery f t c g o a) = (str, a) where
    str = T.unpack $ T.concat [
        "select ", T.intercalate ", " f,
        " from ", T.intercalate ", " t,
        nonullcat c [" where ", T.intercalate " and " (map (T.cons '(' . (`T.snoc` ')')) c)],
        nonullcat g [" group by ", T.intercalate ", " g],
        nonullcat o [" order by ", T.intercalate ", " o]]
    nonullcat [] _ = ""
    nonullcat _ s = T.concat s

runQuery :: (PS.HasPostgres m, MonadLog m, PS.FromRow r) => [PreQuery] -> m [r]
runQuery qs = query (fromString compiled) a where
    (compiled, a) = strQuery (mconcat qs)

intQuery :: (PS.HasPostgres m, MonadLog m) => [PreQuery] -> m Integer
intQuery qs = do
    rs <- runQuery qs
    case rs of
        [] -> error "Int query returns no rows"
        ((PS.Only r):_) -> do
            log Debug $ T.concat ["Int query result: ", T.pack (show r)]
            return r

str :: String -> String
str = id

withinDay :: T.Text -> T.Text -> [T.Text]
withinDay st field = [
    T.concat [field, " - '", st, "' < '1 day'"],
    T.concat [field, " - '", st, "' >= '0 days'"]]

programs :: Dictionary -> [(T.Text, [T.Text])]
programs = maybe [] (map (id &&& (return . id))) . keys ["Programs"]

-- | Create ARC report for year and month
-- select count(*) from calltbl where (date_trunc('day', calltbl.callDate) = TIMESTAMP '2012-08-06');
-- select count (*) from casetbl, servicetbl where ('case:' || casetbl.id = servicetbl.parentId) and (servicetbl.type = 'tech') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where ('case:' || casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (position('tech' in casetbl.services) != 0) and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where ('case:' || casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where ('case:' || casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (casetbl.diagnosis1 = 'dtp') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where ('case:' || casetbl.id = servicetbl.parentId) and (servicetbl.type not in ('tech', 'towage')) and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where ('case:' || casetbl.id = servicetbl.parentId) and (servicetbl.clientSatisfied = 'notSatis') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
arcReport :: (PS.HasPostgres m, MonadLog m) => Dictionary -> Integer -> Int -> m ()
arcReport d year month = scope "arc" $ do
    log Info $ T.concat ["Creating arc report for ", fromString (formatTime defaultTimeLocale "%m.%Y" (fromGregorian year month 1))]
    tzMins <- liftM timeZoneMinutes $ liftIO getCurrentTimeZone
    let
        daysCount = gregorianMonthLength year month
        currentMonth = formatTime defaultTimeLocale "%Y-%m-%d" (fromGregorian year month 1)
        args = [
            "tz" %= tzMins,
            "month" %= currentMonth,
            "serviceSelect" %= (format "select count(*), casetbl.program, date_part('day', servicetbl.createTime + '$tz minutes') dy from casetbl, servicetbl where ('case: || casetbl.id = servicetbl.parentId)" ["tz" %= tzMins]),
            "inThisMonth" %= (format "(date_trunc('month', servicetbl.createTime) = timestamp '$month')" ["month" %= currentMonth]),
            "serviceGroup" %= ("group by casetbl.program, dy order by casetbl.program, dy" :: String),
            "towage" %= ("(servicetbl.type = 'towage')" :: String)]

        groupByProgram :: [(Integer, T.Text, Integer)] -> [(T.Text, [(Integer, Integer)])]
        groupByProgram = map (first head) . map unzip . groupBy ((==) `on` fst) . map reorder where
            reorder (cnt, pname, d) = (pname, (d, cnt))

        fillZeroDays :: [(Integer, Integer)] -> [Integer]
        fillZeroDays dayCounts = map (\i -> fromMaybe 0 (lookup i dayCounts)) [1..toInteger daysCount]

        makeRow :: T.Text -> [(Integer, T.Text, Integer)] -> M.Map T.Text [(T.Text, [Integer])]
        makeRow name dat = M.fromList $ map (second toRow) $ groupByProgram dat where
            toRow :: [(Integer, Integer)] -> [(T.Text, [Integer])]
            toRow rs = [(name, fillZeroDays rs)]

        queryFmt lns = query_ $ fromString $ T.unpack $ format (concat lns) args

    totalCalls <- queryFmt [
        "select count(*), program, date_part('day', calltbl.callDate + '$tz minutes') dy",
        " from calltbl where (date_trunc('month', calltbl.callDate + '$tz minutes') = timestamp '$month')",
        " group by program, dy order by program, dy"]
    techServices <- queryFmt [
        "$serviceSelect and (servicetbl.type = 'tech') and $inThisMonth $serviceGroup"]
    towageServicesWithTech <- queryFmt [
        "$serviceSelect and $towage and (position('tech' in casetbl.services) != 0) and $inThisMonth $serviceGroup"]
    towageServices <- queryFmt ["$serviceSelect and $towage and $inThisMonth $serviceGroup"]
    towageDtps <- queryFmt ["$serviceSelect  and $towage and (casetbl.diagnosis1 = 'dtp') and $inThisMonth $serviceGroup"]
    others <- queryFmt ["$serviceSelect and (servicetbl.type not in ('tech', 'towage')) and $inThisMonth $serviceGroup"]
    notSatisfied <- queryFmt ["$serviceSelect and (servicetbl.clientSatisfied = 'notSatis') and $inThisMonth $serviceGroup"]

    log Trace "Saving to ARC.xlsx"

    let
        reportData = M.unionsWith (++) [
            makeRow "Total calls" totalCalls,
            makeRow "Techs" techServices,
            makeRow "Towage-techs" towageServicesWithTech,
            makeRow "Towages" towageServices,
            makeRow "DTPs" towageDtps,
            makeRow "Others" others,
            makeRow "Not satisfied" notSatisfied]

        oneRow :: (T.Text, [Integer]) -> [Xlsx.CellValue]
        oneRow (nm, vals) = Xlsx.CellText rowName : map (Xlsx.CellDouble . fromIntegral) vals where
            rowName = maybe nm id $ look ["ARC", nm] d
        oneProgram :: (T.Text, [(T.Text, [Integer])]) -> [[Xlsx.CellValue]]
        oneProgram (nm, rs) = map ((Xlsx.CellText progName :) . oneRow) rs where
            progName = maybe nm id $ look ["Programs", nm] d
        sheetData = concatMap oneProgram $ M.toList reportData
        header = replicate 2 (Xlsx.CellText "") ++ map (Xlsx.CellDouble . fromIntegral) [1.. daysCount]

    liftIO $ saveXlsx "ARC.xlsx" header sheetData
    log Info "Report saved to ARC.xlsx"

saveXlsx :: FilePath -> [Xlsx.CellValue] -> [[Xlsx.CellValue]] -> IO ()
saveXlsx f ts fs = Xlsx.writeXlsx f [sheet] where
    allRows = ts : fs
    sheet = Xlsx.Worksheet {
        Xlsx.wsName = T.pack "arc",
        Xlsx.wsMinX = 1,
        Xlsx.wsMaxX = 1 + length ts,
        Xlsx.wsMinY = 1,
        Xlsx.wsMaxY = 1 + length allRows,
        Xlsx.wsColumns = [],
        Xlsx.wsRowHeights = M.empty,
        Xlsx.wsCells = cls }
    cls = M.unions $ zipWith row [1..] allRows
    row r rdata = M.unions $ zipWith (mkCell r) [1..] rdata
    mkCell r c = M.singleton (c, r) . Xlsx.CellData Nothing . Just

saveArcReport :: FilePath -> [T.Text] -> [[Integer]] -> IO ()
saveArcReport f ts fs = saveXlsx f names fields where
    names = map (Xlsx.CellText) ts
    fields = map (map (Xlsx.CellDouble . fromIntegral)) fs

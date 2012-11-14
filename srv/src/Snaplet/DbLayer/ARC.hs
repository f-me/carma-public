{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.ARC (
    query_, query,
    PreQuery(..), preQuery, preQuery_,
    runQuery, intQuery,

    arcReport
    ) where

import Prelude hiding (log, catch)

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Monoid
import Data.List
import qualified Data.Map as M
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Database.PostgreSQL.Simple.ToField as PS
import qualified Database.PostgreSQL.Simple.ToRow as PS
import Snaplet.DbLayer.Dictionary
import System.Locale

import qualified Codec.Xlsx as Xlsx
import qualified Codec.Xlsx.Parser as Xlsx
import qualified Codec.Xlsx.Writer as Xlsx

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

runQuery :: (PS.HasPostgres m, MonadLog m, PS.FromRow r) => [PreQuery] -> m [r]
runQuery qs = query compiled a where
    (PreQuery f t c g o a) = mconcat qs
    compiled = fromString $ T.unpack $ T.concat [
        "select ", T.intercalate ", " f,
        " from ", T.intercalate ", " t,
        nonullcat c [" where ", T.intercalate " and " (map (T.cons '(' . (`T.snoc` ')')) c)],
        nonullcat g [" group by ", T.intercalate ", " g],
        nonullcat o [" order by ", T.intercalate ", " o]]
    nonullcat [] s = ""
    nonullcat _ s = T.concat s

intQuery :: (PS.HasPostgres m, MonadLog m) => [PreQuery] -> m Integer
intQuery qs = do
    rs <- runQuery qs
    case rs of
        [] -> error "Int query returns no rows"
        ((PS.Only r):_) -> do
            log Debug $ T.concat ["Int query result: ", T.pack (show r)]
            return r
        _ -> error "Int query returns invalid result"

str :: String -> String
str = id

withinDay st field = [
    T.concat [field, " - '", st, "' < '1 day'"],
    T.concat [field, " - '", st, "' >= '0 days'"]]

rows :: [(T.Text, T.Text -> [T.Text] -> PreQuery)]
rows = queries where
    cst = preQuery_ ["count(*)"] ["casetbl", "servicetbl"] ["casetbl.id = servicetbl.parentId"] [] []
    inday st = preQuery_ ["count(*)"] [] (withinDay st "servicetbl.createTime") [] []
    inList :: T.Text -> [T.Text] -> PreQuery
    inList tbl lst = preQuery ["count(*)"] [tbl] [T.concat [tbl, ".program in ?"]] [] [] [PS.In lst]
    csday st ps = mconcat [cst, inday st, inList "casetbl" ps]
    
    totalCalls st ps = mconcat [inList "calltbl" ps, preQuery_ ["count(*)"] ["calltbl"] (withinDay st "calltbl.callDate") [] []]
    techServices st ps = mconcat [csday st ps, preQuery [] [] ["servicetbl.type = ?"] [] [] [str "tech"]]
    towageServicesWithTech st ps = mconcat [csday st ps, preQuery [] [] ["servicetbl.type = ?", "position(? in casetbl.services) != 0"] [] [] [str "towage", str "tech"]]
    towageServices st ps = mconcat [csday st ps, preQuery [] [] ["servicetbl.type = ?"] [] [] [str "towage"]]
    towageDtps st ps = mconcat [csday st ps, preQuery [] [] ["servicetbl.type = ?", "casetbl.diagnosis1 = ?"] [] [] [str "towage", str "dtp"]]
    others st ps = mconcat [csday st ps, preQuery [] [] ["servicetbl.type not in (?, ?)"] [] [] [str "tech", str "towage"]]
    notSatisfied st ps = mconcat [csday st ps, preQuery [] [] ["servicetbl.clientSatisfied = ?"] [] [] [str "0"]]
    
    queries = [
        ("Total calls", totalCalls),
        ("Techs", techServices),
        ("Towage-techs", towageServicesWithTech),
        ("Towages", towageServices),
        ("DTPs", towageDtps),
        ("Others", others),
        ("Not satisfied", notSatisfied)]

programs :: Dictionary -> [(T.Text, [T.Text])]
programs = maybe [] (map (id &&& (return . id))) . keys ["Programs"]

-- | Create ARC report for year and month
arcReport :: (PS.HasPostgres m, MonadLog m) => Dictionary -> Integer -> Int -> m ()
arcReport d year month = scope "arc" $ do
    log Info $ T.concat ["Creating arc report for ", fromString (formatTime defaultTimeLocale "%m.%Y" (fromGregorian year month 1))]
    tz <- liftIO getCurrentTimeZone
    let
        daysCount = gregorianMonthLength year month
        startOfDay n = localTimeToUTC tz localTm where
            localTm = LocalTime (fromGregorian year month n) midnight
        starts :: [T.Text]
        starts = map (fromString . formatTime defaultTimeLocale "%F %T" . startOfDay) [1..daysCount]
    reportData <- forM (programs d) $ \(pname, pprog) -> scope pname $ do
        programData <- forM rows $ \(rname, rquery) -> scope rname $ do
            counts <- forM (zip [1..daysCount] starts) $ \(nday, st) -> scope (T.pack $ show nday) $ do
                intQuery [rquery st pprog]
            log Debug $ T.concat ["Counts for month: ", T.pack $ show counts]
            return (rname, counts)
        return (pname, programData)
    
    log Trace "Saving to ARC.xlsx"
    let
        oneRow :: (T.Text, [Integer]) -> [Xlsx.CellValue]
        oneRow (nm, vals) = Xlsx.CellText rowName : map (Xlsx.CellDouble . fromIntegral) vals where
            rowName = maybe nm id $ look ["ARC", nm] d
        oneProgram :: (T.Text, [(T.Text, [Integer])]) -> [[Xlsx.CellValue]]
        oneProgram (nm, rs) = map ((Xlsx.CellText progName :) . oneRow) rs where
            progName = maybe nm id $ look ["Programs", nm] d
        sheetData = concatMap oneProgram reportData
        header = replicate 2 (Xlsx.CellText "") ++ map (Xlsx.CellDouble . fromIntegral) [1..daysCount]
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

-- ARC
-- select count(*) from calltbl where (date_trunc('day', calltbl.callDate) = TIMESTAMP '2012-08-06');
-- select count (*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type = 'tech') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (position('tech' in casetbl.services) != 0) and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type = 'towage') and (casetbl.diagnosis1 = 'dtp') and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- select count(*) from casetbl, servicetbl where (casetbl.id = servicetbl.parentId) and (servicetbl.type not in ('tech', 'towage')) and (date_trunc('day', servicetbl.createTime) = TIMESTAMP '2012-08-06');
-- 

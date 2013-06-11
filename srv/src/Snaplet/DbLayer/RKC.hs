{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.RKC (
  Filter(..), todayFilter,
  rkc,
  rkcFront,
  partners
  ) where

import Prelude hiding (log)

import Control.Arrow
import Control.Monad

import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.List (intersect, sort, nub)
import Data.String
import qualified Data.List as L (groupBy)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Time
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Format

import qualified Database.PostgreSQL.Simple.ToField as PS
import Database.PostgreSQL.Simple.SqlQQ
import qualified Snap.Snaplet.PostgresqlSimple as PS

import Snaplet.Auth.PGUsers
import Snaplet.DbLayer.Dictionary
import Snaplet.DbLayer.ARC

import Snap.Snaplet.SimpleLog hiding ((%=))

import Util

-------------------------------------------------------------------------------
-- | query fmt
fquery_ :: (PS.HasPostgres m, MonadLog m, PS.FromRow r) => String -> FormatArgs -> m [r]
fquery_ fmt args = query_ (fromString $ T.unpack $ format fmt args)

fquery :: (PS.HasPostgres m, MonadLog m, PS.ToRow q, PS.FromRow r) => String -> FormatArgs -> q -> m [r]
fquery fmt args v = query (fromString $ T.unpack $ format fmt args) v

runQuery_ :: (PS.HasPostgres m, MonadLog m, PS.FromRow r) => PreQuery -> m [r]
runQuery_ pq = runQuery [relations pq]

-- Column of table is in list
inList :: T.Text -> T.Text -> [T.Text] -> PreQuery
inList tbl col vals = preQuery [] [tbl] [T.concat [tbl, ".", col, " in ?"]] [] [] [PS.In vals]

equals :: T.Text -> T.Text -> T.Text -> PreQuery
equals tbl col val = preQuery [] [tbl] [T.concat [tbl, ".", col, " = ?"]] [] [] [val]

equalsTo :: T.Text -> T.Text -> T.Text -> PreQuery
equalsTo tbl expr val = preQuery [] [tbl] [T.concat [expr, " = ?"]] [] [] [val]

withinToday :: T.Text -> T.Text -> PreQuery
withinToday tbl col = thisDay `mappend` notNull tbl col where
  thisDay = preQuery_ [] [tbl] [equalsNow] [] []
  equalsNow = T.concat ["date_trunc('day', ", tbl, ".", col, " + '4 hours') = date_trunc('day', now())"]

betweenTime :: UTCTime -> UTCTime -> T.Text -> T.Text -> PreQuery
betweenTime from to tbl col = mconcat [
  notNull tbl col,
  preQuery [] [tbl] [T.concat [tbl, ".", col, " >= ?"]] [] [] [asLocal from],
  preQuery [] [tbl] [T.concat [tbl, ".", col, " < ?"]] [] [] [asLocal to]]

asLocal :: UTCTime -> LocalTime
asLocal = utcToLocalTime utc

count :: PreQuery
count = preQuery_ ["count(*)"] [] [] [] []

count' :: T.Text -> PreQuery
count' name = preQuery_ [T.concat ["count(*) ", name]] [] [] [] []

sumOf :: T.Text -> T.Text -> PreQuery
sumOf tbl col = preQuery_ [T.concat ["sum(", tbl, ".", col, ")"]] [tbl] [] [] []

notNull :: T.Text -> T.Text -> PreQuery
notNull tbl col = preQuery_ [] [tbl] [T.concat [tbl, ".", col, " is not null"]] [] []

cond :: [T.Text] -> T.Text -> PreQuery
cond tbls c = preQuery_ [] tbls [c] [] []

groupBy :: T.Text -> T.Text -> PreQuery
groupBy tbl col = preQuery_ [] [tbl] [] [T.concat [tbl, ".", col]] []

orderBy :: T.Text -> T.Text -> PreQuery
orderBy tbl col = preQuery_ [] [tbl] [] [] [T.concat [tbl, ".", col]]

--
-- Queries for case block
--

-- | Done services
doneServices :: PreQuery
doneServices = inList "servicetbl" "status" [
  "serviceInProgress",
  "serviceOk",
  "serviceClosed"]

serviceCaseRel :: PreQuery
serviceCaseRel = cond ["casetbl", "servicetbl"] "'case:' || casetbl.id = servicetbl.parentId"

consultationCaseRel :: PreQuery
consultationCaseRel = cond ["casetbl", "consultationtbl"] "'case:' || casetbl.id = consultationtbl.parentId"

mechanic :: PreQuery
mechanic = equals "calltbl" "callType" "client" `mappend` inList "calltbl" "callerType" [
  "mechanicConsOk",
  "mechanicConsNotOk"]

towageTech :: PreQuery
towageTech = inList "servicetbl" "type" ["towage", "tech"]

averageTowageTechStart :: PreQuery
averageTowageTechStart = averageStart `mappend` towageTech
  --cond ["servicetbl"] "(servicetbl.suburbanMilage = 0) or (servicetbl.suburbanMilage is null)"]

averageTowageTechEnd :: PreQuery
averageTowageTechEnd = averageEnd `mappend` towageTech
  --cond ["servicetbl"] "(servicetbl.suburbanMilage = 0) or (servicetbl.suburbanMilage is null)"]

averageStart :: PreQuery
averageStart = mconcat [
  averageTime ("servicetbl", "times_factServiceStart") ("casetbl", "callDate"),
  serviceCaseRel]

averageEnd :: PreQuery
averageEnd = mconcat [
  averageTime ("servicetbl", "times_factServiceEnd") ("servicetbl", "times_factServiceStart")]

satisfaction :: PreQuery
satisfaction = mconcat [
  count,
  equals "servicetbl" "clientSatisfied" "satis"]

satisfactionCount :: PreQuery
satisfactionCount = mconcat [
  count]

cost :: T.Text -> PreQuery
cost col = mconcat [
  sumOf "servicetbl" col,
  notNull "servicetbl" col]

calculatedCost :: PreQuery
calculatedCost = cost "payment_partnerCost"

limitedCost :: PreQuery
limitedCost = cost "payment_limitedCost"

select :: T.Text -> T.Text -> PreQuery
select tbl col = preQuery_ [T.concat [tbl, ".", col]] [tbl] [] [] []

selectExp :: [T.Text] -> T.Text -> PreQuery
selectExp tbls ex = preQuery_ [ex] tbls [] [] []

--
-- Queries for back block
--

undoneAction :: PreQuery
undoneAction = equals "actiontbl" "closed" "f"

averageTime :: (T.Text, T.Text) -> (T.Text, T.Text) -> PreQuery
averageTime (tbl1, col1) (tbl2, col2) = mconcat [
  selectExp [tbl1, tbl2] (T.concat ["extract(epoch from avg(", tbl1, ".", col1, " - ", tbl2, ".", col2, "))::int8"]),
  notNull tbl1 col1,
  notNull tbl2 col2,
  cond [tbl1, tbl2] (T.concat [tbl1, ".", col1, " > ", tbl2, ".", col2])]

averageActionTime :: PreQuery
averageActionTime = averageTime ("actiontbl", "closeTime") ("actiontbl", "openTime")

actionServiceRel :: PreQuery
actionServiceRel = cond ["servicetbl", "actiontbl"] "servicetbl.type || ':' || servicetbl.id = actiontbl.parentid"

actionCaseRel :: PreQuery
actionCaseRel = cond ["casetbl", "actiontbl"] "'case:' || casetbl.id = actiontbl.caseId"

-- | Add relations on tables
relations :: PreQuery -> PreQuery
relations q = q `mappend` mconcat (map snd $ filter (hasTables . fst) tableRelations) where
  qtables = preTables q
  hasTables ts = sort (qtables `intersect` ts) == sort ts
  tableRelations = [
    (["casetbl", "actiontbl"], actionCaseRel),
    (["casetbl", "servicetbl"], serviceCaseRel),
    (["casetbl", "consultationtbl"], consultationCaseRel),
    (["servicetbl", "actiontbl"], actionServiceRel)]

mintQuery :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m (Maybe Integer)
mintQuery qs = do
    rs <- runQuery [relations qs]
    case rs of
        [] -> error "Int query returns no rows"
        (PS.Only r:_) -> return r

caseSummary :: (PS.HasPostgres m, MonadLog m) => Filter -> PreQuery -> m Value
caseSummary filt@(Filter fromDate toDate program city partner) constraints = scope "caseSummary" $ do
  log Trace "Loading summary"
  [t, m, d, dur, calc, lim, sat] <- sequence [
    trace "total" (run count),
    trace "mechanics" mech,
    trace "average tech start" (run averageTowageTechStart),
    trace "average tech end" (run averageTowageTechEnd),
    trace "calculated cost" (run calculatedCost),
    trace "limited cost" (run limitedCost),
    liftM2 percentage (run satisfaction) (run satisfactionCount)]
  return $ object [
    "total" .= t,
    "mech" .= m,
    "delay" .= d,
    "duration" .= dur,
    "calculated" .= calc,
    "limited" .= lim,
    "satisfied" .= sat]
  where
    percentage _ 0 = 100
    percentage n d = n * 100 `div` d
    run p = liftM (fromMaybe 0) $ mintQuery $ mconcat [p, constraints, betweenTime fromDate toDate "servicetbl" "createTime"]

    (mechanicL, mechanicLActions) = strQuery $ mconcat [
      count' "cnt",
      equals "calltbl" "callerType" "client",
      inList "calltbl" "callType" ["mechanicConsOk", "mechanicConsNotOk"],
      betweenTime fromDate toDate "calltbl" "callDate",
      ifNotNull program $ equals "calltbl" "program",
      ifNotNull city $ equals "calltbl" "city"]
      -- TODO: partner?
    (mechanicR, mechanicRActions) = strQuery $ mconcat [
      count' "cnt",
      equals "consultationtbl" "constype" "mech",
      inList "consultationtbl" "status" ["serviceInProgress", "serviceOk", "serviceClosed", "serviceOrdered", "serviceDelayed"],
      betweenTime fromDate toDate "consultationtbl" "createTime",
      consultationCaseRel,
      ifNotNull program $ equals "casetbl" "program",
      ifNotNull city $ equals "casetbl" "city",
      ifNotNull partner $ equalsTo "consultationtbl" "trim(consultationtbl.contractor_partner)"]

    mech = liftM oneInt $ query
      (fromString $ "select sum(cnt)::integer from (" ++ mechanicL ++ " union " ++ mechanicR ++ ") as foo")
      (mechanicLActions ++ mechanicRActions)
      where
        oneInt :: [PS.Only Integer] -> Integer
        oneInt = maybe 0 PS.fromOnly . listToMaybe

caseServices :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> PreQuery -> [T.Text] -> m Value
caseServices fromDate toDate constraints names = scope "caseServices" $ do
  [totals, startAvgs, endAvgs, calcs, lims] <- mapM todayAndGroup [count, averageStart, averageEnd, calculatedCost, limitedCost]
  let
    makeServiceInfo n = object [
      "name" .= n,
      "total" .= lookAt totals,
      "delay" .= lookAt startAvgs,
      "duration" .= lookAt endAvgs,
      "calculated" .= lookAt calcs,
      "limited" .= lookAt lims]
      where
        lookAt :: [(T.Text, Integer)] -> Integer
        lookAt = fromMaybe 0 . lookup n
  return $ toJSON $ map makeServiceInfo names
  where
    todayAndGroup p = trace "result" $ runQuery_ $ mconcat [select "servicetbl" "type", p, constraints, betweenTime fromDate toDate "servicetbl" "createTime", groupBy "servicetbl" "type"]  

rkcCase :: (PS.HasPostgres m, MonadLog m) => Filter -> PreQuery -> [T.Text] -> m Value
rkcCase filt@(Filter fromDate toDate program city partner) constraints services = scope "rkcCase" $ do
  s <- caseSummary filt (mconcat [doneServices, constraints])
  ss <- caseServices fromDate toDate (mconcat [constraints, doneServices]) services
  return $ object [
    "summary" .= s,
    "services" .= ss]

actionsSummary :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> PreQuery -> m Value
actionsSummary fromDate toDate constraints = scope "actionsSummary" $ do
  log Trace "Loading summary"
  t <- trace "total" (run count)
  u <- trace "undone" (run (mconcat [count, undoneAction]))
  return $ object [
    "total" .= t,
    "undone" .= u]
  where
    run p = liftM (fromMaybe 0) $ mintQuery $ mconcat [p, constraints, betweenTime fromDate toDate "actiontbl" "duetime"]

actionsActions :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> PreQuery -> [T.Text] -> m Value
actionsActions fromDate toDate constraints actions = scope "actionsActions" $ do
  [totals, undones, avgs] <- mapM todayAndGroup [
    (count, "duetime"),
    (mconcat [count, undoneAction], "duetime"),
    (averageActionTime, "closeTime")]
  let
    makeActionInfo n = object [
      "name" .= n,
      "total" .= lookAt totals,
      "undone" .= lookAt undones,
      "average" .= lookAt avgs]
      where
        lookAt :: [(T.Text, Integer)] -> Integer
        lookAt = fromMaybe 0 . lookup n
  return $ toJSON $ map makeActionInfo actions
  where
    todayAndGroup (p, tm) = trace "result" $ runQuery_ $ mconcat [select "actiontbl" "name", notNull "actiontbl" "name", p, constraints, betweenTime fromDate toDate "actiontbl" tm, groupBy "actiontbl" "name"]

rkcActions :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> PreQuery -> [T.Text] -> m Value
rkcActions fromDate toDate constraints actions = scope "rkcActions" $ do
  s <- actionsSummary fromDate toDate constraints
  as <- actionsActions fromDate toDate constraints actions
  return $ object [
    "summary" .= s,
    "actions" .= as]

-- | Average time for each operator and action
rkcEachActionOpAvg :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> PreQuery -> [(T.Text, T.Text)] -> [T.Text] -> m Value
rkcEachActionOpAvg fromDate toDate constraints usrs acts = scope "rkcEachActionOpAvg" $ do
  r <- trace "result" $ runQuery_ $ mconcat [
    constraints,
    select "actiontbl" "assignedTo",
    select "actiontbl" "name",
    averageActionTime,
    count,
    notNull "actiontbl" "name",
    notNull "actiontbl" "assignedTo",
    betweenTime fromDate toDate "actiontbl" "duetime",
    groupBy "actiontbl" "assignedTo",
    groupBy "actiontbl" "name",
    orderBy "actiontbl" "assignedTo",
    orderBy "actiontbl" "name"]
  return $ toJSON $ mapMaybe (toInfo (groupResult r)) usrs
  where
    groupResult :: [(T.Text, T.Text, Integer, Integer)] -> [(T.Text, [(T.Text, (Integer, Integer))])]
    groupResult = map (first head . unzip) . L.groupBy ((==) `on` fst) . map (\(aTo, nm, avgTm, cnt) -> (aTo, (nm, (avgTm, cnt))))

    toInfo :: [(T.Text, [(T.Text, (Integer, Integer))])] -> (T.Text, T.Text) -> Maybe Value
    toInfo stats (n, label) = fmap fromStat $ lookup n stats where
      fromStat :: [(T.Text, (Integer, Integer))] -> Value
      fromStat st = object [
        "name" .= label,
        "avg" .= avgSum st,
        "avgs" .= map (`lookup` st) acts]
        where
          avgSum [] = (0, 0)
          avgSum st' = (average *** sum) $ unzip $ map snd st'
          average l = sum l `div` fromIntegral (length l)

rkcComplaints :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> PreQuery -> m Value
rkcComplaints fromDate toDate constraints = scope "rkcComplaints" $ do
  compls <- trace "result" $ runQuery_ $ mconcat [
    constraints,
    select "casetbl" "id",
    notNull "casetbl" "id",
    notNull "servicetbl" "type",
    selectExp ["servicetbl"] "string_agg(servicetbl.type, ' ')",
    betweenTime fromDate toDate "servicetbl" "createTime",
    equals "servicetbl" "clientSatisfied" "notSatis",
    groupBy "casetbl" "id",
    orderBy "casetbl" "id"]
  return $ toJSON $ map toCaseId compls
  where
    toCaseId :: (Integer, T.Text) -> Value
    toCaseId (i, srvs) = object [
      "caseid" .= i,
      "services" .= T.words srvs]

-- | Calculate @stats@ numbers of @/rkc@ response (average processing
-- times).
rkcStats :: (PS.HasPostgres m, MonadLog m) => Filter -> m Value
rkcStats (Filter from to program city partner) = scope "rkcStats" $ do
  let qParams = ( T.null program
                , program
                , T.null city
                , city
                , T.null partner
                , partner
                , from
                , to
                )
  rsp1 <- PS.query procAvgTimeQuery qParams
  rsp2 <- PS.query towStartAvgTimeQuery qParams
  let procAvgTime :: Maybe Double
      procAvgTime = head $ head rsp1
      towStartAvgTime :: Maybe Double
      towStartAvgTime = head $ head rsp2
  return $ object [ "procAvgTime" .= procAvgTime
                  , "towStartAvgTime" .= towStartAvgTime
                  ]

dictKeys :: T.Text -> Dictionary -> [T.Text]
dictKeys d = fromMaybe [] . keys [d]

serviceNames :: Dictionary -> [T.Text]
serviceNames = dictKeys "Services"

actionNames :: Dictionary -> [T.Text]
actionNames = dictKeys "ActionNames"

ifNotNull :: T.Text -> (T.Text -> PreQuery) -> PreQuery
ifNotNull value f = if T.null value then mempty else f value

data Filter = Filter {
  filterFrom :: UTCTime,
  filterTo :: UTCTime,
  filterProgram :: T.Text,
  filterCity :: T.Text,
  filterPartner :: T.Text }
    deriving (Eq, Show)

todayFilter :: IO Filter
todayFilter = do
  tz <- getCurrentTimeZone
  now <- fmap zonedTimeToLocalTime getZonedTime
  let
    startOfToday = now { localTimeOfDay = midnight }
    startOfTomorrow = startOfToday { localDay = addDays 1 (localDay startOfToday) }

  return Filter {
    filterFrom = localTimeToUTC tz startOfToday,
    filterTo = localTimeToUTC tz startOfTomorrow,
    filterProgram = "",
    filterCity = "",
    filterPartner = "" }

traceFilter :: MonadLog m => Filter -> m ()
traceFilter (Filter from to prog city partner) = do
  logTrace "Program: " prog
  logTrace "City: " city
  logTrace "From: " $ fromString $ show from
  logTrace "To: " $ fromString $ show to
  logTrace "Partner: " partner
  where
    logTrace :: MonadLog m => T.Text -> T.Text -> m ()
    logTrace prefix value = log Trace $ T.concat [prefix, value]

rkc :: (PS.HasPostgres m, MonadLog m) => UsersList -> Filter -> m Value
rkc (UsersList usrs) filt@(Filter fromDate toDate program city partner) = scope "rkc" $ do
  traceFilter filt
  dicts <- scope "dictionaries" . loadDictionaries $ "resources/site-config/dictionaries"
  c <- rkcCase filt constraints (serviceNames dicts)
  a <- rkcActions fromDate toDate constraints (actionNames dicts)
  ea <- rkcEachActionOpAvg fromDate toDate constraints usrs' (actionNames dicts)
  compls <- rkcComplaints fromDate toDate constraints
  s <- rkcStats filt
  return $ object [
    "case" .= c,
    "back" .= a,
    "eachopactions" .= ea,
    "complaints" .= compls,
    "stats" .= s]
  where
    constraints = mconcat [
      ifNotNull program $ equals "casetbl" "program",
      ifNotNull city $ equals "casetbl" "city",
      ifNotNull partner $ equalsTo "servicetbl" "trim(servicetbl.contractor_partner)"]
    usrs' = sort $ nub $ map toUsr usrs
    -- Build value-label pair from UserMeta, used to map logins to
    -- realNames
    toUsr m = (k, v)
        where
          k = T.decodeUtf8 $ m HM.! "value"
          v = T.decodeUtf8 $ m HM.! "label"
        

rkcFront :: (PS.HasPostgres m, MonadLog m) => Filter -> m Value
rkcFront filt@(Filter fromDate toDate program city _) = scope "rkc" $ scope "front" $ do
  traceFilter filt

  let
    args = [
      "program" %= (if T.null program then "" else "and (program is not null) and (program = ?)" :: String),
      "city" %= (if T.null city then "" else "and (city is not null) and (city = ?)" :: String)]

    callq = concat [
      "select callertype, calltype, count(*) from calltbl where",
      " (calldate >= ?) and (calldate < ?) and (calldate is not null) $program $city",
      " group by callertype, calltype order by callertype, calltype"]
    opCallsq = concat [
      "select calltaker, count(*) from calltbl where",
      " (calldate >= ?) and (calldate < ?) and (calldate is not null) $program $city",
      " group by calltaker order by calltaker"]
    opCasesq = concat [
      "select calltaker, count(*) from casetbl where",
      " (calldate >= ?) and (calldate < ?) and (calldate is not null) $program $city",
      " group by calltaker order by calltaker"]

    dateArgs = map PS.toField [asLocal fromDate, asLocal toDate]
    progCityArgs = map PS.toField $ filter (not . T.null) [program, city]

  calls <- trace "result" $ fquery callq args (dateArgs ++ progCityArgs)
  opCalls <- trace "op calls" $ fquery opCallsq args (dateArgs ++ progCityArgs)
  opCases <- trace "op cases" $ fquery opCasesq args (dateArgs ++ progCityArgs)

  let
    sums :: [(Integer, Integer)] -> (Integer, Integer)
    sums = foldr1 sumPair where
      sumPair (lx, ly) (rx, ry) = (lx + rx, ly + ry)
    opCallsCases :: [(Maybe T.Text, (Integer, Integer))]
    opCallsCases = map ((head *** sums) . unzip)
      $ L.groupBy ((==) `on` fst)
      $ sort
      $ map (\(name, i) -> (name, (i, 0))) opCalls ++ map (\(name, i) -> (name, (0, i))) opCases

  return $ object [
    "calls" .= map toCall calls,
    "ops" .= map toOp opCallsCases]

  where
    toCall :: (Maybe T.Text, Maybe T.Text, Integer) -> Value
    toCall (callerType, callType, callsCount) = object [
      "callertype" .= callerType,
      "calltype" .= callType,
      "callcount" .= callsCount]

    toOp :: (Maybe T.Text, (Integer, Integer)) -> Value
    toOp (name, (calls, cases)) = object [
      "name" .= name,
      "calls" .= calls,
      "cases" .= cases]

-- | All partners on services within time interval
partners :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> m Value
partners fromDate toDate = scope "rkc" $ scope "partners" $ do
  log Trace $ T.concat ["From: ", fromString $ show fromDate]
  log Trace $ T.concat ["To: ", fromString $ show toDate]

  let
    q = [
      "select trim(contractor_partner) c from servicetbl where",
      " (createTime >= ?) and (createTime < ?) and (createTime is not null) group by c order by c"]

  ps <- trace "result" $ queryFmt q [] [asLocal fromDate, asLocal toDate]
  return $ toJSON (mapMaybe PS.fromOnly ps :: [T.Text])

queryFmt_ :: (PS.HasPostgres m, MonadLog m, PS.FromRow r) => [String] -> FormatArgs -> m [r]
queryFmt_ lns args = query_ (fromString $ T.unpack $ format (concat lns) args)

queryFmt :: (PS.HasPostgres m, MonadLog m, PS.ToRow q, PS.FromRow r) => [String] -> FormatArgs -> q -> m [r]
queryFmt lns args = query (fromString $ T.unpack $ format (concat lns) args)

-- | Calculate average service processing time (in seconds).
--
-- Parametrized by: program (boolean flag & value for this parameter,
-- where flag is *false* if filtering by that parameter should be
-- active), city (flag & value), partner name (flag & value), from
-- (value, always active), to (always active).
procAvgTimeQuery :: PS.Query
procAvgTimeQuery = [sql|
WITH actiontimes AS (
 SELECT (max(a2.closeTime - a1.ctime))
 FROM casetbl c, servicetbl s, actiontbl a1, actiontbl a2
 WHERE a1.parentid=a2.parentid
 AND (a2.result='serviceOrdered' 
      OR a2.result='serviceOrderedSMS')
 AND a1.name='orderService'
 AND a1.parentid=concat(s.type, ':', s.id)
 AND cast(split_part(a1.caseid, ':', 2) as integer)=c.id
 AND s.times_expectedServiceStart <= (a1.ctime + INTERVAL '01:00:00')
 AND (? or c.program = ?)
 AND (? or c.city = ?)
 AND (? or s.contractor_partner = ?)
 AND c.calldate >= ?
 AND c.calldate < ?
 GROUP BY a1.parentid)
SELECT extract(epoch from avg(max)) FROM actiontimes;
|]

-- | Calculate average tower arrival time (in seconds).
--
-- Parametrized in the same way as 'procAvgTimeQuery'.
towStartAvgTimeQuery :: PS.Query
towStartAvgTimeQuery = [sql|
WITH
 services AS (
  SELECT type, id, times_factServiceStart, contractor_partner, suburbanmilage
    FROM techtbl
  UNION ALL
  SELECT type, id, times_factServiceStart, contractor_partner, suburbanmilage
    FROM towagetbl),
 actiontimes AS (
 SELECT (max(s.times_factServiceStart - a.ctime))
 FROM actiontbl a, casetbl c, services s
 WHERE a.parentid = concat(s.type, ':', s.id)
 AND cast(split_part(a.caseid, ':', 2) as integer)=c.id
 AND a.name='orderService'
 AND a.ctime < s.times_factServiceStart
 AND (s.type='towage' OR s.type='tech')
 AND (? or c.program = ?)
 AND (? or c.city = ?)
 AND (? or s.contractor_partner = ?)
 AND s.suburbanmilage = '0'
 AND c.calldate >= ?
 AND c.calldate < ?
 GROUP BY a.parentid)
SELECT extract(epoch from avg(max)) FROM actiontimes;
|]

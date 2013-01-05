{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.RKC (
  rkc,
  rkcFront
  ) where

import Prelude hiding (log)

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.List (intersect, sort, nub)
import Data.String
import qualified Data.List as L (groupBy)
import qualified Data.Map as M
import Data.Time
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Snap.Snaplet.PostgresqlSimple as PS

import Snaplet.DbLayer.Dictionary
import Snaplet.DbLayer.ARC

import Snap.Snaplet.SimpleLog

import Util

-------------------------------------------------------------------------------
-- TODO: Use 'model.field' instead of 'table.column'
-------------------------------------------------------------------------------

runQuery_ :: (PS.HasPostgres m, MonadLog m, PS.FromRow r) => PreQuery -> m [r]
runQuery_ pq = runQuery [relations pq]

-- Column of table is in list
inList :: T.Text -> T.Text -> [T.Text] -> PreQuery
inList tbl col vals = preQuery [] [tbl] [T.concat [tbl, ".", col, " in ?"]] [] [] [PS.In vals]

equals :: T.Text -> T.Text -> T.Text -> PreQuery
equals tbl col val = preQuery [] [tbl] [T.concat [tbl, ".", col, " = ?"]] [] [] [val]

withinToday :: T.Text -> T.Text -> PreQuery
withinToday tbl col = thisDay `mappend` notNull tbl col where
  thisDay = preQuery_ [] [tbl] [equalsNow] [] []
  equalsNow = T.concat ["date_trunc('day', ", tbl, ".", col, " + '4 hours') = date_trunc('day', now())"]

betweenTime :: UTCTime -> UTCTime -> T.Text -> T.Text -> PreQuery
betweenTime from to tbl col = mconcat [
  notNull tbl col,
  preQuery [] [tbl] [T.concat [tbl, ".", col, " >= ?"]] [] [] [asLocal from],
  preQuery [] [tbl] [T.concat [tbl, ".", col, " < ?"]] [] [] [asLocal to]]
  where
    asLocal :: UTCTime -> LocalTime
    asLocal = utcToLocalTime utc

count :: PreQuery
count = preQuery_ ["count(*)"] [] [] [] []

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
    (["servicetbl", "actiontbl"], actionServiceRel)]

mintQuery :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m (Maybe Integer)
mintQuery qs = do
    rs <- runQuery [relations qs]
    case rs of
        [] -> error "Int query returns no rows"
        (PS.Only r:_) -> return r

caseSummary :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> PreQuery -> m Value
caseSummary fromDate toDate constraints = scope "caseSummary" $ do
  log Trace "Loading summary"
  [t, m, d, dur, calc, lim, sat] <- sequence [
    trace "total" (run count),
    trace "mechanics" (run (mconcat [count, mechanic])),
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

rkcCase :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> PreQuery -> [T.Text] -> m Value
rkcCase fromDate toDate constraints services = scope "rkcCase" $ do
  s <- caseSummary fromDate toDate (mconcat [doneServices, constraints])
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
rkcEachActionOpAvg :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> [(T.Text, T.Text, T.Text)] -> [T.Text] -> m Value
rkcEachActionOpAvg fromDate toDate usrs acts = scope "rkcEachActionOpAvg" $ do
  r <- trace "result" $ runQuery_ $ mconcat [
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

    toInfo :: [(T.Text, [(T.Text, (Integer, Integer))])] -> (T.Text, T.Text, T.Text) -> Maybe Value
    toInfo stats (n, label, _) = fmap fromStat $ lookup n stats where
      fromStat :: [(T.Text, (Integer, Integer))] -> Value
      fromStat st = object [
        "name" .= label,
        "avg" .= avgSum st,
        "avgs" .= map (`lookup` st) acts]
        where
          avgSum [] = (0, 0)
          avgSum st' = (average *** sum) $ unzip $ map snd st'
          average l = sum l `div` fromIntegral (length l)


dictKeys :: T.Text -> Dictionary -> [T.Text]
dictKeys d = fromMaybe [] . keys [d]

serviceNames :: Dictionary -> [T.Text]
serviceNames = dictKeys "Services"

actionNames :: Dictionary -> [T.Text]
actionNames = dictKeys "ActionNames"

ifNotNull :: T.Text -> (T.Text -> PreQuery) -> PreQuery
ifNotNull value f = if T.null value then mempty else f value

rkc :: (PS.HasPostgres m, MonadLog m) => UsersDict -> UTCTime -> UTCTime -> T.Text -> T.Text -> m Value
rkc (UsersDict usrs) fromDate toDate program city = scope "rkc" $ do
  log Trace $ T.concat ["Program: ", program]
  log Trace $ T.concat ["City: ", city]
  log Trace $ T.concat ["From: ", fromString $ show fromDate]
  log Trace $ T.concat ["To: ", fromString $ show toDate]
  dicts <- scope "dictionaries" . liftIO . loadDictionaries $ "resources/site-config/dictionaries"
  c <- rkcCase fromDate toDate constraints (serviceNames dicts)
  a <- rkcActions fromDate toDate constraints (actionNames dicts)
  ea <- rkcEachActionOpAvg fromDate toDate usrs' (actionNames dicts)
  return $ object [
    "case" .= c,
    "back" .= a,
    "eachopactions" .= ea]
  where
    constraints = mconcat [
      ifNotNull program $ equals "casetbl" "program",
      ifNotNull city $ equals "casetbl" "city"]
    usrs' = sort $ nub $ map toUsr usrs
    toUsr m = (maybe "" T.decodeUtf8 $ M.lookup "value" m, maybe "" T.decodeUtf8 $ M.lookup "label" m, maybe "" T.decodeUtf8 $ M.lookup "roles" m)

rkcFront :: (PS.HasPostgres m, MonadLog m) => UTCTime -> UTCTime -> T.Text -> T.Text -> m Value
rkcFront fromDate toDate program city = scope "rkc" $ scope "front" $ do
  log Trace $ T.concat ["Program: ", program]
  log Trace $ T.concat ["City: ", city]
  log Trace $ T.concat ["From: ", fromString $ show fromDate]
  log Trace $ T.concat ["To: ", fromString $ show toDate]
  calls <- trace "result" $ runQuery_ $ mconcat [
    select "calltbl" "callertype",
    select "calltbl" "calltype",
    count,

    betweenTime fromDate toDate "calltbl" "calldate",
    ifNotNull program $ equals "calltbl" "program",
    ifNotNull city $ equals "calltbl" "city",
    
    groupBy "calltbl" "callertype",
    groupBy "calltbl" "calltype",

    orderBy "calltbl" "callertype",
    orderBy "calltbl" "calltype"]

  return $ object [
    "calls" .= map toCall calls]

  where
    toCall :: (Maybe T.Text, Maybe T.Text, Integer) -> Value
    toCall (callerType, callType, callsCount) = object [
      "callertype" .= callerType,
      "calltype" .= callType,
      "callcount" .= callsCount]

{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.RKC (
  ActionOpAvgInformation(..),
  CaseSummary(..), CaseServiceInfo(..), CaseInformation(..),
  ActionsSummary(..), ActionInfo(..), ActionsInformation(..),
  Information(..),
  rkc,
  rkcFront
  ) where

import Prelude hiding (log)

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.List (intersect, sort, nub)
import qualified Data.List as L (groupBy)
import qualified Data.Map as M
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

programIs :: T.Text -> PreQuery
programIs p = mconcat [equals "casetbl" "program" p]

cost :: T.Text -> PreQuery
cost col = mconcat [
  sumOf "servicetbl" col,
  notNull "servicetbl" col]

calculatedCost :: PreQuery
calculatedCost = cost "payment_partnerCost"

limitedCost :: PreQuery
limitedCost = cost "payment_limitedCost"

inCity :: T.Text -> PreQuery
inCity = equals "casetbl" "city"

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

data CaseSummary = CaseSummary {
  summaryTotalServices :: Integer,
  summaryTotalMechanics :: Integer,
  summaryAverageStart :: Integer,
  summaryAverateEnd :: Integer,
  summaryCalculatedCost :: Integer,
  summaryLimitedCost :: Integer,
  summarySatisfied :: Integer }
    deriving (Eq, Ord, Read, Show)

instance FromJSON CaseSummary where
  parseJSON (Object v) = CaseSummary <$>
    (v .: "total") <*>
    (v .: "mech") <*>
    (v .: "delay") <*>
    (v .: "duration") <*>
    (v .: "calculated") <*>
    (v .: "limited") <*>
    (v .: "satisfied")
  parseJSON _ = empty

instance ToJSON CaseSummary where
  toJSON (CaseSummary t m d dur calc lim sat) = object [
    "total" .= t,
    "mech" .= m,
    "delay" .= d,
    "duration" .= dur,
    "calculated" .= calc,
    "limited" .= lim,
    "satisfied" .= sat]

data CaseServiceInfo = CaseServiceInfo {
  serviceName :: T.Text,
  serviceTotal :: Integer,
  serviceAverageStart :: Integer,
  serviceAverageEnd :: Integer,
  serviceCalculatedCost :: Integer,
  serviceLimitedCost :: Integer }
    deriving (Eq, Ord, Read, Show)

instance FromJSON CaseServiceInfo where
  parseJSON (Object v) = CaseServiceInfo <$>
    (v .: "name") <*>
    (v .: "total") <*>
    (v .: "delay") <*>
    (v .: "duration") <*>
    (v .: "calculated") <*>
    (v .: "limited")
  parseJSON _ = empty

instance ToJSON CaseServiceInfo where
  toJSON (CaseServiceInfo n t d dur calc lim) = object [
    "name" .= n,
    "total" .= t,
    "delay" .= d,
    "duration" .= dur,
    "calculated" .= calc,
    "limited" .= lim]

data CaseInformation = CaseInformation {
  caseInfoSummary :: CaseSummary,
  caseInfoServices :: [CaseServiceInfo] }
    deriving (Eq, Ord, Read, Show)

instance FromJSON CaseInformation where
  parseJSON (Object v) = CaseInformation <$>
    (v .: "summary") <*>
    (v .: "services")
  parseJSON _ = empty


instance ToJSON CaseInformation where
  toJSON (CaseInformation s ss) = object [
    "summary" .= s,
    "services" .= ss]

data ActionsSummary = ActionsSummary {
  actionsSummaryTotalActions :: Integer,
  actionsSummaryTotalIncompleted :: Integer }
    deriving (Eq, Ord, Read, Show)

instance FromJSON ActionsSummary where
  parseJSON (Object v) = ActionsSummary <$>
    (v .: "total") <*>
    (v .: "undone")
  parseJSON _ = empty

instance ToJSON ActionsSummary where
  toJSON (ActionsSummary t u) = object [
    "total" .= t,
    "undone" .= u]

data ActionInfo = ActionInfo {
  actionName :: T.Text,
  totalActions :: Integer,
  totalIncompleted :: Integer,
  actionsAverage :: Integer }
    deriving (Eq, Ord, Read, Show)

instance FromJSON ActionInfo where
  parseJSON (Object v) = ActionInfo <$>
    (v .: "name") <*>
    (v .: "total") <*>
    (v .: "undone") <*>
    (v .: "average")
  parseJSON _ = empty

instance ToJSON ActionInfo where
  toJSON (ActionInfo n t u a) = object [
    "name" .= n,
    "total" .= t,
    "undone" .= u,
    "average" .= a]

data ActionsInformation = ActionsInformation {
  backInfoSummary :: ActionsSummary,
  backInfoActions :: [ActionInfo] }
    deriving (Eq, Ord, Read, Show)

instance FromJSON ActionsInformation where
  parseJSON (Object v) = ActionsInformation <$>
    (v .: "summary") <*>
    (v .: "actions")
  parseJSON _ = empty

instance ToJSON ActionsInformation where
  toJSON (ActionsInformation s as) = object [
    "summary" .= s,
    "actions" .= as]

data ActionOpAvgInformation = ActionOpAvgInformation {
  actionOpName :: T.Text,
  actionOpAvg :: (Integer, Integer),
  actionOpAvgs :: [Maybe (Integer, Integer)] }
    deriving (Eq, Ord, Read, Show)

instance FromJSON ActionOpAvgInformation where
  parseJSON (Object v) = ActionOpAvgInformation <$>
    (v .: "name") <*>
    (v .: "avg") <*>
    (v .: "avgs")
  parseJSON _ = empty

instance ToJSON ActionOpAvgInformation where
  toJSON (ActionOpAvgInformation nm avg avgs) = object [
    "name" .= nm,
    "avg" .= avg,
    "avgs" .= avgs]

data Information = Information {
  rkcCaseInformation :: CaseInformation,
  rkcActionsInformation :: ActionsInformation,
  rkcEachActionOpInfo :: [ActionOpAvgInformation] }
    deriving (Eq, Ord, Read, Show)

instance FromJSON Information where
  parseJSON (Object v) = Information <$>
    (v .: "case") <*>
    (v .: "back") <*>
    (v .: "eachopactions")
  parseJSON _ = empty

instance ToJSON Information where
  toJSON (Information c b ea) = object [
    "case" .= c,
    "back" .= b,
    "eachopactions" .= ea]

mintQuery :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m (Maybe Integer)
mintQuery qs = do
    rs <- runQuery [relations qs]
    case rs of
        [] -> error "Int query returns no rows"
        (PS.Only r:_) -> return r

caseSummary :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m CaseSummary
caseSummary constraints = scope "caseSummary" $ do
  log Trace "Loading summary"
  return CaseSummary `ap`
    trace "total" (run count) `ap`
    trace "mechanics" (run (mconcat [count, mechanic])) `ap`
    trace "average tech start" (run averageTowageTechStart) `ap`
    trace "average tech end" (run averageTowageTechEnd) `ap`
    trace "calculated cost" (run calculatedCost) `ap`
    trace "limited cost" (run limitedCost) `ap`
    liftM2 percentage (run satisfaction) (run satisfactionCount)
  where
    percentage _ 0 = 100
    percentage n d = n * 100 `div` d
    run p = liftM (fromMaybe 0) $ mintQuery $ mconcat [p, constraints, withinToday "servicetbl" "createTime"]

caseServices :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> m [CaseServiceInfo]
caseServices constraints names = scope "caseServices" $ do
  [totals, startAvgs, endAvgs, calcs, lims] <- mapM todayAndGroup [count, averageStart, averageEnd, calculatedCost, limitedCost]
  let
    makeServiceInfo n = CaseServiceInfo n (lookAt totals) (lookAt startAvgs) (lookAt endAvgs) (lookAt calcs) (lookAt lims) where
      lookAt = fromMaybe 0 . lookup n
  return $ map makeServiceInfo names
  where
    todayAndGroup p = trace "result" $ runQuery_ $ mconcat [select "servicetbl" "type", p, constraints, withinToday "servicetbl" "createTime", groupBy "servicetbl" "type"]

rkcCase :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> m CaseInformation
rkcCase constraints services = scope "rkcCase" (return CaseInformation  `ap` caseSummary (mconcat [doneServices, constraints]) `ap` caseServices (mconcat [constraints, doneServices]) services)

actionsSummary :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m ActionsSummary
actionsSummary constraints = scope "actionsSummary" $ do
  log Trace "Loading summary"
  return ActionsSummary `ap`
    trace "total" (run count) `ap`
    trace "undone" (run (mconcat [count, undoneAction]))
  where
    run p = liftM (fromMaybe 0) $ mintQuery $ mconcat [p, constraints, withinToday "actiontbl" "duetime"]

actionsActions :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> m [ActionInfo]
actionsActions constraints actions = scope "backAction" $ do
  [totals, undones, avgs] <- mapM todayAndGroup [
    (count, "duetime"),
    (mconcat [count, undoneAction], "duetime"),
    (averageActionTime, "closeTime")]
  let
    makeActionInfo n = ActionInfo n (lookAt totals) (lookAt undones) (lookAt avgs) where
      lookAt = fromMaybe 0 . lookup n
  return $ map makeActionInfo actions
  where
    todayAndGroup (p, tm) = trace "result" $ runQuery_ $ mconcat [select "actiontbl" "name", notNull "actiontbl" "name", p, constraints, withinToday "actiontbl" tm, groupBy "actiontbl" "name"]

rkcActions :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> m ActionsInformation
rkcActions constraints actions = scope "rkcActions" (return ActionsInformation `ap` actionsSummary constraints `ap` actionsActions constraints actions)

-- | Average time for each operator and action
rkcEachActionOpAvg :: (PS.HasPostgres m, MonadLog m) => [(T.Text, T.Text, T.Text)] -> [T.Text] -> m [ActionOpAvgInformation]
rkcEachActionOpAvg usrs acts = scope "rkcEachActionOpAvg" $ do
  r <- trace "result" $ runQuery_ $ mconcat [
    select "actiontbl" "assignedTo",
    select "actiontbl" "name",
    averageActionTime,
    count,
    notNull "actiontbl" "name",
    notNull "actiontbl" "assignedTo",
    withinToday "actiontbl" "duetime",
    groupBy "actiontbl" "assignedTo",
    groupBy "actiontbl" "name",
    orderBy "actiontbl" "assignedTo",
    orderBy "actiontbl" "name"]
  return $ mapMaybe (toInfo (groupResult r)) usrs
  where
    groupResult :: [(T.Text, T.Text, Integer, Integer)] -> [(T.Text, [(T.Text, (Integer, Integer))])]
    groupResult = map (first head . unzip) . L.groupBy ((==) `on` fst) . map (\(aTo, nm, avgTm, cnt) -> (aTo, (nm, (avgTm, cnt))))

    toInfo :: [(T.Text, [(T.Text, (Integer, Integer))])] -> (T.Text, T.Text, T.Text) -> Maybe ActionOpAvgInformation
    toInfo stats (n, label, _) = fmap fromStat $ lookup n stats where
      fromStat :: [(T.Text, (Integer, Integer))] -> ActionOpAvgInformation
      fromStat st = ActionOpAvgInformation label (avgSum st) (map (`lookup` st) acts) where
        avgSum [] = (0, 0)
        avgSum st' = (average *** sum) $ unzip $ map snd st'
        average l = sum l `div` fromIntegral (length l)

dictKeys :: T.Text -> Dictionary -> [T.Text]
dictKeys d = fromMaybe [] . keys [d]

serviceNames :: Dictionary -> [T.Text]
serviceNames = dictKeys "Services"

actionNames :: Dictionary -> [T.Text]
actionNames = dictKeys "ActionNames"

rkc :: (PS.HasPostgres m, MonadLog m) => UsersDict -> T.Text -> T.Text -> m Information
rkc (UsersDict usrs) program city = scope "rkc" $ do
  log Trace $ T.concat ["Program: ", program]
  log Trace $ T.concat ["City: ", city]
  dicts <- scope "dictionaries" . liftIO . loadDictionaries $ "resources/site-config/dictionaries"
  c <- rkcCase constraints (serviceNames dicts)
  a <- rkcActions constraints (actionNames dicts)
  ea <- rkcEachActionOpAvg usrs' (actionNames dicts)
  return $ Information c a ea
  where
    constraints = mconcat [ifNotNull program programIs, ifNotNull city inCity]
    ifNotNull value f = if T.null value then mempty else f value
    usrs' = sort $ nub $ map toUsr usrs
    toUsr m = (maybe "" T.decodeUtf8 $ M.lookup "value" m, maybe "" T.decodeUtf8 $ M.lookup "label" m, maybe "" T.decodeUtf8 $ M.lookup "roles" m)

rkcFront :: (PS.HasPostgres m, MonadLog m) => T.Text -> T.Text -> m Value
rkcFront program city = scope "rkc" $ scope "front" $ do
  log Trace $ T.concat ["Program: ", program]
  log Trace $ T.concat ["City: ", city]
  calls <- trace "result" $ runQuery_ $ mconcat [
    select "calltbl" "callertype",
    select "calltbl" "calltype",
    count,

    --withinToday "calltbl" "calldate",
    ifNotNull program $ equals "calltbl" "program",
    ifNotNull city $ equals "calltbl" "city",
    
    groupBy "calltbl" "callertype",
    groupBy "calltbl" "calltype",

    orderBy "calltbl" "callertype",
    orderBy "calltbl" "calltype"]

  return $ object [
    "calls" .= map toCall calls]

  where
    ifNotNull value f = if T.null value then mempty else f value

    toCall :: (Maybe T.Text, Maybe T.Text, Integer) -> Value
    toCall (callerType, callType, callsCount) = object [
      "callertype" .= callerType,
      "calltype" .= callType,
      "callcount" .= callsCount]

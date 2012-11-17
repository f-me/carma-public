{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.RKC (
  CaseSummary(..), CaseServiceInfo(..), CaseInformation(..),
  BackSummary(..), BackActionInfo(..), BackInformation(..),
  Information(..),
  rkc
  ) where

import Prelude hiding (log, catch)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO
import qualified Control.Exception as E

import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.List (intersect, sort, nub)
import qualified Data.Map as M
import Data.Time
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Database.PostgreSQL.Simple.FromField as PS

import Snaplet.DbLayer.Dictionary
import Snaplet.DbLayer.ARC

import System.Locale
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

withinDay :: T.Text -> T.Text -> UTCTime -> PreQuery
withinDay tbl col tm = preQuery_ [] [tbl] [afterStart, beforeEnd] [] [] where
  st = fromString $ formatTime defaultTimeLocale "%F %T" tm
  afterStart = T.concat [tbl, ".", col, " - '", st, "' >= '0 days'"]
  beforeEnd = T.concat [tbl, ".", col, " - '", st, "' < '1 day'"]

withinToday :: T.Text -> T.Text -> PreQuery
withinToday tbl col = thisDay `mappend` notNull tbl col where
  thisDay = preQuery_ [] [tbl] [equalsNow] [] []
  equalsNow = T.concat ["date_trunc('day', ", tbl, ".", col, " + '4 hours') = date_trunc('day', now())"]

-- Get start of this day for timezone
startOfDay :: TimeZone -> UTCTime -> UTCTime
startOfDay tz = localTimeToUTC tz . dropTime . utcToLocalTime tz where
  dropTime t = t { localTimeOfDay = midnight }

-- Get start of current day for current timezone
startOfThisDay :: IO UTCTime
startOfThisDay = startOfDay <$> getCurrentTimeZone <*> getCurrentTime

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
  "mistake",
  "falseCall",
  "serviceOk",
  "serviceClosed"]

serviceCaseRel :: PreQuery
serviceCaseRel = cond ["casetbl", "servicetbl"] "casetbl.id = servicetbl.parentId"

mechanic :: PreQuery
mechanic = equals "calltbl" "garbage -> 'callType'" "client" `mappend` inList "calltbl" "garbage -> 'callerType'" [
  "mechanicConsOk",
  "mechanicConsNotOk"]

towageTech :: PreQuery
towageTech = inList "servicetbl" "type" ["towage", "tech"]

averageTowageTechStart :: PreQuery
averageTowageTechStart = mconcat [
  averageTime ("servicetbl", "times_factServiceStart") ("casetbl", "callDate"),
  serviceCaseRel,
  towageTech,
  cond ["servicetbl"] "(servicetbl.suburbanMilage = 0) or (servicetbl.suburbanMilage is null)"]

averageTowageTechEnd :: PreQuery
averageTowageTechEnd = mconcat [
  averageTime ("servicetbl", "times_factServiceEnd") ("servicetbl", "times_factServiceStart"),
  towageTech,
  cond ["servicetbl"] "(servicetbl.suburbanMilage = 0) or (servicetbl.suburbanMilage is null)"]

satisfaction :: PreQuery
satisfaction = mconcat [
  count,
  equals "servicetbl" "clientSatisfied" "satis"]

satisfactionCount :: PreQuery
satisfactionCount = mconcat [
  count,
  inList "servicetbl" "clientSatisfied" ["satis", "notSatis"]]

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
inCity = equals "casetbl" "garbage -> 'city'"

select :: T.Text -> T.Text -> PreQuery
select tbl col = preQuery_ [T.concat [tbl, ".", col]] [tbl] [] [] []

selectExp :: [T.Text] -> T.Text -> PreQuery
selectExp tbls ex = preQuery_ [ex] tbls [] [] []

--
-- Queries for front block
--
frontOps :: PreQuery
frontOps = mconcat [
  select "actiontbl" "assignedTo",
  averageActionTime,
  equals "actiontbl" "closed" "t",
  withinToday "actiontbl" "openTime",
  groupBy "actiontbl" "assignedTo"]

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
relations q = q `mappend` (mconcat $ map snd $ filter (hasTables . fst) tableRelations) where
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

data FrontInformation = FrontInformation {
  frontOperators :: [FrontOperatorInfo] }
    deriving (Eq, Ord, Read, Show)

instance FromJSON FrontInformation where
  parseJSON (Object v) = FrontInformation <$>
    (v .: "operators")
  parseJSON _ = empty

instance ToJSON FrontInformation where
  toJSON (FrontInformation f) = object [
    "operators" .= f]

data FrontOperatorInfo = FrontOperatorInfo {
  frontOperatorName :: T.Text,
  frontOperatorAvgTime :: Integer,
  frontOperatorRoles :: T.Text }
    deriving (Eq, Ord, Read, Show)

instance FromJSON FrontOperatorInfo where
  parseJSON (Object v) = FrontOperatorInfo <$>
    (v .: "name") <*>
    (v .: "avg") <*>
    (v .: "roles")
  parseJSON _ = empty

instance ToJSON FrontOperatorInfo where
  toJSON (FrontOperatorInfo n a r) = object [
    "name" .= n,
    "avg" .= a,
    "roles" .= r]

data BackSummary = BackSummary {
  backSummaryTotalActions :: Integer,
  backSummaryTotalIncompleted :: Integer }
    deriving (Eq, Ord, Read, Show)

instance FromJSON BackSummary where
  parseJSON (Object v) = BackSummary <$>
    (v .: "total") <*>
    (v .: "undone")
  parseJSON _ = empty

instance ToJSON BackSummary where
  toJSON (BackSummary t u) = object [
    "total" .= t,
    "undone" .= u]

data BackActionInfo = BackActionInfo {
  backActionName :: T.Text,
  backTotalActions :: Integer,
  backTotalIncompleted :: Integer,
  backAverage :: Integer }
    deriving (Eq, Ord, Read, Show)

instance FromJSON BackActionInfo where
  parseJSON (Object v) = BackActionInfo <$>
    (v .: "name") <*>
    (v .: "total") <*>
    (v .: "undone") <*>
    (v .: "average")
  parseJSON _ = empty

instance ToJSON BackActionInfo where
  toJSON (BackActionInfo n t u a) = object [
    "name" .= n,
    "total" .= t,
    "undone" .= u,
    "average" .= a]

data BackInformation = BackInformation {
  backInfoSummary :: BackSummary,
  backInfoActions :: [BackActionInfo] }
    deriving (Eq, Ord, Read, Show)

instance FromJSON BackInformation where
  parseJSON (Object v) = BackInformation <$>
    (v .: "summary") <*>
    (v .: "actions")
  parseJSON _ = empty

instance ToJSON BackInformation where
  toJSON (BackInformation s as) = object [
    "summary" .= s,
    "actions" .= as]

data Information = Information {
  rkcCaseInformation :: CaseInformation,
  rkcFrontInformation :: FrontInformation,
  rkcBackInformation :: BackInformation }
    deriving (Eq, Ord, Read, Show)

instance FromJSON Information where
  parseJSON (Object v) = Information <$>
    (v .: "case") <*>
    (v .: "front") <*>
    (v .: "back")
  parseJSON _ = empty

instance ToJSON Information where
  toJSON (Information c f b) = object [
    "case" .= c,
    "front" .= f,
    "back" .= b]

mintQuery :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m (Maybe Integer)
mintQuery qs = do
    rs <- runQuery [relations qs]
    case rs of
        [] -> error "Int query returns no rows"
        ((PS.Only r):_) -> return r
        _ -> error "Int query returns invalid result"

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
    (liftM2 percentage (run satisfaction) (run satisfactionCount))
  where
    percentage _ 0 = 100
    percentage n d = n * 100 `div` d
    run p = liftM (fromMaybe 0) $ mintQuery $ mconcat [p, constraints, withinToday "servicetbl" "createTime"]

caseServices :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> m [CaseServiceInfo]
caseServices constraints names = scope "caseServices" $ do
  [totals, startAvgs, endAvgs, calcs, lims] <- mapM todayAndGroup [count, averageTowageTechStart, averageTowageTechEnd, calculatedCost, limitedCost]
  let
    makeServiceInfo n = CaseServiceInfo n (look totals) (look startAvgs) (look endAvgs) (look calcs) (look lims) where
      look = fromMaybe 0 . lookup n
  return $ map makeServiceInfo names
  where
    todayAndGroup p = trace "result" $ runQuery_ $ mconcat [select "servicetbl" "type", p, constraints, withinToday "servicetbl" "createTime", groupBy "servicetbl" "type"]

rkcCase :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> m CaseInformation
rkcCase constraints services = scope "rkcCase" $ (return CaseInformation  `ap` caseSummary (mconcat [doneServices, constraints]) `ap` caseServices (mconcat [constraints, doneServices]) services)

rkcFront :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [(T.Text, T.Text, T.Text)] -> m FrontInformation
rkcFront constraints usrs = scope "rkcFront" $ do
  log Trace "Loading front info"
  vals <- runQuery_ $ mconcat [frontOps, constraints]
  let
    makeOpInfo (n, label, roles) = FrontOperatorInfo label (fromMaybe 0 $ lookup n vals) roles
  return $ FrontInformation $ map makeOpInfo usrs

backSummary :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m BackSummary
backSummary constraints = scope "backSummary" $ do
  log Trace "Loading summary"
  return BackSummary `ap`
    trace "total" (run count) `ap`
    trace "undone" (run (mconcat [count, undoneAction]))
  where
    run p = liftM (fromMaybe 0) $ mintQuery $ mconcat [p, constraints, withinToday "actiontbl" "openTime"]

backActions :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> m [BackActionInfo]
backActions constraints actions = scope "backAction" $ do
  [totals, undones, avgs] <- mapM todayAndGroup [count, mconcat [count, undoneAction], averageActionTime]
  let
    makeActionInfo n = BackActionInfo n (look totals) (look undones) (look avgs) where
      look = fromMaybe 0 . lookup n
  return $ map makeActionInfo actions
  where
    todayAndGroup p = trace "result" $ runQuery_ $ mconcat [select "actiontbl" "name", notNull "actiontbl" "name", p, constraints, withinToday "actiontbl" "openTime", groupBy "actiontbl" "name"]

rkcBack :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> m BackInformation
rkcBack constraints actions = scope "rkcBack" $ (return BackInformation `ap` backSummary constraints `ap` backActions constraints actions)

dictKeys :: T.Text -> Dictionary -> [T.Text]
dictKeys d = fromMaybe [] . keys [d]

serviceNames :: Dictionary -> [T.Text]
serviceNames = dictKeys "Services"

programNames :: Dictionary -> [T.Text]
programNames = dictKeys "Programs"

actionNames :: Dictionary -> [T.Text]
actionNames = dictKeys "ActionNames"

rkc :: (PS.HasPostgres m, MonadLog m) => UsersDict -> T.Text -> T.Text -> m Information
rkc (UsersDict usrs) program city = liftIO startOfThisDay >>= rkc' where
  rkc' today = scope "rkc" $ scope pname $ scope cname $ do
    dicts <- scope "dictionaries" . liftIO . loadDictionaries $ "resources/site-config/dictionaries"
    c <- rkcCase constraints (serviceNames dicts)
    f <- rkcFront constraints usrs'
    b <- rkcBack constraints (actionNames dicts)
    return $ Information c f b
    where
      constraints = mconcat [ifNotNull program programIs, ifNotNull city inCity]
      pname = if T.null program then "all" else program
      cname = if T.null city then "all" else city
      ifNotNull value f = if T.null value then mempty else f value
  usrs' = sort $ nub $ map toUsr usrs
  toUsr m = (maybe "" T.decodeUtf8 $ M.lookup "value" m, maybe "" T.decodeUtf8 $ M.lookup "label" m, maybe "" T.decodeUtf8 $ M.lookup "roles" m)

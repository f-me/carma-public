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
import Data.List (intersect)
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
withinToday tbl col = preQuery_ [] [tbl] [equalsNow] [] [] where
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
--averageTowageTechStart = mconcat [
--  preQuery_ ["extract(epoch from avg(servicetbl.times_factServiceStart - casetbl.callDate))::int8"] [] [],
--  notNull "servicetbl" "times_factServiceStart",
--  notNull "casetbl" "callDate",
--  serviceCaseRel,
--  towageTech,
--  cond ["servicetbl"] "(servicetbl.suburbanMilage = 0) or (servicetbl.suburbanMilage is null)",
--  cond ["servicetbl", "casetbl"] "servicetbl.times_factServiceStart > casetbl.callDate"]

averageTowageTechEnd :: PreQuery
averageTowageTechEnd = mconcat [
  averageTime ("servicetbl", "times_factServiceEnd") ("servicetbl", "times_factServiceStart"),
  towageTech,
  cond ["servicetbl"] "(servicetbl.suburbanMilage = 0) or (servicetbl.suburbanMilage is null)"]
--averageTowageTechEnd = mconcat [
--  preQuery_ ["extract(epoch from avg(servicetbl.times_factServiceEnd - servicetbl.times_factServiceStart))::int8"] [] [],
--  notNull "servicetbl" "times_factServiceEnd",
--  notNull "servicetbl" "times_factServiceStart",
--  towageTech,
--  cond ["servicetbl"] "(servicetbl.suburbanMilage = 0) or (servicetbl.suburbanMilage is null)",
--  cond ["servicetbl"] "servicetbl.times_factServiceEnd > servicetbl.times_factServiceStart"]

satisfaction :: PreQuery
satisfaction = mconcat [
  count,
  equals "servicetbl" "clientSatisfied" "satis"]

satisfactionCount :: PreQuery
satisfactionCount = mconcat [
  count,
  inList "servicetbl" "clientSatisfied" ["satis", "notSatis"]]

serviceIs :: T.Text -> PreQuery
serviceIs = equals "servicetbl" "type"

programIs :: T.Text -> PreQuery
programIs p = mconcat [equals "casetbl" "program" p, serviceCaseRel]

cost :: T.Text -> PreQuery
cost col = mconcat [
  sumOf "servicetbl" col,
  notNull "servicetbl" col]

calculatedCost :: PreQuery
calculatedCost = cost "payment_calculatedCost"

limitedCost :: PreQuery
limitedCost = cost "payment_limitedCost"

inCity :: T.Text -> PreQuery
inCity = equals "casetbl" "garbage -> 'city'"

withProgram :: T.Text -> PreQuery
withProgram = equals "casetbl" "program"

select :: T.Text -> T.Text -> PreQuery
select tbl col = preQuery_ [T.concat [tbl, ".", col]] [tbl] [] [] []

selectExp :: [T.Text] -> T.Text -> PreQuery
selectExp tbls ex = preQuery_ [ex] tbls [] [] []

--
-- Queries for front block
--
--  "select assignedTo, extract(epoch from avg(closetime - opentime)) from actiontbl where (closetime is not null) and (opentime is not null) and (closetime > opentime) and (date_trunc('day', opentime + '4 hours') = date_trunc('day', now())) group by assignedto;"
frontOps :: PreQuery
frontOps = mconcat [
  select "actiontbl" "assignedTo",
  averageActionTime,
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
--averageActionTime = mconcat [
--  selectExp ["actiontbl"] "extract(epoch from avg(actiontbl.closeTime - actiontbl.openTime))::int8",
--  notNull "actiontbl" "openTime",
--  notNull "actiontbl" "closeTime",
--  cond ["actiontbl"] "actiontbl.closeTime > actiontbl.openTime"]

actionIs :: T.Text -> PreQuery
actionIs = equals "actiontbl" "name"

actionServiceRel :: PreQuery
actionServiceRel = cond ["servicetbl", "actiontbl"] "servicetbl.type || ':' || servicetbl.id = actiontbl.parentid"

actionCaseRel :: PreQuery
actionCaseRel = cond ["casetbl", "actiontbl"] "'case:' || casetbl.id = actiontbl.caseId"

-- | Add relations on tables
relations :: PreQuery -> PreQuery
relations q = q `mappend` (mconcat $ map snd $ filter (hasTables . fst) tableRelations) where
  qtables = preTables q
  hasTables ts = qtables `intersect` ts == ts
  tableRelations = [
    (["casetbl", "actiontbl"], actionCaseRel),
    (["casetbl", "servicetbl"], serviceCaseRel),
    (["servicetbl", "actiontbl"], actionServiceRel)]

data AnyValue = AnyValue { toAnyValue :: ByteString }
    deriving (Eq, Ord, Read, Show)

instance PS.FromField AnyValue where
    fromField _ Nothing = return $ AnyValue C8.empty
    fromField _ (Just s) = return $ AnyValue s

oneQuery :: (PS.HasPostgres m, MonadLog m) => [PreQuery] -> m T.Text
oneQuery qs = do
  rs <- runQuery qs
  case rs of
    [] -> error "oneQuery returns no rows"
    ((PS.Only r):_) -> do
      log Debug $ T.concat ["oneQuery result: ", T.decodeUtf8 (toAnyValue r)]
      return $ T.decodeUtf8 (toAnyValue r)
    _ -> error "oneQuery returns invalid result"

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
  frontOperatorAvgTime :: Integer }
    deriving (Eq, Ord, Read, Show)

instance FromJSON FrontOperatorInfo where
  parseJSON (Object v) = FrontOperatorInfo <$>
    (v .: "name") <*>
    (v .: "avg")
  parseJSON _ = empty

instance ToJSON FrontOperatorInfo where
  toJSON (FrontOperatorInfo n a) = object [
    "name" .= n,
    "avg" .= a]

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

tryQ :: MonadLog m => T.Text -> a -> T.Text -> m a -> m a
tryQ sc v s act = scope sc $ catch (log Trace s >> act) (onError v) where
  onError :: MonadLog m => a -> E.SomeException -> m a
  onError x _ = return x

caseSummary :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m CaseSummary
caseSummary today = scope "caseSummary" $ do
  log Trace "Loading summary"
  return CaseSummary `ap`
    tryQ "total" 0 "Total services today" (intQuery [count, today]) `ap`
    tryQ "mechanics" 0 "Total mechanics today" (intQuery [count, mechanic, today]) `ap`
    tryQ "delay" 0 "Average time of start service" (intQuery [averageTowageTechStart, today]) `ap`
    tryQ "duration" 0 "Average time of end service" (intQuery [averageTowageTechEnd, today]) `ap`
    tryQ "calculated" 0 "Total calculated cost" (intQuery [calculatedCost, today]) `ap`
    tryQ "limited" 0 "Total limited cost" (intQuery [limitedCost, today]) `ap`
    tryQ "satisfied" 0 "Clients satisfied" (liftM2 percentage (intQuery [satisfaction, today]) (intQuery [satisfactionCount, today]))
  where
    percentage _ 0 = 100
    percentage n d = n * 100 `div` d

caseService :: (PS.HasPostgres m, MonadLog m) => PreQuery -> T.Text -> m CaseServiceInfo
caseService today name = scope "caseService" $ scope name $ do
  log Trace $ T.concat ["Loading info for service ", name]
  return (CaseServiceInfo name) `ap`
    tryQ "total" 0 (T.concat ["Total ", name, "'s today"]) (intQuery [count, serviceIs name, today]) `ap`
    tryQ "delay" 0 (T.concat ["Average time of start for ", name]) (intQuery [averageTowageTechStart, serviceIs name, today]) `ap`
    tryQ "duration" 0 (T.concat ["Average time of end for ", name]) (intQuery [averageTowageTechEnd, serviceIs name, today]) `ap`
    tryQ "calculated" 0 (T.concat ["Calculated cost for ", name]) (intQuery [calculatedCost, serviceIs name, today]) `ap`
    tryQ "limited" 0 (T.concat ["Limited cost for ", name]) (intQuery [limitedCost, today])

rkcCase :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> T.Text -> T.Text -> m CaseInformation
rkcCase today services p c = scope "rkcCase" $ scope pname $ scope cname $ do
  s <- caseSummary $ mconcat [today, pprog, ccity]
  ss <- mapM (caseService $ mconcat [today, pprog, ccity]) services
  return $ CaseInformation s ss
  where
    pname = if T.null p then "all" else p
    pprog = if T.null p then mempty else mconcat [programIs p, serviceCaseRel]
    cname = if T.null c then "all" else c
    ccity = if T.null c then mempty else mconcat [inCity c, serviceCaseRel]

rkcFront :: (PS.HasPostgres m, MonadLog m) => T.Text -> T.Text -> m FrontInformation
rkcFront progname city = scope "rkcFront" $ scope pname $ scope cname $ do
  log Trace "Loading front info"
  vals <- runQuery_ $ mconcat [
    frontOps,
    ifNotNull progname programIs,
    ifNotNull city inCity]
  return $ FrontInformation $ map (\(name, avg) -> FrontOperatorInfo name avg) vals
  where
    pname = if T.null progname then "all" else progname
    cname = if T.null city then "all" else city
    ifNotNull value f = if T.null value then mempty else f value

backSummary :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m BackSummary
backSummary today = scope "backSummary" $ do
  log Trace "Loading summary"
  return BackSummary `ap`
    tryQ "total" 0 "Total actions today" (intQuery [count, today]) `ap`
    tryQ "undone" 0 "Total undone actions today" (intQuery [count, undoneAction, today])

backAction :: (PS.HasPostgres m, MonadLog m) => PreQuery -> T.Text -> m BackActionInfo
backAction today name = scope "backAction" $ scope name $ do
  return (BackActionInfo name) `ap`
    tryQ "total" 0 (T.concat ["Total ", name, "'s today"]) (intQuery [count, actionIs name, today]) `ap`
    tryQ "undone" 0 (T.concat ["Total incomplete ", name, "'s today"]) (intQuery [count, actionIs name, undoneAction, today]) `ap`
    tryQ "average" 0 (T.concat ["Average time for ", name, " today"]) (intQuery [count, actionIs name, averageActionTime, today])

rkcBack :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> T.Text -> T.Text -> m BackInformation
rkcBack today actions p c = scope "rkcBack" $ scope pname $ scope cname $ do
  s <- backSummary $ mconcat [today, pprog, ccity]
  as <- mapM (backAction $ mconcat [today, pprog, ccity]) actions
  return $ BackInformation s as
  where
    pname = if T.null p then "all" else p
    pprog = if T.null p then mempty else mconcat [programIs p, actionCaseRel, serviceCaseRel]
    cname = if T.null c then "all" else c
    ccity = if T.null p then mempty else mconcat [inCity c, actionCaseRel, serviceCaseRel]

dictKeys :: T.Text -> Dictionary -> [T.Text]
dictKeys d = fromMaybe [] . keys [d]

serviceNames :: Dictionary -> [T.Text]
serviceNames = dictKeys "Services"

programNames :: Dictionary -> [T.Text]
programNames = dictKeys "Programs"

actionNames :: Dictionary -> [T.Text]
actionNames = dictKeys "ActionNames"

rkc :: (PS.HasPostgres m, MonadLog m) => T.Text -> T.Text -> m Information
rkc program city = liftIO startOfThisDay >>= rkc' where
  rkc' today = scope "rkc" $ do
    dicts <- scope "dictionaries" . liftIO . loadDictionaries $ "resources/site-config/dictionaries"
    c <- rkcCase serviceToday (serviceNames dicts) program city
    f <- rkcFront program city
    b <- rkcBack actionToday (actionNames dicts) program city
    return $ Information c f b
    where
      serviceToday = withinDay "servicetbl" "createTime" today
      caseToday = withinDay "casetbl" "callDate" today
      actionToday = withinDay "actiontbl" "ctime" today

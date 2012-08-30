{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.RKC (
  Summary(..), ServiceInfo(..), ProgramInformation(..),
  rkc,
  test
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
import Data.Time
import Data.String
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Database.PostgreSQL.Simple.FromField as PS
import qualified Database.PostgreSQL.Simple.ToField as PS
import qualified Database.PostgreSQL.Simple.ToRow as PS

import Snaplet.DbLayer.Dictionary
import Snaplet.DbLayer.ARC

import System.Locale
import Snap.Snaplet.SimpleLog

-- Column of table is in list
inList :: T.Text -> T.Text -> [T.Text] -> PreQuery
inList tbl col vals = preQuery [] [tbl] [T.concat [tbl, ".", col, " in ?"]] [PS.In vals]

equals :: T.Text -> T.Text -> T.Text -> PreQuery
equals tbl col val = preQuery [] [tbl] [T.concat [tbl, ".", col, " = ?"]] [val]

-- | Done services
doneServices :: PreQuery
doneServices = inList "servicetbl" "status" [
  "serviceOrdered",
  "serviceDelayed",
  "serviceInProgress",
  "serviceOk",
  "serviceClosed"]

count :: PreQuery
count = preQuery_ ["count(*)"] [] []

sumOf :: T.Text -> T.Text -> PreQuery
sumOf tbl col = preQuery_ [T.concat ["sum(", tbl, ".", col, ")"]] [tbl] []

notNull :: T.Text -> T.Text -> PreQuery
notNull tbl col = preQuery_ [] [tbl] [T.concat [tbl, ".", col, " is not null"]]

cond :: [T.Text] -> T.Text -> PreQuery
cond tbls c = preQuery_ [] tbls [c]

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
  preQuery_ ["extract(epoch from avg(servicetbl.times_factServiceStart - casetbl.callDate))::int8"] [] [],
  notNull "servicetbl" "times_factServiceStart",
  notNull "casetbl" "callDate",
  serviceCaseRel,
  towageTech,
  cond ["servicetbl", "casetbl"] "servicetbl.times_factServiceStart > casetbl.callDate"]

averageTowageTechEnd :: PreQuery
averageTowageTechEnd = mconcat [
  preQuery_ ["extract(epoch from avg(servicetbl.times_factServiceEnd - servicetbl.times_factServiceStart))::int8"] [] [],
  notNull "servicetbl" "times_factServiceEnd",
  notNull "servicetbl" "times_factServiceStart",
  towageTech,
  cond ["servicetbl"] "servicetbl.times_factServiceEnd > servicetbl.times_factServiceStart"]

satisfaction :: PreQuery
satisfaction = mconcat [
  count,
  equals "servicetbl" "clientSatisfied" "1"]

satisfactionCount :: PreQuery
satisfactionCount = mconcat [
  count,
  preQuery_ [] ["servicetbl"] []]

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

withinDay :: T.Text -> T.Text -> UTCTime -> PreQuery
withinDay tbl col tm = preQuery_ [] [tbl] [afterStart, beforeEnd] where
  st = fromString $ formatTime defaultTimeLocale "%F %T" tm
  afterStart = T.concat [tbl, ".", col, " - '", st, "' >= '0 days'"]
  beforeEnd = T.concat [tbl, ".", col, " - '", st, "' < '1 day'"]

-- Get start of this day for timezone
startOfDay :: TimeZone -> UTCTime -> UTCTime
startOfDay tz = localTimeToUTC tz . dropTime . utcToLocalTime tz where
  dropTime t = t { localTimeOfDay = midnight }

-- Get start of current day for current timezone
startOfThisDay :: IO UTCTime
startOfThisDay = startOfDay <$> getCurrentTimeZone <*> getCurrentTime

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

data Summary = Summary {
  summaryTotalServices :: Integer,
  summaryTotalMechanics :: Integer,
  summaryAverageStart :: Integer,
  summaryAverateEnd :: Integer,
  summaryCalculatedCost :: Integer,
  summaryLimitedCost :: Integer,
  summarySatisfied :: Integer }
    deriving (Eq, Ord, Read, Show)

data ServiceInfo = ServiceInfo {
  serviceName :: T.Text,
  serviceTotal :: Integer,
  serviceAverageStart :: Integer,
  serviceAverageEnd :: Integer,
  serviceCalculatedCost :: Integer,
  serviceLimitedCost :: Integer }
    deriving (Eq, Ord, Read, Show)

data ProgramInformation = ProgramInformation {
  programInfoSummary :: Summary,
  programInfoServices :: [ServiceInfo] }
    deriving (Eq, Ord, Read, Show)

instance FromJSON Summary where
  parseJSON (Object v) = Summary <$>
    (v .: "total") <*>
    (v .: "mech") <*>
    (v .: "delay") <*>
    (v .: "duration") <*>
    (v .: "calculated") <*>
    (v .: "limited") <*>
    (v .: "satisfied")
  parseJSON _ = empty

instance ToJSON Summary where
  toJSON (Summary t m d dur calc lim sat) = object [
    "total" .= t,
    "mech" .= m,
    "delay" .= d,
    "duration" .= dur,
    "calculated" .= calc,
    "limited" .= lim,
    "satisfied" .= sat]

instance FromJSON ServiceInfo where
  parseJSON (Object v) = ServiceInfo <$>
    (v .: "name") <*>
    (v .: "total") <*>
    (v .: "delay") <*>
    (v .: "duration") <*>
    (v .: "calculated") <*>
    (v .: "limited")
  parseJSON _ = empty

instance ToJSON ServiceInfo where
  toJSON (ServiceInfo n t d dur calc lim) = object [
    "name" .= n,
    "total" .= t,
    "delay" .= d,
    "duration" .= dur,
    "calculated" .= calc,
    "limited" .= lim]

instance FromJSON ProgramInformation where
  parseJSON (Object v) = ProgramInformation <$>
    (v .: "summary") <*>
    (v .: "services")
  parseJSON _ = empty


instance ToJSON ProgramInformation where
  toJSON (ProgramInformation s ss) = object [
    "summary" .= s,
    "services" .= ss]

tryQ :: MonadLog m => T.Text -> a -> T.Text -> m a -> m a
tryQ sc v s act = scope sc $ catch (log Trace s >> act) (onError v) where
  onError :: MonadLog m => a -> E.SomeException -> m a
  onError x _ = return x

summary :: (PS.HasPostgres m, MonadLog m) => PreQuery -> m Summary
summary today = scope "summary" $ do
  log Trace "Loading summary"
  return Summary `ap`
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

service :: (PS.HasPostgres m, MonadLog m) => PreQuery -> T.Text -> m ServiceInfo
service today name = scope name $ do
  log Trace $ T.concat ["Loading info for service ", name]
  return (ServiceInfo name) `ap`
    tryQ "total" 0 (T.concat ["Total ", name, "'s today"]) (intQuery [count, serviceIs name, today]) `ap`
    tryQ "delay" 0 (T.concat ["Average time of start for ", name]) (intQuery [averageTowageTechStart, serviceIs name, today]) `ap`
    tryQ "duration" 0 (T.concat ["Average time of end for ", name]) (intQuery [averageTowageTechEnd, serviceIs name, today]) `ap`
    tryQ "calculated" 0 (T.concat ["Calculated cost for ", name]) (intQuery [calculatedCost, serviceIs name, today]) `ap`
    tryQ "limited" 0 (T.concat ["Limited cost for ", name]) (intQuery [limitedCost, today])

serviceNames :: Dictionary -> [T.Text]
serviceNames = fromMaybe [] . keys ["Services"]

programNames :: Dictionary -> [T.Text]
programNames = fromMaybe [] . keys ["Programs"]

rkcProgram :: (PS.HasPostgres m, MonadLog m) => PreQuery -> [T.Text] -> T.Text -> m ProgramInformation
rkcProgram today services p = scope pname $ do
  s <- summary ptoday
  ss <- scope "services" $ mapM (service ptoday) services
  return $ ProgramInformation s ss
  where
    pname = if T.null p then "all" else p
    ptoday = (if T.null p then id else mappend (programIs p)) today

rkc :: (PS.HasPostgres m, MonadLog m) => T.Text -> m ProgramInformation
rkc program = liftIO startOfThisDay >>= rkc' where
  rkc' today = scope "rkc" $ do
    dicts <- scope "dictionaries" . liftIO . loadDictionaries $ "resources/site-config/dictionaries"
    rkcProgram serviceToday (serviceNames dicts) program
    where
      serviceToday = withinDay "servicetbl" "createTime" today
      caseToday = withinDay "casetbl" "callDate" today

-- Test function for log
test :: (PS.HasPostgres m, MonadLog m) => m ()
test = liftIO startOfThisDay >>= test' where
  test' today = scope "RKC" $ do
    log Info "Testing RKC"
    let
      serviceToday = withinDay "servicetbl" "createTime" today
      caseToday = withinDay "casetbl" "callDate" today
    log Trace "Count of services today"
    intQuery [count, serviceToday]
    log Trace "Count of done services today"
    intQuery [count, serviceToday, doneServices]
    log Trace "Count of mechanics today"
    intQuery [count, serviceToday, mechanic]
    log Trace "Average time of start service"
    ((PS.Only r):_) <- runQuery [averageTowageTechStart]
    log Trace $ T.concat ["Result: ", T.decodeUtf8 (toAnyValue r)]
    log Trace "Average time of end service"
    ((PS.Only r):_) <- runQuery [averageTowageTechEnd]
    log Trace $ T.concat ["Result: ", T.decodeUtf8 (toAnyValue r)]
    --log Trace "Sum of all calculated costs for towage"
    --intQuery [calculatedCost "towage"]
    --log Trace "Sum of all limited costs for tech"
    --intQuery [limitedCost "tech"]
    log Trace "Client satisfied for all time"
    sat <- intQuery [satisfaction]
    log Trace "All satisfactions"
    satCount <- intQuery [satisfactionCount]
    log Trace $ T.concat ["Precentage satisfaction: ", T.pack $ show (sat * 100 `div` satCount)]


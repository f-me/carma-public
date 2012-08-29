{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.RKC (
	test
	) where

import Prelude hiding (log, catch)

import Control.Applicative
import Control.Monad.IO.Class

import Data.Monoid
import Data.Time
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified Database.PostgreSQL.Simple.FromField as PS
import qualified Database.PostgreSQL.Simple.ToField as PS
import qualified Database.PostgreSQL.Simple.ToRow as PS

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

mechanic :: PreQuery
mechanic = equals "calltbl" "garbage -> 'callType'" "client" `mappend` inList "calltbl" "garbage -> 'callerType'" [
	"mechanicConsOk",
	"mechanicConsNotOk"]

towageTech :: PreQuery
towageTech = inList "servicetbl" "type" ["towage", "tech"]

averageTowageTechStart :: PreQuery
averageTowageTechStart = mconcat [
	preQuery_ ["avg(servicetbl.times_factServiceStart - casetbl.callDate)"] [] [],
	notNull "servicetbl" "times_factServiceStart",
	notNull "casetbl" "callDate",
	towageTech,
	cond ["servicetbl", "casetbl"] "servicetbl.times_factServiceStart > casetbl.callDate"]

averageTowageTechEnd :: PreQuery
averageTowageTechEnd = mconcat [
	preQuery_ ["avg(servicetbl.times_factServiceEnd - servicetbl.times_factServiceStart)"] [] [],
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
	notNull "servicetbl" "clientSatisfied"]

cost :: T.Text -> T.Text -> PreQuery
cost col service = mconcat [
	sumOf "servicetbl" col,
	notNull "servicetbl" col,
	equals "servicetbl" "type" service]

calculatedCost :: T.Text -> PreQuery
calculatedCost = cost "payment_calculatedCost"

limitedCost :: T.Text -> PreQuery
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


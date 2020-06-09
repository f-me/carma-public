{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

{-|

Rukovoditel Koll Centra screen.

-}

module AppHandlers.RKC
    (
      rkcHandler
    , rkcWeatherHandler
    , rkcPartners
    )

where

import           BasicPrelude                       hiding ( intercalate
                                                           , groupBy
                                                           )
import qualified BasicPrelude                       as L ( intercalate
                                                         , groupBy
                                                         )

import           Control.Monad.State.Class
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.Aeson
import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString.Lazy               as LB
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time
import           Data.Semigroup

import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.ToField   as PS
import qualified Database.PostgreSQL.Simple.FromField as PS
import qualified Snap.Snaplet.PostgresqlSimple        as PS
import           Snap

import           WeatherApi                         (getWeather', tempC)

import           Data.Model
import qualified Data.Model.Sql                     as Sql
import           Data.Model.Utils.LegacyModel

import qualified Carma.Model.ActionResult           as AResult
import qualified Carma.Model.ActionType             as AType
import           Carma.Model.City                   (City)
import qualified Carma.Model.ConsultationType       as CT
import           Carma.Model.Program                (Program)
import qualified Carma.Model.Satisfaction           as Satis
import qualified Carma.Model.ServiceStatus          as SS
import qualified Carma.Model.ServiceType            as ST

import           Application
import           Util

-- rkc helpers
getFromTo :: AppHandler (Maybe Day, Maybe Day)
getFromTo = do
  fromDate <- getParam "from"
  toDate <- getParam "to"
  let
    parseDate :: ByteString -> Maybe Day
    parseDate = parseTimeM True defaultTimeLocale "%d.%m.%Y" . bToString

    fromDate' = parseDate =<< fromDate
    toDate' = parseDate =<< toDate

  return (fromDate', toDate')

getParamOrEmpty :: ByteString -> AppHandler T.Text
getParamOrEmpty = liftM (maybe T.empty T.decodeUtf8) . getParam

mkRkcFilter :: AppHandler Filter
mkRkcFilter = do
  p <- getParamOrEmpty "program"
  c <- getParamOrEmpty "city"
  part <- getParamOrEmpty "partner"
  (from, to) <- getFromTo

  flt <- liftIO todayFilter
  return $
    flt {
      filterFrom = fromMaybe (filterFrom flt) from,
      filterTo = fromMaybe (filterTo flt) to,
      filterProgram = p,
      filterCity = c,
      filterPartner = part }

rkcHandler :: AppHandler ()
rkcHandler = logExceptions "handler/rkc" $ do
  flt' <- mkRkcFilter
  info <- rkc flt'
  writeJSON info

rkcWeatherHandler :: AppHandler ()
rkcWeatherHandler = logExceptions "handler/rkc/weather" $ do
  let defaults = ["Moskva", "Sankt-Peterburg"]
  cities <- (fromMaybe defaults . (>>= (Aeson.decode . LB.fromStrict)))
    <$> getParam "cities"

  syslogJSON Info "handler/rkc/weather" ["cities" .= L.intercalate ", " cities]

  conf <- gets weatherCfg
  let weatherForCity = liftIO . getWeather' conf . filter (/= '\'')
  let toTemp t city = Aeson.object [
        "city" .= city,
        "temp" .= either (const "-") (show.tempC) t]

  temps <- mapM weatherForCity cities
  writeJSON $ Aeson.object [
    "weather" .= zipWith toTemp temps cities]


rkcPartners :: AppHandler ()
rkcPartners = logExceptions "handler/rkc/partners" $ do
  flt <- liftIO todayFilter
  (from, to) <- getFromTo

  let
    flt' = flt {
      filterFrom = fromMaybe (filterFrom flt) from,
      filterTo = fromMaybe (filterTo flt) to }

  res <- partners (filterFrom flt') (filterTo flt')
  writeJSON res

trace :: (Show a, MonadIO m) => T.Text -> m a -> m a
trace name fn = do
  val <- fn
  syslogJSON Info "rkc/trace" [name .= show val]
  return val

query :: (PS.HasPostgres m, MonadBaseControl IO m, PS.ToRow q, PS.FromRow r) => PS.Query -> q -> m [r]
query s v = do
    bs <- PS.formatQuery s v
    syslogJSON Debug "RKC/query" ["query" .= T.decodeUtf8 bs]
    PS.query s v

-- pre-query, holds fields, table names and conditions in separate list to edit
data PreQuery = PreQuery {
    _preFields     :: [T.Text],
    preTables      :: [T.Text],
    _preConditions :: [T.Text],
    _preGroups     :: [T.Text],
    _preOrders     :: [T.Text],
    _preArgs       :: [PS.Action] }

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

runQuery :: (PS.HasPostgres m, MonadBaseControl IO m, PS.FromRow r) => [PreQuery] -> m [r]
runQuery qs = query (fromString compiled) a where
    (compiled, a) = strQuery (mconcat qs)

runQuery_ :: (PS.HasPostgres m, MonadBaseControl IO m, PS.FromRow r) => PreQuery -> m [r]
runQuery_ pq = runQuery [relations pq]

-- Column of table is in list
inList :: T.Text -> T.Text -> [T.Text] -> PreQuery
inList tbl col vals = preQuery [] [tbl] [T.concat [tbl, ".", col, " in ?"]] [] [] [PS.In vals]

equals :: T.Text -> T.Text -> T.Text -> PreQuery
equals tbl col val = preQuery [] [tbl] [T.concat [tbl, ".", col, " = ?"]] [] [] [val]

equalsTo :: T.Text -> T.Text -> T.Text -> PreQuery
equalsTo tbl expr val = preQuery [] [tbl] [T.concat [expr, " = ?"]] [] [] [val]

betweenTime :: Day -> Day -> T.Text -> T.Text -> PreQuery
betweenTime from to tbl col = mconcat [
  notNull tbl col,
  preQuery [] [tbl] [T.concat [tbl, ".", col, " >= ?"]] [] [] [from],
  preQuery [] [tbl] [T.concat [tbl, ".", col, " < ?"]] [] [] [to]]

count :: PreQuery
count = preQuery_ ["count(*)"] [] [] [] []

count' :: T.Text -> PreQuery
count' name = preQuery_ [T.concat ["count(*) ", name]] [] [] [] []

sumOf :: T.Text -> T.Text -> PreQuery
sumOf tbl col = preQuery_ [T.concat ["sum(", tbl, ".", col, ")"]] [tbl] [] [] []

notNull :: T.Text -> T.Text -> PreQuery
notNull tbl col = preQuery_ [] [tbl] [T.concat [tbl, ".", col, " is not null"]] [] []

isNull :: T.Text -> T.Text -> PreQuery
isNull tbl col = preQuery_ [] [tbl] [T.concat [tbl, ".", col, " is null"]] [] []

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
doneServices =
  inList "servicetbl" "status" $
    map identToRawFieldValue [SS.inProgress, SS.ok, SS.closed]

serviceCaseRel :: PreQuery
serviceCaseRel =
  cond ["casetbl", "servicetbl"] "casetbl.id = servicetbl.parentId"

consultationCaseRel :: PreQuery
consultationCaseRel =
  cond ["casetbl", "consultationtbl"] "casetbl.id = consultationtbl.parentId"

towageTech :: PreQuery
towageTech =
  inList "servicetbl" "type" $ map identToRawFieldValue [ST.tech, ST.towage]

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
  equals "servicetbl" "clientSatisfied" (identToRawFieldValue Satis.ok)]

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
undoneAction = isNull "actiontbl" "result"

averageTime :: (T.Text, T.Text) -> (T.Text, T.Text) -> PreQuery
averageTime (tbl1, col1) (tbl2, col2) = mconcat [
  selectExp [tbl1, tbl2] (T.concat ["extract(epoch from avg(", tbl1, ".", col1, " - ", tbl2, ".", col2, "))::int8"]),
  notNull tbl1 col1,
  notNull tbl2 col2,
  cond [tbl1, tbl2] (T.concat [tbl1, ".", col1, " > ", tbl2, ".", col2])]

averageActionTime :: PreQuery
averageActionTime = averageTime ("actiontbl", "closeTime") ("actiontbl", "openTime")

actionServiceRel :: PreQuery
actionServiceRel = cond ["servicetbl", "actiontbl"] "servicetbl.id = actiontbl.serviceid"

actionCaseRel :: PreQuery
actionCaseRel = cond ["casetbl", "actiontbl"] "casetbl.id = actiontbl.caseId"

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

mintQuery
  :: (ToJSON num, PS.FromField num, PS.HasPostgres m, MonadBaseControl IO m)
  => PreQuery -> m (Maybe num)
mintQuery qs = do
    rs <- runQuery [relations qs]
    case rs of
        [] -> error "Int query returns no rows"
        (PS.Only r:_) -> return r

caseSummary :: (PS.HasPostgres m, MonadBaseControl IO m) => Filter -> PreQuery -> m Value
caseSummary (Filter fromDate toDate program city partner) constraints = logExceptions "rkc/caseSummary" $ do
  syslogTxt Info "rkc/caseSummary" "Loading summary"
  calc <- trace "calculated cost" (run calculatedCost)
  [t, m, d, dur, lim, sat] <- sequence [
    trace "total" (run count),
    trace "mechanics" mech,
    trace "average tech start" (run averageTowageTechStart),
    trace "average tech end" (run averageTowageTechEnd),
    trace "limited cost" (run limitedCost),
    liftM2 percentage (run satisfaction) (run satisfactionCount)]
  return $ object [
    "total" .= int t,
    "mech" .= m,
    "delay" .= d,
    "duration" .= dur,
    "calculated" .= float calc,
    "limited" .= lim,
    "satisfied" .= sat]
  where
    percentage _ 0 = 100
    percentage n d = n * 100 `div` d
    run p = liftM (fromMaybe 0) $ mintQuery $ mconcat [p, constraints, betweenTime fromDate toDate "servicetbl" "createTime"]

    (mechanicL, mechanicLActions) = strQuery $ mconcat [
      count' "cnt",
      equals "calltbl" "callerType::text" "1",
      -- FIXME: callType have new values
      -- inList "calltbl" "callType" ["mechanicConsOk", "mechanicConsNotOk"],
      betweenTime fromDate toDate "calltbl" "callDate",
      ifNotNull program $ equals "calltbl" "program"
      -- ifNotNull city $ equals "calltbl" "city"
      ]
      -- TODO: partner?
    (mechanicR, mechanicRActions) = strQuery $ mconcat
      [ count' "cnt"
      , equals "consultationtbl" "constype" (identToRawFieldValue CT.mech)
      , inList "consultationtbl" "status" $
               map identToRawFieldValue [ SS.inProgress
                                        , SS.ok
                                        , SS.closed
                                        , SS.ordered
                                        , SS.delayed
                                        ]
      , betweenTime fromDate toDate "consultationtbl" "createTime"
      , consultationCaseRel
      , ifNotNull program $ equals "casetbl" "program"
      , ifNotNull city $ equals "casetbl" "city"
      , ifNotNull partner $
                  equalsTo "consultationtbl"
                           "trim(consultationtbl.contractor_partner)"
      ]

    mech = liftM oneInt $ query
      (fromString $ "select sum(cnt)::integer from (" ++ mechanicL ++ " union " ++ mechanicR ++ ") as foo")
      (mechanicLActions ++ mechanicRActions)
      where
        oneInt :: [PS.Only Integer] -> Integer
        oneInt = maybe 0 PS.fromOnly . listToMaybe

caseServices :: (PS.HasPostgres m, MonadBaseControl IO m) =>
                Day
             -> Day
             -> PreQuery
             -> [IdentI ST.ServiceType]
             -> m Value
caseServices fromDate toDate constraints names = logExceptions "rkc/caseServices" $ do
  calcs <- todayAndGroup calculatedCost
  [totals, startAvgs, endAvgs, lims] <- mapM todayAndGroup [count, averageStart, averageEnd, limitedCost]
  let
    makeServiceInfo n@(Ident m) = object [
      "name" .= m,
      "total" .= int (lookAt totals),
      "delay" .= lookAt startAvgs,
      "duration" .= lookAt endAvgs,
      "calculated" .= float (lookAt calcs),
      "limited" .= lookAt lims]
      where
        lookAt :: Num n => [(IdentI ST.ServiceType, n)] -> n
        lookAt = fromMaybe 0 . lookup n
  return $ toJSON $ map makeServiceInfo names
  where
    todayAndGroup p = trace "result" $ runQuery_ $ mconcat [select "servicetbl" "type", p, constraints, betweenTime fromDate toDate "servicetbl" "createTime", groupBy "servicetbl" "type"]

rkcCase :: (PS.HasPostgres m, MonadBaseControl IO m) =>
           Filter
        -> PreQuery
        -> [IdentI ST.ServiceType]
        -> m Value
rkcCase filt@(Filter fromDate toDate _ _ _) constraints services = logExceptions "rkc/rkcCase" $ do
  s <- caseSummary filt (mconcat [doneServices, constraints])
  ss <- caseServices fromDate toDate (mconcat [constraints, doneServices]) services
  return $ object [
    "summary" .= s,
    "services" .= ss]

actionsSummary :: (PS.HasPostgres m, MonadBaseControl IO m) => Day -> Day -> PreQuery -> m Value
actionsSummary fromDate toDate constraints = logExceptions "rkc/actionsSummary" $ do
  syslogTxt Info "rkc/actionsSummary" "Loading summary"
  t <- trace "total" (run count)
  u <- trace "undone" (run (mconcat [count, undoneAction]))
  return $ object [
    "total" .= int t,
    "undone" .= int u]
  where
    run p = liftM (fromMaybe 0) $ mintQuery $ mconcat [p, constraints, betweenTime fromDate toDate "actiontbl" "duetime"]

actionsActions :: (PS.HasPostgres m, MonadBaseControl IO m) => Day -> Day -> PreQuery -> [IdentI AType.ActionType] -> m Value
actionsActions fromDate toDate constraints actions = logExceptions "rkc/actionsActions" $ do
  [totals, undones, avgs] <- mapM todayAndGroup [
    (count, "duetime"),
    (mconcat [count, undoneAction], "duetime"),
    (averageActionTime, "closeTime")]
  let
    makeActionInfo n@(Ident m) = object [
      "name" .= m,
      "total" .= lookAt totals,
      "undone" .= lookAt undones,
      "average" .= lookAt avgs]
      where
        lookAt :: [(IdentI AType.ActionType, Integer)] -> Integer
        lookAt = fromMaybe 0 . lookup n
  return $ toJSON $ map makeActionInfo actions
  where
    todayAndGroup (p, tm) = trace "result" $ runQuery_ $ mconcat [select "actiontbl" "type", p, constraints, betweenTime fromDate toDate "actiontbl" tm, groupBy "actiontbl" "type"]

rkcActions :: (PS.HasPostgres m, MonadBaseControl IO m) =>
              Day
           -> Day
           -> PreQuery
           -> [IdentI AType.ActionType]
           -> m Value
rkcActions fromDate toDate constraints actions = logExceptions "rkc/rkcActions" $ do
  s <- actionsSummary fromDate toDate constraints
  as <- actionsActions fromDate toDate constraints actions
  return $ object [
    "summary" .= s,
    "actions" .= as]

-- | Average time for each operator and action
rkcEachActionOpAvg :: (PS.HasPostgres m, MonadBaseControl IO m) => Day -> Day -> PreQuery -> [IdentI AType.ActionType] -> m Value
rkcEachActionOpAvg fromDate toDate constraints acts = logExceptions "rkc/rkcEachActionOpAvg" $ do
  r <- trace "result" $ runQuery_ $ mconcat [
    constraints,
    select "actiontbl" "assignedTo::text",
    select "actiontbl" "type",
    averageActionTime,
    count,
    notNull "actiontbl" "assignedTo",
    betweenTime fromDate toDate "actiontbl" "duetime",
    groupBy "actiontbl" "assignedTo",
    groupBy "actiontbl" "type",
    orderBy "actiontbl" "assignedTo",
    orderBy "actiontbl" "type"]
  return $ toJSON $ toInfo (groupResult r)
  where
    groupResult :: [(T.Text, IdentI AType.ActionType, Integer, Integer)]
                -> [(T.Text, [(IdentI AType.ActionType, (Integer, Integer))])]
    groupResult = map (first head . unzip) . L.groupBy ((==) `on` fst) . map (\(aTo, nm, avgTm, cnt) -> (aTo, (nm, (avgTm, cnt))))

    toInfo :: [(T.Text, [(IdentI AType.ActionType, (Integer, Integer))])] -> [Value]
    toInfo = map fromStat where
      fromStat (n,st) = object [
        "name" .= n,
        "avg" .= avgSum st,
        "avgs" .= map (`lookup` st) acts]
        where
          avgSum [] = (0, 0)
          avgSum st' = (average *** sum) $ unzip $ map snd st'
          average l = sum l `div` fromIntegral (length l)

rkcComplaints :: (PS.HasPostgres m, MonadBaseControl IO m) => Day -> Day -> PreQuery -> m Value
rkcComplaints fromDate toDate constraints = logExceptions "rkc/rkcComplaints" $ do
  compls <- trace "result" $ runQuery_ $ mconcat [
    constraints,
    select "casetbl" "id",
    notNull "casetbl" "id",
    notNull "servicetbl" "type",
    selectExp ["servicetbl"] "string_agg(servicetbl.type::text, ' ')",
    betweenTime fromDate toDate "servicetbl" "createTime",
    equals "servicetbl" "clientSatisfied" (identToRawFieldValue Satis.none),
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
rkcStats :: (PS.HasPostgres m, MonadBaseControl IO m) => Filter -> m Value
rkcStats (Filter from to program city partner) = logExceptions "rkc/rkcStats" $ do
  let qParams = sqlFlagPair (Ident 0) id program' PS.:.
                sqlFlagPair (Ident 0) id city' PS.:.
                (T.null partner, partner, from, to)
      orders       = PS.In [AType.orderService]
      orderResults = PS.In [ AResult.serviceOrdered
                           , AResult.serviceOrderedSMS
                           ]
      city' :: Maybe (IdentI City)
      city' = rawFieldValueToIdent city
      program' :: Maybe (IdentI Program)
      program' = rawFieldValueToIdent program
  rsp1 <- PS.query procAvgTimeQuery $
          (orders, orderResults) PS.:. qParams
  rsp2 <- PS.query towStartAvgTimeQuery
    $ PS.Only from PS.:. PS.Only from PS.:. PS.Only from PS.:. qParams
  rsp3 <- PS.query assignAvgTimeQuery $ (PS.Only orders) PS.:. qParams
  rsp4 <- PS.query realprocAvgTimeQuery $
          (orders, orderResults) PS.:. qParams
  let procAvgTime :: Maybe Double
      procAvgTime = head $ head rsp1
      assignAvgTime :: Maybe Double
      assignAvgTime        = head $ head rsp3
      realprocAvgTime :: Maybe Double
      realprocAvgTime = head $ head rsp4
      towStartAvgTime :: Maybe Double
      towStartAvgTime = head $ head rsp2
  return $ object [ "procAvgTime" .= procAvgTime
                  , "towStartAvgTime" .= towStartAvgTime
                  , "assignAvgTime"   .= assignAvgTime
                  , "realprocAvgTime" .= realprocAvgTime
                  ]

ifNotNull :: T.Text -> (T.Text -> PreQuery) -> PreQuery
ifNotNull value f = if T.null value then mempty else f value

data Filter = Filter {
  filterFrom    :: Day,
  filterTo      :: Day,
  filterProgram :: T.Text,
  filterCity    :: T.Text,
  filterPartner :: T.Text }
    deriving (Eq, Show)

todayFilter :: IO Filter
todayFilter = do
  now <- getCurrentTime
  let
    startOfToday = utctDay now
    startOfTomorrow = addDays 1 startOfToday

  return Filter {
    filterFrom = startOfToday,
    filterTo = startOfTomorrow,
    filterProgram = "",
    filterCity = "",
    filterPartner = "" }

traceFilter :: MonadIO m => String -> Filter -> m ()
traceFilter tag (Filter from to prog city partner)
  = syslogJSON Info tag
    ["program" .= prog
    ,"city"    .= city
    ,"from"    .= show from
    ,"to"      .= show to
    ,"partner" .= partner
    ]

rkc :: (PS.HasPostgres m, MonadBaseControl IO m) => Filter -> m Value
rkc filt@(Filter fromDate toDate program city partner) = logExceptions "rkc/rkc" $ do
  traceFilter "rkc/rkc" filt
  serviceNames <- PS.liftPG' $ Sql.select ST.ident
  actionNames <- PS.liftPG' $ Sql.select AType.ident
  let unOnly (PS.Only n) = n
  c <- rkcCase filt constraints $ map unOnly serviceNames
  a <- rkcActions fromDate toDate constraints $ map unOnly actionNames
  ea <- rkcEachActionOpAvg fromDate toDate constraints $ map unOnly actionNames
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


-- | All partners on services within time interval
partners :: (PS.HasPostgres m, MonadBaseControl IO m) => Day -> Day -> m Value
partners fromDate toDate = logExceptions "rkc/partners" $ do
  syslogJSON Info "rkc/partners" ["from" .= show fromDate, "to" .= show toDate]

  let
    q = [
      "select trim(contractor_partner) c from servicetbl where",
      " (createTime >= ?) and (createTime < ?) and (createTime is not null) group by c order by c"]

  ps <- trace "result" $ queryFmt q [fromDate, toDate]
  return $ toJSON (mapMaybe PS.fromOnly ps :: [T.Text])

queryFmt :: (PS.HasPostgres m, MonadBaseControl IO m, PS.ToRow q, PS.FromRow r)
         => [String] -> q -> m [r]
queryFmt lns = query $ fromString $ concat lns

-- | Calculate average service processing time (in seconds).
--
-- Parametrized by: orderService types (IN value with ActionType
-- keys), serviceOrdered results (IN value with ActionResult keys),
-- the rest of arguments are the same as for 'towStartAvgTimeQuery'.
--
-- The query uses timespan between the earliest order action and the
-- latest action which resulted in an ordered service.
procAvgTimeQuery :: PS.Query
procAvgTimeQuery = [sql|
WITH actiontimes AS (
 SELECT (max(a2.closeTime - a1.ctime))
 FROM casetbl c, servicetbl s, actiontbl a1, actiontbl a2
 WHERE a1.serviceid=a2.serviceid
 AND (a1.type IN ?)
 AND (a2.result IN ?)
 AND a1.serviceid=s.id
 AND a1.caseid = c.id
 AND s.times_expectedServiceStart <= (a1.ctime + INTERVAL '01:00:00')
 AND (? or c.program = ?)
 AND (? or c.city = ?)
 AND (? or s.contractor_partner = ?)
 AND c.calldate >= ?
 AND c.calldate < ?
 GROUP BY a1.serviceid)
SELECT extract(epoch from avg(max)) FROM actiontimes;
|]

-- | Calculate average service assigning time (in seconds).

-- Almost same as previous
--
-- Parametrized by: orderService types (IN value with ActionType
-- keys) the rest of arguments are the same as for 'towStartAvgTimeQuery'.
assignAvgTimeQuery :: PS.Query
assignAvgTimeQuery = [sql|
WITH actiontimes AS (
 SELECT (max(a.assignTime - a.ctime))
 FROM casetbl c, servicetbl s, actiontbl a
 WHERE (a.type IN ?)
 AND a.serviceid=s.id
 AND a.caseid = c.id
 AND s.times_expectedServiceStart <= (a.ctime + INTERVAL '01:00:00')
 AND (? or c.program = ?)
 AND (? or c.city = ?)
 AND (? or s.contractor_partner = ?)
 AND c.calldate >= ?
 AND c.calldate < ?
 GROUP BY a.serviceid)
SELECT extract(epoch from avg(max)) FROM actiontimes;
|]

-- | Calculate average service processing time (in seconds).

-- Almost same as previous
--
-- Parametrized by: orderService types (IN value with ActionType
-- keys), serviceOrdered results (IN value with ActionResult keys),
-- the rest of arguments are the same as for 'towStartAvgTimeQuery'.
realprocAvgTimeQuery :: PS.Query
realprocAvgTimeQuery = [sql|
WITH actiontimes AS (
 SELECT (max(a2.closeTime - a1.assignTime))
 FROM casetbl c, servicetbl s, actiontbl a1, actiontbl a2
 WHERE a1.serviceid=a2.serviceid
 AND (a1.type IN ?)
 AND (a2.result IN ?)
 AND a1.serviceid=s.id
 AND a1.caseid = c.id
 AND s.times_expectedServiceStart <= (a1.ctime + INTERVAL '01:00:00')
 AND (? or c.program = ?)
 AND (? or c.city = ?)
 AND (? or s.contractor_partner = ?)
 AND c.calldate >= ?
 AND c.calldate < ?
 GROUP BY a1.serviceid)
SELECT extract(epoch from avg(max)) FROM actiontimes;
|]

-- | Calculate average tower arrival time (in seconds).
--
-- Parametrized by: program (boolean flag & value for this parameter,
-- where flag is *false* if filtering by that parameter should be
-- active), city (flag & value), partner name (flag & value), from
-- (value, always active), to (always active).
towStartAvgTimeQuery :: PS.Query
towStartAvgTimeQuery = [sql|
WITH
 services AS (
  SELECT type, id, parentid, times_factServiceStart, times_expectedDispatch,
         times_expectedServiceStart,
         contractor_partner, suburbanmilage
    FROM techtbl
    WHERE createtime >= ?
  UNION ALL
  SELECT type, id, parentid, times_factServiceStart, times_expectedDispatch,
         times_expectedServiceStart,
         contractor_partner, suburbanmilage
    FROM towagetbl
    WHERE createtime >= ?
  UNION ALL
  SELECT type, id, parentid, times_factServiceStart, times_expectedDispatch,
         times_expectedServiceStart,
         contractor_partner, suburbanmilage
    FROM "BikeTowage"
    WHERE createtime >= ?    )
SELECT extract(epoch from avg(s.times_factServiceStart - s.times_expectedDispatch))
FROM casetbl c, services s
WHERE s.parentid = c.id
AND (select min(a1.ctime) from actiontbl a1 where a1.serviceid = s.id and a1.type = 2)
    - s.times_expectedServiceStart <= interval '1 hour'
AND EXISTS
  (select 1 from actiontbl a2
    where a2.serviceid = s.id AND (a2.result = 1 OR a2.result = 2))
AND (s.times_factServiceStart > s.times_expectedDispatch)
AND (? or c.program = ?)
AND (? or c.city = ?)
AND (? or s.contractor_partner = ?)
AND coalesce(s.suburbanmilage, '0') = '0'
AND c.calldate >= ?
AND c.calldate < ?;
|]


-- type coersions
int :: Integer -> Integer
int = id

float :: Double -> Double
float = id

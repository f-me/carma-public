{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module AppHandlers.KPI ( getStat
                       , getStatFiles
                       , getOper
                       , getGroup
                       , updateOperKPI
                       ) where

import           Control.Monad (forM, join)
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS.Strict as RWS

import           Data.String     (fromString)
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Time.Calendar (Day)
import           Data.Time.Clock    (DiffTime)
import           Data.Text (Text)

import           Snap (getParam, Handler)

import           Application
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.SqlQQ.Alt as SQ
import           Snap.Snaplet.PostgresqlSimple

import           Data.Model (IdentI)
import           Data.Model.Patch
import           Data.Model.Utils.LegacyModel (rawBSFieldValueToIdent)
import qualified Carma.Model.KPI.Stat  as S
import qualified Carma.Model.KPI.Oper  as O
import qualified Carma.Model.KPI.Group as G
import           Carma.Model.Usermeta (Usermeta)
import           Carma.Model.Types (UserStateVal(..))
import qualified Carma.Model.CallType as CT

import           Util


getStatFiles :: AppHandler ()
getStatFiles = do
  Just f <- getParam "from"
  Just t <- getParam "to"
  writeJSON =<< selectStatFiles f t
  where
    selectStatFiles :: ByteString -> ByteString -> AppHandler (Int, Int)
    selectStatFiles f t =
      head <$> query [sql| SELECT * FROM get_KPI_attachments(?, ?) |] (f, t)

getStat :: AppHandler ()
getStat = do
  u      <- getParam "uid"
  Just f <- getParam "from"
  Just t <- getParam "to"
  case u of
   Just u' -> writeJSON =<< selectStatDays u' f t
   Nothing -> writeJSON =<< selectStat f t

selectStat :: ByteString -> ByteString
           -> AppHandler [Patch S.StatKPI]
selectStat from to = do
  [Only usrs] <- query_ activeUsersQ
  (states, _) <- RWS.execRWST fillKPIs (usrs, from, to) M.empty
  return $ M.elems states

selectStatDays :: ByteString -> ByteString -> ByteString
               -> AppHandler [Patch S.StatKPI]
selectStatDays rawuid from to = do
  (states, _) <-
    RWS.execRWST fillKPIsDays (V.singleton(raw2uid rawuid), from, to) M.empty
  return $ M.elems states
  where
    raw2uid bs =
      fromMaybe (error "Can't parse ident") (rawBSFieldValueToIdent bs)

activeUsersQ :: Query
activeUsersQ = [sql|
SELECT coalesce(array_agg(id), ARRAY[]::int[])
  FROM usermetatbl
  WHERE isActive = 't'
  AND   showKPI  = 't'
 |]

type State k     = M.Map k (Patch S.StatKPI)
type Params      = (Vector (IdentI Usermeta), ByteString, ByteString)
type HandlerSt k = RWS.RWST Params () (State k) AppHandler ()

type HandlerKPI     = HandlerSt (IdentI Usermeta)
type HandlerKPIDays = HandlerSt (IdentI Usermeta, Day)


fillKPIsDays :: HandlerKPIDays
fillKPIsDays = do
  fill states   =<< qry "get_KPI_userstates_days"
  fillTotalStates
  fill calls    =<< qry "get_KPI_calls_days"
  fill actions  =<< qry "get_KPI_actions_days"
  fill mergeKPI =<< qry "get_KPI_sumcalls_days"
  fill mergeKPI =<< qry "get_KPI_controll_actions_days"
  fill mergeKPI =<< qry "get_KPI_utilization_days"
  fill mergeKPI =<< qry "get_KPI_avg_actdo_days"
  fill mergeKPI =<< qry "get_KPI_sum_orderactions_days"
  fill mergeKPI =<< qry "get_KPI_actions_relation_days"
  fill mergeKPI =<< qry "get_KPI_time_relation_days"
  where
  fill :: (a -> HandlerKPIDays) -> [a] -> HandlerKPIDays
  fill disp d = mapM_ disp d

  states (u, day, s, v) =
    case s of
     Ready        -> addState (u, day) S.inReady v
     Busy         -> addState (u, day) S.inBusy v
     Dinner       -> addState (u, day) S.inDinner v
     NA           -> addState (u, day) S.inNA v
     Rest         -> addState (u, day) S.inRest v
     ServiceBreak -> addState (u, day) S.inServiceBreak v
     LoggedOut    -> addState (u, day) S.inLoggedOut v

  calls (u, day, tpe, t, a)
    | tpe == CT.info       = putInSt (u, day) (S.infoTime, t) (S.infoCount, a)
    | tpe == CT.newCase    = putInSt (u, day) (S.newTime, t)  (S.newCount, a)
    | tpe == CT.secondCall = putInSt (u, day) (S.procTime, t) (S.procCount, a)
    | otherwise            = error $ "Check get_KPI_calls," ++ (show tpe) ++
                                     " type should not be there."
  actions (u, day, tpe, t, a) =
    case tpe :: Text of
      "control"      -> putInSt (u, day) (S.controlT, t)      (S.controlC, a)
      "orderService" -> putInSt (u, day) (S.orderServiceT, t) (S.orderServiceC, a)
      "rushOrder"    -> putInSt (u, day) (S.rushOrderT, t)    (S.rushOrderC, a)
      "tellMeMore"   -> putInSt (u, day) (S.tellMeMoreT,   t) (S.tellMeMoreC,   a)
      "callMeMaybe"  -> putInSt (u, day) (S.callMeMaybeT,  t) (S.callMeMaybeC,  a)
      errVal -> error $ "Check get_KPI_actions," ++ (show errVal) ++
                        " type should not be there."

  putInSt u@(_, day) (f1, v1) (f2, v2) = RWS.modify $ M.insertWith union u $
    put f1 v1 $ put f2 v2 $ put S.day day empty

  mergeKPI (W p) = RWS.modify $ M.adjust (union p) $
                   (fromMaybe (error "No user field") $ get p S.user
                   ,fromMaybe (error "No day field") $ get p S.day
                   )
  qry pgfn = do
    prms <- RWS.ask
    lift $ query (fromString $
      "SELECT * FROM " ++ pgfn ++ "(?, ?, ?)") prms

  addState k@(u, d) f v = RWS.modify $ M.insertWith union k $
    put f v $ put S.day d $ put S.user u empty

fillKPIs :: HandlerKPI
fillKPIs = do
  fill states   =<< qry "get_KPI_userstates"
  fillTotalStates
  fill calls    =<< qry "get_KPI_calls"
  fill actions  =<< qry "get_KPI_actions"
  fill mergeKPI =<< qry "get_KPI_sumcalls"
  fill mergeKPI =<< qry "get_KPI_controll_actions"
  fill mergeKPI =<< qry "get_KPI_utilization"
  fill mergeKPI =<< qry "get_KPI_avg_actdo"
  fill mergeKPI =<< qry "get_KPI_actions_relation"
  fill mergeKPI =<< qry "get_KPI_time_relation"
  fill mergeKPI =<< qry "get_KPI_sum_orderactions"
  where
  fill :: (a -> HandlerKPI) -> [a] -> HandlerKPI
  fill disp d = mapM_ disp d

  states (u, s, v) =
    case s of
     Ready        -> addState u S.inReady v
     Busy         -> addState u S.inBusy v
     Dinner       -> addState u S.inDinner v
     NA           -> addState u S.inNA v
     Rest         -> addState u S.inRest v
     ServiceBreak -> addState u S.inServiceBreak v
     LoggedOut    -> addState u S.inLoggedOut v

  calls (u, tpe, t, a)
    | tpe == CT.info       = putInSt u (S.infoTime, t) (S.infoCount, a)
    | tpe == CT.newCase    = putInSt u (S.newTime,  t)  (S.newCount, a)
    | tpe == CT.secondCall = putInSt u (S.procTime, t) (S.procCount, a)
    | otherwise         =  error $ "Check get_KPI_calls," ++ (show tpe) ++
                                   " type should not be there."

  actions (u, tpe, t, a) =
    case tpe :: Text of
      "control"      -> putInSt u (S.controlT, t)      (S.controlC, a)
      "orderService" -> putInSt u (S.orderServiceT, t) (S.orderServiceC, a)
      "rushOrder"    -> putInSt u (S.rushOrderT, t)    (S.rushOrderC, a)
      "tellMeMore"   -> putInSt u (S.tellMeMoreT,   t) (S.tellMeMoreC,   a)
      "callMeMaybe"  -> putInSt u (S.callMeMaybeT,  t) (S.callMeMaybeC,  a)
      errVal -> error $ "Check get_KPI_actions," ++ (show errVal) ++
                        " type should not be there."

  putInSt u (f1, v1) (f2, v2) =
    RWS.modify $ M.adjust (put f1 v1 . put f2 v2) u

  mergeKPI (W p) = RWS.modify $ M.adjust (union p) $
                   fromMaybe (error "No KPI field in user") $ get p S.user
  qry pgfn = do
    prms <- RWS.ask
    lift $ query (fromString $
      "SELECT * FROM " ++ pgfn ++ "(?, ?, ?)") prms

  addState k f v = RWS.modify $ M.insertWith union k $
    put f v $ put S.user k empty

fillTotalStates :: HandlerSt k
fillTotalStates = RWS.modify $ M.map $ \p ->
  put S.totalLoggedIn (Just $ sum $ mapMaybe join
    [get p S.inReady, get p S.inBusy, get p S.totalRest]) $
  put S.totalRest (Just $ sum $ mapMaybe join
    [get p S.inDinner, get p S.inRest, get p S.inServiceBreak])
  p

fillTotalStates' :: Patch O.OperKPI -> Patch O.OperKPI
fillTotalStates' p =
  put O.totalLoggedIn (Just $ sum $ mapMaybe join
    [get p O.inReady, get p O.inBusy, get p O.totalRest]) $
  put O.totalRest (Just $ sum $ mapMaybe join
    [get p O.inDinner, get p O.inRest, get p O.inServiceBreak])
  p


--------------------------------------------------------------------------------
-- Oper kpis

getOper :: AppHandler ()
getOper = do
  [Only usrs] <- query_ activeUsersQ
  writeJSON =<< updateOperKPI usrs

updateOperKPI :: HasPostgres (Handler b b1)
              => Vector (IdentI Usermeta) -> Handler b b1 [Patch O.OperKPI]
updateOperKPI usrs = do
  opers <- query operTotalQ (usrs, usrs)
  forM opers $ \(W o) -> do
    let l = fromMaybe (error "OperKPI: there is no logintTime field") $
            get o O.loginTime
        u = fromMaybe (error "OperKPI: there is no user field") $ get o O.user
    kpis <- query "SELECT * FROM get_KPI_userstates(?, ?, now())"
            (V.singleton u, l)

    return $ fillTotalStates' $ union o (foldl state2kpi empty kpis)

  where
    state2kpi :: Patch O.OperKPI
              -> (IdentI Usermeta, UserStateVal, Maybe DiffTime)
              -> Patch O.OperKPI
    state2kpi p (_, s, v) =
      case s of
       Ready        -> put O.inReady v p
       Busy         -> put O.inBusy v p
       Dinner       -> put O.inDinner v p
       NA           -> put O.inNA v p
       Rest         -> put O.inRest v p
       ServiceBreak -> put O.inServiceBreak v p
       LoggedOut    -> put O.inLoggedOut v p


operTotalQ :: Query
operTotalQ = [sql|
WITH
  lastStates as (
    SELECT u.id, u.userid
         , u.state, now() - u.ctime as inCurrent
         , e.modelName, e.modelId
         , c.id as caseId
         , a.type as actionType
         , um.lastAvayaSnapshot
    FROM "UserState" u
    INNER JOIN "Event" e ON u.eventid = e.id
    LEFT  JOIN "usermetatbl" um
      ON u.userid = um.id
    LEFT  JOIN "actiontbl" a
      ON a.id = e.modelId AND e.modelName = 'Action' AND u.state = 'Busy'
    LEFT  JOIN casetbl c     ON a.caseId = c.id
    WHERE u.id IN (
      SELECT max(id) FROM "UserState"
      GROUP BY userid HAVING userid = ANY(?))),

  preLast AS (
    SELECT id, userid, state from "UserState"
    WHERE id IN (
      SELECT max(u.id) AS id FROM "UserState" u
      INNER JOIN lastStates l
      ON l.userid = u.userid AND u.id < l.id
      GROUP BY u.userid
      ORDER BY u.userid)),

  lastLogin AS (
    SELECT userid, max(ctime) AS lastlogin
    FROM "Event" WHERE type = 'Login' AND userid = ANY(?)
    GROUP BY userid)

SELECT l.userid     AS userid
     , l.state      AS currentState
     , l.inCurrent
     , l.lastAvayaSnapshot
     , l.caseId     AS currentCase
     , l.actionType AS currentAType
     , p.state      AS lastState
     , ll.lastLogin AS loginTime
FROM lastStates l
LEFT JOIN preLast p ON l.userid = p.userid
INNER JOIN lastLogin ll ON ll.userid = l.userid
|]

-------- Group KPIs ------------------------------------------------------------

getGroup :: AppHandler ()
getGroup = do
  Just f <- getParam "from"
  Just t <- getParam "to"
  writeJSON =<< selectGroup f t

selectGroup :: ByteString -> ByteString -> AppHandler (Patch G.GroupKPI)
selectGroup from to = do
  p <- uncurry query $ [SQ.sql|
    SELECT sumcall.*
         , all_actions.*
         , services_done.*
         , allservices.*
         , controll_actions.*
         , utilization.*
         , avgSrvProcessing.*
         , avgSrvFinish.*
         , satisfiedClients.*
         , claims.*
         , towStartAvg.*

    FROM      group_kpi_sumcalls($(from)$, $(to)$)         as sumcall
    LEFT JOIN group_kpi_all_actions($(from)$, $(to)$)      as all_actions
    ON true
    LEFT JOIN group_kpi_services_done($(from)$, $(to)$)    as services_done
    ON true
    LEFT JOIN group_kpi_allservices($(from)$, $(to)$)      as allservices
    ON true
    LEFT JOIN group_kpi_controll_actions($(from)$, $(to)$) as controll_actions
    ON true
    LEFT JOIN group_kpi_utilization($(from)$, $(to)$)      as utilization
    ON true
    LEFT JOIN group_kpi_avgSrvProcessing($(from)$, $(to)$) as avgSrvProcessing
    ON true
    LEFT JOIN group_kpi_avgSrvFinish($(from)$, $(to)$)      as avgSrvFinish
    ON true
    LEFT JOIN group_kpi_satisfiedClients($(from)$, $(to)$)  as satisfiedClients
    ON true
    LEFT JOIN group_kpi_claims($(from)$, $(to)$)            as claims
    ON true
    LEFT JOIN group_kpi_towStartAvg($(from)$, $(to)$)       as towStartAvg
    ON true

    |]

  let p' =
        case p of
          []  -> empty
          [p''] -> unW p''
          _   -> error "imposible happen, more than 1 record in query results"

  rawCalls   <- query [sql| select * from group_kpi_calls(?, ?)  |] (from, to)
  rawActions <- query [sql| select * from group_kpi_actions(?, ?)|] (from, to)
  return $ foldl actions (foldl calls p' rawCalls) rawActions

  where
    calls p (tpe, t, a)
      | tpe == CT.info       = putTuple p (G.infoTime, t) (G.infoCount, a)
      | tpe == CT.newCase    = putTuple p (G.newTime, t)  (G.newCount, a)
      | tpe == CT.secondCall = putTuple p (G.procTime, t) (G.procCount, a)
      | otherwise         = error $ "Check group_kpi_calls," ++ (show tpe) ++
                                    " type should not be there."
    actions p (tpe, t, a) =
      case tpe :: Text of
        "control"      -> putTuple p (G.controlT, t)      (G.controlC, a)
        "orderService" -> putTuple p (G.orderServiceT, t) (G.orderServiceC, a)
        "tellMeMore"   -> putTuple p (G.tellMeMoreT,   t) (G.tellMeMoreC,   a)
        "callMeMaybe"  -> putTuple p (G.callMeMaybeT,  t) (G.callMeMaybeC,  a)
        errVal -> error $ "Check group_kpi_actions," ++ (show errVal) ++
                          " type should not be there."

    putTuple p (f1, v1) (f2, v2) = put f1 v1 $ put f2 v2 p

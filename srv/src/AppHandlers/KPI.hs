{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module AppHandlers.KPI (getStat, getOper, updateOperKPI) where

import           Control.Monad (forM)
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS.Strict as RWS

import           Data.String     (fromString)
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Data.Vector (Vector, singleton)
-- import           Data.Time.Clock (DiffTime)
import           Data.Text       (Text)

import           Snap (getParam, Handler)

import           Application
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Snaplet.PostgresqlSimple

import           Data.Model (IdentI)
import           Data.Model.Patch
import qualified Carma.Model.KPI.Stat as S
import qualified Carma.Model.KPI.Oper as O
import           Carma.Model.Usermeta (Usermeta)

import           AppHandlers.Util

getStat :: AppHandler ()
getStat = do
  Just f <- getParam "from"
  Just t <- getParam "to"
  writeJSON =<< selectStat f t

selectStat :: ByteString -> ByteString -> AppHandler [Patch S.StatKPI]
selectStat from to = do
  [Only usrs] <- query_ activeUsersQ
  states <- query [sql| SELECT * FROM get_KPI_timeinstate(?, tstzrange(?, ?))
    |] (usrs, from, to)
  (states', _) <-
    RWS.execRWST fillKPIs (usrs, from, to) (smap $ map unW states)
  return $ M.elems states'
  where
    smap :: [Patch S.StatKPI] -> M.Map (IdentI Usermeta) (Patch S.StatKPI)
    smap =
      foldl (\a v -> maybe a (\mu -> M.insert mu v a) (get v S.user)) M.empty

activeUsersQ :: Query
activeUsersQ = [sql|
SELECT coalesce(array_agg(id), ARRAY[]::int[])
  FROM usermetatbl
  WHERE isActive = 't'
  AND   showKPI  = 't'
 |]

type State     = M.Map (IdentI Usermeta) (Patch S.StatKPI)
type Params    = (Vector (IdentI Usermeta), ByteString, ByteString)
type HandlerSt = RWS.RWST Params () State AppHandler ()

fillKPIs :: HandlerSt
fillKPIs = do
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
  fill :: (a -> HandlerSt) -> [a] -> HandlerSt
  fill disp d = mapM_ disp d

  calls (u, tpe, t, a) =
    case tpe :: Text of
      "info"           -> putInSt u (S.infoTime, t) (S.infoCount, a)
      "newCase"        -> putInSt u (S.newTime, t)  (S.newCount, a)
      "processingCase" -> putInSt u (S.procTime, t) (S.procCount, a)
      errVal -> error $ "Check get_KPI_calls," ++ (show errVal) ++
                        " type should not be there."
  actions (u, tpe, t, a) =
    case tpe :: Text of
      "control"      -> putInSt u (S.controlT, t)      (S.controlC, a)
      "orderService" -> putInSt u (S.orderServiceT, t) (S.orderServiceC, a)
      "tellMeMore"   -> putInSt u (S.tellMeMoreT,   t) (S.tellMeMoreC,   a)
      "callMeMaybe"  -> putInSt u (S.callMeMaybeT,  t) (S.callMeMaybeC,  a)
      errVal -> error $ "Check get_KPI_actions," ++ (show errVal) ++
                        " type should not be there."
  putInSt u (f1, v1) (f2, v2) =
    RWS.modify $ M.adjust (\a -> put f1 v1 $ put f2 v2 a) u

  mergeKPI (W p) = RWS.modify $ M.adjust (union p) $
                   fromMaybe (error "No KPI field in user") $ get p S.user
  qry pgfn = do
    prms <- RWS.ask
    lift $ query (fromString $
      "SELECT * FROM " ++ pgfn ++ "(?, ?, ?)") prms


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
    let l = fromMaybe (error "OperKPI: here is no logintTime field") $
            get o O.loginTime
        u = fromMaybe (error "OperKPI: here is no user field") $ get o O.user
    kpis <- query "SELECT * FROM get_KPI_timeinstate(?, tstzrange(?, now()));"
            (singleton u, l)
    case kpis of
      -- It's something strange, we have login event with no state, bug?
      []      -> return o
      [kpis'] -> return $ union o (unW kpis')
      _       -> error "There is something terribly wrong with damn states"

operTotalQ :: Query
operTotalQ = [sql|
WITH
  lastStates as (
    SELECT u.id, u.userid
         , u.state, now() - u.ctime as inCurrent
         , e.modelName, e.modelId
    FROM "UserState" u
    INNER JOIN "Event" e ON u.eventid = e.id
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

SELECT l.userid     as userid
     , l.state      as currentState
     , l.inCurrent
     , CASE WHEN l.modelName = 'Case' THEN l.modelId END AS currentCase
     , p.state      as lastState
     , ll.lastLogin as loginTime
FROM lastStates l
LEFT JOIN preLast p ON l.userid = p.userid
INNER JOIN lastLogin ll ON ll.userid = l.userid
|]

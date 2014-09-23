{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module AppHandlers.KPI (getStat) where

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS.Strict as RWS

import           Data.String     (fromString)
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Data.Vector (Vector)
-- import           Data.Time.Clock (DiffTime)
import           Data.Text       (Text)

import           Snap (getParam)

import           Application
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Snaplet.PostgresqlSimple

import           Data.Model (IdentI)
import           Data.Model.Patch
import           Carma.Model.KPI
import           Carma.Model.Usermeta (Usermeta)

import           AppHandlers.Util

getStat :: AppHandler ()
getStat = do
  Just f <- getParam "from"
  Just t <- getParam "to"
  writeJSON =<< selectStat f t

selectStat :: ByteString -> ByteString -> AppHandler [Patch StatKPI]
selectStat from to = do
  [Only usrs] <- query_ activeUsersQ
  states <- query [sql| SELECT * FROM get_KPI_timeinstate(?, tstzrange(?, ?))
    |] (usrs, from, to)
  (states', _) <-
    RWS.execRWST fillKPIs (usrs, from, to) (smap $ map unW states)
  return $ M.elems states'
  where
    smap :: [Patch StatKPI] -> M.Map (IdentI Usermeta) (Patch StatKPI)
    smap = foldl (\a v -> maybe a (\mu -> M.insert mu v a) (get v user)) M.empty

activeUsersQ :: Query
activeUsersQ = [sql|
SELECT coalesce(array_agg(id), ARRAY[]::int[])
  FROM usermetatbl
  WHERE isActive = 't'
 |]

type State     = M.Map (IdentI Usermeta) (Patch StatKPI)
type Params    = (Vector (IdentI Usermeta), ByteString, ByteString)
type HandlerSt = RWS.RWST Params () State AppHandler ()

fillKPIs :: HandlerSt
fillKPIs = do
  fill calls   =<< qry "get_KPI_calls"
  fill actions =<< qry "get_KPI_actions"
  fill mergeKPI =<< qry "get_KPI_sumcalls"
  fill mergeKPI =<< qry "get_KPI_controll_actions"
  fill mergeKPI =<< qry "get_KPI_utilization"
  fill mergeKPI =<< qry "get_KPI_avg_actdo"
  fill mergeKPI =<< qry "get_KPI_actions_relation"
  fill mergeKPI =<< qry "get_KPI_time_relation"
  where
  fill :: (a -> HandlerSt) -> [a] -> HandlerSt
  fill disp d = mapM_ disp d

  calls (u, tpe, t, a) =
    case tpe :: Text of
      "info"           -> putInSt u (infoTime, t) (infoCount, a)
      "newCase"        -> putInSt u (newTime, t)  (newCount, a)
      "processingCase" -> putInSt u (procTime, t) (procCount, a)
      errVal -> error $ "Check get_KPI_calls," ++ (show errVal) ++
                        " type should not be there."
  actions (u, tpe, t, a) =
    case tpe :: Text of
      "control"      -> putInSt u (controlT, t)      (controlC, a)
      "orderService" -> putInSt u (orderServiceT, t) (orderServiceC, a)
      "tellMeMore"   -> putInSt u (tellMeMoreT,   t) (tellMeMoreC,   a)
      "callMeMaybe"  -> putInSt u (callMeMaybeT,  t) (callMeMaybeC,  a)
      errVal -> error $ "Check get_KPI_actions," ++ (show errVal) ++
                        " type should not be there."
  putInSt u (f1, v1) (f2, v2) =
    RWS.modify $ M.adjust (\a -> put f1 v1 $ put f2 v2 a) u

  mergeKPI (W p) = RWS.modify $ M.adjust (union p) $
                   fromMaybe (error "No KPI field in user") $ get p user
  qry pgfn = do
    prms <- RWS.ask
    lift $ query (fromString $
      "SELECT * FROM " ++ pgfn ++ "(?, ?, ?)") prms

{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}

module AppHandlers.KPI (getStat) where

-- import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Strict as ST

import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import           Data.Vector (Vector)
import           Data.Time.Clock (DiffTime)
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

activeUsersQ :: Query
activeUsersQ = [sql|
SELECT coalesce(array_agg(id), ARRAY[]::int[])
  FROM usermetatbl
  WHERE isActive = 't'
 |]

selectStat :: ByteString -> ByteString -> AppHandler [Patch StatKPI]
selectStat from to = do
  [Only usrs] :: [Only (Vector (IdentI Usermeta))] <- query_ activeUsersQ
  states <- query [sql|
    SELECT * FROM get_KPI_timeinstate(?, tstzrange(?, ?)) |] (usrs, from, to)
  states' <- (flip ST.execStateT) (smap $ map unW states) $ do
    fillCalls =<<
      (lift $ query [sql| SELECT * FROM get_KPI_calls(?, ?, ?) |] (usrs, from, to))

  return $ M.elems states'
  where
    smap :: [Patch StatKPI] -> M.Map (IdentI Usermeta) (Patch StatKPI)
    smap = foldl (\a v -> maybe a (\mu -> M.insert mu v a) (get v user)) M.empty
    fillCalls :: [(IdentI Usermeta, Text, DiffTime, Int)]
              -> ST.StateT (M.Map (IdentI Usermeta) (Patch StatKPI)) AppHandler [()]
    fillCalls calls =
      forM calls $ \(uid, tpe, time, amount) ->
        case tpe of
          "info"           -> putInSt uid (infoTime, time) (infoCount, amount)
          "newCase"        -> putInSt uid (newTime, time)  (newCount, amount)
          "processingCase" -> putInSt uid (procTime, time) (procCount, amount)
          errVal -> error $ "Check get_KPI_calls," ++ (show errVal) ++
                          " type should not be there."

    putInSt u (f1, v1) (f2, v2) = ST.modify $
      M.adjust (\a -> put f1 v1 $ put f2 v2 a) u


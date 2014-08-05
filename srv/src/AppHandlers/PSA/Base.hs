{-# LANGUAGE QuasiQuotes #-}

{-|

  Non-handler helpers and SQL queries used by PSA integration.

-}

module AppHandlers.PSA.Base
    ( psaQuery
    , rtQuery
    , rtQuery'
    , repTowages
    )

where

import Data.Text (Text)
import Data.List
import qualified Data.Vector as V

import Database.PostgreSQL.Simple hiding (query)
import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.PostgresqlSimple

import Carma.Model.FalseCall
import Carma.Model.ServiceStatus
import Carma.Model.TechType


-- | Query used to select exportable case ids, parametrized by vector
-- of program ids, used by @/psaCases@.
psaQuery :: Query
psaQuery = [sql|
SELECT id FROM casetbl
WHERE psaExportNeeded='yes'
AND  (program = ANY (?))
AND  (NOT psaexported='yes' OR psaexported IS NULL);
|]


-- | Towages within previous 30 days, parametrized by case id, service
-- status list and required falseCall; used in @/repTowages@.
rtQuery :: Query
rtQuery = [sql|
WITH parentcase AS (select calldate, car_vin, comment from casetbl where id=?)
SELECT concat(s.type, ':', s.id) FROM casetbl c INNER JOIN towagetbl s
ON c.id=cast(split_part(s.parentid, ':', 2) as integer)
WHERE s.parentid is not null
AND c.car_vin=(SELECT car_vin FROM parentcase)
AND (s.status = ANY (?))
AND s.falseCall=?
AND c.calldate >= ((SELECT calldate FROM parentcase) - INTERVAL '30 days')
AND c.calldate < (SELECT calldate FROM parentcase)
AND c.comment=(SELECT comment FROM parentcase);
|]


-- | Recharges within previous 48 hours, parametrized by case id,
-- service status list, required falseCall and techType; used in
-- @/repTowages@.
rtQuery' :: Query
rtQuery' = [sql|
WITH parentcase AS (select calldate, car_vin, comment from casetbl where id=?)
SELECT concat(s.type, ':', s.id) FROM casetbl c INNER JOIN techtbl s
ON c.id=cast(split_part(s.parentid, ':', 2) as integer)
WHERE s.parentid is not null
AND c.car_vin=(SELECT car_vin FROM parentcase)
AND (s.status = ANY (?))
AND s.falseCall= ?
AND s.techType = ?
AND c.calldate >= ((SELECT calldate FROM parentcase) - INTERVAL '2 days')
AND c.calldate <= (SELECT calldate FROM parentcase)
AND c.comment=(SELECT comment FROM parentcase);
|]


-- | Given a case id, if it contains a repeated towages, return a list
-- of towage/tech service references for the same car (as indicated by
-- matching VIN and case comment) which occured within 30 day period
-- prior to the case creation date or battery recharges of the same
-- car within previous 48 hours.
repTowages :: HasPostgres m =>
              Int
           -- ^ Case ID.
           -> m [Text]
repTowages n = do
  let statuses = V.fromList [ok, closed]
  rows <- query rtQuery (n, statuses, none)
  rows' <- query rtQuery' (n, statuses, none, charge)
  return $ nub $ map head (rows ++ rows')

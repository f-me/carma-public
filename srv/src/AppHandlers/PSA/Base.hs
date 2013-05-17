{-# LANGUAGE QuasiQuotes #-}

{-|

  Non-handler helpers and SQL queries used by PSA integration.

-}

module AppHandlers.PSA.Base
    ( psaQuery
    , psaQuery0
    , rtQuery
    , rtQuery'
    , repTowages
    )

where

import Data.List
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ


-- | Query used to select exportable case ids, parametrized by program
-- name, used by @/psaCases@.
psaQuery :: Query
psaQuery = [sql|
SELECT id FROM casetbl
WHERE caseStatus='s2'
AND  (program=?)
AND  (NOT psaexported='yes' OR psaexported IS NULL)
AND  (calldate > car_warrantystart AND calldate < car_warrantyend);
|]


-- | Non-parametric query for @/psaCases@, includes @citroen@ and
-- @peugeot@ programs.
psaQuery0 :: Query
psaQuery0 = [sql|
SELECT id FROM casetbl
WHERE caseStatus='s2'
AND  (program='citroen' OR program='peugeot')
AND  (NOT psaexported='yes' OR psaexported IS NULL)
AND  (calldate > car_warrantystart AND calldate < car_warrantyend);
|]


-- | Towages within previous 30 days, parametrized by case id, used in
-- @/repTowages@.
rtQuery :: Query
rtQuery = [sql|
WITH parentcase AS (select calldate, car_vin, comment from casetbl where id=?)
SELECT c.id FROM casetbl c INNER JOIN towagetbl s
ON c.id=cast(split_part(s.parentid, ':', 2) as integer)
WHERE s.parentid is not null
AND c.car_vin=(SELECT car_vin FROM parentcase)
AND (s.status='serviceOk' OR s.status='serviceClosed')
AND c.calldate >= ((SELECT calldate FROM parentcase) - INTERVAL '30 days')
AND c.calldate < (SELECT calldate FROM parentcase)
AND c.comment=(SELECT comment FROM parentcase);
|]


-- | Recharges within previous 24 hours, parametrized by case id, used
-- in @/repTowages@.
rtQuery' :: Query
rtQuery' = [sql|
WITH parentcase AS (select calldate, car_vin, comment from casetbl where id=?)
SELECT s.id FROM casetbl c INNER JOIN techtbl s
ON c.id=cast(split_part(s.parentid, ':', 2) as integer)
WHERE s.parentid is not null
AND c.car_vin=(SELECT car_vin FROM parentcase)
AND s.falseCall='bill'
AND s.techType='charge'
AND c.calldate >= ((SELECT calldate FROM parentcase) - INTERVAL '1 day')
AND c.calldate <= (SELECT calldate FROM parentcase)
AND c.comment=(SELECT comment FROM parentcase);
|]


-- | Given a case id, serve a list of towage service id's
-- corresponding to repeated towages of the same car (as indicated by
-- matching VIN and case comment) which occured within 30 day period
-- prior to the case creation date or battery recharges of the same
-- car within previous 24 hours.
repTowages :: Connection
           -> Int
           -- ^ Case ID.
           -> IO [Int]
repTowages c n = do
  rows <- query c rtQuery [n]
  rows' <- query c rtQuery' [n]
  return $ nub $ map head (rows ++ rows')

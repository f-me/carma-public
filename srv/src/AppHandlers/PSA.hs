{-# LANGUAGE QuasiQuotes #-}

{-|

  API methods used by PSA integration points.

-}

module AppHandlers.PSA
    ( psaCases
    , repTowages
    )

where

import Data.ByteString.Char8 (readInt)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Snap

import AppHandlers.Util
import Application


-- | Query parametrized by program name, used in 'psaCases'.
psaQuery :: Query
psaQuery = [sql|
SELECT id FROM casetbl
WHERE caseStatus='s2'
AND  (program=?)
AND  (NOT psaexported='yes' OR psaexported IS NULL)
AND  ((calldate > car_servicestart AND calldate < car_serviceend)
 OR   (calldate > car_warrantystart AND calldate < car_warrantyend));
|]


-- | Non-parametric query for 'psaCases', includes @citroen@ and
-- @peugeot@ programs.
psaQuery0 :: Query
psaQuery0 = [sql|
SELECT id FROM casetbl
WHERE caseStatus='s2'
AND  (program='citroen' OR program='peugeot')
AND  (NOT psaexported='yes' OR psaexported IS NULL)
AND  ((calldate > car_servicestart AND calldate < car_serviceend)
 OR   (calldate > car_warrantystart AND calldate < car_warrantyend));
|]


-- | Read program name from @program@ request parameter, serve JSON
-- list of case numbers for that program to be exported to SAGAI, as
-- selected by 'psaQuery'. If @program@ is not present, serve list of
-- all exportable case numbers according to 'psaQuery0'.
psaCases :: AppHandler ()
psaCases = do
  program <- getParam "program"
  rows <- withPG pg_search $
          \c -> case program of
                  Just p -> query c psaQuery [p]
                  Nothing -> query_ c psaQuery0
  writeJSON (map head rows :: [Int])


rtQuery :: Query
rtQuery = [sql|
WITH parentcase AS (select calldate, car_vin, comment from casetbl where id=?)
SELECT c.id FROM casetbl c INNER JOIN towagetbl s
ON c.id=cast(split_part(s.parentid, ':', 2) as integer)
WHERE s.parentid is not null
AND (s.status='serviceOk' OR s.status='serviceClosed')
AND c.calldate >= ((SELECT calldate FROM parentcase) - INTERVAL '30 days')
AND c.calldate < (SELECT calldate FROM parentcase)
AND c.comment=(SELECT comment FROM parentcase)
AND c.car_vin=(SELECT car_vin FROM parentcase);
|]


-- | Read case id from @id@ request parameter, serve JSON list of case
-- ids corresponding to towages of the same car (as indicated by
-- matching VIN and case comment) which occured within 30 day period
-- prior to the case creation date.
repTowages :: AppHandler ()
repTowages = do
 cid <- (liftM readInt) <$> getParam "id"
 case cid of
   Just (Just (n, _)) -> do
               rows <- withPG pg_search $ \c -> query c rtQuery [n]
               writeJSON (map head rows :: [Int])
   _ -> error "Could not read case id from request"

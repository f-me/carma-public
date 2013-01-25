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
import Snap

import AppHandlers.Util
import Application


psaQuery :: Query
psaQuery = [str|
SELECT id FROM casetbl
WHERE caseStatus='s2'
AND  (program='citroen' OR program='peugeot')
AND  (NOT psaexported='yes' OR psaexported IS NULL)
AND  ((calldate > car_servicestart AND calldate < car_serviceend)
 OR   (calldate > car_warrantystart AND calldate < car_warrantyend));
|]


-- | Serve JSON list of case numbers to be exported to SAGAI.
psaCases :: AppHandler ()
psaCases = do
  rows <- withPG pg_search $ \c -> query_ c psaQuery
  writeJSON (map head rows :: [Int])


rtQuery :: Query
rtQuery = [str|
WITH parentcase AS (select calldate, car_vin from casetbl where id=?)
SELECT c.id FROM casetbl c INNER JOIN towagetbl s
ON c.id=cast(split_part(s.parentid, ':', 2) as integer)
WHERE s.parentid is not null
AND (s.status='serviceOk' OR s.status='serviceClosed')
AND c.calldate >= ((SELECT calldate FROM parentcase) - INTERVAL '30 days')
AND c.calldate < (SELECT calldate FROM parentcase)
AND c.car_vin=(SELECT car_vin FROM parentcase);
|]


-- | Read case id from @id@ request parameter, serve JSON list of case
-- ids corresponding to towages of the same car (as indicated by
-- matching VINs) which occured within 30 day period prior to the case
-- creation date.
repTowages :: AppHandler ()
repTowages = do
 cid <- (liftM readInt) <$> getParam "id"
 case cid of
   Just (Just (n, _)) -> do
               rows <- withPG pg_search $ \c -> query c rtQuery [n]
               writeJSON (map head rows :: [Int])
   _ -> error "Could not read case id from request"

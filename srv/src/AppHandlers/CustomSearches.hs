{-# LANGUAGE QuasiQuotes #-}
module AppHandlers.CustomSearches where

import Control.Applicative
import Data.String (fromString)
import Data.Maybe
import Data.Map (Map)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Snap
import Snap.Snaplet.Auth
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
--------------------------------------------------------------------
import Application
import AppHandlers.Util
import Utils.HttpErrors

type MBS = Maybe ByteString


allPartnersHandler :: AppHandler ()
allPartnersHandler
  = join (selectPartners
    <$> getParam "city"
    <*> getParam "isActive"
    <*> getParam "isDealer"
    <*> getParam "makes")
  >>= writeJSON

partnersForSrvHandler :: AppHandler ()
partnersForSrvHandler =
    join (selectPartnersForSrv <$>
          getParam "city"      <*>
          getParam "isActive"  <*>
          getParam "srv"       <*>
          getParam "makes")
    >>= writeJSON

selectPartnersForSrv :: MBS -> MBS -> MBS -> MBS
                     -> AppHandler [Map ByteString ByteString]
selectPartnersForSrv city isActive service makes = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString
    $  "SELECT p.id::text, p.name, p.city,"
    ++ "       p.comment, p.addrDeFacto, p.phone1, p.workingTime,"
    ++ "       (p.isDealer::int)::text, (p.isMobile::int)::text,"
    ++ "       s.priority1, s.priority2, s.priority3"
    ++ "   FROM partnertbl p"
    ++ "   INNER JOIN partner_servicetbl s"
    ++ "   ON p.id = cast(split_part(s.parentid, ':', 2) as integer)"
    ++ "   AND s.parentid is not null"
    ++ "   AND s.parentid != ''"
    ++ "   AND s.parentid != 'partner:null'"
    ++ "   WHERE true"
    ++ (maybe "" (\x -> "  AND p.city = " ++ quote x) city)
    ++ (maybe "" (\x -> "  AND p.isActive = " ++ toBool x) isActive)
    ++ (maybe "" (\x -> "  AND s.servicename = " ++ quote x) service)
    -- array_dims(make) IS NULL is array emptiness check
    ++ (maybe " AND array_dims(makes) IS NULL"
              (\x -> "  AND (array_dims(makes) IS NULL OR "
                     ++ quote x ++ " = ANY (makes))")
              makes)

  let fields =
        ["id","name","city","comment" ,"addrDeFacto"
        ,"phone1","workingTime","isDealer","isMobile"
        ,"priority1", "priority2", "priority3"
        ]
  return $ mkMap fields rows

selectPartners :: MBS -> MBS -> MBS -> MBS -> AppHandler [Map ByteString ByteString]
selectPartners city isActive isDealer makes = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString
    $  "SELECT id::text, name, city,"
    ++ "       comment, addrDeFacto, phone1, workingTime,"
    ++ "       (isDealer::int)::text, (isMobile::int)::text"
    ++ "  FROM partnertbl WHERE true"
    ++ (maybe "" (\x -> "  AND city = " ++ quote x) city)
    ++ (maybe "" (\x -> "  AND isActive = " ++ toBool x) isActive)
    ++ (maybe "" (\x -> "  AND isDealer = " ++ toBool x) isDealer)
    ++ (maybe "" (\x -> "  AND " ++ quote x ++ " = ANY (makes)") makes)
  let fields =
        ["id","name","city","comment" ,"addrDeFacto"
        ,"phone1","workingTime","isDealer","isMobile"
        ]
  return $ mkMap fields rows

allActionsHandler :: AppHandler ()
allActionsHandler
  = join (selectActions
    <$> getParam "closed"
    <*> getParam "assignedTo"
    <*> getParam "targetGroup"
    <*> getParam "duetimeFrom"
    <*> getParam "duetimeTo")
  >>= writeJSON

selectActions
  :: MBS -> MBS -> MBS -> MBS -> MBS
  -> AppHandler [Map ByteString ByteString]
selectActions mClosed mAssignee mRole mFrom mTo = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString
    $  "SELECT a.id::text, a.caseId, a.parentId,"
    ++ "       (a.closed::int)::text, a.name, a.assignedTo, a.targetGroup,"
    ++ "       (extract (epoch from a.duetime at time zone 'UTC')::int8)::text, "
    ++ "       a.result, a.priority, a.description, a.comment,"
    ++ "       c.city, c.program,"
    ++ "       (extract (epoch from"
    ++ "         coalesce(s.times_expectedServiceStart, a.duetime)"
    ++ "          at time zone 'UTC')::int8)::text"
    ++ "  FROM "
    ++ "    (actiontbl a LEFT JOIN servicetbl s"
    ++ "      ON  s.id::text = substring(a.parentid, ':(.*)')"
    ++ "      AND s.type::text = substring(a.parentId, '(.*):')),"
    ++ "    casetbl c WHERE true"
    ++ "                   AND c.id::text = substring(a.caseId, ':(.*)')"
    ++ (maybe "" (\x -> "  AND closed = " ++ toBool x) mClosed)
    ++ (maybe "" (\x -> "  AND assignedTo = " ++ quote x) mAssignee)
    ++ (maybe "" (\x -> "  AND targetGroup = " ++ quote x) mRole)
    ++ (maybe "" (\x -> "  AND extract (epoch from duetime) >= " ++ int x) mFrom)
    ++ (maybe "" (\x -> "  AND extract (epoch from duetime) <= " ++ int x) mTo)
  let fields
        = ["id", "caseId", "parentId", "closed", "name"
          ,"assignedTo", "targetGroup", "duetime", "result"
          ,"priority", "description", "comment","city", "program"
          ,"times_expectedServiceStart"]
  return $ mkMap fields rows


searchCallsByPhone :: AppHandler ()
searchCallsByPhone = do
  -- This two magic lines are required because `getParam` interprets
  -- '+' symbol in url as space
  -- FIXME: maybe better strip '+' from phones everywhere
  uri <- rqURI <$> getRequest
  let phone = last $ B.split '/' uri

  rows <- withPG pg_search $ \c -> query c (fromString
    $  "SELECT wazzup, callerName_name, city, program, make, model,"
    ++ "       callTaker, callType,"
    ++ "       extract (epoch from callDate at time zone 'UTC')::int8::text"
    ++ "  FROM calltbl"
    ++ "  WHERE callerName_phone1 = ?") [phone]
  let fields =
        ["wazzup","callerName_name", "city", "program"
        ,"make", "model", "callTaker", "callType", "callDate"]
  writeJSON $ mkMap fields rows


getActionsForCase :: AppHandler ()
getActionsForCase = do
  Just caseId <- getParam "id"
  let caseId' = B.append "case:" caseId
  rows <- withPG pg_search $ \c -> query c (fromString
    $  "SELECT extract (epoch from closeTime at time zone 'UTC')::int8::text,"
    ++ "       result, name, assignedTo, comment"
    ++ "  FROM actiontbl"
    ++ "  WHERE caseId = ?") [caseId']
  let fields =
        ["closeTime", "result", "name", "assignedTo", "comment"]
  writeJSON $ mkMap fields rows

getCancelsForCase :: AppHandler ()
getCancelsForCase = do
  Just caseId <- getParam "id"
  let caseId' = B.append "case:" caseId
  rows <- withPG pg_search $ \c -> query c (fromString
    $  "SELECT extract (epoch from c.ctime at time zone 'UTC')::int8::text,"
    ++ "       c.partnerId, c.serviceId, c.partnerCancelReason, c.comment,"
    ++ "       c.owner, p.name"
    ++ "  FROM partnercanceltbl c"
    ++ "  LEFT JOIN partnertbl p"
    ++ "  ON p.id = cast(split_part(c.partnerId, ':', 2) as integer)"
    ++ "  WHERE c.caseId = ?") [caseId']
  let fields =
        [ "ctime", "partnerId", "serviceId", "partnerCancelReason"
        , "comment", "owner", "partnerName"
        ]
  writeJSON $ mkMap fields rows

selectContracts :: AppHandler ()
selectContracts = do
  dateFrom <- fromMaybe "1970-01-01" <$> getParam "from"
  dateTo   <- fromMaybe "2970-01-01" <$> getParam "to"
  Just prg <- getParam "program"
  Just usr <- with auth currentUser

  rows <- withPG pg_search $ \c -> query c [sql|
    SELECT c.id::text,
      extract (epoch from ctime at time zone 'UTC')::int8::text,
      c.isActive::text,
      carVin, carMake, carModel, carColor, carPlateNum, cardNumber::text,
      carMakeYear::text, carCheckPeriod::text,
      extract (epoch from carBuyDate at time zone 'UTC')::int8::text,
      extract (epoch from warrantyStart at time zone 'UTC')::int8::text,
      extract (epoch from contractValidFromDate at time zone 'UTC')::int8::text,
      extract (epoch from contractValidUntilDate at time zone 'UTC')::int8::text,
      contractValidUntilMilage::text, milageTO::text, cardOwner, manager,
      carSeller, carDealerTO,
      u.realname
      FROM contracttbl c, usermetatbl u
      WHERE dixi
        AND u.login = ? AND ? = ANY (u.programs)
        AND (coalesce(u.isDealer,false) = false OR c.owner = u.uid::text)
        AND c.program = ? AND date(ctime) between ? AND ?
    |] (userLogin usr, prg, prg, dateFrom, dateTo)
  let fields =
        [ "id", "ctime", "isActive", "carVin", "carMake", "carModel", "carColor"
        , "carPlateNum", "cardNumber", "carMakeYear", "carCheckPeriod"
        , "carBuyDate", "warrantyStart", "contractValidFromDate"
        , "contractValidUntilDate", "contractValidUntilMilage"
        , "milageTO", "cardOwner", "manager", "carSeller", "carDealerTO"
        , "owner"
        ]
  writeJSON $ mkMap fields rows

busyOpsq :: String
busyOpsq = [sql|
  SELECT assignedTo, count(1)::text
  FROM   actiontbl
  WHERE  closed = 'f'
  GROUP BY assignedTo
  HAVING   assignedTo is not null AND assignedTo != ''
  |]

busyOps :: AppHandler ()
busyOps = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString busyOpsq
  writeJSON $ mkMap ["name", "count"] rows

boUsers :: AppHandler ()
boUsers = do
  rows <- withPG pg_search $ \c -> query_ c [sql|
    SELECT realname, login
      FROM usermetatbl
      WHERE (lastlogout IS NULL OR lastlogout < lastactivity)
        AND now() - lastactivity < '20 min'
        AND roles && ARRAY[
          'head','back','supervisor','parguy','account',
          'analyst','op_checker','op_close','op_dealer']
    |]
  writeJSON $ mkMap ["name", "login"] rows

allDealersForMake :: AppHandler ()
allDealersForMake = do
  Just make <- getParam "make"
  rows <- withPG pg_search $ \c -> query c [sql|
    SELECT id::text, name
      FROM partnertbl
      WHERE isActive AND isDealer AND ? = ANY (makes)
    |] [make]
  writeJSON $ mkMap ["id", "name"] rows

getLatestCases :: AppHandler ()
getLatestCases = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString $ [sql|
    SELECT
      id::text, contact_name,
      extract (epoch from callDate at time zone 'UTC')::int8::text,
      contact_phone1, car_plateNum, car_vin, program, comment
    FROM casetbl
    ORDER BY callDate DESC
    LIMIT 120
    |]
  writeJSON $ mkMap
    ["id", "contact_name", "callDate", "contact_phone1"
    ,"car_plateNum", "car_vin", "program", "comment"]
    rows

searchCases :: AppHandler ()
searchCases = do
  Just q <- getParam "q"
  rows <- withPG pg_search $ \c -> query c (fromString $ [sql|
    SELECT
      id::text, contact_name,
      extract (epoch from callDate at time zone 'UTC')::int8::text,
      contact_phone1, car_plateNum, car_vin, program, comment
    FROM CaseSearch(?)
    ORDER BY callDate DESC
    LIMIT 100
    |]) [q]
  writeJSON $ mkMap
    ["id", "contact_name", "callDate", "contact_phone1"
    ,"car_plateNum", "car_vin", "program", "comment"]
    rows


findSameContract :: AppHandler ()
findSameContract = do
  cvin <- getParam "carVin"
  num  <- getParam "cardNumber"
  cid  <- getParam "id"

  case cid of
    Nothing  -> finishWithError 403 "need id param"
    Just id' -> do
      rows <- withPG pg_search $ \c -> query_ c $ fromString
        $  " SELECT id::text, to_char(ctime, 'YYYY-MM-DD HH24:MI')"
        ++ " FROM contracttbl"
        ++ " WHERE ctime > now() - interval '30 days'"
        ++ " AND id != " ++ quote id'
        ++ "AND (false "
        ++ (maybe "" (\x -> " OR carVin = "     ++ quote x) cvin)
        ++ (maybe "" (\x -> " OR cardNumber = " ++ quote x) num)
        ++ ")"
      writeJSON $ mkMap ["id", "ctime"] rows

vinReverseLookup :: AppHandler ()
vinReverseLookup = do
  carvin <- fromJust <$> getParam "vin"
  q      <- withPG pg_search $ \c -> query c (fromString $ [sql|
     SELECT carvin
          , concat('', carmake)
          , concat('', carmodel)
          , concat('', program)
          , to_char(carBuyDate, 'DD.MM.YYYY')
     FROM contracttbl
     WHERE id IN
     (SELECT max(id)
      FROM contracttbl
      WHERE isactive = 't' AND dixi = 't'
      GROUP BY carvin, program
      HAVING lower(carvin) like '%' || lower(?) || '%'
      ORDER BY max(id) DESC
      LIMIT 15)
    |]) [carvin]
  writeJSON $ mkMap ["vin", "make", "model", "program", "buyDate"] q


-- | Serve a list of contract suggestions given a program and
-- cardNumber (possibly a part of the whole string) in request
-- parameters.
cardNumberLookup :: AppHandler ()
cardNumberLookup = do
  p  <- getParam "program"
  cn <- getParam "cardNumber"
  case (p, cn) of
    (Just program, Just cardNumber) -> do
      q      <- withPG pg_search $ \c -> query c (fromString $ [sql|
        SELECT id::text
             , cardNumber
             , concat('', carVin)
             , concat('', carmake)
             , concat('', carmodel)
             , to_char(carBuyDate, 'DD.MM.YYYY')
        FROM contracttbl
        WHERE id IN
        (SELECT max(c.id)
         FROM contracttbl c
         LEFT JOIN programtbl p ON p.id::text = c.program
         WHERE c.isactive AND c.dixi AND p.value = ?
         GROUP BY cardNumber, c.program
         HAVING lower(c.cardNumber) like '%' || lower(?) || '%'
         ORDER BY max(c.id) DESC
         LIMIT 15)
       |]) [program, cardNumber]
      writeJSON $ mkMap ["cid", "cardNumber", "vin", "make", "model", "buyDate"] q
    (_, _) -> error "Bad request: program & cardNumber parameters not set"

cardOwnerLookup :: AppHandler ()
cardOwnerLookup = do
  cardOwner <- fromJust <$> getParam "q"
  res <- withPG pg_search $ \c -> query c (fromString $ [sql|
     SELECT cardOwner FROM contracttbl c, programtbl p
      WHERE isactive AND dixi
        AND p.id::text = c.program
        AND p.value = 'vtb24'
        AND cardOwner ilike '%' || ? || '%'
        LIMIT 15
    |]) [cardOwner]
  writeJSON $ mkMap ["cardOwner"] res

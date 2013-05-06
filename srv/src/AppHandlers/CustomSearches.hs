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
          getParam "srv")
    >>= writeJSON

selectPartnersForSrv :: MBS -> MBS -> MBS
                     -> AppHandler [Map ByteString ByteString]
selectPartnersForSrv city isActive service = do
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


selectContracts :: AppHandler ()
selectContracts = do
  dateFrom <- fromMaybe "1970-01-01" <$> getParam "from"
  dateTo   <- fromMaybe "2970-01-01" <$> getParam "to"
  Just prg <- getParam "program"
  Just usr <- with auth currentUser

  rows <- withPG pg_search $ \c -> query c (fromString
    $  "SELECT c.id::text,"
    ++ "  extract (epoch from ctime at time zone 'UTC')::int8::text,"
    ++ "  carVin, carMake, carModel, carColor, carPlateNum, cardNumber::text,"
    ++ "  carMakeYear::text, carCheckPeriod::text,"
    ++ "  extract (epoch from carBuyDate at time zone 'UTC')::int8::text,"
    ++ "  extract (epoch from warrantyStart at time zone 'UTC')::int8::text,"
    ++ "  extract (epoch from contractValidFromDate at time zone 'UTC')::int8::text,"
    ++ "  extract (epoch from contractValidUntilDate at time zone 'UTC')::int8::text,"
    ++ "  contractValidUntilMilage::text, milageTO::text, cardOwner, manager,"
    ++ "  carSeller, carDealerTO"
    ++ "  FROM contracttbl c, usermetatbl u"
    ++ "  WHERE u.login = ? AND ? = ANY (u.programs)"
    ++ "    AND (coalesce(u.isDealer,false) = false OR c.owner = u.login)"
    ++ "    AND c.program = ? AND date(ctime) between ? AND ?")
    (userLogin usr, prg, prg, dateFrom, dateTo)
  let fields =
        [ "id", "ctime", "carVin", "carMake", "carModel", "carColor"
        , "carPlateNum", "cardNumber", "carMakeYear", "carCheckPeriod"
        , "carBuyDate", "warrantyStart", "contractValidFromDate"
        , "contractValidUntilDate", "contractValidUntilMilage"
        , "milageTO", "cardOwner", "manager", "carSeller", "carDealerTO"
        ]
  writeJSON $ mkMap fields rows


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
    FROM casetbl
    WHERE
      lower(id::text
        || ' ' || to_char(callDate + '4:00','DD.MM.YYYY')
        || ' ' || coalesce(comment, '')
        || ' ' || coalesce(betaComment, '')
        || ' ' || coalesce(city, '')
        || ' ' || coalesce(dealerCause, '')
        || ' ' || coalesce(contact_name, '')
        || ' ' || coalesce(contact_phone1, '')
        || ' ' || coalesce(contact_phone2, '')
        || ' ' || coalesce(contact_phone3, '')
        || ' ' || coalesce(contact_phone4, '')
        || ' ' || coalesce(contact_ownerEmail, '')
        || ' ' || coalesce(contact_ownerName, '')
        || ' ' || coalesce(contact_ownerPhone1, '')
        || ' ' || coalesce(contact_ownerPhone2, '')
        || ' ' || coalesce(contact_ownerPhone3, '')
        || ' ' || coalesce(car_vin, '')
        || ' ' || coalesce(car_plateNum, '')
        || ' ' || coalesce(car_make, '')
        || ' ' || coalesce(car_model, '')
        || ' ' || coalesce(car_makeYear::text, '')
        || ' ' || coalesce(to_char(car_buyDate + '4:00','DD.MM.YYYY'), '')
        || ' ' || coalesce(to_char(car_checkupDate + '4:00','DD.MM.YYYY'), '')
        || ' ' || coalesce(car_seller, '')
        || ' ' || coalesce(car_dealerTO, '')
        || ' ' || coalesce(cardNumber_cardNumber, '')
        || ' ' || coalesce(cardNumber_cardOwner, '')
        || ' ' || coalesce(caseAddress_address, '')
        || ' ' || coalesce(program, '')
      ) like lower('%' || ? || '%')
    ORDER BY callDate DESC
    LIMIT 100
    |]) [q]
  writeJSON $ mkMap
    ["id", "contact_name", "callDate", "contact_phone1"
    ,"car_plateNum", "car_vin", "program", "comment"]
    rows

findSameContract :: AppHandler ()
findSameContract = do
  vin <- getParam "carVin"
  num <- getParam "cardNumber"
  rows <- withPG pg_search $ \c -> query_ c $ fromString
    $  " SELECT id::text, ctime::text"
    ++ " FROM contracttbl"
    ++ " WHERE ctime > now() - interval '30 days' AND (false "
    ++ (maybe "" (\x -> " OR carVin = "     ++ quote x) vin)
    ++ (maybe "" (\x -> " OR cardNumber = " ++ quote x) num)
    ++ ")"
  writeJSON $ mkMap ["id", "ctime"] rows
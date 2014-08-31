{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppHandlers.CustomSearches
    ( allActionsHandler
    , selectActions

    , searchCallsByPhone
    , getActionsForCase
    , getCancelsForCase

    , opStats
    , busyOps
    , actStats
    , boUsers

    , allDealersForMake
    , getLatestCases
    , searchCases
    , findSameContract

    , searchContracts
    )

where

import Control.Applicative
import Control.Monad

import Data.Aeson as A

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map as M (Map, (!), delete, fromList)
import Data.String (fromString)
import qualified Data.Vector as V

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Snap

import Application
import AppHandlers.CustomSearches.Contract
import AppHandlers.Util
import Utils.HttpErrors
import Util hiding (withPG)

import qualified Carma.Model.Role as Role

type MBS = Maybe ByteString



-- | Read @closed@, @assignedTo@, @targetGroup@ (comma-separated),
-- @duetimeFrom@, @duetimeTo@ (timestamps) parameters and serve a list
-- of matched actions.
allActionsHandler :: AppHandler ()
allActionsHandler = do
  let getRoles = do
          tg <- getParam "targetGroup"
          return $ B.split ',' <$> tg
  acts <- join (selectActions
          <$> getParam "closed"
          <*> getParamT "assignedTo"
          <*> getRoles
          <*> getParam "duetimeFrom"
          <*> getParam "duetimeTo")
  dn <- liftIO $ projNow id
  writeJSON $ A.object [ "actions" .= acts
                       , "reqTime" .= dn
                       ]


selectActions
  :: MBS -> Maybe Text -> Maybe [ByteString] -> MBS -> MBS
  -> AppHandler [Map Text Text]
selectActions mClosed mAssignee mRoles mFrom mTo = do
  let actQ = [sql|
     SELECT a.id::text, a.caseId, a.parentId,
           (a.closed::int)::text, a.name, a.assignedTo, a.targetGroup,
           (extract (epoch from a.duetime at time zone 'UTC')::int8)::text,
           (extract (epoch from a.ctime at time zone 'UTC')::int8)::text,
           (extract (epoch from a.assigntime at time zone 'UTC')::int8)::text,
           (extract (epoch from a.opentime at time zone 'UTC')::int8)::text,
           (extract (epoch from a.closetime at time zone 'UTC')::int8)::text,
           a.result, a.priority, a.description, a.comment,
           c.city, c.program::text,
           (extract (epoch from
             coalesce(s.times_expectedServiceStart, a.duetime)
              at time zone 'UTC')::int8)::text
     FROM
       (actiontbl a LEFT JOIN servicetbl s
         ON  s.id::text = substring(a.parentid, ':(.*)')
         AND s.type::text = substring(a.parentId, '(.*):')),
       casetbl c
     WHERE c.id::text = substring(a.caseId, ':(.*)')
     AND (? OR closed = ?)
     AND (? OR a.assignedTo = ?)
     AND (? OR targetGroup IN ?)
     AND (? OR extract (epoch from duetime) >= ?)
     AND (? OR extract (epoch from duetime) <= ?);
     |]
  rows <- withPG pg_search $ \c -> query c actQ $
          (sqlFlagPair False   (== "1") mClosed)               :.
          (sqlFlagPair ""      id       mAssignee)             :.
          (sqlFlagPair (In []) In       mRoles)                :.
          (sqlFlagPair 0       fst      (mFrom >>= B.readInt)) :.
          (sqlFlagPair 0       fst      (mTo >>= B.readInt))
  let fields
        = [ "id", "caseId", "parentId", "closed", "name"
          , "assignedTo", "targetGroup", "duetime"
          , "ctime", "assignTime", "openTime", "closeTime"
          , "result"
          , "priority", "description", "comment","city", "program"
          , "times_expectedServiceStart"]
  return $ mkMap fields rows


searchCallsByPhone :: AppHandler ()
searchCallsByPhone = do
  -- This two magic lines are required because `getParam` interprets
  -- '+' symbol in url as space
  -- FIXME: maybe better strip '+' from phones everywhere
  uri <- rqURI <$> getRequest
  let phone = last $ B.split '/' uri

  rows <- withPG pg_search $ \c -> query c (fromString
    $  "SELECT w.label, callerName_name, city, program::text, make, model,"
    ++ "       u.login, callType,"
    ++ "       extract (epoch from callDate at time zone 'UTC')::int8::text"
    ++ "  FROM calltbl c"
    ++ "  LEFT OUTER JOIN \"Wazzup\" w ON w.id = wazzup"
    ++ "  LEFT OUTER JOIN usermetatbl u ON u.id = c.calltaker"
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


opStatsQ :: Query
opStatsQ = [sql|
  SELECT u.login, ca.name, ca.caseId,
         (extract (epoch from ca.openTime)::int8)::text,
         (extract (epoch from ca.closeTime)::int8)::text
  FROM (SELECT a.*, row_number() OVER
        (PARTITION BY assignedto ORDER BY openTime DESC)
        FROM actiontbl a
        WHERE openTime IS NOT NULL) ca,
  usermetatbl u
  WHERE ca.row_number = 1
  AND u.login = ca.assignedTo
  AND (? :: int = ANY (u.roles))
  ORDER BY closeTime;
  |]


-- | Serve backoffice operator stats, containing last opened action
-- info for every user with backoffice role:
--
-- > {
-- >   reqTime: 123892,
-- >   stats: [{login:.., aName:.., caseId:.., openTime:.., closeTime:..}, ..]
-- > }
--
-- fields `aName`, `caseId`, `openTime`, `closeTime` and `reqTime`.
opStats :: AppHandler ()
opStats = do
  rows <- withPG pg_search $
          \c -> query c opStatsQ (Only Role.back)
  let obj = mkMap [ "login"
                  , "aName"
                  , "caseId"
                  , "openTime"
                  , "closeTime"]
            rows
      obj' = M.fromList $ map (\m -> (m ! "login", M.delete "login" m)) obj
  dn <- liftIO $ projNow id
  writeJSON $ A.object [ "reqTime" .= dn
                       , "stats" .= obj'
                       ]


busyOpsQ :: Query
busyOpsQ = [sql|
  SELECT assignedTo, count(1)::text
  FROM   actiontbl
  WHERE  closed = 'f'
  GROUP BY assignedTo
  HAVING   assignedTo is not null AND assignedTo != ''
  |]


busyOps :: AppHandler ()
busyOps = do
  rows <- withPG pg_search $ \c -> query_ c busyOpsQ
  writeJSON $ mkMap [ "login", "count"] rows


actStatsQ :: Query
actStatsQ = [sql|
  SELECT count(*)::text
  FROM actiontbl
  WHERE (assignedTo IS NULL OR assignedTo = '') AND closed = 'f'
  AND name = ANY (?)
  AND (? OR extract (epoch from duetime) >= ?)
  AND (? OR extract (epoch from duetime) <= ?);
  |]


-- | Serve JSON object with fields `order` and `control`, each
-- containing a number of unassigned actions in that category. Accepts
-- `duetimeFrom` and `duetimeTo` request parameters.
actStats :: AppHandler ()
actStats = do
  f <- getParam "duetimeFrom"
  t <- getParam "duetimeTo"
  let (fromF, fromDate) =
        case f of
          Just val -> (False, val)
          Nothing -> (True, "0")
  let (toF, toDate) =
        case t of
          Just val -> (False, val)
          Nothing -> (True, "0")
  let flags = (fromF, fromDate, toF, toDate)
      orderNames :: [ByteString]
      orderNames = [ "orderService"
                   , "orderServiceAnalyst"
                   , "tellMeMore"
                   , "callMeMaybe"
                   ]
      controlNames :: [ByteString]
      controlNames = [ "tellClient"
                     , "checkStatus"
                     , "tellDelayClient"
                     , "checkEndOfService"
                     ]
  (Only orders:_) <-
      withPG pg_search $
      \c -> query c actStatsQ ((Only $ V.fromList orderNames) :. flags)
  (Only controls:_) <-
      withPG pg_search $
      \c -> query c actStatsQ ((Only $ V.fromList controlNames) :. flags)
  writeJSON $ M.fromList
                ([ ("order", orders)
                 , ("control", controls)] :: [(Text, Text)])


-- | Serve users to which actions can be assigned (head, back or
-- supervisor roles, active within last 20 minutes).
boUsers :: AppHandler ()
boUsers = do
  rows <- withPG pg_search $ \c -> query c [sql|
    SELECT realname, login
      FROM usermetatbl
      WHERE (lastlogout IS NULL OR lastlogout < lastactivity)
        AND now() - lastactivity < '20 min'
        AND roles && (?)::int[];
    |] (Only $ V.fromList [Role.head, Role.back, Role.supervisor])
  writeJSON $ mkMap ["name", "login"] rows


allDealersForMake :: AppHandler ()
allDealersForMake = do
  Just make <- getParam "make"
  rows <- withPG pg_search $ \c -> query c [sql|
    SELECT id::text, name
      FROM partnertbl
      WHERE isActive AND isDealer AND ?::int = ANY (makes)
    |] [make]
  writeJSON $ mkMap ["id", "name"] rows


getLatestCases :: AppHandler ()
getLatestCases = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString $ [sql|
    SELECT
      casetbl.id::text, contact_name,
      extract (epoch from callDate at time zone 'UTC')::int8::text,
      contact_phone1, car_plateNum, car_vin, program::text, w.label
    FROM casetbl
    LEFT OUTER JOIN "Wazzup" w ON w.id = comment
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
      cs.id::text, contact_name,
      extract (epoch from callDate at time zone 'UTC')::int8::text,
      contact_phone1, car_plateNum, car_vin, program::text, w.label
    FROM CaseSearch(?) cs
    LEFT OUTER JOIN "Wazzup" w ON w.id = comment
    ORDER BY callDate DESC
    LIMIT 100
    |]) [q]
  writeJSON $ mkMap
    ["id", "contact_name", "callDate", "contact_phone1"
    ,"car_plateNum", "car_vin", "program", "comment"]
    rows


findSameContract :: AppHandler ()
findSameContract = do
  cvin <- getParam "vin"
  num  <- getParam "cardNumber"
  cid  <- getParam "id"

  case cid of
    Nothing  -> finishWithError 403 "need id param"
    Just id' -> do
      rows <- withPG pg_search $ \c -> query_ c $ fromString
        $  " SELECT id::text, to_char(ctime, 'YYYY-MM-DD HH24:MI')"
        ++ " FROM \"Contract\""
        ++ " WHERE ctime > now() - interval '30 days'"
        ++ " AND id != " ++ quote id'
        ++ "AND (false "
        ++ (maybe "" (\x -> " OR vin = "        ++ quote x) cvin)
        ++ (maybe "" (\x -> " OR cardNumber = " ++ quote x) num)
        ++ ")"
      writeJSON $ mkMap ["id", "ctime"] rows

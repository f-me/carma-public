{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppHandlers.CustomSearches
    (
      -- * Back office data
      allActionsHandler
    , selectActions
    , opStats
    , busyOps
    , actStats
    , boUsers
    , loggedUsers

      -- * Case screen
    , searchContracts
      -- ** History
    , caseHistory
    , partnerKPI

      -- ** CTI
    , findContractByPhone

      -- ** Helpers
    , allDealersForMake
    , getLatestCases
    , relevantCases
    , searchCases
    , findSameContract
    , suspendedServices
    , abandonedServices
    )

where

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Aeson as A hiding (json)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Map as M (Map, (!), delete, fromList)
import           Data.String (fromString)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Time.Clock

import           Database.Persist hiding (In)
import           Database.PostgreSQL.Simple hiding (query, query_)
import           Database.PostgreSQL.Simple.SqlQQ

import           Snap
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.PostgresqlSimple
import           Snaplet.Auth.PGUsers

import           Data.Model.Types
import           Data.Model.Utils.PostgreSQL.InterpolationHelpers
import           Data.Model.Utils.LegacyModel (rawBSFieldValueToIdent)

import qualified Carma.Model.ActionType              as AType
import           Carma.Model.Contract.Persistent
import qualified Carma.Model.Role                    as Role
import qualified Carma.Model.UserState               as UserState
import qualified Carma.Model.ServiceStatus           as ServiceStatus

import           AppHandlers.CustomSearches.Contract
import           AppHandlers.Users
import           Application
import           Utils.HttpErrors
import           Util


type MBS = Maybe ByteString


findContractByPhone :: AppHandler ()
findContractByPhone = do
  p <- maybe T.empty decodeUtf8 <$> getParam "phone"
  when (T.length p < 7) $
    finishWithError 403 "phone parameter must be at least 7 characters long"
  -- Generate several phone variants to have more luck with our
  -- contract database: for +7XXX we will search for +XXX, +7XXX and
  -- +8XXX.
  let phoneVariants =
        p:maybe [] (\s -> map (<> s) ["+8", "+"]) (T.stripPrefix "+7" p)
  res <- with db2 $ runPersist $
    selectList [ FilterOr $ map ((ContractPhone ==.) . Just) phoneVariants
               , ContractDixi     ==. True
               , ContractIsActive ==. True
               ]
    [Desc ContractCtime, LimitTo 1]
  writeJSON res


-- | Read @closed@, @assignedTo@, @targetGroup@ (comma-separated),
-- @duetimeFrom@, @duetimeTo@ (timestamps) parameters and serve a list
-- of matched actions.
allActionsHandler :: AppHandler ()
allActionsHandler = do
  let getRoles = do
          tg <- getParam "targetGroup"
          return $ B.split ',' <$> tg

  acts <- join $ selectActions
          <$> getParam "closed"
          <*> (fmap Ident <$> getIntParam "assignedTo")
          <*> getRoles
          <*> getParam "duetimeFrom"
          <*> getParam "duetimeTo"

  dn <- liftIO $ projNow id

  writeJSON $ A.object [ "actions" .= acts
                       , "reqTime" .= dn
                       ]


selectActions :: MBS
              -- ^ If @Just "1"@, only actions with non-null results
              -- will be selected. @Just "0"@ selects actions with no
              -- results. @Nothing@ ignores action results.
              -> Maybe (IdentI Usermeta) -> Maybe [ByteString] -> MBS -> MBS
              -> AppHandler [Map Text Text]
selectActions mClosed mAssignee mRoles mFrom mTo = do
  let nid = Ident 0
      clToRes :: ByteString -> PlainText
      clToRes "1" = PT "NOT"
      clToRes _   = PT ""

      actQ = [sql|
        SELECT a.id::text
             , coalesce(a.caseId, calltbl.caseId)::text
             , a.serviceId::text
             , s.type::text
             , a.type::text
             , a.assignedTo::text
             , a.targetGroup::text
             , (extract(epoch from a.duetime at time zone 'UTC')::int8)::text
             , (extract(epoch from a.ctime at time zone 'UTC')::int8)::text
             , (extract(epoch from a.assigntime at time zone 'UTC')::int8)::text
             , (extract(epoch from a.opentime at time zone 'UTC')::int8)::text
             , (extract(epoch from a.closetime at time zone 'UTC')::int8)::text
             , a.result::text
             , at.priority::text
             , a.comment
             , c.city::text
             , c.program::text

             , (extract(
                 epoch from
                 coalesce(s.times_expectedServiceStart, a.duetime)
                 at time zone 'UTC'
               )::int8)::text

             , ( coalesce(
                   extract(epoch from a.closetime at time zone 'UTC')::int8,
                   extract(epoch from now() at time zone 'UTC')::int8
                 ) - extract(epoch from a.ctime at time zone 'UTC')::int8
               )::text

        FROM actiontbl a LEFT JOIN servicetbl s ON  s.id = a.serviceId
                         LEFT JOIN casetbl c ON c.id = a.caseId
                         LEFT JOIN calltbl ON calltbl.id = a.callId

           , "ActionType" at

        WHERE at.id = a.type
          AND (? OR result IS ? NULL)
          AND (? OR a.assignedTo = ?)
          AND (? OR targetGroup IN ?)
          AND (? OR extract (epoch from duetime) >= ?)
          AND (? OR extract (epoch from duetime) <= ?)
     |]

  rows <- query actQ
       $  sqlFlagPair (PT "") clToRes  mClosed
       :. sqlFlagPair nid     id       mAssignee
       :. sqlFlagPair (In []) In       mRoles
       :. sqlFlagPair 0       fst      (mFrom >>= B.readInt)
       :. sqlFlagPair 0       fst      (mTo >>= B.readInt)

  let fields = [ "id", "caseId", "serviceId", "serviceType", "name"
               , "assignedTo", "targetGroup", "duetime"
               , "ctime", "assignTime", "openTime", "closeTime"
               , "result"
               , "priority", "comment","city", "program"
               , "times_expectedServiceStart"
               , "executionDuration"
               ]

  return $ mkMap fields rows


opStatsQ :: Query
opStatsQ = [sql|
  SELECT u.login, ca.type::text, ca.caseId::text,
         (extract (epoch from ca.openTime)::int8)::text,
         (extract (epoch from ca.closeTime)::int8)::text
  FROM (SELECT a.*, row_number() OVER
        (PARTITION BY assignedto ORDER BY openTime DESC)
        FROM actiontbl a
        WHERE openTime IS NOT NULL) ca,
  usermetatbl u
  WHERE ca.row_number = 1
  AND u.id = ca.assignedTo
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
  rows <- query opStatsQ (Only Role.back)
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
  SELECT assignedTo::text, count(1)::text
  FROM   actiontbl
  WHERE  result IS NOT NULL
  GROUP BY assignedTo
  HAVING   assignedTo is not null
  |]


busyOps :: AppHandler ()
busyOps = do
  rows <- query_ busyOpsQ
  writeJSON $ mkMap [ "login", "count"] rows


actStatsQ :: Query
actStatsQ = [sql|
  SELECT count(*)::text
  FROM actiontbl
  WHERE (assignedTo IS NULL) AND result IS NULL
  AND type IN ?
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
      orderNames = [ AType.orderService
                   , AType.orderServiceAnalyst
                   , AType.tellMeMore
                   , AType.callMeMaybe
                   ]
      controlNames = [ AType.tellClient
                     , AType.checkStatus
                     , AType.cancelService
                     , AType.checkEndOfService
                     , AType.makerApproval
                     , AType.tellMakerDeclined
                     ]
  (Only orders:_) <-
      query actStatsQ $ (Only $ In orderNames) :. flags
  (Only controls:_) <-
      query actStatsQ $ (Only $ In controlNames) :. flags
  writeJSON $ M.fromList
                ([ ("order", orders)
                 , ("control", controls)] :: [(Text, Text)])


boUsers :: AppHandler ()
boUsers
  = [Role.head, Role.back, Role.supervisor] `usersInStates` [UserState.Ready]

loggedUsers :: AppHandler ()
loggedUsers = do
  Just role <- (>>= rawBSFieldValueToIdent) <$> getParam "role"
  usersInStates [role]
    [ UserState.Ready, UserState.Busy
    , UserState.Rest, UserState.Dinner
    , UserState.ServiceBreak, UserState.NA
    ]

allDealersForMake :: AppHandler ()
allDealersForMake = do
  Just make <- getParam "make"
  rows <- query [sql|
    SELECT id::text, name
      FROM partnertbl
      WHERE isActive AND isDealer AND ?::int = ANY (makes)
    |] [make]
  writeJSON $ mkMap ["id", "name"] rows


getLatestCases :: AppHandler ()
getLatestCases = do
  rows <- query_ $ [sql|
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

relevantCases :: AppHandler ()
relevantCases = do
  Just c <- getParam "caseId"
  rows <- query ([sql|
    SELECT
      c2.id::text,
      to_char(c2.callDate, 'YYYY-MM-DD HH24:MI')
    FROM casetbl c1, casetbl c2
    WHERE c1.id = ?
      AND c2.id <> c1.id
      AND lower(c1.contractIdentifier) = lower(c2.contractIdentifier)
      AND length(c2.contractIdentifier) > 4
    ORDER BY c2.callDate DESC
    LIMIT 25
    |]) [c]
  writeJSON $ mkMap ["caseId", "caseDate"] rows

searchCases :: AppHandler ()
searchCases = do
  Just q <- getParam "q"
  rows <- query ([sql|
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


caseHistory :: AppHandler ()
caseHistory = do
  caseId <- getIntParam "caseId"
  limit  <- fromMaybe 50 <$> getIntParam "limit"
  rows <- query
          [sql|SELECT datetime, who, json FROM "CaseHistory" WHERE caseId = ? LIMIT ?|]
          (caseId, limit)
  writeJSON (rows :: [(UTCTime, Text, Value)])


partnerKPI :: AppHandler ()
partnerKPI = do
  svcId     <- getIntParam "svcid"
  partnerId <- getIntParam "partnerid"
  [[json]]  <- query [sql| WITH
                             p AS (SELECT (GetPartnerPayment(?, NULL, NULL)).*)
                           SELECT coalesce(row_to_json(p.*), '{}') FROM p
                           WHERE partnerId = ? |]
                     (svcId, partnerId)
  writeJSON (json :: Value)


findSameContract :: AppHandler ()
findSameContract = do
  cvin <- getParam "vin"
  num  <- getParam "cardNumber"
  cid  <- getParam "id"

  case cid of
    Nothing  -> finishWithError 403 "need id param"
    Just id' -> do
      rows <- query_ $ fromString
        $  " SELECT c.id::text, to_char(c.ctime, 'YYYY-MM-DD HH24:MI')"
        ++ " FROM \"Contract\" c, \"Contract\" same"
        ++ " WHERE c.dixi AND c.ctime > now() - interval '30 days'"
        ++ " AND c.id != " ++ quote id'
        ++ " AND same.id = " ++ quote id'
        ++ " AND c.subprogram = same.subprogram"
        ++ " AND (false "
        ++ (maybe "" (\x -> " OR c.vin = "        ++ quote x) cvin)
        ++ (maybe "" (\x -> " OR c.cardNumber = " ++ quote x) num)
        ++ ")"
      writeJSON $ mkMap ["id", "ctime"] rows


suspendedServices :: AppHandler ()
suspendedServices = do
  svcs <- query [sql|
    select row_to_json(x) from
      (select
          s.parentid as "caseId",
          s.id as "svcId",
          t.label as "type"
        from servicetbl s
          join "ServiceType" t on (s.type = t.id)
        where status = ?
        order by createtime desc nulls last) x
    |] [ServiceStatus.suspended]
  writeJSON (map fromOnly svcs :: [A.Value])


abandonedServices :: AppHandler ()
abandonedServices = do
  usr <- getParam "usr"
  svcs <- query [sql|
    select row_to_json(x) from
      (select
          extract(epoch from createtime) :: int as ctime,
          s.parentid as "caseId",
          s.id as "svcId",
          t.label as "type",
          owner as "userId"
        from servicetbl s
          join "ServiceType" t on (s.type = t.id)
        where status = ?
          and (not ? or owner = ?)
        order by createtime desc nulls last) x
    |] (ServiceStatus.creating, isJust usr, usr)
  writeJSON (map fromOnly svcs :: [A.Value])

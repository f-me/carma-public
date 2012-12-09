
module AppHandlers.CustomSearches where

import Control.Applicative
import Data.String (fromString)

import Data.Map (Map)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Snap
import Database.PostgreSQL.Simple
--------------------------------------------------------------------
import Application
import AppHandlers.Util


type MBS = Maybe ByteString


allPartnersHandler :: AppHandler ()
allPartnersHandler
  = join (selectPartners
    <$> getParam "city"
    <*> getParam "isActive"
    <*> getParam "isDealer")
  >>= writeJSON


selectPartners :: MBS -> MBS -> MBS -> AppHandler [Map ByteString ByteString]
selectPartners city isActive isDealer = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString
    $  "SELECT id::text, name, city,"
    ++ "       comment, addrDeFacto, phone1, workingTime,"
    ++ "       (isDealer::int)::text, (isMobile::int)::text"
    ++ "  FROM partnertbl WHERE true"
    ++ (maybe "" (\x -> "  AND city = " ++ quote x) city)
    ++ (maybe "" (\x -> "  AND isActive = " ++ toBool x) isActive)
    ++ (maybe "" (\x -> "  AND isDealer = " ++ toBool x) isDealer)
  let fields =
        ["id","name","city","comment" ,"addrDeFacto"
        ,"phone1","workingTime","isDealer","isMobile"
        ]
  return $ mkMap fields rows



allActionsHandler :: AppHandler ()
allActionsHandler
  = join (selectActions
    <$> getParam "closed"
    <*> pure Nothing
    <*> getParam "targetGroup"
    <*> getParam "duetimeFrom"
    <*> getParam "duetimeTo")
  >>= writeJSON


selectActions
  :: MBS -> MBS -> MBS -> MBS -> MBS
  -> AppHandler [Map ByteString ByteString]
selectActions mClosed mAssignee mRole mFrom mTo = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString
    $  "SELECT id::text, caseId, parentId,"
    ++ "       (closed::int)::text, name, assignedTo, targetGroup,"
    ++ "       (extract (epoch from duetime at time zone 'UTC')::int)::text, "
    ++ "       result, priority, description, comment"
    ++ "  FROM actiontbl WHERE true"
    ++ (maybe "" (\x -> "  AND closed = " ++ toBool x) mClosed)
    ++ (maybe "" (\x -> "  AND assignedTo = " ++ quote x) mAssignee)
    ++ (maybe "" (\x -> "  AND targetGroup = " ++ quote x) mRole)
    ++ (maybe "" (\x -> "  AND extract (epoch from duetime) >= " ++ int x) mFrom)
    ++ (maybe "" (\x -> "  AND extract (epoch from duetime) <= " ++ int x) mTo)
  let fields
        = ["id", "caseId", "parentId", "closed", "name"
          ,"assignedTo", "targetGroup", "duetime", "result"
          ,"priority", "description", "comment"]
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
    ++ "       extract (epoch from callDate at time zone 'UTC')::int::text"
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
    $  "SELECT extract (epoch from closeTime at time zone 'UTC')::int::text,"
    ++ "       result, name, assignedTo, comment"
    ++ "  FROM actiontbl"
    ++ "  WHERE closeTime IS NOT NULL AND caseId = ?") [caseId']
  let fields =
        ["closeTime", "result", "name", "assignedTo", "comment"]
  writeJSON $ mkMap fields rows

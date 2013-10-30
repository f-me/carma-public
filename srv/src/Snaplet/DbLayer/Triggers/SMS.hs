{-# LANGUAGE QuasiQuotes #-}
module Snaplet.DbLayer.Triggers.SMS where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad
import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe
import Data.Time
import System.Locale

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl

import qualified Snap.Snaplet.PostgresqlSimple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Data.Model as Model
import Carma.Model.SmsTemplate (SmsTemplate)

import DictionaryCache
import Util as U


formatDate :: String -> IO Text
formatDate unix = do
  res <- try $ utcToLocalZonedTime $ readTime defaultTimeLocale "%s" unix
  return $ case res :: Either SomeException ZonedTime of
    Right tm -> T.pack $ formatTime defaultTimeLocale "%F %R" tm
    Left  _  -> "неизвестно"


parseInt :: ByteString -> Int
parseInt txt = res
  where
    Right (res,_) =  T.decimal $ T.decodeUtf8 txt


sendSMS :: MonadTrigger m b => ByteString -> Model.Ident SmsTemplate -> m b ()
sendSMS actId tplId = do
  dic <- liftDb $ getDict id

  svcId  <- actId  `get` "parentId"
  caseId <- svcId  `get` "parentId"
  phone  <- caseId `get` "contact_phone1"

  opName  <- T.decodeUtf8 <$> actId `get` "assignedTo"
  cityVal <- T.decodeUtf8 <$> caseId `get` "city"
  program <- T.decodeUtf8 <$> caseId `get` "program"
  let sender = T.encodeUtf8
        $ Map.findWithDefault "RAMC" program
        $ smsTokenVal dic Map.! "program_from_name"

  let pInfo  = Map.lookup program $ smsTokenVal dic Map.! "program_info"
  let pCInfo = Map.lookup program $ smsTokenVal dic Map.! "program_contact_info"
  when (isJust pInfo && isJust pCInfo) $ do
    eSvcTm <- svcId `get` "times_expectedServiceStart"
    eSvcStart <- liftIO $ formatDate $ T.unpack $ T.decodeUtf8 eSvcTm
    fSvcTm <- svcId `get` "times_factServiceStart"
    fSvcStart <- liftIO $ formatDate $ T.unpack $ T.decodeUtf8 fSvcTm

    let varMap = Map.fromList
          [("program_info", fromJust pInfo)
          ,("program_contact_info", fromJust pCInfo)
           -- TODO Use actual realName from user meta
          ,("case.backoperator_name", opName)
          ,("case.city", Map.findWithDefault "Город" cityVal $ city dic)
          ,("case.id", (!!1) . T.splitOn ":" $ T.decodeUtf8 caseId)
          ,("service.times_factServiceStart", fSvcStart)
          ,("service.times_expectedServiceStart", eSvcStart)
          ]
    [[templateText]] <- liftDb $ PG.query
      [sql| select text from "SmsTemplate" where id = ? |]
      [tplId]
    let msg = T.encodeUtf8 $ U.render varMap templateText

    now <- dateNow id
    smsId <- new "sms" $ Map.fromList
      [("ctime", now)
      ,("caseId", caseId)
      ,("svcId", svcId)
      ,("phone", phone)
      ,("template", T.encodeUtf8 $ T.pack $ show $ identVal tplId)
      ,("auto", "true")
      ,("msg", msg)
      ,("sender", sender)
      ]
    Right _ <- redisLPush "smspost" [smsId]
    return ()

updateSMS :: MonadTrigger m b => ObjectId -> m b ()
updateSMS smsId = do
  auto <- get smsId "auto"
  when (auto /= "true") $ do
    caseNum <- get smsId "caseId"
    let caseId = B.append "case:" caseNum

    let add x i y m = do
          yVal <- T.decodeUtf8 <$> get i y
          return $! Map.insert x yVal m
    varMap <- return Map.empty
      >>= return . Map.insert "case.id" (T.decodeUtf8 caseNum)
      >>= add "case.contact_name" caseId "contact_name"
      >>= add "case.caseAddress_address" caseId "caseAddress_address"

    _ <- get smsId "template"
    tplId <- get smsId "template"
    [[tmp]] <- liftDb $ PG.query
      [sql| select text from "SmsTemplate" where id = ?::int |]
      [tplId]

    msg <- get smsId "msg"
    when (msg == "" && tmp /= "") $ do
      let txt = T.encodeUtf8 $ U.render varMap tmp
      set smsId "msg" txt

    phone <- get smsId "phone"
    when (phone == "") $ do
      get caseId "contact_phone1" >>= set smsId "phone"

    dic <- liftDb $ getDict id
    program <- T.decodeUtf8 <$> caseId `get` "program"
    let sender = T.encodeUtf8
          $ Map.findWithDefault "RAMC" program
          $ smsTokenVal dic Map.! "program_from_name"
    set smsId "sender" sender

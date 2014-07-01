{-# LANGUAGE QuasiQuotes #-}
module Snaplet.DbLayer.Triggers.MailToDealer
  ( sendMailToDealer
  , tryRepTowageMail
  ) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad
import Control.Concurrent

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isNumber)

import Data.Configurator (require)
import Network.Mail.Mime

import Carma.HTTP

import Data.Model as Model

import qualified Carma.Model.PaymentType as PT
import qualified Carma.Model.Program as Program

import AppHandlers.PSA.Base

import qualified Snap.Snaplet.PostgresqlSimple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Snap.Snaplet (getSnapletUserConfig)
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import Snaplet.DbLayer.Types

import Util as U

-- FIXME: store this in DB or in file
mailTemplate :: Text
mailTemplate = T.pack
  $  "<p>На территорию Вашего ДЦ был доставлен а/м по программе Assistance.</p>"
  ++ "<br />"
  ++ "Кейс в РАМК: $caseId$<br />"
  ++ "VIN номер: $car_vin$<br />"
  ++ "Госномер: $car_plateNum$<br />"
  ++ "Дата доставки а/м: $caseDate$<br />"
  ++ "Марка: $car_make$<br />"
  ++ "Модель: $car_model$<br />"
  ++ "Неисправность со слов Клиента: $wazzup$<br />"
  ++ "<p> Просим Вас предоставить дополнительную информацию, после диагностики "
  ++ "а/м в виде таблицы на электронный адрес psa@ruamc.ru :</p>"
  ++ "<table border=\"1\">"
  ++ "  <tr bgcolor=\"SkyBlue\">"
  ++ "    <th>Код дилера</th>"
  ++ "    <th>VIN номер автомобиля</th>"
  ++ "    <th>Пробег а/м на момент поломки</th>"
  ++ "    <th>Номер заказа-наряда ремонта у дилера</th>"
  ++ "    <th>Время/Дата поступления автомобиля</th>"
  ++ "    <th>Время/дата диагностики</th>"
  ++ "    <th>Запланированное время/дата окончания работ</th>"
  ++ "    <th>Реальное время/дата окончания работ</th>"
  ++ "    <th>Гарантия / негарантия</th>"
  ++ "    <th>Описание причины неисправности</th>"
  ++ "    <th>Система автомобиля, в которой произошла неисправность</th>"
  ++ "    <th>Неисправная деталь</th>"
  ++ "  </tr>"
  ++ "  <tr align=\"center\">"
  ++ "    <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>"
  ++ "    <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>"
  ++ "  </tr>"
  ++ "  <tr align=\"center\">"
  ++ "    <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>"
  ++ "    <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>"
  ++ "  </tr>"
  ++ "  <tr align=\"center\">"
  ++ "    <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>"
  ++ "    <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>"
  ++ "  </tr>"
  ++ "</table>"
  ++ "<p>Заранее благодарим за своевременный ответ, в течение 24 часов.</p>"


fillVars :: MonadTrigger m b => ObjectId -> m b (Map Text Text)
fillVars caseId
  =   (return $ Map.empty)
  >>= add "caseId"       (return $ T.filter isNumber caseId)
  >>= add "caseDate"     (get caseId "callDate" >>= U.formatTimestamp)
  >>= add "car_vin"      (get caseId "car_vin")
  >>= add "car_plateNum" (get caseId "car_plateNum")
  >>= add "wazzup"       (get caseId "customerComment")
  >>= add "car_make"     (sqlRes <$> carMake)
  >>= add "car_model"    (sqlRes <$> carModel)
  where
    add k f m = f >>= \v -> return (Map.insert k v m)
    sqlRes = \case {[[res]] -> res; _ -> ""}
    carModel = liftDb $ PG.query
      [sql|
        select md.label
        from casetbl cs, "CarModel" md
        where cs.id = substring(?, ':(.*)') :: int
          and md.id = cs.car_model |]
      [caseId]
    carMake = liftDb $ PG.query
      [sql|
        select mk.label
        from casetbl cs, "CarMake" mk
        where cs.id = substring(?, ':(.*)') :: int
          and mk.id = cs.car_make |]
      [caseId]


sendMailToDealer :: MonadTrigger m b => ObjectId -> m b ()
sendMailToDealer actionId = do
  svcId  <- get actionId "parentId"
  let svcName = head $ T.splitOn ":" svcId
  when (svcId /= "" && svcName == "towage") $ do
    caseId  <- get actionId "caseId"
    program <- get caseId   "program"
    when (program `elem` (map identFv [Program.peugeot, Program.citroen])) $ do
      payType <- get svcId "payType"
      when (payType `elem` (map identFv [PT.ruamc, PT.mixed, PT.refund])) $ do
        dealerId <- get svcId "towDealer_partnerId"
        when (dealerId /= "") $ do
          dms <- T.encodeUtf8 <$> get dealerId "emails"
          let mails = getAllKeyedJsonValues dms "close"
          when (mails /= []) $ do
            sendMailActually actionId caseId $
              B.intercalate "," mails

sendMailActually
  :: MonadTrigger m b
  => ObjectId -> ObjectId -> ByteString -> m b ()
sendMailActually actId caseId addrTo = do
  cfg <- liftDb getSnapletUserConfig
  cfgFrom <- liftIO $ require cfg "psa-smtp-from"
  cfgTo'  <- liftIO $ require cfg "psa-smtp-copyto"
  cfgReply<- liftIO $ require cfg "psa-smtp-replyto"
  let cfgTo = if cfgTo' /= ""
        then T.decodeUtf8 addrTo `T.append` "," `T.append` cfgTo'
        else cfgTo'

  varMap <- fillVars caseId

  let body = Part "text/html; charset=utf-8"
        QuotedPrintableText Nothing []
        (TL.encodeUtf8 $ TL.fromStrict $ U.render varMap mailTemplate)

  let subj = "Доставлена машина на ремонт / " `T.append` actId
  -- NB. notice `forkIO` below
  -- it also saves us from exceptions thrown while sending an e-mail
  void $ liftIO $ forkIO $ do
    syslogJSON Info "trigger/mailToDealer/sendMailToDealer" ["caseId" .= caseId]
    logExceptions "trigger/mailToDealer/sendMailToDealer"
      $ sendEximMail cfgFrom cfgTo cfgReply subj body


-- | If a case has towage services which is a repeated towage, send a
-- mail to PSA.
tryRepTowageMail :: MonadTrigger m b =>
                    ObjectId
                 -- ^ A reference to a case in the trigger monad
                 -- context.
                 -> m b ()
tryRepTowageMail caseRef = do
  serviceRefs <- T.splitOn "," <$> get caseRef "services"

  -- Extract references and proceed to sendRepTowageMail, which
  -- actually does the job.

  -- Check if a service is a towage
  forM_ serviceRefs $ \svcRef ->
    case T.splitOn ":" svcRef of
      "towage":t:_ -> do
          -- Extract corresponding case id.
          case T.splitOn ":" caseRef of
            "case":n:_ ->
                case ((T.decimal :: T.Reader Int) t, T.decimal n) of
                  (Right (_ , _), Right (cid, _)) -> do
                      prevRefs <- liftDb $ repTowages cid
                      program <- get caseRef "program"
                      let ident | program == identFv Program.citroen =
                                   Just Program.citroen
                                | program == identFv Program.peugeot =
                                    Just Program.peugeot
                                | otherwise = Nothing
                      case (prevRefs, ident) of
                        ([], _) -> return ()
                        (pr, Just i) ->
                          sendRepTowageMail caseRef svcRef (last pr) i
                        _ -> return ()
                  _ -> return ()
            _ -> return ()
      _ -> return ()


-- | Send a mail to PSA, reporting on a repeated towage.
sendRepTowageMail :: MonadTrigger m b =>
                     ObjectId
                  -- ^ Case reference.
                  -> ObjectId
                  -- ^ Towage service reference.
                  -> ObjectId
                  -- ^ Reference to a previous service for this car.
                  -> (IdentI Program.Program)
                  -> m b ()
sendRepTowageMail caseRef towageRef prevRef program = do
  cfg      <- liftDb getSnapletUserConfig

  mailFrom <- liftIO $ require cfg "reptowage-smtp-from"
  replyTo  <- liftIO $ require cfg "psa-smtp-replyto"
  citrTo   <- liftIO $ require cfg "reptowage-citroen-recipients"
  peugTo   <- liftIO $ require cfg "reptowage-peugeot-recipients"
  copyTo   <- liftIO $ require cfg "psa-smtp-copyto"
  bodyTpl  <- liftIO $ require cfg "reptowage-template"

  -- Gather data and render mail body
  vin <- get caseRef "car_vin"
  twgData <- readObject towageRef
  prevData <- readObject prevRef

  let timeField = "times_expectedServiceStart"
  timesFirst <- U.formatTimestamp
             =<< return (Map.findWithDefault "-" timeField prevData)
  timesSecond <- U.formatTimestamp
              =<< return (Map.findWithDefault "-" timeField twgData)

  tplContext <- Map.insert "times_first" timesFirst .
                Map.insert "times_second" timesSecond <$>
                fillVars caseRef

  let mailText = U.render tplContext bodyTpl
      mailBody = Part "text/plain; charset=utf-8"
                 QuotedPrintableText Nothing []
                 (TL.encodeUtf8 $ TL.fromStrict mailText)

      -- Pick recipient and subject line depending on case program
      (mailTo, subjPrefix) | program == Program.citroen = (citrTo, "FRRM01R")
                           | program == Program.peugeot = (peugTo, "RUMC01R")
      mailSubj = T.concat [subjPrefix, " ", vin]

  -- Fire off mail-sending process
  void $ liftIO $ forkIO $ do
       syslogJSON Info "trigger/mailToDealer/sendRepTowageMail" ["caseId" .= caseRef]
       logExceptions "trigger/mailToDealer/sendRepTowageMail"
         $ sendEximMail
             mailFrom (T.concat [mailTo, ",", copyTo])
             replyTo mailSubj mailBody


-- | Send a mail using exim.
sendEximMail :: Text -> Text -> Text -> Text -> Part -> IO ()
sendEximMail mailFrom mailTo mailReplyTo mailSubj mailBody =
    renderSendMailCustom "/usr/sbin/sendmail" ["-t", "-r", T.unpack mailFrom] $
    (emptyMail $ Address Nothing mailFrom)
    { mailTo = map (Address Nothing . T.strip) $ T.splitOn "," mailTo
    , mailHeaders = [("Subject", mailSubj), ("Reply-To", mailReplyTo)]
    , mailParts = [[mailBody]]
    }

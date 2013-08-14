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
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isNumber)

import System.Log.Simple
import System.Log.Simple.Base (scoperLog)
import Data.Configurator (require)
import Network.Mail.Mime

import Carma.HTTP

import AppHandlers.PSA.Base

import Snap.Snaplet (getSnapletUserConfig)
import Snaplet.DbLayer.Types (getDict)
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import DictionaryCache

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


fillVars :: MonadTrigger m b => ByteString -> m b (Map Text Text)
fillVars caseId
  =   (return $ Map.empty)
  >>= add "caseId"       (return $ T.filter isNumber $ txt caseId)
  >>= add "caseDate"     (get caseId "callDate" >>= U.formatTimestamp)
  >>= add "car_vin"      (txt <$> get caseId "car_vin")
  >>= add "car_plateNum" (txt <$> get caseId "car_plateNum")
  >>= add "wazzup"       (get caseId "comment"   >>= tr wazzup . txt)
  >>= add "car_make"     (get caseId "car_make"  >>= tr carMake . txt)
  >>= add "car_model"    getCarModel
  -- TODO Refactor this to a separate monad
  where
    txt = T.decodeUtf8
    add k f m = f >>= \v -> return (Map.insert k v m)
    tr d v = Map.findWithDefault v v <$> liftDb (getDict d)
    getCarModel = do
      mk <- txt <$> get caseId "car_make"
      md <- txt <$> get caseId "car_model"
      dc <- liftDb $ getDict carModel
      return
        $ Map.findWithDefault md md
        $ Map.findWithDefault Map.empty mk dc

sendMailToDealer :: MonadTrigger m b => ByteString -> m b ()
sendMailToDealer actionId = do
  svcId  <- get actionId "parentId"
  let svcName = head $ B.split ':' svcId
  when (svcId /= "" && svcName == "towage") $ do
    caseId  <- get actionId "caseId"
    program <- get caseId   "program"
    when (program `elem` ["peugeot", "citroen"]) $ do
      payType <- get svcId "payType"
      when (payType `elem` ["ruamc", "mixed", "refund"]) $ do
        dealerId <- get svcId "towDealer_partnerId"
        when (dealerId /= "") $ do
          dms <- get dealerId "emails"
          let mails = getAllKeyedJsonValues dms "fact"
          when (mails /= []) $ do
            sendMailActually actionId caseId $
              B.intercalate "," mails

sendMailActually
  :: MonadTrigger m b
  => ByteString -> ByteString -> ByteString -> m b ()
sendMailActually actId caseId addrTo = do
  cfg <- liftDb getSnapletUserConfig
  cfgFrom <- liftIO $ require cfg "psa-smtp-from"
  cfgTo'  <- liftIO $ require cfg "psa-smtp-copyto"
  let cfgTo = if cfgTo' /= ""
        then T.decodeUtf8 addrTo `T.append` "," `T.append` cfgTo'
        else cfgTo'

  varMap <- fillVars caseId

  let body = Part "text/html; charset=utf-8"
        QuotedPrintableText Nothing []
        (TL.encodeUtf8 $ TL.fromStrict $ U.render varMap mailTemplate)

  let subj = "Доставлена машина на ремонт / " `T.append` T.decodeUtf8 actId
  l <- liftDb askLog
  -- NB. notice `forkIO` below
  -- it also saves us from exceptions thrown while sending an e-mail
  void $ liftIO $ forkIO
    $ scoperLog l (T.concat ["sendMailToDealer(", T.decodeUtf8 caseId, ")"])
    $ sendEximMail cfgFrom cfgTo subj body


-- | If a case has towage services which is a repeated towage, send a
-- mail to PSA.
tryRepTowageMail :: MonadTrigger m b =>
                    ByteString
                 -- ^ A reference to a case in the trigger monad
                 -- context.
                 -> m b ()
tryRepTowageMail caseRef = do
  serviceRefs <- B.split ',' <$> get caseRef "services"

  -- Extract references and proceed to sendRepTowageMail, which
  -- actually does the job.

  -- Check if a service is a towage
  forM_ serviceRefs $ \svcRef ->
    case B.split ':' svcRef of
      "towage":t:_ -> do
          -- Extract corresponding case id.
          case B.split ':' caseRef of
            "case":n:_ ->
                case (B.readInt t, B.readInt n) of
                  (Just (_, _), Just (cid, _)) -> do
                      prevRefs <- liftDb $ repTowages cid
                      program <- get caseRef "program"
                      case (prevRefs, program) of
                        ([], _) -> return ()
                        (pr, "citroen") ->
                          sendRepTowageMail caseRef svcRef (last pr) Citroen
                        (pr, "peugeot") ->
                          sendRepTowageMail caseRef svcRef (last pr) Peugeot
                        _ -> return ()
                  _ -> return ()
            _ -> return ()
      _ -> return ()


data PSAProgram = Citroen
                | Peugeot


-- | Send a mail to PSA, reporting on a repeated towage.
sendRepTowageMail :: MonadTrigger m b =>
                     ByteString
                  -- ^ Case reference.
                  -> ByteString
                  -- ^ Towage service reference.
                  -> ByteString
                  -- ^ Reference to a previous service for this car.
                  -> PSAProgram
                  -> m b ()
sendRepTowageMail caseRef towageRef prevRef program = do
  cfg      <- liftDb getSnapletUserConfig

  mailFrom <- liftIO $ require cfg "reptowage-smtp-from"
  citrTo   <- liftIO $ require cfg "reptowage-citroen-recipients"
  peugTo   <- liftIO $ require cfg "reptowage-peugeot-recipients"
  bodyTpl  <- liftIO $ require cfg "reptowage-template"

  -- Gather data and render mail body
  vin <- T.decodeUtf8 <$> get caseRef "car_vin"
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
      (mailTo, subjPrefix) =
          case program of
            Citroen -> (citrTo, "FRRM01R")
            Peugeot -> (peugTo, "RUMC01R")
      mailSubj = T.concat [subjPrefix, " ", vin]

  l <- liftDb askLog

  -- Fire off mail-sending process
  void $ liftIO $ forkIO $
       scoperLog l (T.append "sendRepTowageMail " $ T.decodeUtf8 caseRef) $
       sendEximMail mailFrom mailTo mailSubj mailBody


-- | Send a mail using exim.
sendEximMail :: Text -> Text -> Text -> Part -> IO ()
sendEximMail mailFrom mailTo mailSubj mailBody =
    renderSendMailCustom "/usr/sbin/exim" ["-t", "-r", T.unpack mailFrom] $
    (emptyMail $ Address Nothing mailFrom)
    { mailTo = map (Address Nothing . T.strip) $ T.splitOn "," mailTo
    , mailHeaders = [("Subject", mailSubj)]
    , mailParts = [[mailBody]]
    }


{-# LANGUAGE FlexibleInstances #-}
module Snaplet.DbLayer.Triggers.MailToDealer where

import Control.Monad.Trans (lift,liftIO)
import Control.Monad
import Control.Concurrent

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text.Lazy (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import Data.Map (Map)
import qualified Data.Map as Map
import System.Log.Simple
import System.Log.Simple.Base (scoperLog)
import Data.Configurator (require)
import Network.Mail.SMTP

import Snap.Snaplet (getSnapletUserConfig)
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl


-- FIXME: store this in DB or in file
mailTemplate :: Text
mailTemplate = T.pack
  $  "<p>На территорию Вашего ДЦ был доставлен а/м по программе Assistance.</p>"
  ++ "<table>"
  ++ "  <tr><td>Кейс в РАМК</td><td>$caseId$</td></tr>"
  ++ "  <tr><td>VIN номер</td><td>$car_vin$</td></tr>"
  ++ "  <tr><td>Госномер</td><td>$car_plateNum$</td></tr>"
  ++ "  <tr><td>Дата доставки а/м</td><td>$caseDate$</td></tr>"
  ++ "  <tr><td>Марка</td><td>$car_make$</td></tr>"
  ++ "  <tr><td>Модель</td><td>$car_model$</td></tr>"
  ++ "  <tr><td>Неисправность со слов Клиента</td><td>$wazzup$</td></tr>"
  ++ "</table>"
  ++ "<p> Просим Вас предоставить дополнительную информацию, после диагностики "
  ++ "а/м в виде таблицы на электронный адрес psa@ruamc.ru :</p>"
  ++ "<table>"
  ++ "  <th>"
  ++ "    <td>Код дилера</td>"
  ++ "    <td>VIN номер автомобиля</td>"
  ++ "    <td>Пробег а/м на момент поломки</td>"
  ++ "    <td>Номер заказа-наряда ремонта у дилера</td>"
  ++ "    <td>Время/Дата поступления автомобиля</td>"
  ++ "    <td>Время/дата диагностики</td>"
  ++ "    <td>Запланированное время/дата окончания работ</td>"
  ++ "    <td>Реальное время/дата окончания работ</td>"
  ++ "    <td>Гарантия / негарантия</td>"
  ++ "    <td>Описание причины неисправности</td>"
  ++ "    <td>Система автомобиля, в которой произошла неисправность</td>"
  ++ "    <td>Неисправная деталь</td>"
  ++ "  </th>"
  ++ "  <tr></tr>"
  ++ "</table>"
  ++ "<p>Заранее благодарим за своевременный ответ, в течение 24 часов.</p>"


fillVars :: ByteString -> TriggerMonad b (Map Text Text)
fillVars caseId
  =   (return $ Map.empty)
  >>= add "caseId"       (return caseId)
  >>= add "car_vin"      (get caseId "car_vin")
  >>= add "car_plateNum" (get caseId "car_plateNum")
  >>= add "car_make"     (get caseId "car_make")
  >>= add "car_model"    (get caseId "car_model")
  >>= add "wazzup"       (get caseId "comment")
  where
    add k f m = do
      v <- f
      return $! Map.insert k (T.fromStrict $ T.decodeUtf8 v) m


sendMailToDealer :: ByteString -> TriggerMonad b ()
sendMailToDealer actionId = do
  svcId  <- get actionId "parentId"
  let svcName = head $ B.split ':' svcId
  when (svcId /= "" && svcName == "towage") $ do
    caseId  <- get actionId "caseId"
    program <- get caseId   "program"
    when (program `elem` ["peugeot", "citroen"]) $ do
      dealerId <- get svcId "towDealer_partnerId"
      when (dealerId /= "") $ do
        dealer'sMail <- get dealerId "closeTicketEmail"
        when (dealer'sMail /= "") $ do
          sendMailActually caseId

sendMailActually :: ByteString -> TriggerMonad b ()
sendMailActually caseId = do
  cfg <- lift $ getSnapletUserConfig
  cfgHost <- liftIO $ require cfg "psa-smtp-host"
  cfgUser <- liftIO $ require cfg "psa-smtp-user"
  cfgPass <- liftIO $ require cfg "psa-smtp-pass"
  cfgFrom <- liftIO $ require cfg "psa-smtp-from"
  cfgTo   <- liftIO $ require cfg "psa-smtp-recipients"

  varMap <- fillVars caseId

  let eml = Address Nothing
  let msg = simpleMail
        (eml cfgFrom)
        (map eml $ TS.splitOn "," cfgTo)
        [] []
        "Доставлена машина на ремонт"
        [htmlPart $ render varMap mailTemplate]

  l <- lift askLog
  -- NB. notice `forkIO` below
  -- it also saves us from exceptions thrown while sending an e-mail
  void $ liftIO $ forkIO $ do
    let scopeName = "sendMailToDealer(" ++ show caseId ++ ")"
    scoperLog l (TS.pack scopeName)
      $ sendMailWithLogin cfgHost cfgUser cfgPass msg


-- FIXME: copypaste from SMS.hs
render :: Map Text Text -> Text -> Text
render varMap = T.concat . loop
  where
    loop tpl = case T.breakOn "$" tpl of
      (txt, "") -> [txt]
      (txt, tpl') -> case T.breakOn "$" $ T.tail tpl' of
        (expr, "")    -> [txt, evalVar expr]
        (expr, tpl'') -> txt : evalVar expr : loop (T.tail tpl'')

    evalVar v = Map.findWithDefault v v varMap

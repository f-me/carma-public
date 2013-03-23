
module Snaplet.DbLayer.Triggers.MailToDealer
  (sendMailToDealer
  ) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad
import Control.Concurrent

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isNumber)
import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)

import System.Log.Simple
import System.Log.Simple.Base (scoperLog)
import Data.Configurator (require)
import Network.Mail.Mime

import Snap.Snaplet (getSnapletUserConfig)
import Snaplet.DbLayer.Types (getDict)
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import DictionaryCache


-- FIXME: store this in DB or in file
mailTemplate :: Text
mailTemplate = T.pack
  $  "<p>На территорию Вашего ДЦ был доставлен а/м по программе Assistance.</p>"
  ++ "<table border=\"1\">"
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
  >>= add "caseDate"     (get caseId "callDate" >>= formatDate)
  >>= add "car_vin"      (txt <$> get caseId "car_vin")
  >>= add "car_plateNum" (txt <$> get caseId "car_plateNum")
  >>= add "wazzup"       (get caseId "comment"   >>= tr wazzup . txt)
  >>= add "car_make"     (get caseId "car_make"  >>= tr carMake . txt)
  >>= add "car_model"    getCarModel
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

    formatDate tm = case B.readInt tm of
      Just (s,"") -> do
        tz <- liftIO getCurrentTimeZone
        return $ T.pack $ formatTime defaultTimeLocale "%d/%m/%Y"
          $ utcToLocalTime tz
          $ posixSecondsToUTCTime $ fromIntegral s
      _ -> return "???"


sendMailToDealer :: MonadTrigger m b => ByteString -> m b ()
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
          sendMailActually actionId caseId dealer'sMail

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
        QuotedPrintableText Nothing [] (render varMap mailTemplate)

  let subj = "Доставлена машина на ремонт / " `T.append` T.decodeUtf8 actId
  l <- liftDb askLog
  -- NB. notice `forkIO` below
  -- it also saves us from exceptions thrown while sending an e-mail
  void $ liftIO $ forkIO
    $ scoperLog l (T.concat ["sendMailToDealer(", T.decodeUtf8 caseId, ")"])
    $ renderSendMailCustom "/usr/sbin/exim" ["-t", "-r", T.unpack cfgFrom]
    $ (emptyMail $ Address Nothing cfgFrom)
        {mailTo = map (Address Nothing . T.strip) $ T.splitOn "," cfgTo
        ,mailHeaders = [("Subject", subj)]
        ,mailParts = [[body]]
        }


-- FIXME: copypaste from SMS.hs
render :: Map Text Text -> Text -> BL.ByteString
render varMap = TL.encodeUtf8 . TL.fromChunks . loop
  where
    loop tpl = case T.breakOn "$" tpl of
      (txt, "") -> [txt]
      (txt, tpl') -> case T.breakOn "$" $ T.tail tpl' of
        (expr, "")    -> [txt, evalVar expr]
        (expr, tpl'') -> txt : evalVar expr : loop (T.tail tpl'')

    evalVar v = Map.findWithDefault v v varMap

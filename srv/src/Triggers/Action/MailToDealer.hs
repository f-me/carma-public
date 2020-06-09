module Triggers.Action.MailToDealer (sendMailToDealer) where

import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import qualified Data.Text as T
import Text.InterpolatedString.QM (qns)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')

import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ

import Snap.Snaplet.PostgresqlSimple (liftPG)

import Data.Configurator (require)

import Data.Model as Model
import Carma.Model.Service (Service)

import Snap.Snaplet (getSnapletUserConfig)
import Application (AppHandler)
import Util hiding (render)


sendMailToDealer :: IdentI Service -> AppHandler (IO ())
sendMailToDealer svcId = do
  let addr = T.strip
  let addrList = map addr . T.splitOn ","
  cfg      <- getSnapletUserConfig
  cfgFrom  <- liftIO $ addr     <$> require cfg "psa-smtp-from"
  cfgReply <- liftIO $ addr     <$> require cfg "psa-smtp-reply"
  cfgCopy  <- liftIO $ addrList <$> require cfg "psa-smtp-copy2"

  liftPG $ \pg -> return $ do
    syslogJSON Info "trigger/mailToDealer" ["svcId" .= svcId]
    let txt = T.pack . show
    let err e = syslogJSON Error
          "trigger/mailToDealer"
          ["svcId" .= svcId, "error" .= (e :: Text)]
    PG.query pg q [svcId] >>= \case
        [[vals]] -> newHtmlMail pg
          cfgFrom (T.splitOn "," $ render "$emails$" vals)
          cfgCopy cfgReply
          ("Доставлена машина на ремонт / " <> txt svcId)
          (render msgTemplate vals)
          ["foo" .= ("dealer"::Text)
          ,"svc" .= txt svcId
          ]
        []    -> err "empty query result"
        _     -> err "ambiguous query result"


render :: Text -> Aeson.Value -> Text
render tpl (Aeson.Object vals)
  = foldl' (\txt (key, Aeson.String val) -> T.replace key val txt) tpl
  $ HM.toList vals
render _ v = error $ "BUG! invalid JSON in MailToDealer.render: " ++ show v


q :: PG.Query
q = [sql|
    with
      partnerEmails as
        (select p.id as pid, string_agg(m.value->>'value', ',') as addrs
          from partnertbl p, json_array_elements(p.emails) m
          where m.value->>'key' = 'close'
          group by p.id),
      msgValues as
        (select
            c.id                             :: text as "$case_id$",
            to_char(c.callDate, 'DD.MM.YYYY'):: text as "$case_date$",
            upper(coalesce(c.car_vin, '-'))  :: text as "$car_vin$",
            coalesce(c.car_plateNum, '-')    :: text as "$car_plate$",
            coalesce(c.customerComment, '-') :: text as "$wazzup$",
            make.label                       :: text as "$car_make$",
            model.label                      :: text as "$car_model$",
            email.addrs                      :: text as "$emails$"
          from
            towagetbl t, casetbl c,
            "CarMake" make, "CarModel" model,
            partnertbl p, partnerEmails email
          where true
            and make.id = c.car_make
            and model.id = c.car_model
            and t.parentId = c.id
            and t.towDealer_partnerId = p.id
            and email.pid = p.id
            and t.id = ?)
    select row_to_json(msg.*) from msgValues msg
  |]



msgTemplate :: Text
msgTemplate = [qns|
    <p>На территорию Вашего ДЦ был доставлен а/м по программе Assistance.</p>
    <br />
    Кейс в РАМК: $case_id$<br />
    VIN номер: $car_vin$<br />
    Госномер: $car_plate$<br />
    Дата доставки а/м: $case_date$<br />
    Марка: $car_make$<br />
    Модель: $car_model$<br />
    Неисправность со слов Клиента: $wazzup$<br />
    <p>
      Просим Вас,
      <u>используя функцию <font color="red">«ОТВЕТИТЬ ВСЕМ»</font></u>,
      предоставить дополнительную информацию, после диагностики а/м
      в виде таблицы.
    </p>
    <table border="1">
      <tr bgcolor="SkyBlue">
        <th>Код дилера</th>
        <th>VIN номер автомобиля</th>
        <th>Пробег а/м на момент поломки</th>
        <th>Номер заказа-наряда ремонта у дилера</th>
        <th>Время/Дата поступления автомобиля</th>
        <th>Время/дата диагностики</th>
        <th>Запланированное время/дата окончания работ</th>
        <th>Реальное время/дата окончания работ</th>
        <th>Гарантия / негарантия</th>
        <th>Описание причины неисправности</th>
        <th>Система автомобиля, в которой произошла неисправность</th>
        <th>Неисправная деталь</th>
      </tr>
      <tr align="center">
        <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>
        <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>
      </tr>
      <tr align="center">
        <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>
        <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>
      </tr>
      <tr align="center">
        <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>
        <td>-</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td>
      </tr>
    </table>
    <p>Заранее благодарим за своевременный ответ в течение 24 часов.</p>
  |]

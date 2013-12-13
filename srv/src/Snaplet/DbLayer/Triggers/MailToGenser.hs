
{-# LANGUAGE QuasiQuotes #-}

module Snaplet.DbLayer.Triggers.MailToGenser
  ( sendMailToGenser
  ) where


import Prelude hiding (log)
import Control.Monad.Trans (liftIO)
import Control.Monad
import Control.Concurrent

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import qualified Snap.Snaplet.PostgresqlSimple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Data.Configurator (require)
import Network.Mail.Mime
import System.Log.Simple

import Snap.Snaplet (getSnapletUserConfig)
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import Carma.HTTP

q :: PG.Query
q = [sql|
  with p as
    (select p.id, p.name, p.addr->>'value' as addr
      from (select id, name, json_array_elements(addrs) as addr
        from partnertbl) p
      where p.addr->>'key' = 'fact')
  select
    'Заявка на эвакуацию, офис ' || p.name
      || ', VIN: ' || coalesce(upper(c.car_vin), 'N/A'),

    E'\nДата отправки: '
        || to_char(statement_timestamp() at time zone 'MSK', 'YYYY-MM-DD HH24:MM:SS')
    || E'\nДата создания заявки на эвакуацию: '
        || to_char(t.createTime at time zone 'MSK', 'YYYY-MM-DD HH24:MM:SS')
    || E'\n№ заявки в системе учета оператора услуги: '
        || c.id
    || E'\nСтатус заявки: '
        || (case ?
            when 'serviceOk' then 'Услуга оказана'
            when 'serviceOrdered' then 'Услуга заказана'
            when 'cancelService' then 'Отказ от услуги'
            when 'clientCanceled' then 'Клиент отказался от услуги'
            else '-' end)
    || E'\nФ.И.О клиента: '
        || coalesce(initcap(c.contact_name), '')
    || E'\nМарка автомобиля: '
        || coalesce(mk.label, c.car_make, '-')
    || E'\nМодель автомобиля: '
        || coalesce(mdl.label, c.car_model, '-')
    || E'\nVIN автомобиля: '
        || coalesce(upper(c.car_vin), '')
    || E'\nКонтактный телефон клиента: '
        || coalesce(c.contact_phone1, '')
    || E'\nАдрес доставки (СЦ Genser): '
        || coalesce(t.towAddress_address, '')
    || E'\nКраткое описание неисправности (со слов клиента): '
        || coalesce(diag.label, c.comment, '')
    || E'\nАдрес местонахождения автомобиля: '
        || coalesce(c.caseAddress_address, '')
    || E'\nВремя прибытия эвакуатора: '
        || coalesce(to_char(t.times_factServiceStart at time zone 'MSK', 'YYYY-MM-DD HH24:MM:SS'), '')
    || E'\nСтоимость услуги, объявленная клиенту на этапе регистрации заявки: '
        || coalesce(t.payment_partnercost::text, '-')
    || E'\nПризнак физическое лицо/юридическое лицо: '
        || (case c.car_legalForm
            when 'individual' then 'Физическое лицо'
            when 'corporation' then 'Юридическое лицо'
            else '-' end)

    from towagetbl t, partnertbl p, casetbl c
      left join "Diagnosis0" diag on (diag.value = c.comment)
      left join "CarMake" mk on (mk.value = c.car_make)
      left join "CarModel" mdl on (mdl.value = c.car_model)
    where t.id = substring(?, ':(.*)')::int
      and c.id::text = substring(t.parentId, ':(.*)')
      and p.id::text = substring(t.towDealer_partnerId, ':(.*)')
  |]


sendMailToGenser :: MonadTrigger m b => ByteString -> m b ()
sendMailToGenser svcId = do
  liftDb $ log Trace (T.pack $ "sendMailToGenser(" ++ show svcId ++ ")")
  -- we need new status value but from postgres we can get only the old one
  svcStatus <- get svcId "status"
  dealerId <- get svcId "towDealer_partnerId"
  when (dealerId /= "") $ do
    dms <- get dealerId "emails"
    case getAllKeyedJsonValues dms "list" of
      [] -> return ()
      partnerMail :_ -> do
        cfg      <- liftDb getSnapletUserConfig
        mailCopy <- liftIO $ require cfg "genser-smtp-copy"
        mailFrom <- liftIO $ require cfg "genser-smtp-from"
        let mailTo = T.intercalate "," [T.decodeUtf8 partnerMail, mailCopy]
        [(subjTxt, bodyTxt)] <- liftDb $ PG.query q [svcStatus, svcId]
        let body = Part "text/plain; charset=utf-8"
                   QuotedPrintableText Nothing [] bodyTxt

        liftDb $ log Trace $ T.concat ["sendMailToGenser: ", subjTxt]
        void $ liftIO $ forkIO
          $ sendEximMail mailFrom mailTo subjTxt body


sendEximMail :: Text -> Text -> Text -> Part -> IO ()
sendEximMail mailFrom mailTo mailSubj mailBody =
    -- NB: hardcoded psa@ruamc.ru
    renderSendMailCustom "/usr/sbin/exim" ["-t", "-r", "psa@ruamc.ru"] $
    (emptyMail $ Address Nothing mailFrom)
    { mailTo = map (Address Nothing . T.strip) $ T.splitOn "," mailTo
    , mailHeaders = [("Subject", mailSubj)]
    , mailParts = [[mailBody]]
    }

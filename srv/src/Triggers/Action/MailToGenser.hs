module Triggers.Action.MailToGenser (sendMailToGenser) where

import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import qualified Data.Text as T

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ.Alt
import Data.Configurator (require)

import Snap.Snaplet.PostgresqlSimple (liftPG)

import Data.Model as Model
import Carma.Model.Service (Service)
import qualified Carma.Model.TowType as TowType
import qualified Carma.Model.LegalForm as LegalForm

import Snap.Snaplet (getSnapletUserConfig)
import Application (AppHandler)
import Util


sendMailToGenser :: Model.IdentI Service -> AppHandler (IO ())
sendMailToGenser svcId = do
  cfg      <- getSnapletUserConfig
  cfgFrom  <- liftIO (require cfg "genser-mail-from"  :: IO Text)
  cfgReply <- liftIO (require cfg "genser-mail-reply" :: IO Text)
  cfgCopy  <- T.splitOn "," <$> liftIO (require cfg "genser-mail-reply")

  liftPG $ \pg -> return $ do
    syslogJSON Info "trigger/mailToGenser" ["svcId" .= svcId]
    [partnerAddr:subj:bodyLines] <- uncurry (PG.query pg)
        [sql|
          with
            partnerEmail as
              (select id as partner_id, email->>'value' as addr
                from partnertbl, json_array_elements(emails) as email
                where email->>'key' = 'list')
          select
              eml.addr,
              'Заявка на эвакуацию, офис '
                || p.name || ', VIN: ' || coalesce(upper(c.car_vin), 'N/A'),
              'Дата отправки: '
                || to_char(now(), 'YYYY-MM-DD HH24:MI:SS'),
              'Дата создания заявки на эвакуацию: '
                || to_char(t.createTime, 'YYYY-MM-DD HH24:MI:SS'),
              '№ заявки в системе учета оператора услуги: ' || c.id,
              'Статус заявки: '              || s.label,
              'Ф.И.О клиента: '              || coalesce(initcap(c.contact_name), ''),
              'Марка автомобиля: '           || coalesce(mk.label, '-'),
              'Модель автомобиля: '          || coalesce(mdl.label, '-'),
              'VIN автомобиля: '             || coalesce(upper(c.car_vin), 'N/A'),
              'Контактный телефон клиента: ' || coalesce(c.contact_phone1, ''),
              'Адрес доставки (СЦ Genser): ' || coalesce(t.towAddress_address, ''),
              'Краткое описание неисправности (со слов клиента): '
                || coalesce(c.customerComment, diag.label, ''),
              'Адрес местонахождения автомобиля: '
                || coalesce(c.caseAddress_address, ''),
              'Время прибытия эвакуатора: '
                || to_char(t.times_expectedServiceStart, 'YYYY-MM-DD HH24:MI:SS'),
              'Стоимость услуги, объявленная клиенту на этапе регистрации заявки: '
                || (case
                  when t.payment_paidByClient ~ E'^\\d{1,7}(\\.\\d{1,2}){0,1}$'
                  then t.payment_paidByClient
                  else '-' end),
              'Признак физическое лицо/юридическое лицо: '
                || (case cntr.legalForm
                  when $(LegalForm.person)$  then 'Физическое лицо'
                  when $(LegalForm.company)$ then 'Юридическое лицо'
                  else '-' end)

            from towagetbl t, partnertbl p, "ServiceStatus" s, partnerEmail eml, casetbl c
              left join "Contract" cntr on (cntr.id = c.contract)
              left join "Wazzup" diag on (diag.id = c.comment)
              left join "CarMake" mk on (mk.id = c.car_make)
              left join "CarModel" mdl on (mdl.id = c.car_model)
            where true
              and t.towType = $(TowType.dealer)$
              and c.id = t.parentId
              and p.id = t.towDealer_partnerId
              and p.id = eml.partner_id
              and s.id = t.status
              and t.id = $(svcId)$
          |]


    let body = T.unlines $ bodyLines
          ++ [""
             , "Пожалуйста, не отвечайте на это письмо. \
               \В случае вопросов обратитесь на genser@ruamc.ru."
             ]

    newTextMail pg cfgFrom [partnerAddr] cfgCopy cfgReply subj body
      ["foo" .= ("genser"::Text)
      ,"svc" .= show svcId
      ]

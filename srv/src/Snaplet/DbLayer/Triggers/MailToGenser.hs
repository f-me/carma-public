{-# LANGUAGE QuasiQuotes #-}

module Snaplet.DbLayer.Triggers.MailToGenser
  ( sendMailToGenser
  ) where


import Prelude hiding (log)
import qualified Data.Text as T
import Data.ByteString (ByteString)

import qualified Snap.Snaplet.PostgresqlSimple as PG
import Database.PostgreSQL.Simple.SqlQQ
import System.Log.Simple

import Snaplet.DbLayer.Triggers.Types (MonadTrigger, liftDb)
import Snaplet.DbLayer.Triggers.Dsl (get)


q :: PG.Query
q = [sql|
  with
    partnerEmail as
      (select id as partner_id, email->>'value' as addr
        from partnertbl, json_array_elements(emails) as email
        where email->>'key' = 'list'),
    message as
      (select
          c.id as case_id,
          t.id as service_id,
          p.id as partner_id,
          p.foreignIdent as partner_foreign_id,
          p.name as partner_name,
          to_char(t.createTime at time zone 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"')
            as svc_create_time,
          to_char(t.times_expectedServiceStart at time zone 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"')
            as svc_start_time,
          (case ?
              when '19' then 'Услуга оказана'
              when '15' then 'Услуга заказана'
              when '14' then 'Отказ от услуги'
              when '4' then 'Клиент отказался от услуги'
              else '-' end)
            as svc_status,
          coalesce(mk.label, c.car_make, '-') as car_make,
          coalesce(mdl.label, c.car_model, '-') as car_model,
          coalesce(upper(c.car_vin), 'N/A') as car_vin,
          coalesce(initcap(c.contact_name), '') as contact_name,
          coalesce(c.contact_phone1, '') as contact_phone,
          coalesce(t.towAddress_address, '') as tow_addr,
          coalesce(c.customerComment, diag.label, '') as problem_desc,
          coalesce(c.caseAddress_address, '') as case_addr,
          (case
              when t.payment_paidByClient ~ E'^\\d{1,7}(\\.\\d{1,2}){0,1}$'
              then t.payment_paidByClient::numeric
              else 0 end)
            as svc_cost,
          (case coalesce(cntr.legalForm, 1)
              when 2 then true
              else false end)
            as isOrganisation

        from towagetbl t, partnertbl p, casetbl c
          left join "Contract" cntr on (cntr.id = c.contract)
          left join "Wazzup" diag on (diag.id = c.comment)
          left join "CarMake" mk on (mk.value = c.car_make)
          left join "CarModel" mdl on (mdl.value = c.car_model)
        where c.id = substring(t.parentId, ':(.*)') :: int
          and p.id = substring(t.towDealer_partnerId, ':(.*)') :: int
          and t.id = substring(?, ':(.*)')::int
      )
    insert into "MessageToGenser" (msgData, email, status)
    select row_to_json(msg.*) as msgData, email.addr as email, 'please-send' as status
      from message msg
        left join partnerEmail email on (email.partner_id = msg.partner_id)
    returning id
  |]


sendMailToGenser :: MonadTrigger m b => ByteString -> m b ()
sendMailToGenser svcId = do
  liftDb $ log Trace (T.pack $ "sendMailToGenser(" ++ show svcId ++ ")")
  -- we need new status value but from postgres we can get only the old one
  svcStatus <- get svcId "status"
  res <- liftDb $ PG.query q [svcStatus, svcId]
  liftDb $ log Trace (T.pack $ "sendMailToGenser(" ++ show svcId ++ ":" ++ show (res::[[Int]]) ++ ")")

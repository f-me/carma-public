
module Carma.Backoffice.Action.MailToGenser (sendMailToGenser) where


import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Data.Pool as Pool

import Data.Model as Model
import Carma.Model.Service (Service)
import Trigger.Dsl (FutureContext(..))

import Application (AppHandler)
import Util


sendMailToGenser :: Model.IdentI Service -> FutureContext -> AppHandler (IO ())
sendMailToGenser svcId fc
  = return
  $ Pool.withResource (fc_pgpool fc) $ \pg -> do
    syslogJSON Info "trigger/mailToGenser" ["svcId" .= svcId]
    res <- PG.query pg q [svcId]
    syslogJSON Info "trigger/mailToGenser" ["svcId" .= svcId, "res" .= show (res::[[Int]])]


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
          s.label as svc_status,
          coalesce(mk.label, '-') as car_make,
          coalesce(mdl.label, '-') as car_model,
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

        from towagetbl t, partnertbl p, "ServiceStatus" s, casetbl c
          left join "Contract" cntr on (cntr.id = c.contract)
          left join "Wazzup" diag on (diag.id = c.comment)
          left join "CarMake" mk on (mk.id = c.car_make)
          left join "CarModel" mdl on (mdl.id = c.car_model)
        where true
          and t.towType = 1
          and c.id = t.parentId
          and p.id = t.towDealer_partnerId
          and s.id = t.status
          and t.id = ?
      )
    insert into "MessageToGenser" (msgData, email, status)
    select row_to_json(msg.*) as msgData, email.addr as email, 'please-send' as status
      from message msg
        left join partnerEmail email on (email.partner_id = msg.partner_id)
    returning id
  |]



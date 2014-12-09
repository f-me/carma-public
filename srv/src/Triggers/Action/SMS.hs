module Triggers.Action.SMS (sendSMS) where

import Control.Monad (void)

import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ
import Data.Pool as Pool

import Data.Model as Model
import Carma.Model.Service     (Service)
import Carma.Model.SmsTemplate as SmsTemplate

import Triggers.DSL (FutureContext(..))
import Application (AppHandler)
import Util


sendSMS
  :: Model.IdentI SmsTemplate -> Model.IdentI Service
  -> FutureContext -> AppHandler (IO ())
sendSMS tplId svcId fc
  = return
  $ Pool.withResource (fc_pgpool fc) $ \pg ->
    PG.query pg messageInfo (tplId, svcId) >>= \case
      [fields] -> do
        let msgInfo = Map.map T.tail $ Map.fromList $ map (T.breakOn "=") fields
        let msg = render msgInfo (msgInfo ! "tpl")
        let params = (msgInfo ! "case.id", msgInfo ! "phone", msgInfo ! "sender", tplId, msg)
        void $ PG.execute pg insertSms params
      res -> syslogJSON Error "backoffice/sendSMS"
        ["err" .= ("unexpected query result" :: T.Text)
        ,"res" .= T.pack (show res)
        ,"tpl" .= T.pack (show tplId)
        ,"svc" .= T.pack (show svcId)
        ]


insertSms :: PG.Query
insertSms = [sql|
    insert into "Sms" (caseRef, phone, sender, template, msgText, status)
    values (?, ?, ?, ?, ?, 'please-send')
  |]


messageInfo :: PG.Query
messageInfo = [sql|
    select
      'tpl='                  || tpl.text,
      'phone='                || coalesce(cs.contact_phone1, ''),
      'sender='               || sprog.smsSender,
      'case.id='              || cs.id::text,
      'case.city='            || coalesce("City".label, ''),
      'service.type='         || "ServiceType".label,
      'program_info='         || sprog.smsProgram,
      'program_contact_info=' || sprog.smsContact,
      'service.times_expectedServiceStart='
        || coalesce(to_char(svc.times_expectedServiceStart, 'HH24:MI DD-MM-YYYY'), ''),
      'service.times_factServiceStart='
        || coalesce(to_char(svc.times_factServiceStart, 'HH24:MI DD-MM-YYYY'), '')
    from
      casetbl cs left join "City" on ("City".id = cs.city),
      servicetbl svc,
      "ServiceType",
      "SubProgram" sprog,
      "SmsTemplate" tpl
    where true
      and tpl.id   = ?
      and svc.id   = ?
      and svc.type = "ServiceType".id
      and cs.id    = svc.parentId
      and sprog.id = cs.subprogram
  |]

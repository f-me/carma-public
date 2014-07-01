{-# LANGUAGE QuasiQuotes #-}
module Snaplet.DbLayer.Triggers.SMS (sendSMS) where

import Control.Monad (void)
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Util

import qualified Snap.Snaplet.PostgresqlSimple as PG
import Snap.Snaplet.PostgresqlSimple ((:.)(..))
import Database.PostgreSQL.Simple.SqlQQ

import Data.Model as Model
import Data.Model.Sql

import Carma.Model.SmsTemplate (SmsTemplate)
import qualified Carma.Model.SmsTemplate as SmsTemplate

import Util as U


sendSMS :: ObjectId -> Model.IdentI SmsTemplate -> DbHandler b ()
sendSMS actId tplId = do
  res <- PG.query
        [sql|
          select
            cs.id::text,
            coalesce("City".label, ''),
            coalesce(cs.contact_phone1, ''),
            coalesce(act.assignedTo, ''),
            coalesce(to_char(svc.times_expectedServiceStart, 'MM-DD HH24:MI'), ''),
            coalesce(to_char(svc.times_factServiceStart, 'MM-DD HH24:MI'), ''),
            prog.smsSender, prog.smsProgram, prog.smsContact
          from
            casetbl cs left join "City" on ("City".value = cs.city),
            actiontbl act, servicetbl svc,
            "SubProgram" prog
          where true
            and act.id   = substring(?, ':(.*)')::int
            and svc.id   = substring(act.parentId, ':(.*)')::int
            and svc.type = substring(act.parentId, '(.*):')
            and cs.id    = substring(act.caseId, ':(.*)')::int
            and prog.id  = cs.subprogram
        |]
        [actId]
  case res of
    [[caseId, city, phone, opName, eSvcStart, fSvcStart, sender, pInfo, pCInfo]]
      -> do
        let varMap = Map.fromList
              [("program_info", pInfo)
              ,("program_contact_info", pCInfo)
               -- TODO Use actual realName from user meta
              ,("case.backoperator_name", opName)
              ,("case.city", city)
              ,("case.id", caseId)
              ,("service.times_factServiceStart", fSvcStart)
              ,("service.times_expectedServiceStart", eSvcStart)
              ]

        [PG.Only templateText :. ()]
            <- selectDb $ SmsTemplate.text :. SmsTemplate.ident `eq` tplId

        let msg = T.encodeUtf8 $ U.render varMap templateText

        void $ PG.execute
          [sql|
            insert into "Sms"
                (caseRef, phone, sender, template, msgText, status)
              values
                (?, ?, ?, ?, ?, 'please-send')
          |]
          (caseId, phone, sender, tplId, msg)
    _ -> return ()

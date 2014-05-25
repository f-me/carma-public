{-# LANGUAGE QuasiQuotes #-}
module Snaplet.DbLayer.Triggers.SMS (sendSMS) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map

import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Util

import qualified Snap.Snaplet.PostgresqlSimple as PG
import Snap.Snaplet.PostgresqlSimple ((:.)(..))
import Database.PostgreSQL.Simple.SqlQQ

import Data.Model as Model
import Data.Model.Sql

import Carma.Model.SmsTemplate (SmsTemplate)
import qualified Carma.Model.SmsTemplate as SmsTemplate

import Util as U


sendSMS :: MonadTrigger m b => ByteString -> Model.IdentI SmsTemplate -> m b ()
sendSMS actId tplId = do
  res <- liftDb $ PG.query
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
            and act.id::text = substring(?, ':(.*)')
            and svc.id::text = substring(act.parentId, ':(.*)')
            and svc.type     = substring(act.parentId, '(.*):')
            and cs.id::text  = substring(act.caseId, ':(.*)')
            and prog.id      = cs.subprogram
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
            <- liftDb
              $ selectDb $ SmsTemplate.text :. SmsTemplate.ident `eq` tplId

        let msg = T.encodeUtf8 $ U.render varMap templateText

        void $ liftDb $ PG.execute
          [sql|
            insert into "Sms"
                (caseRef, phone, sender, template, msgText, status)
              values
                (?, ?, ?, ?, ?, 'please-send')
          |]
          (caseId, phone, sender, tplId, msg)
    _ -> return ()

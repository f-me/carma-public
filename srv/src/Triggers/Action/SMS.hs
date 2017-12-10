{-# LANGUAGE ViewPatterns #-}

module Triggers.Action.SMS (sendSMS) where

import Control.Monad (void)

import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Snap.Snaplet.PostgresqlSimple as SPG (liftPG, query, execute)

import qualified Data.Model                 as Model
import qualified Carma.Model.Sms            as Sms
import qualified Carma.Model.SmsTemplate    as SmsTemplate
import qualified Carma.Model.SubProgram     as SubProgram
import qualified Carma.Model.ServiceType    as ServiceType
import qualified Carma.Model.Service        as Service
import qualified Carma.Model.Service.Towage as Towage
import qualified Carma.Model.Case           as Case
import qualified Carma.Model.City           as City

import Utils.Model.MSqlQQ hiding (parseQuery)
import Application (AppHandler)
import Util (syslogJSON, render, Priority (Error), (.=))


sendSMS
  :: Model.IdentI SmsTemplate.SmsTemplate
  -> Model.IdentI Service.Service
  -> AppHandler (IO ())
sendSMS tplId svcId =
  withNoIO $ uncurry SPG.query messageInfo >>= \case

    [fields] ->
      let msgInfo = Map.map T.tail $ Map.fromList $ map (T.breakOn "=") fields
       in void $ uncurry SPG.execute $ insertSms msgInfo

    res -> syslogJSON Error "backoffice/sendSMS"
      [ "err" .= ("unexpected query result" :: T.Text)
      , "res" .= T.pack (show res)
      , "tpl" .= T.pack (show tplId)
      , "svc" .= T.pack (show svcId)
      ]

  where

  -- withNoIO = (pure () <$)
  withNoIO = (putStrLn "... TESING IT ..." <$)

  messageInfo = [msql|
    select
      'tpl='                   || tpl.$(F|SmsTemplate.text)$,
      'phone='                 || coalesce(cs.$(F|Case.contact_phone1)$, ''),
      'sender='                || sprog.$(F|SubProgram.smsSender)$,
      'case.id='               || cs.$(F|Case.ident)$::text,
      'case.city='             || coalesce(city.$(F|City.label)$, ''),
      'case.car_make='         || cs.$(F|Case.car_make)$,  -- TODO resolve label by id
      'case.car_model='        || cs.$(F|Case.car_model)$, -- TODO resolve label by id
      'case.customer_phone='   || cs.$(F|Case.contact_phone1)$,
      'case.breakage_address=' || cs.$(F|Case.caseAddress_address)$,
      'service.type='          || svct.$(F|ServiceType.label)$,
      'program_info='          || sprog.$(F|SubProgram.smsProgram)$,
      'program_contact_info='  || sprog.$(F|SubProgram.smsContact)$,

      -- 'service.towage.towage_to_address=' ||
      --   coalesce(towage.$(F|Towage.towAddress_address)$, ''),

      'service.times_expectedServiceStart=' ||
        coalesce(to_char(
          svc.$(F|Service.times_expectedServiceStart)$,
          'HH24:MI DD-MM-YYYY'
        ), ''),

      'service.dates_expectedServiceStart=' ||
        coalesce(to_char(
          svc.$(F|Service.times_expectedServiceStart)$,
          'DD-MM-YYYY'
        ), ''),

      'service.times_factServiceStart=' ||
        coalesce(to_char(
          svc.$(F|Service.times_factServiceStart)$,
          'HH24:MI DD-MM-YYYY'
        ), '')

    from
      $(T|Case)$ cs

        left join $(T|City)$ city
        on ( city.$(F|City.ident)$ = cs.$(F|Case.city)$ ),

        -- TODO get field `Service.parentId` from parent model
        --      (as `Towage.parentId`)
        --      see also TODO in Utils.Model.MSqlQQ
        -- left join $(T|Towage)$ towage
        -- on ( towage.$(F|Service.parentId)$ = cs.$(F|Case.ident)$ ),

      $(T|Service)$     svc,
      $(T|ServiceType)$ svct,
      $(T|SubProgram)$  sprog,
      $(T|SmsTemplate)$ tpl

    where true
      and tpl.$(F|SmsTemplate.ident)$  = $(V|tplId)$
      and svc.$(F|Service.ident)$      = $(V|svcId)$
      and svc.$(F|Service.svcType)$    = svct.$(F|ServiceType.ident)$
      and cs.$(F|Case.ident)$          = svc.$(F|Service.parentId)$
      and sprog.$(F|SubProgram.ident)$ = cs.$(F|Case.subprogram)$
  |]

  insertSms msgInfo = [msql|
    insert into $(T|Sms)$
      ( $(F|Sms.caseRef)$
      , $(F|Sms.phone)$
      , $(F|Sms.sender)$
      , $(F|Sms.template)$
      , $(F|Sms.msgText)$
      , $(F|Sms.status)$
      )
      values
      ( $(V|msgInfo ! "case.id")$
      , $(V|msgInfo ! "phone")$
      , $(V|msgInfo ! "sender")$
      , $(V|tplId)$
      , $(V|render msgInfo $ msgInfo ! "tpl")$
      , 'please-send'
      )
  |]

module Triggers.Action.SMS (sendSMS) where

import Control.Monad (void)

import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Snap.Snaplet.PostgresqlSimple as SPG (query, execute)
import Carma.Backoffice.DSL.Types (SendSmsTo (..))

import qualified Data.Model                 as Model
import qualified Carma.Model.Sms            as Sms
import qualified Carma.Model.SmsTemplate    as SmsTemplate
import qualified Carma.Model.SubProgram     as SubProgram
import qualified Carma.Model.ServiceType    as ServiceType
import qualified Carma.Model.Service        as Service
import qualified Carma.Model.Service.Towage as Towage
import qualified Carma.Model.Partner        as Partner
import qualified Carma.Model.Case           as Case
import qualified Carma.Model.City           as City
import qualified Carma.Model.CarMake        as CarMake
import qualified Carma.Model.CarModel       as CarModel
import qualified Carma.Model.Usermeta       as Usermeta

import Data.Model.Utils.PostgreSQL.MSqlQQ hiding (parseQuery)
import Application (AppHandler)
import Util (syslogJSON, render, Priority (Error), (.=))
import Text.InterpolatedString.QM


sendSMS
  :: Model.IdentI SmsTemplate.SmsTemplate
  -> Model.IdentI Service.Service
  -> Model.IdentI Usermeta.Usermeta
  -> SendSmsTo
  -> AppHandler (IO ())
sendSMS tplId svcId sendBy sendTo =
  (pure () <$) $ uncurry SPG.query messageInfo >>= \case

    [fields] ->
      let
        msgInfo = Map.map T.tail $ Map.fromList $ map (T.breakOn "=") fields
        numbers = "0123456789" :: String
        caseId  = msgInfo ! "case.id"
        phone   = msgInfo ! "phone"
        save    = uncurry SPG.execute $ insertSms msgInfo

        caseIdValidator =
          guard
            -- Empty or number means it's valid (empty means `NULL`)
            (T.all (`elem` numbers) caseId)
            [qm| Case id is invalid: "{caseId}" |]

        phoneValidator = do
          guard
            (not $ T.null phone)
            [qm| {phoneView} is NULL or empty |]
          guard
            -- Matching +7**********
            ( T.length phone == 12 &&
              T.take 2 phone == "+7" &&
              T.all (`elem` numbers) (T.drop 2 phone) )
            [qm| {phoneView} is invalid: "{phone}" |]
      in
        whenValid (caseIdValidator >> phoneValidator) $ void save

    res -> reportError
      [ "err" .= ("unexpected query result" :: T.Text)
      , "res" .= T.pack (show res)
      ]

  where

  whenValid :: Either T.Text () -> AppHandler () -> AppHandler ()
  whenValid (Left msg) _ = reportError ["err" .= msg]
  whenValid (Right ()) m = m

  guard :: Bool -> T.Text -> Either T.Text ()
  guard condition errMsg = if condition then Right () else Left errMsg

  phoneView :: T.Text
  phoneView = case sendTo of
                   SendSmsToCaller            -> "caller's phone"
                   SendSmsToContractorPartner -> "partner's phone"

  reportError json =
    syslogJSON Error "backoffice/sendSMS" $ json ++ params
    where params = [ "tpl" .= T.pack (show tplId)
                   , "svc" .= T.pack (show svcId)
                   , "usr" .= T.pack (show sendBy)
                   ]

  messageInfo = [msql|
    select
      'tpl='                   || tpl.$(F|SmsTemplate.text)$,
      'sender='                || sprog.$(F|SubProgram.smsSender)$,
      'case.id='               || cs.$(F|Case.ident)$::text,
      'case.city='             || coalesce(city.$(F|City.label)$, ''),
      'case.customer_name='    || coalesce(cs.$(F|Case.contact_name)$, ''),
      'case.customer_phone='   || coalesce(cs.$(F|Case.contact_phone1)$, ''),

      'case.breakage_address=' ||
        coalesce(cs.$(F|Case.caseAddress_address)$, ''),

      'service.type='          || svct.$(F|ServiceType.label)$,
      'program_info='          || sprog.$(F|SubProgram.smsProgram)$,
      'program_contact_info='  || sprog.$(F|SubProgram.smsContact)$,

      'phone=' ||
        coalesce((
          case
            when $(V|sendTo == SendSmsToCaller)$
              then cs.$(F|Case.contact_phone1)$
            when $(V|sendTo == SendSmsToContractorPartner)$
              then GetPartnerSmsPhone(partner.$(F|Partner.ident)$)
          end
        ), ''),

      'case.car_make=' ||
        coalesce((
          select $(F|CarMake.label)$ from $(T|CarMake)$
          where $(F|CarMake.ident)$ = cs.$(F|Case.car_make)$
        ), ''),

      'case.car_model=' ||
        coalesce((
          select $(F|CarModel.label)$ from $(T|CarModel)$
          where $(F|CarModel.ident)$ = cs.$(F|Case.car_model)$
        ), ''),

      'case.car_plateNum=' || coalesce(cs.$(F|Case.car_plateNum)$, ''),

      'service.towage.towage_to_address=' ||
        coalesce(towage.$(F|Towage.towAddress_address)$, ''),

      'service.times_expectedServiceStart=' ||
        coalesce(to_char(
          svc.$(F|Service.times_expectedServiceStart)$,
          'DD.MM.YYYY HH24:MI:SS'
        ), ''),
      'service.times_expectedServiceStart_caseCityTZ=' ||
        coalesce(
          case
            when city.$(F|City.ident)$ is not null then to_char(
              timezone(
                city.$(F|City.timezone)$,
                svc.$(F|Service.times_expectedServiceStart)$
              ),
              'DD.MM.YYYY HH24:MI:SS'
            )
          end
        , ''),

      'service.dates_expectedServiceStart=' ||
        coalesce(to_char(
          svc.$(F|Service.times_expectedServiceStart)$,
          'DD.MM.YYYY'
        ), ''),
      'service.dates_expectedServiceStart_caseCityTZ=' ||
        coalesce(
          case
            when city.$(F|City.ident)$ is not null then to_char(
              timezone(
                city.$(F|City.timezone)$,
                svc.$(F|Service.times_expectedServiceStart)$
              ),
              'DD.MM.YYYY'
            )
          end
        , ''),

      'service.times_factServiceStart=' ||
        coalesce(to_char(
          svc.$(F|Service.times_factServiceStart)$,
          'DD.MM.YYYY HH24:MI:SS'
        ), ''),
      'service.times_factServiceStart_caseCityTZ=' ||
        coalesce(
          case
            when city.$(F|City.ident)$ is not null then to_char(
              timezone(
                city.$(F|City.timezone)$,
                svc.$(F|Service.times_factServiceStart)$
              ),
              'DD.MM.YYYY HH24:MI:SS'
            )
          end
        , '')

    from
      $(T|Case)$ cs
        left join $(T|City)$ city
          on ( city.$(F|City.ident)$ = cs.$(F|Case.city)$ ),

      $(T|Service)$ svc
        left join $(T|Towage)$ towage
          on ( towage.$(F|Towage.ident)$ = svc.$(F|Service.ident)$ )
        left join $(T|Partner)$ partner
          on (
            partner.$(F|Partner.ident)$ =
            svc.$(F|Service.contractor_partnerId)$
          ),

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
      , $(F|Sms.userRef)$
      , $(F|Sms.phone)$
      , $(F|Sms.sender)$
      , $(F|Sms.template)$
      , $(F|Sms.msgText)$
      , $(F|Sms.status)$
      )
      values
      ( nullif($(V|msgInfo ! "case.id")$, '')::integer
      , $(V|sendBy)$
      , $(V|msgInfo ! "phone")$
      , $(V|msgInfo ! "sender")$
      , $(V|tplId)$
      , $(V|render msgInfo $ msgInfo ! "tpl")$
      , 'please-send'
      )
  |]

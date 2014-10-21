
module Carma.Backoffice.Action.MailToPSA (sendMailToPSA) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T (decimal)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.ByteString (ByteString)

import Data.Aeson ((.=))
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)

import Database.PostgreSQL.Simple.SqlQQ.Alt
import Database.PostgreSQL.Simple as PG
import Data.Pool as Pool

import Data.Model as Model
import Carma.Model.Service (Service)
import qualified Carma.Model.ServiceType as ServiceType
import qualified Carma.Model.ServiceStatus as ServiceStatus
import qualified Carma.Model.Engine as Engine
import qualified Carma.Model.Program as Program
import qualified Carma.Model.TechType as TT
import Trigger.Dsl (FutureContext(..))

import Snap.Snaplet
import Application (AppHandler)
import Data.Configurator (require)
import Network.Mail.Mime
import Util (syslogJSON, logExceptions, Priority(..))



sendMailToPSA :: IdentI Service -> FutureContext -> AppHandler (IO ())
sendMailToPSA svcId fc = do
  cfg      <- getSnapletUserConfig
  cfgFrom  <- liftIO $ require cfg "psa-smtp-from"
  cfgTo    <- liftIO $ require cfg "psa-smtp-recipients"
  cfgReply <- liftIO $ require cfg "psa-smtp-replyto"

  return $ do
    syslogJSON Info "trigger/mailToPSA" ["svcId" .= svcId]
    let err e = syslogJSON Error
          "trigger/mailToPSA"
          ["svcId" .= svcId, "error" .= (e :: Text)]
    Pool.withResource (fc_pgpool fc) $ \pg ->
      getMsgData pg svcId >>= \case
        [res] -> case render $ map (fmap T.decodeUtf8) res of
          Left msg  -> err msg
          Right msg -> sendMailActually cfgFrom cfgTo cfgReply msg
        []    -> err "empty query result"
        _     -> err "ambiguous query result"


sendMailActually :: Text -> Text -> Text -> Text -> IO ()
sendMailActually from to reply msg = do
  let bodyPart = Part "text/plain; charset=utf-8"
        QuotedPrintableText Nothing [] (TL.encodeUtf8 $ TL.fromChunks [msg])
  logExceptions "trigger/mailToPSA/sendMailToPSA"
    $ renderSendMailCustom "/usr/sbin/sendmail" ["-t", "-r", T.unpack from]
      $ (emptyMail $ Address Nothing from)
        {mailTo = map (Address Nothing . T.strip) $ T.splitOn "," to
        ,mailHeaders = [("Reply-To", reply) , ("Subject", "RAMC")]
        ,mailParts = [[bodyPart]]
        }


render :: [Maybe Text] -> Either Text Text
render = loop ""
  where
    loop res (Just fld : Just lenTxt : Just val : xs)
      = case T.decimal lenTxt of
        Right (len,_)
          -> loop (res <> fld <> " : " <> T.take (len::Int) val <> "\n") xs
        _ -> Left $ "Bug: invalid field length: " <> fld <> "=" <> lenTxt
    loop _ (Just fld : _ : Nothing : _)
      = Left $ "Required field is empty: " <> fld
    loop res [] = Right res
    loop _ _  = Left "BUG: misaligned query result"


-- empty result
--  - Error: no such service
--  - Ok:    invalid tech type
-- nulls in result
--  - some required field is missing

-- NB: Why 'ByteString' instead of 'Text'?
-- PostgreSQL inferes 'unknown' type for string literals and
-- postgresql-simple does not allow values of 'unknown' type to be
-- converted to 'Text'.
getMsgData :: PG.Connection -> IdentI Service -> IO [[Maybe ByteString]]
getMsgData con svcId = uncurry (PG.query con)
  [sql|
    select
      'BeginOfFile'::text,          '4'::text, 'True'::text,
      'Assistance Code'::text,     '50'::text,
        case c.program
          when $(Program.peugeot)$ then 'RUMC01R'
          when $(Program.citroen)$ then 'FRRM01R'
        end,
      'Country Code'::text,         '2'::text, 'RU'::text,
      'Task Id'::text,              '9'::text, 'M' || lpad(c.id::text, 8, '0'),
      'Time of Incident'::text,     '5'::text, to_char(c.callDate at time zone 'MSK'::text, 'HH24:MI'),
      'Make'::text,                 '3'::text,
        case c.program
          when $(Program.peugeot)$ then 'PEU'
          when $(Program.citroen)$ then 'CIT'
        end,
      'Model'::text,               '13'::text, car_model.label,
      'Energie'::text,              '1'::text,
        case c.car_engine
          when $(Engine.petrol)$ then 'E'
          when $(Engine.diesel)$ then 'D'
        end,
      'Date put on road'::text,    '10'::text, to_char(c.car_buyDate, 'DD/MM/YYYY'),
      'VIN number'::text,          '17'::text, c.car_vin,
      'Reg No'::text,              '10'::text, c.car_plateNum,
      'Customer effet'::text,     '150'::text,
        case svc.status
          when $(ServiceStatus.canceled)$ then coalesce(svc.clientCancelReason)
          else coalesce(c.customerComment, '')
        end,
      'Component fault'::text,    '150'::text, coalesce(c.dealerCause, ''),
      'Date of Opening'::text,     '10'::text, to_char(c.callDate, 'DD/MM/YYYY'),
      'Date of Response'::text,    '10'::text,
        to_char(svc.times_factServiceStart at time zone 'MSK'::text, 'DD/MM/YYYY'),
      'Time of Response'::text,     '5'::text,
        to_char(svc.times_factServiceStart at time zone 'MSK'::text, 'HH24:MI'),
      'Breakdown Location'::text,  '100'::text, c.caseAddress_address,
      'Breakdown Area'::text,       '20'::text, city.label,
      'Breakdown Service'::text,   '100'::text, coalesce(ctr.name, ''),
      'Service Tel Number 1'::text, '20'::text, coalesce((ctr_phone_disp.value->'value')::text, ''),
      'Service Tel Number 2'::text, '20'::text, coalesce((ctr_phone_close.value->'value')::text, ''),
      'Patrol Address 1'::text,    '100'::text, coalesce((ctr_addr_fact.value->'value')::text, ''),
      'Patrol Address 2'::text,    '100'::text, ''::text,
      'Patrol Address V'::text,    '100'::text, ''::text,
      'User Name'::text,            '50'::text, upper(c.contact_name),
      'User Tel Number'::text,      '20'::text, c.contact_phone1,
      'User Name P'::text,          '50'::text,
        case c.contact_contactOwner
          when true then upper(c.contact_name)
          else coalesce(upper(c.contact_ownerName), '')
        end,
      'Job Type'::text,              '4'::text,
        case svc.type
          when $(ServiceType.tech)$         then 'DEPA'
          when $(ServiceType.towage)$       then 'REMO'
          when $(ServiceType.consultation)$ then 'TELE'
        end,
      'Dealer Address G'::text,    '200'::text, coalesce(tow.towAddress_address, ''),
      'Dealer Address 1'::text,    '200'::text, ''::text,
      'Dealer Address 2'::text,    '200'::text, ''::text,
      'Dealer Address V'::text,    '200'::text, ''::text,
      'Dealer Tel Number'::text,    '20'::text, coalesce((tow_dealer.phones->0->'value')::text, ''),
      'End Of File'::text,           '4'::text, 'True'::text
    from
      servicetbl svc
      left join towagetbl tow on svc.id = tow.id
      left join techtbl tech on svc.id = tech.id
      join casetbl c on svc.parentid = c.id
      join "CarModel" car_model
        on (car_model.parent = c.car_make and car_model.id = c.car_model)
      join "City" city on city.id = c.city
      left join partnertbl ctr on ctr.id = svc.contractor_partnerId
      left join json_array_elements(ctr.phones) ctr_phone_disp
        on (ctr_phone_disp.value->'key')::text = 'disp'
      left join json_array_elements(ctr.phones) ctr_phone_close
        on (ctr_phone_close.value->'key')::text = 'close'
      left join json_array_elements(ctr.addrs) ctr_addr_fact
        on (ctr_addr_fact.value->'key')::text = 'fact'
      left join partnertbl tow_dealer on tow_dealer.id = tow.towDealer_partnerId
    where svc.id = $(svcId)$
      and (tech.id is null or tech.techType in ($(TT.charge)$, $(TT.starter)$, $(TT.ac)$))
  |]

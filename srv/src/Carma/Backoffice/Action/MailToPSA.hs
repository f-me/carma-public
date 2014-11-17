
module Carma.Backoffice.Action.MailToPSA (sendMailToPSA) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T (decimal)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.ByteString (ByteString)

import Data.Aeson ((.=))
import Data.Monoid ((<>))

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
  let addr = Address Nothing . T.strip
  let addrList = map addr . T.splitOn ","
  cfgFrom  <- liftIO $ addr     <$> require cfg "psa-smtp-from"
  cfgReply <- liftIO $ addr     <$> require cfg "psa-smtp-reply"
  cfgTo    <- liftIO $ addrList <$> require cfg "psa-smtp-to"
  cfgCopy  <- liftIO $ addrList <$> require cfg "psa-smtp-copy1"

  return $ do
    syslogJSON Info "trigger/mailToPSA" ["svcId" .= svcId]
    let err e = syslogJSON Error
          "trigger/mailToPSA"
          ["svcId" .= svcId, "error" .= (e :: Text)]
    Pool.withResource (fc_pgpool fc) $ \pg ->
      getMsgData pg svcId >>= \case
        [res] -> case render $ map (fmap T.decodeUtf8) res of
          Left msg  -> err msg
          Right msg -> sendMailActually cfgFrom cfgTo cfgReply cfgCopy msg
        []    -> err "empty query result"
        _     -> err "ambiguous query result"


sendMailActually
  :: Address -> [Address] -> Address -> [Address] -> Text
  -> IO ()
sendMailActually from to reply copy msg = do
  let bodyPart = Part "text/plain; charset=utf-8"
        QuotedPrintableText Nothing [] (TL.encodeUtf8 $ TL.fromChunks [msg])
  let email = (emptyMail from)
        {mailTo      = to
        ,mailCc      = copy
        ,mailHeaders = [("Reply-To", addressEmail reply) , ("Subject", "RAMC")]
        ,mailParts   = [[bodyPart]]
        }
  logExceptions "trigger/mailToPSA/sendMailToPSA"
    $ renderSendMailCustom
      "/usr/sbin/sendmail"
      ["-t", "-r", T.unpack $ addressEmail from]
      email


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
      'BeginOfFile',          '4', 'True',
      'Assistance Code',     '50',
        case c.program
          when $(Program.peugeot)$ then 'RUMC01R'
          when $(Program.citroen)$ then 'FRRM01R'
        end,
      'Country Code',         '2', 'RU',
      'Task Id',              '9', 'M' || lpad(c.id::text, 8, '0'),
      'Time of Incident',     '5', to_char(c.callDate at time zone 'MSK', 'HH24:MI'),
      'Make',                 '3', 
        case c.program
          when $(Program.peugeot)$ then 'PEU'
          when $(Program.citroen)$ then 'CIT'
        end,
      'Model',               '13', car_model.label,
      'Energie',              '1',
        case c.car_engine
          when $(Engine.petrol)$ then 'E'
          when $(Engine.diesel)$ then 'D'
          else 'E'
        end,
      'Date put on road',    '10', to_char(c.car_buyDate, 'DD/MM/YYYY'),
      'VIN number',          '17', c.car_vin,
      'Reg No',              '10', c.car_plateNum,
      'Customer effet',     '150', coalesce(c.customerComment, ''),
      'Component fault',    '150',
        case svc.status
          when $(ServiceStatus.canceled)$ then coalesce(svc.clientCancelReason, '')
          else coalesce(c.dealerCause, '')
          end,
      'Date of Opening',     '10', to_char(c.callDate, 'DD/MM/YYYY'),
      'Date of Response',    '10',
        to_char(svc.times_factServiceStart at time zone 'MSK', 'DD/MM/YYYY'),
      'Time of Response',     '5',
        to_char(svc.times_factServiceStart at time zone 'MSK', 'HH24:MI'),
      'Breakdown Location',  '100', c.caseAddress_address,
      'Breakdown Area',       '20', city.label,
      'Breakdown Service',   '100', coalesce(ctr.name, ''),
      'Service Tel Number 1', '20', coalesce((ctr_phone_disp.value->'value')::text, ''),
      'Service Tel Number 2', '20', coalesce((ctr_phone_close.value->'value')::text, ''),
      'Patrol Address 1',    '100', coalesce((ctr_addr_fact.value->'value')::text, ''),
      'Patrol Address 2',    '100', '',
      'Patrol Address V',    '100', '',
      'User Name',            '50', upper(c.contact_name),
      'User Tel Number',      '20', c.contact_phone1,
      'User Name P',          '50',
        case c.contact_contactOwner
          when true then upper(c.contact_name)
          else coalesce(upper(c.contact_ownerName), '')
        end,
      'Job Type',              '4',
        case svc.type
          when $(ServiceType.tech)$         then 'DEPA'
          when $(ServiceType.towage)$       then 'REMO'
          when $(ServiceType.consultation)$ then 'TELE'
        end,
      'Dealer Address G',    '200', coalesce(tow.towAddress_address, ''),
      'Dealer Address 1',    '200', '',
      'Dealer Address 2',    '200', '',
      'Dealer Address V',    '200', '',
      'Dealer Tel Number',    '20', coalesce((tow_dealer.phones->0->'value')::text, ''),
      'End Of File',           '4', 'True'
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


module Snaplet.DbLayer.Triggers.MailToPSA
  (sendMailToPSA
  ) where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad
import Control.Concurrent

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Text.Printf
import Data.Char
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)

import System.Log.Simple
import System.Log.Simple.Base (scoperLog)
import Data.Configurator (require)
import Network.Mail.Mime

import Snap.Snaplet (getSnapletUserConfig)
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl


sendMailToPSA :: MonadTrigger m b => ByteString -> m b ()
sendMailToPSA actionId = do
  svcId  <- get actionId "parentId"
  isValidSvc <- case B.split ':' svcId of
    "tech":_
      -> (`elem` ["charge", "starter", "condition"])
      <$> get svcId "techType"
    "towage":_ -> return True
    _ -> return False
  caseId  <- get actionId "caseId"
  program <- get caseId   "program"
  when (isValidSvc && program `elem` ["peugeot", "citroen"])
    $ sendMailActually actionId

sendMailActually :: MonadTrigger m b => ByteString -> m b ()
sendMailActually actionId = do
    liftDb $ log Trace (T.pack $ "sendMailToPSA(" ++ show actionId ++ ")")
    tz <- liftIO getCurrentTimeZone

    svcId   <- get actionId "parentId"
    caseId  <- get actionId "caseId"
    program <- get caseId   "program"

    let (programCode, carMake) = case program of
          "peugeot" -> ("RUMC01R", "PEU")
          "citroen" -> ("FRRM01R", "CIT")
          _ -> error $ "Invalid program: " ++ show program

    let caseNum = case B.readInt $ B.dropWhile (not.isDigit) caseId of
          Just (n,"") -> n
          _ -> error $ "Invalid case id: " ++ show caseId

    callDate <- fromMaybe (error "Invalid callDate") . toLocalTime tz
      <$> get caseId "callDate"

    carModel <- get caseId "car_model" -- FIXME: tr
    carEngine <- get caseId "car_engine" >>= \case
      "dis" -> return "D"
      _     -> return "E"

    carVIN      <- get caseId "car_vin"
    carPlateNum <- get caseId "car_plateNum"

    factServiceStart
      <- fromMaybe (error "Invalid factServiceStart") . toLocalTime tz
      <$> get svcId "times_factServiceStart"

    caseAddr     <- get caseId "caseAddress_address"
    city         <- get caseId "city"
    contactName  <- get caseId "contact_name"
    contactPhone <- get caseId "contact_phone1"
    contactOwner <- get caseId "contact_contactOwner" >>= \case
      "0" -> return contactName
      _   -> get caseId "contact_ownerName"

    let jobType = case B.split ':' svcId of
          "tech":_ -> "DEPA"
          "towage":_ -> "REMO"
          _ -> error $ "Invalid jobType: " ++ show svcId

    towAddr <- get svcId "towAddress_address"
    partnerId <- get svcId "contractor_partnerId"
    partnerName   <- get partnerId "name"
    partnerAddr   <- get partnerId "addrDeFacto"
    partnerPhone1 <- get partnerId "phone1"
    partnerPhone2 <- get partnerId "closeTicketPhone"

    let tmFormat = (T.pack .) . formatTime defaultTimeLocale

    let bodyText = TL.encodeUtf8 $ TL.fromChunks
          $ map ((`T.snoc` '\n') . T.intercalate " : ")
            [["BeginOfFile",          "True"]
            ,["Assistance Code",      programCode]
            ,["Country Сode",         "RU"]
            ,["Task Id",              T.pack $ printf "M%08d" caseNum]
            ,["Time of Incident",     tmFormat "%H:%M" callDate]
            ,["Make",                 carMake]
            ,["Model",                txt 13 carModel]
            ,["Energie",              carEngine]
            ,["Date out on road",     tmFormat "%d.%m.%Y" callDate]
            ,["VIN",                  txt 17 carVIN]
            ,["Rea No",               txt 10 carPlateNum]
            ,["Customer effet",       txt 150 ""] -- FIXME:
            ,["Component fault",      txt 150 ""] -- FIXME:
            ,["Date of Opening",      tmFormat "%d/%m/%Y" callDate]
            ,["Date of Response",     tmFormat "%d/%m/%Y" factServiceStart]
            ,["Time of Response",     tmFormat "%H:%M" factServiceStart]
            ,["Breakdown Location",   txt 100 caseAddr]
            ,["Breakdown Area",       txt 20 city]
            ,["Breakdown Service",    txt 100 partnerName]
            ,["Service Tel Number 1", txt 20 partnerPhone1]
            ,["Service Tel Number 2", txt 20 partnerPhone2]
            ,["Patrol Address 1",     txt 100 partnerAddr]
            ,["Patrol Address 2",     ""]
            ,["Patrol Address V",     ""]
            ,["User Name",            txt 50 contactName]
            ,["User Tel Number",      txt 20 contactPhone]
            ,["User Name P",          txt 50 contactOwner]
            ,["Job Type",             jobType]
            ,["Dealer Address G",     txt 200 towAddr]
            ,["Dealer Address 1",     ""]
            ,["Dealer Address 2",     ""]
            ,["Dealer Address V",     ""]
            ,["Dealer Tel Number",    txt 20 partnerPhone1]
            ,["End Of File",          "True"]
            ]

    let body = Part "text/plain; charset=utf-8"
          QuotedPrintableText Nothing [] bodyText

    cfg     <- liftDb getSnapletUserConfig
    cfgFrom <- liftIO $ require cfg "psa-smtp-from"
    cfgTo   <- liftIO $ require cfg "psa-smtp-recipients"

    l <- liftDb askLog
    -- FIXME: throws `error` if sendmail exits with error code
    void $ liftIO $ forkIO
      $ scoperLog l (T.concat ["sendMailToPSA(", T.decodeUtf8 actionId, ")"])
      $ renderSendMail $ (emptyMail $ Address Nothing cfgFrom)
        {mailTo = map (Address Nothing . T.strip) $ T.splitOn "," cfgTo
        ,mailHeaders
          = [("Subject"
          ,T.concat ["RAMC ", T.decodeUtf8 svcId, " / ", T.decodeUtf8 actionId])]
        ,mailParts = [[body]]
        }


toLocalTime :: TimeZone -> ByteString -> Maybe LocalTime
toLocalTime tz tm = case B.readInt tm of
  Just (s,"") -> Just $ utcToLocalTime tz
    $ posixSecondsToUTCTime $ fromIntegral s
  _ -> Nothing

txt :: Int -> ByteString -> Text
txt n = T.take n . T.decodeUtf8

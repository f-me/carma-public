{-# LANGUAGE QuasiQuotes #-}

-- Allowed fields
-- VIN                                | car_vin
-- Госномер                           | car_platenum
-- Марка                              | car_make
-- Модель                             | car_model
-- ФИО владельца карты                | cardnumber_cardowner
-- Номер карты                        | cardnumber_cardnumber
-- Пробег при регистрации в программе | cardnumber_milageto
-- ФИО менеджера                      | cardnumber_manager
-- Дата регистрации в программе       | cardnumber_validfrom
-- Программа действует до (Дата)      | cardnumber_validuntil
-- Программа действует до (Пробег)    | cardnumber_validuntilmilage
-- Межсервисный интервал              | cardnumber_serviceinterval
-- Дата начала гарантии               | car_warrantystart
-- Дата покупки                       | car_buydate
-- Дилер, продавший автомобиль        | car_seller
-- Заказчик
-- Код заказчика
-- Адрес заказчика


module AppHandlers.ContractGenerator where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B (toStrict)
import           Data.Aeson as Aeson

import           System.FilePath
import           System.Process.ByteString
import           System.Exit (ExitCode(..))

import           Snap
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           Application
import           AppHandlers.Util
import           Snaplet.FileUpload hiding (db)

q :: Query
q = [sql|
     SELECT
          carVin
        , carSeller
        , carMaker.label
        , carModel.label
        , carPlateNum
        , to_char(carBuydate at time zone 'UTC', 'DD/MM/YYYY')
        , cardNumber::text
        , to_char(contractValidFromDate at time zone 'UTC', 'DD/MM/YYYY')
        , to_char(contractValidUntilDate at time zone 'UTC', 'DD/MM/YYYY')
        , contractValidUntilMilage::text
        , milageTO::text
        , carCheckPeriod::text
        , cardOwner
        , manager
        , to_char(warrantyStart at time zone 'UTC', 'DD/MM/YYYY')
        , client, clientCode, clientAddress
     FROM contracttbl c
     INNER JOIN programtbl p ON c.program::int4 = p.id
     LEFT JOIN "CarMaker" carMaker ON carMaker.value = carMake
     LEFT JOIN "CarModel" carModel ON carModel.value = carModel
     WHERE c.id = ?
|]

fields = [ "car_vin"
         , "car_seller"
         , "car_make"
         , "car_model"
         , "car_platenum"
         , "car_buydate"
         , "cardnumber_cardnumber"
         , "cardnumber_validfrom"
         , "cardnumber_validuntil"
         , "cardnumber_validuntilmilage"
         , "cardnumber_milageto"
         , "cardnumber_serviceinterval"
         , "cardnumber_cardowner"
         , "cardnumber_manager"
         , "car_warrantystart"
         , "client"
         , "clientCode"
         , "clientAddress"
         ]

renderContractHandler :: AppHandler ()
renderContractHandler = do
  Just contractId <- fmap T.decodeUtf8 <$> getParam "ctr"
  Just programId  <- fmap T.decodeUtf8 <$> getParam "prog"
  aids <- withPG pg_search $ \c -> query c
                [sql|
                 SELECT a.id::text FROM attachmenttbl a, programtbl p
                 WHERE p.contracts=concat('attachment:', a.id) and p.id = ?
                 |]
                [programId]
  case aids of
    [Only aid] -> do
        tplPath <- with fileUpload $ getAttachmentPath aid
        contracts <- withPG pg_search $ \c -> query c q [contractId]
        case contracts of
          [row] -> do
              let [m] = mkMap fields [row]
                  tplFName = takeFileName tplPath
                  f = T.encodeUtf8
                      $ T.pack
                      $ concat ["attachment; filename=\"", tplFName, "\""]
              (e, out, err) <- liftIO
                        $ readProcessWithExitCode "fill-pdf.sh" [tplPath, "-"]
                        $ B.toStrict $ Aeson.encode m
              case e of
                ExitSuccess   -> do
                    modifyResponse $ setHeader "Content-Disposition" f
                    writeBS out
                ExitFailure _ -> writeBS err
          [] -> error "No contract selected (bad id or broken contract data)"
    [] -> error "No template attached to program or bad program id"

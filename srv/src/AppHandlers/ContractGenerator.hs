{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

-- Allowed fields
-- VIN                                | car_vin
-- Госномер                           | car_platenum
-- Марка                              | car_make
-- Модель                             | car_model
-- ФИО владельца карты                | cardnumber_cardowner
-- Номер карты                        | cardnumber_cardnumber
-- ФИО менеджера                      | cardnumber_manager
-- Дата регистрации в программе       | cardnumber_validfrom
-- Программа действует до (Дата)      | cardnumber_validuntil
-- Межсервисный интервал              | cardnumber_serviceinterval
-- Дата начала гарантии               | car_warrantystart
-- Дата покупки                       | car_buydate
-- Дата первой продажи                | car_firstsaledate
-- Дилер, продавший автомобиль        | car_seller
-- Заказчик
-- Код заказчика
-- Адрес заказчика


module AppHandlers.ContractGenerator where

import           BasicPrelude

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B (toStrict)
import           Data.Aeson as Aeson

import           System.FilePath
import           System.Process.ByteString
import           System.Exit (ExitCode(..))

import           Snap
import           Snap.Snaplet.PostgresqlSimple
import           Database.PostgreSQL.Simple.SqlQQ

import           Application
import           Snaplet.FileUpload
import           Util

q :: Query
q = [sql|
     SELECT
          c.vin
        , s.name
        , carMake.label
        , carModel.label
        , plateNum
        , to_char(c.buyDate, 'DD/MM/YYYY')
        , to_char(c.firstSaleDate, 'DD/MM/YYYY')
        , cardNumber::text
        , to_char(c.validSince, 'DD/MM/YYYY')
        , to_char(c.validUntil, 'DD/MM/YYYY')
        , (c.startMileage + c.checkPeriod)::text
        , c.startMileage::text
        , c.checkPeriod::text
        , c.name
        , c.managerName
        , to_char(c.validSince, 'DD/MM/YYYY')
        , client, clientCode, clientAddress
        , u.realName
        , spgm.label
        , pgm.label
     FROM "Contract" c
     INNER JOIN "SubProgram" spgm ON c.subprogram = spgm.id
     INNER JOIN "Program" pgm ON spgm.parent = pgm.id
     LEFT JOIN "CarMake"  carMake  ON carMake.id  = make
     LEFT JOIN "CarModel" carModel ON carModel.id = model
     LEFT JOIN usermetatbl u ON u.id = c.committer
     LEFT JOIN partnertbl s ON s.id = seller
     WHERE c.id = ?
|]

fields :: [Text]
fields = [ "car_vin"
         , "car_seller"
         , "car_make"
         , "car_model"
         , "car_platenum"
         , "car_buydate"
         , "car_firstsaledate"
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
         , "owner"
         , "subprogram"
         , "program"
         ]

renderContractHandler :: AppHandler ()
renderContractHandler = do
  Just contractId <- getParamT "contract"
  aids <- query
                [sql|
                 SELECT a.id
                 FROM attachmenttbl a, "SubProgram" s, "Contract" c
                 WHERE s.template=concat('Attachment:', a.id)
                 AND c.subprogram = s.id
                 AND c.id = ?
                 |]
                [contractId]
  case aids of
    (Only aid:_) -> do
        tplPath <- with fileUpload $ getAttachmentPath aid
        contracts <- query q [contractId]
        case contracts of
          (row:_) -> do
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
    [] -> error "No template attached to subprogram or bad contract id"

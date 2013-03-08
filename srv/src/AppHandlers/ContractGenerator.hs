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

import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B (toStrict)
import           Data.Aeson as Aeson

import           System.Process.ByteString
import           System.Exit (ExitCode(..))

import           Snap
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           Application
import           AppHandlers.Util

q :: Query
q = [sql|
     SELECT
          carVin
        , carSeller
        , carMake
        , carModel
        , carPlateNum
        , to_char(carBuydate, 'DD/MM/YYYY')
        , cardNumber::text
        , to_char(contractValidFromDate, 'DD/MM/YYYY')
        , to_char(contractValidUntilDate, 'DD/MM/YYYY')
        , contractValidUntilMilage::text
        , milageTO::text
        , carCheckPeriod::text
        , cardOwner
        , manager
        , to_char(warrantyStart, 'DD/MM/YYYY')
        , client, clientCode, clientAddress
    FROM contracttbl c, programtbl p
    WHERE c.id = ? AND p.id = ?
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
  [[tplFName]]    <- withPG pg_search
    $ \c -> query c
      (fromString "SELECT contracts FROM programtbl WHERE id = ?")
      [programId]
  [row] <- withPG pg_search $ \c -> query c q [contractId, programId]
  let [m] = mkMap fields [row]
      f = T.encodeUtf8 $ T.concat ["attachment; filename=\"", tplFName, "\""]
      p = T.concat [ "resources/static/fileupload/program/"
                   , programId
                   , "/contracts/"
                   , tplFName
                   ]
  (e, out, err) <- liftIO
    $ readProcessWithExitCode "fill-pdf.sh" [T.unpack p, "-"]
    $ B.toStrict $ Aeson.encode m
  case e of
    ExitSuccess   -> do
      modifyResponse $ setHeader "Content-Disposition" f
      writeBS out
    ExitFailure _ -> writeBS err



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

import           Control.Applicative ((<$>))

import           Data.Maybe
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as U
import           Data.ByteString.Lazy (toStrict)
import           Data.Aeson as Aeson

import           System.Process.ByteString
import           System.Exit (ExitCode(..))

import           Snap
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ

import           Application
import           AppHandlers.Util
import qualified Snaplet.DbLayer as DB

q = [sql|
     SELECT
          car_vin
        , car_seller
        , car_make
        , car_model
        , car_platenum
        , car_buydate::text
        , cardnumber_cardnumber
        , cardnumber_validfrom::text
        , cardnumber_validuntil::text
        , cardnumber_validuntilmilage
        , cardnumber_milageto
        , cardnumber_serviceinterval
        , cardnumber_cardowner
        , cardnumber_manager
        , car_warrantystart
    FROM casetbl where id = ?
|]

contractGeneratorHandler :: AppHandler ()
contractGeneratorHandler = do
  (caseId, _) <- fromJust <$> B.readInt <$> fromJust <$> getParam "caseId"
  tplId  <- fromJust <$> getParam "tplId"
  c      <- with db $ DB.read "contract" tplId
  rows <- withPG pg_search $ \c ->
    query c q [caseId]
  let m = mkMap fields rows
      t = fromJust $ Map.lookup "templates" c
      f = B.concat ["attachment; filename=\"", t, "\""]
      p = B.concat [ "resources/static/fileupload/contract/"
                   , tplId
                   , "/templates/"
                   , t
                   ]
  (e, out, err) <- liftIO $
                   readProcessWithExitCode "fill-pdf.sh" [U.toString p, "-"] $
                   toStrict $ Aeson.encode $ head m
  case e of
    ExitSuccess   -> do
      modifyResponse $ setHeader "Content-Disposition" f
      writeBS out
    ExitFailure _ -> writeBS err


fields = [
         -- , "calldate"
         -- , "calltaker"
         -- , "comment"
         -- , "diagnosis1"
         -- , "diagnosis2"
         -- , "diagnosis3"
         -- , "diagnosis4"
         -- , "contact_name"
         -- , "contact_phone1"
         -- , "contact_phone2"
         -- , "contact_phone3"
         -- , "contact_phone4"
         -- , "contact_email"
         -- , "contact_contactowner"
         -- , "contact_ownername"
         -- , "contact_ownerphone1"
         -- , "contact_ownerphone2"
         -- , "contact_ownerphone3"
         -- , "contact_ownerphone4"
         -- , "contact_owneremail"
         -- , "program"
           "car_vin"
         , "car_seller"
         , "car_make"
         , "car_model"
         , "car_platenum"
         -- , "car_makeyear"
         -- , "car_color"
         , "car_buydate"
         -- , "car_checkupdate"
         -- , "car_dealerto"
         -- , "car_mileage"
         -- , "car_checkupmileage"
         -- , "car_transmission"
         -- , "car_engine"
         -- , "car_liters"
         -- , "car_capacity"
         -- , "car_dims"
         -- , "car_weight"
         -- , "car_checkperiod"
         -- , "car_class"
         -- , "car_makecode"
         -- , "car_modelcode"
         -- , "car_faultcode"
         , "cardnumber_cardnumber"
         , "cardnumber_validfrom"
         , "cardnumber_validuntil"
         , "cardnumber_validuntilmilage"
         , "cardnumber_milageto"
         , "cardnumber_serviceinterval"
         , "cardnumber_cardowner"
         , "cardnumber_manager"
         -- , "vinchecked"
         -- , "caseaddress_address"
         -- , "caseaddress_comment"
         -- , "caseaddress_coords"
         -- , "caseaddress_map"
         -- , "city"
         -- , "temperature"
         -- , "dealercause"
         -- , "casestatus"
         -- , "claim"
         -- , "betacomment"
         -- , "services"
         -- , "actions"
         -- , "files"
         -- , "comments"
         -- , "repair"
         -- , "tst"
         -- , "vwcreatedate"
         -- , "car_servicestart"
         -- , "car_serviceend"
         , "car_warrantystart"
         -- , "car_warrantyend"
         -- , "accord"
         -- , "psaexported"
         ]

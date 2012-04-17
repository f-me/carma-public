{-# LANGUAGE OverloadedStrings #-}

module Vin.Import where

import           Control.Applicative

import           Data.ByteString (ByteString)
import qualified Database.Redis as R

import           Vin.Utils
import           Vin.FieldParsers


loadFile :: FilePath -> FilePath -> ByteString -> ByteString -> IO (Either FilePath String)
loadFile fInput fError program contentType = do
    case program of
        "vwMotor"  -> loadVinFile vwMotor
        "vwTruck"  -> loadVinFile vwTruck
        "vwRuslan" -> loadVinFile vwTruck
        _          -> return $ Right "unknown program"
  where
    loadVinFile = f redisSetVin fInput fError
    f = case contentType of
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" ->
                loadXlsxFile
            "text/csv" -> loadCsvFile


vwMotor = map mkRecord
  [("program",      fixed "vwMotor")
  ,("make",         fixed "VW")
  ,("model",        carModel ["Модель"])
  ,("modelFull",    str  ["Модель"])
  ,("vin",          notEmpty $ strU ["VIN"])
  ,("buyDate",      date ["Дата передачи АМ Клиенту"])
  ,("color",        str  ["Цвет авт"])
  ,("modelYear",    str  ["Модельный год"])
  ,("dealerCode",   str  ["Код дилера получателя"])
  ,("dealerName",   str  ["Дилер получатель"])
  ,("contractNo",   str  ["No Дог продажи Клиенту"])
  ,("contractDate", date ["Дата договора продажи"])
--  ,("Отч неделя", -- skip
  ,("ownerCompany", str  ["Компания покупатель"])
  ,("ownerContact", str  ["Контактное лицо покупателя"])
  ,("ownerName",    str  ["Фактический получатель ам"])
--  ,("Поле30", -- skip
  ]


vwTruck = map mkRecord
  [("program",      fixed "vwTruck")
  ,("make",         fixed "VW")
  ,("model",        carModel ["модель"])
  ,("modelFull",    str  ["модель"])
  ,("vin",          notEmpty $ strU ["VIN"])
  ,("buyDate",      date ["Дата продажи"])
  ,("plateNumber",  strU ["госномер"])
  ,("validUntil",   date ["Дата окончания карты"])
  ,("dealerName",   str  ["Продавец"])
  ,("cardNumber",   str  ["№ карты"])
  ,("modelYear",    str  ["модельный год"])
  ,("ownerName",    str  ["фамилия", "имя", "отчество"])
  ,("ownerAddress", str  ["индекс"
                         ,"город"
                         ,"адрес частного лица или организации"])
  ,("ownerPhone",   str  ["тел1","тел2"])
  ,("ownerCompany", str  ["название организации"])
  ]


vwRuslan = map mkRecord
  [("program",         fixed "vwRuslan")
  ,("make",            fixed "VW")
  ,("model",           carModel ["Модель Автомобиля VW"])
  ,("vin",             notEmpty $ strU ["VIN номер Автомобиля VW"])
  ,("cardNumber",      str  ["№"])
  ,("manager",         str  ["ФИО ответственного лица, внесшего данные в XLS файл"])
  ,("serviceInterval", str  ["Межсервисный интервал"])
  ,("validFrom",       date ["Дата прохождения ТО (Дата регистрации в программе)"])
  ,("mileageTO",       str  ["Величина пробега на момент регистрации в Программе"])
  ,("validUntil",      date ["Программа действует до (Дата)"])
  -- ,("Программа действует до (Пробега)"
  ]


opel = map mkRecord
  [("program",      fixed "opel")
  ,("make",         fixed "OPEL")
  ,("model",        carModel ["Model"])
  ,("vin",          notEmpty $ strU ["VIN", "Previous VIN (SKD)"])
  ,("buyDate",      date  ["Retail Date"])
--  ,("Brand",
  ,("dealerCode",   strU  ["Retail Dealer"])
  ]


hummer = map mkRecord
  [("program",      fixed "hummer")
  ,("make",         fixed "HUMMER")
  ,("model",        str  ["Model"]) -- FIXME: carModel
  ,("dealerCode",   strU ["Retail Dealer"])
  ,("buyDate",      date ["Retail Date"])
  ,("vin",          notEmpty $ strU ["VIN RUS", "VIN"])
  ]


ford = map mkRecord
  [("program",      fixed "ford")
  ,("make",         fixed "FORD")
  ,("model",        carModel ["MODEL"])
  ,("arcModelCode", str   ["ARC_MODEL_CODE"])
  ,("fddsId",       str   ["FDDS_ID"])
  ,("vin",          strU  ["VIN_NUMBER"])
  ,("dealerCode",   str   ["DEALER_CODE"])
  ,("dealerNameEn", str   ["DEALER_NAME"])
  ,("validFrom",    date  ["VALID_FROM"])
  ,("validUntil",   date  ["VALID_TO"])
  ,("plateNumber",  str   ["LICENCE_PLATE_NO"])
  ,("programRegistartionDate", str ["CREATION_DATE"])
  ,("mileageTO",    str   ["MILEAGE"])
  ]


fordPlus = map mkRecord
  [("program",      fixed "fordPlus")
  ,("make",         fixed "FORD")
  ,("model",        carModel ["Модель"])
  ,("vin",          str  ["VIN"])
  ,("fordUR",       str  ["UR"])
  ,("buyDate",      date ["Дата первой продажи"])
  ,("lastTODate",   date ["Дата прохождения ТО"])
  ,("mileageTO",    str  ["Пробег на момент прохождения ТО"])
  ]

{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception (finally)
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           Database.Redis as Redis

import Utils
import FieldParsers


main = do
  dir:_ <- getArgs
  rConn <- Redis.connect Redis.defaultConnectInfo
  loadFiles dir rConn `finally` runRedis rConn quit


loadFiles dir rConn = do
  let  loadVinFile = loadXFile $ redisSetVin rConn
  let  loadCSVFile key = loadXFile $ redisSet rConn key
  loadVinFile (dir </> "vin/VIN VW легковые.csv") vwMotor_map
  loadVinFile (dir </> "vin/VIN VW коммерческие.csv") vwTruck_map
  loadVinFile (dir </> "vin/VIN OPEL.csv") opel_map
  loadCSVFile "partner" (dir </> "Партнеры.csv") partner_map
  loadCSVFile "dealer"  (dir </> "Дилеры.csv") dealer_map


vwMotor_map =
  [("make",         fixed "VW")
  ,("model",        carModel ["Модель"])
  ,("modelFull",    str ["Модель"])
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


vwTruck_map = 
  [("make",         fixed "VW")
  ,("model",        carModel ["модель"])
  ,("modelFull",    str  ["модель"])
  ,("vin",          notEmpty $ strU ["VIN"])
  ,("buyDate",      date ["Дата продажи"])
  ,("plateNum",     strU ["госномер"])
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

opel_map =
  [("make",         fixed "OPEL")
  ,("model",        carModel ["Model"])
  ,("vin",          oneOf $ map (notEmpty . strU) 
                        [["VIN"], ["Previous VIN (SKD)"]])
  ,("buyDate",      date  ["Retail Date"])
--  ,("Brand",
  ,("dealerCode",   strU  ["Retail Dealer"])
  ]

partner_map =
  [("code",         str ["Code"])
  ,("cityRu",       str ["Город"])
  ,("cityEn",       str ["City"])
  ,("priority1",    str ["Приоритет за городом"])
  ,("priority2",    str ["Приоритет город"])
  ,("serviceRu",    str ["Услуга"])
  ,("serviceEn",    str ["Service"])
  ,("phones",       str ["Телефон Диспетчерской 1"
                        ,"Телефон Диспетчерской 2"
                        ,"Телефон Диспетчерской 3"
                        ,"Телефон Диспетчерской 4"])
  ,("companyName",  str ["Название компании"])
  ,("addrDeJure",   str ["Юридический адрес"])
  ,("addrDeFacto",  str ["Фактический адрес индекс","Фактический адрес улица"])
  ,("contactPerson",str ["Ответственное лицо"])
  ,("contactPhone", str ["Телефон Ответственного за Сотрудничество"])
  ,("eMail",        str ["Электронная почта"])
  ,("fax",          str ["Факс"])
  ,("price",        str ["Тариф"])
  ,("comment",      str ["Комментарий"])
  ,("status",       str ["Статус"])
  ]

dealer_map = 
  [("city",         str ["Город", "Округ"])
  ,("type",         str ["Дилер"])
  ,("name",         str ["Название дилера"])
  ,("salesAddr",    str ["Адрес отдела продаж"])
  ,("salesPhone",   str ["Телефон отдела продаж"])
  ,("salesHours",   str ["Время работы"])
  ,("techAddr",     str ["Адрес сервисного отдела"])
  ,("techPhone",    str ["Телефон сервисного отдела"])
  ,("techHours",    str ["Время работы сервисного отдела"])
  ,("program",      str ["Код_Программы_Клиенты"])
  ,("servicePhone", str ["тел для заказа услуги"])
  ,("carModels",    str ["Автомобили"])
  ,("service",      str ["предоставляемая услуга"])
  ,("workingHours", str ["время работы по предоставлению услуги"])
  ]

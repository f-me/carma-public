
{-# LANGUAGE OverloadedStrings #-}

module Partners where

import           System.FilePath ((</>))

import Utils
import FieldParsers

loadFiles dir rConn = do
  let  loadCSVFile modelName = loadXFile $ redisSet rConn modelName
  loadCSVFile "partner" (dir </> "Партнеры.csv") partner
  loadCSVFile "dealer"  (dir </> "Дилеры.csv") dealer


partner =
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

dealer = 
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

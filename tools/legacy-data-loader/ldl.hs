{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception
import           System.Environment (getArgs)
import           System.FilePath ((</>))

import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M

import           Data.Time.Format (parseTime)
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime 
import           System.Locale (defaultTimeLocale)

import qualified Data.Enumerator as E (tryIO, Iteratee())
import           Data.CSV.Enumerator as CSV
import           Database.Redis as Redis


main = do
  dir:_ <- getArgs
  rConn <- Redis.connect Redis.defaultConnectInfo
  let  loadVinFile = loadXFile $ \m -> redisSetWithKey rConn (m M.! "vin") m
  let  loadCSVFile = loadXFile $ \m -> redisSet rConn m
  do   loadVinFile (dir </> "vin/VIN VW легковые.csv") vwMotor_map
    >> loadVinFile (dir </> "vin/VIN VW коммерческие.csv") vwTruck_map
    >> loadCSVFile (dir </> "Партнеры.csv") partner_map
    >> loadCSVFile (dir </> "Дилеры.csv") dealer_map
    `finally` runRedis rConn quit


loadXFile store fName keyMap
  = foldCSVFile fName defCSVSettings run ()
  >>= print -- Just to force evaluation
  where
    run :: () -> ParsedRow MapRow -> E.Iteratee B.ByteString IO ()
    run _ (ParsedRow (Just r))
      = E.tryIO (store $ remap keyMap r) >> return ()
    run _ _ = return ()

-- | convert MapRow according to transformations described in `keyMap`.
remap keyMap m = M.fromList $ foldl f [] keyMap
  where
    f res (key',f) = case f m of
      "" -> res
      xs -> (key',xs):res


redisSetWithKey c key val = putStr "." --print val
redisSet c val = putStr "." --print val


str' keys m
  = T.unwords 
  $ concatMap (T.words . T.decodeUtf8)
  $ map ((m M.!) . B.fromString) keys

str  = (T.encodeUtf8 .) . str'
strU = ((T.encodeUtf8 . T.toUpper) .) . str'
date = str


vwMotor_map =
  [("carModel",     strU ["Модель"])
  ,("color",        str  ["Цвет авт"])
  ,("modelYear",    str  ["Модельный год"])
  ,("vin",          strU ["VIN"])
  ,("dealerCode",   str  ["Код дилера получателя"])
  ,("dealerName",   str  ["Дилер получатель"])
  ,("contractNo",   str  ["No Дог продажи Клиенту"])
  ,("contractDate", date ["Дата договора продажи"])
  ,("sellDate",     date ["Дата передачи АМ Клиенту"])
--  ,("Отч неделя", -- skip
  ,("ownerCompany", str  ["Компания покупатель"])
  ,("ownerContact", str  ["Контактное лицо покупателя"])
  ,("ownerName",    str  ["Фактический получатель ам"])
--  ,("Поле30", -- skip
  ]


vwTruck_map = 
  [("sellDate",     date ["Дата продажи"])
  ,("validUntil",   date ["Дата окончания карты"])
  ,("dealerName",   str  ["Продавец"])
  ,("cardNumber",   str  ["№ карты"])
  ,("VIN",          strU ["vin"]) -- toUpper
  ,("modelYear",    str  ["модельный год"])
  ,("plateNumber",  strU ["госномер"]) -- toUpper
  ,("carModel",     str  ["модель"])
  ,("ownerName",    str  ["имя", "фамилия", "отчество"])
  ,("ownerAddress", str  ["индекс","город","адрес частного лица или организации"])
  ,("ownerPhone",   str  ["тел1","тел2"])
  ,("ownerCompany", str  ["название организации"])
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

{-
mkTimestamp :: T.Text -> T.Text -> UTCTime
mkTimestamp d t
  = fromJust $ localTimeToUTC (read "+0400")
  <$> (LocalTime
    <$> (maybe (pTm "%m/%d/%Y" "01/02/2003") Just (pTm "%m/%d/%Y" d))
    <*> (maybe (Just midday) Just (pTm "%k:%M" t)))
  where
    pTm f = parseTime defaultTimeLocale f . T.unpack
-}

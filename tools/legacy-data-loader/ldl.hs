{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Control.Applicative
import Control.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe

import Data.Time.Format (parseTime)
import Data.Time.Clock  (UTCTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime 
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)

import Data.Enumerator as E hiding (map)
import qualified Data.Enumerator.Binary as EB
import Data.CSV.Enumerator as CSV
import Data.Aeson
import Data.Aeson.Types
import Database.Redis

-- Time to upgrade to GHC 7.4?
-- 7.0.* still does not work properly with non-ascii filenames
import qualified Codec.Binary.UTF8.String as UTF8


fixName = UTF8.encodeString 



main = do
  dir:_ <- getArgs
  let f = fixName . (dir++)
  rConn <- connect defaultConnectInfo
  runRedis rConn flushdb
  fromCSV (f "/Партнеры.csv") mkPartner rConn
    >> fromCSV (f "/Дилеры.csv") mkDealer rConn
    >> fromCSV (f "/Журнал_звонков.csv") mkCase rConn
    `finally` runRedis rConn quit



fromCSV fname f rConn = E.run_
  $  EB.enumFile fname
  $$ iterCSV defCSVSettings (funToIterIO toRedis) rConn
  where
    toRedis c CSV.EOF = return c
    toRedis c (ParsedRow (Just r)) = runRedis c (f r) >> return c


pStr (Object o) = fromJust . parseMaybe (o .:) :: T.Text -> T.Text
pKey  o = T.toUpper . T.strip . pStr o
pStrs o = T.strip . T.intercalate " " . map (pStr o)
pList o = map (pStr o)

mkObj :: [T.Text] -> [B.ByteString] -> Value
mkObj = (object.) . zipWith (.=)

toBS = T.encodeUtf8 . T.concat :: [T.Text] -> B.ByteString

mkPartner row = do
  set key $ B.concat $ L.toChunks $ encode obj
  lpush (toBS ["partner:companyName:", pStr obj "companyName"])     [key]
  lpush (toBS ["partner:contactPerson:", pStr obj "contactPerson"]) [key]
  lpush (toBS ["partner:contactPhone:", pStr obj "contactPhone"])   [key]
  where
    o = mkObj [T.pack $ show i | i <- [0..]] row
    (p,ps,pL) = (pStr o, pStrs o, pList o)
    key = toBS ["partner:", p "0"]
    obj =  object
      ["cityRu"        .= p "1" -- 1 Город
      ,"cityEn"        .= p "4" -- 4 City
      ,"priority1"     .= p "2" -- 2 Приоритет за городом
      ,"priority2"     .= p "3" -- 3 Приоритет город
      ,"serviceRu"     .= p "5" -- 5 Услуга
      ,"serviceEn"     .= p "6" -- 6 Service
      ,"phones"        .= pL ["9","10","11","13"]  -- Телефон Диспетчерской 1-4
      ,"companyName"   .= p "7" -- 7 Название компании
      ,"addrDeJure"    .= p "8" -- 8 Юридический адрес
      ,"addrDeFacto"   .= ps ["12","18"] -- 12 Фактический адрес индекс -- 18 Фактический адрес улица
      ,"contactPerson" .= p "15" -- 15 Ответственное лицо
      ,"contactPhone"  .= p "14" -- 14 Телефон Ответственного за Сотрудничество
      ,"eMail"         .= p "16" -- 16 Электронная почта
      ,"fax"           .= p "17" -- 17 Факс
      ,"price"         .= p "19" -- 19 Тариф
      ,"comment"       .= p "20" -- 20 Комментарий
      ,"status"        .= p "21" -- 21 Статус
      ]
--

mkDealer row = do
  Right id <- incr "dealer:id"
  let key = B.concat ["dealer:",B.pack $ show id]
  set key $ B.concat $ L.toChunks $ encode obj
  lpush (toBS ["dealer:name:", pKey obj "name"])             [key]
  lpush (toBS ["dealer:program:", pKey obj "program"])       [key]
  lpush (toBS ["dealer:city:", pKey obj "city"])             [key]
  where  
    o = mkObj [T.pack $ show i | i <- [0..]] row
    (p,ps) = (pStr o, pStrs o)
    obj = object
      ["city"         .= ps ["0","2"] -- 0 Город -- 2 Округ
      ,"type"         .= p "1" -- 1 Дилер
      ,"name"         .= p "3" -- 3 Название дилера
      ,"salesAddr"    .= p "4" -- 4 Адрес отдела продаж
      ,"salesPhone"   .= p "5" -- 5 Телефон отдела продаж
      ,"salesHours"   .= p "6" -- 6 Время работы
      ,"techAddr"     .= p "7" -- 7 Адрес сервисного отдела
      ,"techPhone"    .= p "8" -- 8 Телефон сервисного отдела
      ,"techHours"    .= p "9" -- 9 Время работы сервисного отдела
      ,"program"      .= p "10"-- 10 Код_Программы_Клиенты
      ,"servicePhone" .= p "11"-- 11 тел для заказа услуги
      ,"carModels"    .= p "12"-- 12 Автомобили
      ,"service"      .= p "13"-- 13 предоставляемая услуга
      ,"workingHours" .= p "14"-- 14 время работы по предоставлению услуги
      ]


mkCase row = do
  Right id <- incr "case:id"
  let key = B.concat ["case:",B.pack $ show id]
  set key $ B.concat $ L.toChunks $ encode obj
  mapM_ (\w -> lpush (B.concat ["case:txt:",w]) [key]) row 
  where
    o = mkObj [T.pack $ show i | i <- [0..]] row
    (p,ps) = (pStr o, pStrs o)
    obj = object
      ["timestamp" .= mkTimestamp (p"0") (p"1") -- 0  Дата звонка -- 1  Время звонка
      ,"callTaker" .= p "2"  -- Сотрудник (Обязательное поле)
      ,"program"   .= p "3"  -- Клиент (Обязательное поле) FIXME: на самом деле, это программа
      ,"service"   .= p "4"  -- Услуга  (Обязательное поле)
      ,"client"    .= ps ["5","6"] -- 5  Фамилия звонящего -- 6  Имя отчество звонящего
      ,"carOwner"  .= ps ["7","8"] -- 7  Фамилия владельца -- 8  Имя отчество владельца
      ,"phones"    .= ps ["9","10"] -- 9  Мобильный телефон -- 10 Дополнительный телефон
      ,"carModel"  .= ps ["11","12","13"]  -- 11 Марка автомобиля -- 12 Модель автомобиля -- 13 Другая марка / модель авто
      ,"plateNum"  .= p "14" -- Регистрационный номер автомобиля
      ,"color"     .= p "15" -- Цвет
      ,"vin"       .= p "16" -- VIN автомобиля
      ,"vinCheck"  .= p "17" -- 17 VIN Проверен
        -- 18 Дата покупки автомобиля
        -- 19 Дата  прохождения ТО FORD
        -- 20 Пробег автомобиля (км)
        -- 21 Пробег автомобиля на момент прохождения ТО (только для FORD)
        -- 22 Дилер продавший авто / прохождение ТО FORD
        -- 23 Описание неисправности со слов клиента
        -- 24 Система в которой произошла неисправность
        -- 25 Неисправная деталь
        -- 26 Адрес места поломки
        -- 27 Введите дату в формате + или - число -- FIXME: ерунда какая-то
        -- 28 Город дилера куда эвакуируют автомобиль
        -- 29 Название дилера куда эвакуируют автомобиль
        -- 30 Адрес куда эвакуируют автомобиль
        -- 31 Номер происшествия в Arc Time
        -- 32 Название партнёра
        -- 33 Реальное время прибытия на место поломки
        -- 34 Реальное время окончания услуги
        -- 35 Стоимость услуги у партнёра
        -- 36 Расшифровка стоимости
        -- 37 Перепробег (информация от партнера)
        -- 38 Номер заказ-наряда Saga (только для VW)
        -- 39 Статус случая от дилера (Только для VW)
        -- 40 Дата окончания ремонта автомобиля у дилера (только VW)
        -- 41 Описание причины неисправности со слов дилера (только VW)
      ,"comment"    .= p "42" -- 42 Комментарий
      ,"status"     .= p "43"  -- 43 Статус звонка (Обязательное поле)
      ]


mkTimestamp :: T.Text -> T.Text -> UTCTime
mkTimestamp d t
  = fromJust $ localTimeToUTC (read "+0400")
  <$> (LocalTime
    <$> (maybe (pTm "%m/%d/%Y" "01/02/2003") Just (pTm "%m/%d/%Y" d))
    <*> (maybe (Just midday) Just (pTm "%k:%M" t)))
  where
    pTm f = parseTime defaultTimeLocale f . T.unpack

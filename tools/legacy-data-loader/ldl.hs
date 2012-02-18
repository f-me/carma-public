
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Prelude hiding (catch)
import Control.Applicative
import Control.Monad (join, when)
import Control.Exception
import Data.Typeable

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Data.Maybe

import Data.Time.Format (parseTime)
import Data.Time.Clock  (UTCTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime 
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)

import Data.Enumerator as E hiding (mapM)
import qualified Data.Enumerator.Binary as E
import Data.CSV.Enumerator as CSV
import Data.Aeson
import Data.Aeson.Types


-- Time to upgrade to GHC 7.4?
-- 7.0.* still does not work properly with non-ascii filenames
import qualified Codec.Binary.UTF8.String as UTF8


fixName = UTF8.encodeString 



main = do
  dir:_ <- getArgs
  let f = fixName . (dir++)
  fromCSV (f "/Журнал_звонков.csv") $ toRedis case_info


fromCSV fname f = do
  E.run_ $ E.enumFile fname $$ iterCSV defCSVSettings act True
  where
    act x CSV.EOF = yield x E.EOF
    act skip  (ParsedRow (Just row))
        = when (not skip) (tryIO $ f row)
        >> yield False (Chunks [])

toRedis f row = do
  jsn <- return (Just $ f row) 
    `catch`
      \(Ex msg) -> print msg >> mapM_ B.putStrLn row >> return Nothing
  L.putStrLn $ encode jsn

mkObj :: [T.Text] -> [B.ByteString] -> Value
mkObj = (object.) . zipWith (.=)

case_info row = object
  ["timestamp" .= fromMaybe (throw $ Ex "timestamp") (mkTimestamp o)
  ]
  where
    o = mkObj [T.pack $ show i | i <- [0..]] row
    key = "call"

data Ex = Ex String deriving (Show, Typeable)
instance Exception Ex

mkTimestamp :: Value -> Maybe UTCTime
mkTimestamp = join . parseMaybe getCallTime
getCallTime (Object o) = do
  let pTm = parseTime defaultTimeLocale 
  d <- pTm "%m/%d/%Y" <$> o .: "0"
  t <- pTm "%k:%M"    <$> o .: "1"
  let t' = maybe (Just midday) Just t
  return $ localTimeToUTC (read "+0400")
    <$> (LocalTime <$> d <*> t')

{- case info
0  Дата звонка
1  Время звонка
2  Сотрудник РАМК (Обязательное поле)
3  Клиент (Обязательное поле)
4  Услуга  (Обязательное поле)
5  Фамилия звонящего
6  Имя отчество звонящего
7  Фамилия владельца
8  Имя отчество владельца
9  Мобильный телефон
10 Дополнительный телефон
11 Марка автомобиля
12 Модель автомобиля
13 Другая марка / модель авто
14 Регистрационный номер автомобиля
15 Цвет
16 VIN автомобиля
17 VIN Проверен
18 Дата покупки автомобиля
19 Дата  прохождения ТО FORD
20 Пробег автомобиля (км)
21 Пробег автомобиля на момент прохождения ТО (только для FORD)
22 Дилер продавший авто / прохождение ТО FORD
23 Описание неисправности со слов клиента
24 Система в которой произошла неисправность
25 Неисправная деталь
26 Адрес места поломки
27 Введите дату в формате + или - число
28 Город дилера куда эвакуируют автомобиль
29 Название дилера куда эвакуируют автомобиль
30 Адрес куда эвакуируют автомобиль
31 Номер происшествия в Arc Time
32 Название партнёра
33 Реальное время прибытия на место поломки
34 Реальное время окончания услуги
35 Стоимость услуги у партнёра
36 Расшифровка стоимости
37 Перепробег (информация от партнера)
38 Номер заказ-наряда Saga (только для VW)
39 Статус случая от дилера (Только для VW)
40 Дата окончания ремонта автомобиля у дилера (только VW)
41 Описание причины неисправности со слов дилера (только VW)
42 Комментарий
43 Статус звонка (Обязательное поле)
-}
